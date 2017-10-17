/* C Mode */

/* strings.c
   R4RS string primitives for FDScript
   Originally implemented by Ken Haase  and the Machine Understanding Group
     at the MIT Media Laboratory.

   Copyright (C) 1994-2001 Massachusetts Institute of Technology
   Copyright (C) 2001-2005 beingmeta, inc. (A Delaware Corporation)

   This program comes with absolutely NO WARRANTY, including implied
   warranties of merchantability or fitness for any particular purpose.

    Use, modification, and redistribution of this program is permitted
    under the terms of either (at the developer's discretion) the GNU
    General Public License (GPL) Version 2, the GNU Lesser General Public
    License.

    This program is based on the FramerD library released in Fall 2001 by
    MIT under both the GPL and the LGPL licenses, both of which accompany
    this distribution.  Subsequent modifications by beingmeta, inc. are
    also released under both the GPL and LGPL licenses (at the developer's
    discretion).
*/

static char vcid[] = "$Id: strings.c,v 1.19 2005/01/14 16:48:49 haase Exp $";

#include "fdscript.h"
#include <ctype.h>

static void check_string_arg(lisp str)
{
  if (!(FD_STRINGP(str))) fd_type_error(_("not a string"),str);
}

static WIN32_NORETURN void bounds_error(lisp str,int i) NORETURN;
static void bounds_error(lisp str,int i)
{
  char buf[32]; sprintf(buf,"%d",i);
  fd_raise_lisp_exception(fd_Out_Of_Bounds,buf,str);
}

static lisp string_upcase(lisp string);

FASTOP int uni_sgetc(fd_u8char **ss)
{
  if (**ss == '\0') return -1;
  else if (**ss < 0x80) return *((*ss)++);
  else return fd_sgetc(ss);
}

/* string? exists in the basic distribution */


/* make-string k
   make-string k char

Make-string returns a newly allocated string of length k. If char is
given, then all elements of the string are initialized to char,
otherwise the contents of the string are unspecified.
*/  

static lisp make_string_lexpr (lisp args)
{
  fd_lisp size, init;
  int sz; unichar_t init_char;

  fd_get_args("MAKE-STRING",args,&size,FD_VOID,&init,FD_FALSE,NULL);

  FD_CHECK_TYPE(size,FD_FIXNUMP,"not an integer");
  sz=FIXLISP(CAR(args));
  
  if (FD_FALSEP(init)) init_char = ' ';
  else if (FD_FIXNUMP(init)) init_char=FD_FIXLISP(init);
  else if (FD_CHARACTERP(init)) init_char=FD_CHAR_CODE(init);
  else fd_type_error(_("not coercable to a character"),init);

  if (sz == 0) return fd_make_string("");
  else {
    int i=0; struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,sz+1);
    while (i < sz) {fd_sputc(&ss,init_char); i++;}
    return fd_init_string(ss.ptr,ss.size);}
}

/*
 * string char ...
 *
 * Returns a newly allocated string composed of the arguments. 
 */

static lisp string (lisp args)
{
  struct FD_STRING_STREAM ss;
  FD_INITIALIZE_STRING_STREAM(&ss,64);
  {
    DOLIST(arg,args)
      if (!(CHARACTERP(arg)))
	fd_type_error(_("not a character"),arg);
      else fd_sputc(&ss,CHAR_CODE(arg));}

  return fd_init_string(ss.ptr,ss.size);
}


FDSCRIPT_EXPORT
/* fd_string_length
     Arguments: a lisp pointer to a string
     Returns: the length (in unicode characters) of a string
*/
int fd_string_length(lisp str)
{

  if (ASCII_STRINGP(str)) return STRING_LENGTH(str);
  else if (UNICODE_STRINGP(str))
    return fd_utf8_strlen(STRING_DATA(str),STRING_LENGTH(str));
  else fd_type_error(_("not a string"),str);
}

static lisp lisp_string_length_cproc(lisp str)
{
  int len=fd_string_length(str);
  return LISPFIX(len);
}

FDSCRIPT_EXPORT
/* fd_string_ref
     Arguments: a lisp pointer to a string and an int
     Returns: an unsigned int encoding a unicode character
*/
unichar_t fd_string_ref(lisp string,int index)
{
  if (ASCII_STRINGP(string))
    if (index < 0) bounds_error(string,index);
    else if (index < (STRING_LENGTH(string)))
      return STRING_DATA(string)[index];
    else bounds_error(string,index);
  else if (UNICODE_STRINGP(string))
    if (index < 0) bounds_error(string,index);
    else {
      fd_u8char *scan=STRING_DATA(string); 
      int i=0; int c=fd_sgetc(&scan);
      while (c >= 0)
	if (i == index) return c;
	else {c=fd_sgetc(&scan); i++;}
      bounds_error(string,index);}
  else fd_type_error(_("not a string"),string);
}

static lisp lisp_string_ref_cproc(lisp string, lisp k)
{
  int index; unsigned int c;
  if (!(FIXNUMP(k)))
    fd_type_error(_("not a fixnum offset"),k);
  else index=FIXLISP(k);
  c=fd_string_ref(string,index);
  return CODE_CHAR(c);
}
  
FDSCRIPT_EXPORT
/* fd_string_set
     Arguments: a lisp pointer to a string, an int, and a unicode character
     Returns: void
  Modifies the string so that a particular character is changed.
*/
void fd_string_set(fd_lisp str,int index,unichar_t ch)
{
  if (FD_PRIM_TYPEP(str,string_type))
    if (index < 0) bounds_error(str,index);
    else {
      struct FD_STRING_STREAM ss;
      int schar; unsigned int i=0, not_ascii=(ch>=0x80); 
      fd_u8char *scan=STRING_DATA(str), *limit=scan+FD_STRING_LENGTH(str);
      FD_INITIALIZE_STRING_STREAM(&ss,256); 
      while (scan < limit) {
	schar=fd_sgetc(&scan);
	if (i == index) schar=ch;
	fd_sputc(&ss,schar); i++;
	if (schar >= 0x80) not_ascii=1;}
      if (index < i) {
	fd_u8char *old=STRING_DATA(str);
	if (((int)ss.size) != STRING_LENGTH(str))
	  fd_malloc_adjust(ss.size-STRING_LENGTH(str));
	PTR_DATA(str,string)->data=ss.ptr;
	PTR_DATA(str,string)->utf8=not_ascii;
	PTR_DATA(str,string)->length=ss.size;
	free(old);}
      else {free(ss.ptr); bounds_error(str,index);}}
  else if (FD_STRINGP(str))
    fd_type_error(_("not a modifiable string"),str);
  else fd_type_error(_("not a string"),str);
}

/*
 * string-set! string k char
 *
 * k must be a valid index of string%, and char must be a character. 
 * String-set! stores char in element k of string and returns an
 * unspecified value.
 *
 *(define (f) (make-string 3 #\*))
 *(define (g) "***")
 *(string-set! (f) 0 #\?)     =>  unspecified
 *(string-set! (g) 0 #\?)     =>  error
 *(string-set! (symbol->string 'immutable)
 *            0
 *            #\?)           =>  error
 */

static lisp lisp_string_set_cproc(lisp str, lisp k, lisp c)
{
  unichar_t ch; unsigned int index;
  
  if (FIXNUMP(k)) index=FIXLISP(k);
  else fd_type_error(_("not a fixnum offset"),k);
  
  if (CHARACTERP(c)) ch=CHAR_CODE(c);
  else fd_type_error(_("not a character"),c);
  fd_string_set(str,index,ch);

  return FD_VOID;
}

static int string_compare(lisp x1,lisp x2)
{
  fd_lisp_string  s1=PTR_DATA(x1,string), s2=PTR_DATA(x2,string);
  int answer;
  if ((s1->utf8) || (s2->utf8)) {
    struct FD_TEXT_ENCODING *e=fd_get_default_encoding();
    WITH_HANDLING {
      char *ts1=fd_localize_utf8(s1->data,e);
      char *ts2=fd_localize_utf8(s2->data,e);
      answer=strcoll(ts1,ts2);
      fd_xfree(ts1); fd_xfree(ts2);}
    ON_EXCEPTION {
      answer=strcmp(s1->data,s2->data);
      fd_clear_exception();}
    END_HANDLING;
    return answer;}
  else return strcoll(s1->data,s2->data);
}

/*
essential procedure: string=? string1 string2

essential procedure: string-ci=? string1 string2

Returns #t if the two strings are the same length and contain the same
characters in the same positions, otherwise returns #f. String-ci=?
treats upper and lower case letters as though they were the same
character, but string=? treats upper and lower case as distinct
characters.
*/

static lisp string_equal_p(lisp key0, lisp key1)
{
  if (((STRINGP(key0)) && (STRINGP(key1))))
    if ((STRING_LENGTH(key0) != STRING_LENGTH(key1))) return FD_FALSE;
    else if (string_compare(key0,key1)) return FD_FALSE;
    else return FD_TRUE;
  else return FD_FALSE;
}

#define STRING_CI_COMPARE(s1,s2,cs_fn) \
 lisp u1=string_upcase(s1), u2=string_upcase(s2); \
 lisp answer=cs_fn(u1,u2); \
 decref(u1); decref(u2); \
 return answer;

static lisp string_ci_equal_p(lisp s1, lisp s2)
{
  STRING_CI_COMPARE(s1,s2,string_equal_p);
}

/*

essential procedure: string<? string1 string2

essential procedure: string>? string1 string2

essential procedure: string<=? string1 string2

essential procedure: string>=? string1 string2

essential procedure: string-ci<? string1 string2

essential procedure: string-ci>? string1 string2

essential procedure: string-ci<=? string1 string2

essential procedure: string-ci>=? string1 string2

These procedures are the lexicographic extensions to strings of the
corresponding orderings on characters. For example, string<? is the
lexicographic ordering on strings induced by the ordering char<? on
characters. If two strings differ in length but are the same up to the
length of the shorter string, the shorter string is considered to be
lexicographically less than the longer string.

Implementations may generalize these and the string=? and string-ci=?
procedures to take more than two arguments, as with the corresponding
numerical predicates.

*/

static lisp string_lt_p(lisp s1, lisp s2)
{
  if (! ((STRINGP(s1)) && STRINGP(s2)))
    fd_raise_exception("STRING<?: Both arguments must be strings");
  else if ((string_compare(s1,s2)) < 0) return FD_TRUE;
  else return FD_FALSE;
}


static lisp string_gt_p(lisp s1, lisp s2)
{
  if (! ((STRINGP(s1)) && STRINGP(s2)))
    fd_raise_exception("STRING>?: Both arguments must be strings");
  else if ((string_compare(s1,s2)) > 0) return FD_TRUE;
  else return FD_FALSE;
}

static lisp string_lt_equal_p(lisp s1, lisp s2)
{
  if (! ((STRINGP(s1)) && STRINGP(s2)))
    fd_raise_exception("STRING<=?: Both arguments must be strings");
  else if ((string_compare(s1,s2)) <= 0) return FD_TRUE;
  else return FD_FALSE;
}


static lisp string_gt_equal_p(lisp s1, lisp s2)
{
  if (! ((STRINGP(s1)) && STRINGP(s2)))
    fd_raise_exception("STRING>=?: Both arguments must be strings");
  else if ((string_compare(s1,s2)) >= 0) return FD_TRUE;
  else return FD_FALSE;
}

static lisp string_ci_lt_p(lisp s1, lisp s2)
{
  STRING_CI_COMPARE(s1,s2,string_lt_p);
}


static lisp string_ci_gt_p(lisp s1, lisp s2)
{
  STRING_CI_COMPARE(s1,s2,string_gt_p);
}


static lisp string_ci_lt_equal_p(lisp s1, lisp s2)
{
  STRING_CI_COMPARE(s1,s2,string_lt_equal_p);
}


static lisp string_ci_gt_equal_p(lisp s1, lisp s2)
{
  STRING_CI_COMPARE(s1,s2,string_gt_equal_p);
}

/*
essential procedure: substring string start end

String must be a string, and start and end must be exact integers
satisfying

(<= 0 start end (string-length string).) 

Substring returns a newly allocated string formed from the characters of string
beginning with index start (inclusive) and ending with index end (exclusive). 
*/
static lisp substring(lisp lisp_string, lisp lisp_start, lisp lisp_end)
{
  fd_u8char *string, *s_start, *s_end;
  int start, end;

  if (!(STRINGP(lisp_string)))
    fd_type_error(_("not a string"),lisp_string);
  else string=STRING_DATA(lisp_string);

  if (!(FIXNUMP(lisp_start)))
    fd_type_error(_("substring start is not a fixnum offset"),lisp_start);
  else start=FIXLISP(lisp_start);
  
  if (!(FIXNUMP(lisp_end)))
    fd_type_error(_("substring end is not a fixnum offset"),lisp_end);
  else end=FIXLISP(lisp_end);

  s_start=fd_utf8_substring(string,start);
  s_end=fd_utf8_substring(string,end);
  if (s_end == NULL)
    return fd_copy_string(s_start);
  else {
    char *copy=fd_xmalloc((s_end-s_start)+1);
    strncpy(copy,s_start,s_end-s_start); copy[s_end-s_start]='\0';
    return fd_init_string(copy,s_end-s_start);}
}

/*

essential procedure: string-append string ...

Returns a newly allocated string whose characters form the
concatenation of the given strings.

*/
static lisp lisp_string_append_lexpr(lisp strings)
{
  struct FD_STRING_STREAM output;
  FD_INITIALIZE_STRING_STREAM(&output,64);
  {DOLIST(string,strings)
     if (STRINGP(string)) {
       fd_sputs(&output,STRING_DATA(string));}
     else fd_type_error(_("not a string"),string);}
  return fd_init_string(output.ptr,output.size);
}

/*

non-essential procedure: space-append string ...

Like string, append but inserts a spacing character between each appended substring.

*/
static lisp lisp_space_append_lexpr(lisp args)
{
  fd_lisp spacer_arg=fd_get_arg(args,0,FD_VOID);
  fd_lisp strings=fd_get_body(args,1);
  fd_u8char *spacer=fd_strdata(spacer_arg);
  struct FD_STRING_STREAM output; int first=1;
  FD_INITIALIZE_STRING_STREAM(&output,128);
  {DOLIST(string,strings) {
    if (first) first=0; else fd_sputs(&output,spacer);
    if (STRINGP(string)) {
      fd_sputs(&output,STRING_DATA(string));}
    else fd_type_error(_("not a string"),string);}}
  return fd_init_string(output.ptr,output.size);
}

/* string_contains: */

static lisp lisp_string_contains_cproc(lisp string,lisp substring)
{
  if (!(STRINGP(string)))
    fd_type_error(_("key not a string"),string);
  else if (!(STRINGP(substring)))
    fd_type_error(_("context not a string"),substring);
  else {
    fd_u8char *result=strstr(STRING_DATA(string),STRING_DATA(substring));
    if (result) return LISPFIX(result-(STRING_DATA(string)));
    else return FD_FALSE;}
}

/* 

  essential procedure: string->list string

*/

static lisp string_to_list(lisp string)
{
  if (STRING_LENGTH(string) == 0) return FD_EMPTY_LIST;
  else {
    lisp head, tail, *loc=&head;
    fd_u8char *scan=fd_strdata(string);
    fd_u8char *limit=scan+FD_STRING_LENGTH(string);
    while (scan < limit) {
      int ch=((*scan < 0x80) ? (*scan++) : (fd_sgetc(&scan)));
      fd_lisp chp=FD_MAKE_LIST1(fd_make_character(ch));
      *loc=chp; loc=&(PTR_DATA(chp,pair)->cdr);}
    return head;}
}

/* 

essential procedure: list->string chars

String->list returns a newly allocated list of the characters that
make up the given string. List->string returns a newly allocated
string formed from the characters in the list chars. String->list and
list->string are inverses so far as equal? is concerned.

*/

static lisp list_to_string(lisp list)
{
  return string(list);
}

/*

procedure: string-copy string

Returns a newly allocated copy of the given string. 

*/

static lisp string_copy(lisp string)
{
  if (STRINGP(string)) return _fd_copy_lisp_proc(string);
  else fd_type_error(_("not a string"),string);
}

/*

procedure: lisp_qstring_cproc

Primitive for converting a string into a qstring.

*/
static fd_lisp lisp_qstring_cproc(fd_lisp string)
{
  if (FD_QSTRINGP(string)) return fd_incref(string);
  else if (FD_PRIM_TYPEP(string,string_type)) 
    return fd_qify_string(FD_PTR_DATA(string,string));
  else if (FD_SYMBOLP(string))
    return
      fd_make_qstring(FD_SYMBOL_NAME(string),strlen(FD_SYMBOL_NAME(string)));
  else fd_type_error(_("not a string or symbol"),string);
}

/*

procedure: lisp_zstring_cproc

Primitive for converting a string into a qstring.

*/
static fd_lisp lisp_zstring_cproc(fd_lisp string)
{
  if (FD_ZSTRINGP(string)) return fd_incref(string);
  else if ((FD_PRIM_TYPEP(string,string_type)) || (FD_PRIM_TYPEP(string,qstring_type)))
    return fd_zify_string(FD_PTR_DATA(string,string));
  else if (FD_SYMBOLP(string))
    return
      fd_make_zstring(FD_SYMBOL_NAME(string),strlen(FD_SYMBOL_NAME(string)));
  else fd_type_error(_("not a string or symbol"),string);
}

/*

procedure: string-fill! string char

Stores char in every element of the given string and returns an
unspecified value.

*/

static lisp string_fill(lisp string,lisp value)
{
  if (!(STRINGP(string)) )
    fd_type_error(_("not a string"),string);
  else if (!(FD_PRIM_TYPEP(string,string_type)) )
    fd_type_error(_("not a modifiable string"),string);
  else if (!(CHARACTERP(value)))
    fd_type_error(_("not a character"),string);
  else {
    int c=CHAR_CODE(value);
    if (c < 0x80) {
      /* We will actually shrink the bytes in the string in order to
	 keep the length in characters (e.g. unicode code points) constant. */
      int i=0, len_in_chars=fd_string_length(string);
      fd_u8char *s=STRING_DATA(string);
      while (i < len_in_chars) s[i++]=c; s[i]=NUL;
      fd_malloc_adjust(len_in_chars-FD_STRING_LENGTH(string));
      FD_STRING_LENGTH(string)=len_in_chars;
      FD_PTR_DATA(string,string)->utf8=0;
      return FD_VOID;}
    else {
      int c;
      struct FD_STRING_STREAM out;
      fd_u8char *data=STRING_DATA(string), *in=data;
      FD_INITIALIZE_STRING_STREAM(&out,STRING_LENGTH(string)*2);
      while ((c=uni_sgetc(&in)) > 0) fd_sputc(&out,c);
      fd_free(data,STRING_LENGTH(string));
      STRING_DATA(string)=out.ptr; STRING_LENGTH(string)=out.size;
      FD_PTR_DATA(string,string)->utf8=1;
      return FD_VOID;}}
}


/* String Transformations */

static lisp string_downcase(lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_raise_exception("STRING-DOWNCASE needs a string");
  else {
    struct FD_STRING_STREAM out; int c;
    fd_u8char *scan=STRING_DATA(string), *limit=scan+STRING_LENGTH(string);
    FD_INITIALIZE_STRING_STREAM(&out,STRING_LENGTH(string)+4);
    while (scan < limit) {
      int ch=fd_sgetc(&scan);
      fd_sputc(&out,fd_tolower(ch));}
    return fd_init_string(out.ptr,out.size);}
}

static lisp string_upcase(lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_raise_exception("STRING-UPCASE needs a string");
  else {
    struct FD_STRING_STREAM out; int c;
    fd_u8char *scan=STRING_DATA(string), *limit=scan+STRING_LENGTH(string);
    FD_INITIALIZE_STRING_STREAM(&out,STRING_LENGTH(string)+4);
    while (scan < limit) {
      int ch=fd_sgetc(&scan);
      fd_sputc(&out,fd_toupper(ch));}
    return fd_init_string(out.ptr,out.size);}
}

static lisp string_trim(lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_raise_exception("STRING-TRIM needs a string");
  else {
    fd_u8char *data=STRING_DATA(string), *in=data, *start=in, *end=in;
    int c;
    while (1) {
      fd_u8char *here=in; c=uni_sgetc(&in);
      if (fd_isspace(c)) {} else {end=start=in=here; break;}}
    while ((c=uni_sgetc(&in))>0) 
      if (fd_isspace(c)) {} else end=in;
    if (end-start == STRING_LENGTH(string)) return incref(string);
    else return fd_make_substring(start,end);}
}

static lisp string_base(lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_raise_exception("STRING-BASE needs a string");
  else {
    struct FD_STRING_STREAM out; int c;
    fd_u8char *scan=STRING_DATA(string), *limit=scan+STRING_LENGTH(string);
    FD_INITIALIZE_STRING_STREAM(&out,STRING_LENGTH(string)+4);
    while (scan < limit) {
      int ch=fd_sgetc(&scan); fd_u8char *decomp;
      if (fd_ismodifier(ch)) continue;
      decomp=fd_decompose_char(ch);
      if (decomp) 
	fd_sputc(&out,fd_sgetc(&decomp));
      else fd_sputc(&out,ch);}
    return fd_init_string(out.ptr,out.size);}
}

static lisp string_lower_base(lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_raise_exception("STRING-LOWER-BASE needs a string");
  else {
    struct FD_STRING_STREAM out; int c;
    fd_u8char *scan=STRING_DATA(string), *limit=scan+STRING_LENGTH(string);
    FD_INITIALIZE_STRING_STREAM(&out,STRING_LENGTH(string)+4);
    while (scan < limit) {
      int ch=fd_sgetc(&scan); fd_u8char *decomp;
      if (fd_ismodifier(ch)) continue;
      decomp=fd_decompose_char(ch);
      if (decomp) ch=fd_sgetc(&decomp);
      fd_sputc(&out,fd_tolower(ch));}
    return fd_init_string(out.ptr,out.size);}
}


/* Prefix/suffix functions */

static lisp lisp_has_suffix(lisp suffix,lisp string)
{
  if (!(FD_STRINGP(suffix)))
    fd_type_error(_("suffix not a string"),suffix);
  else if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else if (STRING_LENGTH(suffix) > STRING_LENGTH(string)) return FD_FALSE;
  else {
    char *suff=STRING_DATA(suffix), *str=STRING_DATA(string);
    if (strcmp(suff,str+(STRING_LENGTH(string)-STRING_LENGTH(suffix))) == 0)
      return FD_TRUE;
    else return FD_FALSE;}
}

static lisp lisp_has_prefix(lisp prefix,lisp string)
{
  if (!(FD_STRINGP(prefix)))
    fd_type_error(_("prefix not a string"),prefix);
  else if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else if (STRING_LENGTH(prefix) > STRING_LENGTH(string)) return FD_FALSE;
  else {
    char *pref=STRING_DATA(prefix), *str=STRING_DATA(string);
    if (strncmp(pref,str,STRING_LENGTH(prefix)) == 0)
      return FD_TRUE;
    else return FD_FALSE;}
}

/* Checking and patching UTF8 strings */

static fd_lisp lisp_valid_utf8p_cproc(fd_lisp string)
{
  if (FD_STRINGP(string))
    if (FD_PTR_DATA(string,string)->utf8==0) return FD_TRUE;
    else if (fd_valid_utf8p(FD_STRING_DATA(string))) return FD_TRUE;
    else return FD_FALSE;
  else return FD_FALSE;
}

static int utf8_getchar(fd_u8char **ss)
{
  int i, size=1, ch; fd_u8char *start=*ss;
  if (**ss == '\0') return -1;
  else if (**ss < 0x80) ch=**ss;
  else if (**ss < 0xc0) return -1;
  else if (**ss < 0xE0) {size=2; ch=**ss&0x1F;}
  else if (**ss < 0xF0) {size=3; ch=**ss&0x0F;}
  else if (**ss < 0xF8) {size=4; ch=**ss&0x07;}
  else if (**ss < 0xFC) {size=5; ch=**ss&0x3;}     
  else if (**ss < 0xFE) {size=6; ch=**ss&0x1;}
  else {(*ss)++; return 0xFFFD;}
  i=size-1; (*ss)++;
  while (i) {
    if ((**ss<0x80) || (**ss>=0xC0)) {
      *ss=start; return -1;}
    else {ch=(ch<<6)|(**ss&0x3F); (*ss)++; i--;}}
  return ch;
}

static fd_lisp lisp_patch_utf8_string_cproc(fd_lisp string)
{
  if (FD_STRINGP(string))
    if (FD_PTR_DATA(string,string)->utf8==0) return fd_incref(string);
    else if (fd_valid_utf8p(FD_STRING_DATA(string)))
      return fd_incref(string);
    else {
      struct FD_STRING_STREAM ss;
      fd_u8char *scan=FD_STRING_DATA(string),
	*limit=scan+FD_STRING_LENGTH(string);
      FD_INITIALIZE_STRING_STREAM(&ss,FD_STRING_LENGTH(string));
      while (scan < limit) {
	int c=utf8_getchar(&scan);
	if (c < 0) c=*scan++;
	fd_sputc(&ss,c);}
      return fd_stream_string(&ss);}
  else return string;
}


/* Initializing procedures */

FDSCRIPT_EXPORT
void fd_initialize_strings_c()
{
  fd_add_lexpr(NULL,"MAKE-STRING",FD_NORMAL_LEXPR,make_string_lexpr);
  fd_add_cproc(NULL,"STRING-LENGTH",1,lisp_string_length_cproc);
  fd_add_cproc(NULL,"STRING-REF",2,lisp_string_ref_cproc);
  fd_add_cproc(NULL,"STRING-SET!",3,lisp_string_set_cproc);
  fd_add_cproc(NULL,"STRING=?", 2, string_equal_p);
  fd_add_cproc(NULL,"STRING-CI=?", 2, string_ci_equal_p);
  fd_add_cproc(NULL,"SUBSTRING",3,substring);

  fd_add_lexpr(NULL,"STRING",FD_NORMAL_LEXPR, string);
  fd_add_cproc(NULL,"STRING<?", 2, string_lt_p);
  fd_add_cproc(NULL,"STRING>?", 2, string_gt_p);
  fd_add_cproc(NULL,"STRING<=?", 2, string_lt_equal_p);
  fd_add_cproc(NULL,"STRING>=?", 2, string_gt_equal_p);
  fd_add_cproc(NULL,"STRING-CI<?", 2, string_ci_lt_p);
  fd_add_cproc(NULL,"STRING-CI>?", 2, string_ci_gt_p);
  fd_add_cproc(NULL,"STRING-CI<=?", 2, string_ci_lt_equal_p);
  fd_add_cproc(NULL,"STRING-CI>=?", 2, string_ci_gt_equal_p);

  fd_add_lexpr(NULL,"STRING-APPEND",FD_NORMAL_LEXPR,lisp_string_append_lexpr);
  fd_add_lexpr(NULL,"SPACE-APPEND",FD_NORMAL_LEXPR,lisp_space_append_lexpr);
  fd_add_cproc(NULL,"LIST->STRING",1,list_to_string);
  fd_add_cproc(NULL,"STRING->LIST",1,string_to_list);
  fd_add_cproc(NULL,"STRING-COPY",1,string_copy);
  fd_add_cproc(NULL,"STRING-FILL!",2,string_fill);

  fd_add_cproc(NULL,"STRING-CONTAINS?",2,lisp_string_contains_cproc);

  fd_add_cproc(NULL,"STRING-DOWNCASE",1,string_downcase);
  fd_add_cproc(NULL,"STRING-UPCASE",1,string_upcase);
  fd_add_cproc(NULL,"STRING-TRIM",1,string_trim);
  fd_add_cproc(NULL,"STRING-BASE",1,string_base);
  fd_add_cproc(NULL,"STRING-LOWER-BASE",1,string_lower_base);

  fd_add_cproc(NULL,"HAS-PREFIX",2,lisp_has_prefix);
  fd_add_cproc(NULL,"HAS-SUFFIX",2,lisp_has_suffix);

  fd_add_cproc(NULL,"QSTRING",1,lisp_qstring_cproc);
  fd_add_cproc(NULL,"ZSTRING",1,lisp_zstring_cproc);

  fd_add_cproc(NULL,"VALID-UTF8?",1,lisp_valid_utf8p_cproc);
  fd_add_cproc(NULL,"PATCH-UTF8",1,lisp_patch_utf8_string_cproc);
  
  fd_register_source_file("strings",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: strings.c,v $
   Revision 1.19  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.18  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.17  2004/03/03 18:49:03  haase
   Added SPACE-APPEND

   Revision 1.16  2004/02/04 21:40:44  haase
   Added special handling for ASCII strings to utf8 validation and patching

   Revision 1.15  2004/02/04 21:38:28  haase
   Added primitives for validating and patching invalid UTF-8 strings

   Revision 1.14  2003/11/06 09:56:47  haase
   Fixed error message

   Revision 1.13  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.12  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.2  2003/08/06 18:41:08  haase
   Updated copyright notices to 2003

   Revision 1.11.2.1  2003/01/26 20:49:23  haase
   Added zstrings, fixed some repeated UTF-8 translation

   Revision 1.11  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.10  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.9  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.8  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
