/* C Mode */

/* text.c
   Text manipulation primitives for FDScript
   Originally implemented by Ken Haase in the Machine Understanding Group
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

static char vcid[] = "$Id: text.c,v 1.59 2005/01/14 16:48:50 haase Exp $";

/** Miscellaneous declarations **/
/** Utility and inlined functions **/
/** String predicates **/
/** String substitution **/
/** The Text Matcher **/
/** Matcher substitution **/
/** Matcher Binding **/
/** Standard Segmentation **/
/** Gathering Matches **/
/** Matcher Segmentation **/
/** Miscellaneous text analysis **/
/** Converting entity refs **/
/** Timepoint extraction **/
/** Simple morphological analysis **/
/** Support for date and time parsing **/
/** Date and Time Parsing **/
/** Record Streams **/
/** Interface to proper name extraction **/
/** String canonicalization (case, diacritics, etc)  **/
/** Initialization **/


/* Initial definitions */

/* We want both of these to be fast. */

#include <time.h>
#include <sys/stat.h>
#include <assert.h>
#ifndef WIN32
#include <dirent.h>
#endif

#include "fdtext.h"

/** Miscellaneous declarations **/

typedef fd_u8char u8char;

#define EQ(x,y)               FD_LISP_EQ(x,y)

#define dstp(x) ((x)->tm_isdst)

extern fd_hashset
  fd_english_title_abbrevs, fd_english_stop_words, fd_name_suffixes;

static fd_lisp label_symbol, subst_symbol, source_symbol, timestamp_symbol;
fd_lispenv fd_texttools_env;

/** Utility and inlined functions **/

static fd_u8char *token_start(fd_u8char *s)
{
  if (s == NULL) return NULL;
  else {
    fd_u8char *last=s; int c=fd_sgetc(&s);
    while ((c>0) && (!(fd_isalnum(c)))) {last=s; c=fd_sgetc(&s);}
    if (c < 0) return NULL; else return last;}
}

static fd_u8char *token_end(fd_u8char *s)
{
  if (s == NULL) return NULL;
  else {
    fd_u8char *last=s; int c=fd_sgetc(&s);
    while ((c>0) && ((fd_isalnum(c)) || (c == ':') || (c == '.') || (c == '/'))) {
      last=s; c=fd_sgetc(&s);}
    return last;}
}

static int utf8_string_ref(fd_u8char *str)
{
  int c=fd_sgetc(&str);
  return c;
}

static int get_longest(fd_lisp lmatch)
{
  int longest=-1;
  FD_DO_CHOICES(m,lmatch)
    if (!(FD_FIXNUMP(m)))
      fd_type_error(_("invalid matcher result"),m);
    else {
      int c=fd_lisp2int(m); if (c > longest) longest=c;}
  END_FD_DO_CHOICES;
  return longest;
}


/** String predicates **/

static fd_lisp lisp_string_emptyp_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else if (FD_STRING_LENGTH(string) == 0) return FD_TRUE;
  else return FD_FALSE;
}

static fd_lisp lisp_string_multilinep_cproc(fd_lisp string) {
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else if (strchr(FD_STRING_DATA(string),'\n'))
    return FD_TRUE;
  else return FD_FALSE;
}

static fd_lisp lisp_string_lowercasep_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else {
    fd_u8char *scan=FD_STRING_DATA(string), *limit=scan+FD_STRING_LENGTH(string);
    while (scan < limit) {
      int tmp_char=fd_sgetc(&scan);
      if (fd_isupper(tmp_char)) return FD_FALSE;}
    return FD_TRUE;}
}

static fd_lisp lisp_string_uppercasep_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else {
    fd_u8char *scan=FD_STRING_DATA(string), *limit=scan+FD_STRING_LENGTH(string);
    while (scan < limit) {
      int tmp_char=fd_sgetc(&scan);
      if (fd_islower(tmp_char)) return FD_FALSE;}
    return FD_TRUE;}
}

static fd_lisp lisp_string_numericp_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else {
    fd_u8char *scan=FD_STRING_DATA(string), *limit=scan+FD_STRING_LENGTH(string);
    while (scan < limit) {
      int tmp_char=fd_sgetc(&scan);
      if (!(fd_isdigit(tmp_char))) return FD_FALSE;}
    return FD_TRUE;}
}

static fd_lisp lisp_string_capitalizedp_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else {
    fd_u8char *data=FD_STRING_DATA(string);
    int first_char=utf8_string_ref(data);
    if (fd_isupper(first_char)) return FD_TRUE;
    else return FD_FALSE;}
}

static fd_lisp lisp_string_whitespace_percentage_cproc(fd_lisp string)
{
  int whitespace=0, chars=0, size=FD_STRING_LENGTH(string), ratio;
  fd_u8char *scan=fd_strdata(string), *limit=scan+size;
  if (size == 0) return FD_LISPFIX(0);
  while (scan < limit) {
    int each_char=fd_sgetc(&scan); chars++;
    if ((each_char == '\n') || (each_char == ' ')) whitespace++;
    else if (each_char == '\t') whitespace=whitespace+4;
    else if (fd_isspace(each_char)) whitespace++;
    else if (each_char == '<') {
      /* SGML tags count as whitespace */
      fd_u8char *tagstart=scan; int i=0;
      whitespace++; each_char=fd_sgetc(&scan);
      while ((scan < limit) && (each_char != '>') && (i > 60)) {
	each_char=fd_sgetc(&scan); whitespace++; i++;}
      /* If the tag had over 60 characters, go back */
      if (i > 60) {whitespace=whitespace-60; scan=tagstart;}}}
  ratio=(whitespace*100)/chars;
  return FD_LISPFIX(ratio);
}

static fd_lisp lisp_string_alphabetic_percentage_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string))) return FD_LISPFIX(0);
  else {
    int alphabetic=0, chars=0, size=FD_STRING_LENGTH(string), ratio;
    fd_u8char *scan=fd_strdata(string), *limit=scan+size;
    if (size == 0) return FD_LISPFIX(0);
    while (scan < limit) {
      int each_char=fd_sgetc(&scan); chars++;
      if (fd_isalpha(each_char)) alphabetic++;}
    ratio=(alphabetic*100)/chars;
    return FD_LISPFIX(ratio);}
}

static fd_lisp lisp_string_alphanumeric_percentage_cproc(fd_lisp string)
{
  if (!(FD_STRINGP(string))) return FD_LISPFIX(0);
  else {
    int alphanumeric=0, chars=0, size=FD_STRING_LENGTH(string), ratio;
    fd_u8char *scan=fd_strdata(string), *limit=scan+size;
    if (size == 0) return FD_LISPFIX(0);
    while (scan < limit) {
      int each_char=fd_sgetc(&scan); chars++;
      if (fd_isalnum(each_char)) alphanumeric++;}
    ratio=(alphanumeric*100)/chars;
    return FD_LISPFIX(ratio);}
}

static fd_lisp lisp_string_capitalize_cproc(fd_lisp string)
{
  fd_u8char *scan=fd_strdata(string), *limit=scan+fd_strlen(string);
  struct FD_STRING_STREAM out; int word_start=1;
  FD_INITIALIZE_STRING_STREAM(&out,fd_strlen(string)+1);
  while (scan < limit) {
    int ch=fd_sgetc(&scan);
    if (word_start) fd_sputc(&out,fd_toupper(ch));
    else fd_sputc(&out,fd_tolower(ch));
    if ((fd_isspace(ch)) || (fd_ispunct(ch)))
      if ((ch == '\'')) word_start=0; else word_start=1;
    else word_start=0;}
  return fd_init_string(out.ptr,out.size);
}

/** String substitution **/

static fd_lisp lisp_string_subst_cproc
  (fd_lisp original,fd_lisp substring,fd_lisp replacement)
{
  struct FD_STRING_STREAM out; fd_u8char *scan, *search, *subst, *next;
  if (FD_STRING_LENGTH(substring) == 0)
    fd_raise_exception(_("STRING-SUBST: search key is empty"));
  FD_INITIALIZE_STRING_STREAM(&out,1024);
  scan=fd_strdata(original);     search=fd_strdata(substring);
  subst=fd_strdata(replacement); next=strstr(scan,search);
  while (next) {
    fd_sputn(&out,scan,next-scan); fd_sputs(&out,subst);
    scan=next+FD_STRING_LENGTH(substring);
    next=strstr(scan,search);}
  fd_sputs(&out,scan);
  return fd_stream_string(&out);
}

static int get_char_offset(fd_u8char *s,int i)
{
  fd_u8char *pt=s+i; int j=0;
  while (s < pt) {j++; fd_sgetc(&s);}
  return j;
}

static int get_byte_offset(fd_u8char *s,int offset,int len)
{
  fd_u8char *string=s, *lim=s+len; int c=1;
  while ((string < lim) && (offset > 0)) {
    c=fd_sgetc(&string); offset--;}
  if (string >= lim) return -1;
  else return string-s;
}


/** The Text Matcher **/

static fd_lisp lisp_text_matcher_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp pat=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp strings=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_lisp offsets=fd_eval_in_env(fd_get_arg(expr,3,FD_LISPFIX(0)),env);
  fd_lisp answers=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(each_offset,offsets) {
    FD_DO_CHOICES(string,strings) {
      int byte_off=get_byte_offset(fd_strdata(string),fd_fixlisp(each_offset),fd_strlen(string));
      if (byte_off >= 0) {
	int max=-1;
	fd_lisp matches=fd_text_matcher
	  (pat,env,fd_strdata(string),byte_off,fd_strlen(string),
	   (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
	FD_DO_CHOICES(match,matches) {
	  int x=fd_lisp2int(match);
	  if (x < 0)
	    fd_pigs_fly("fd_text_matcher returns negative offset");
	  else if (x > max) max=x;}
	END_FD_DO_CHOICES;
	fd_decref(matches);
	if (max > byte_off) {
	  if (FD_UNICODE_STRINGP(string)) {
	    int char_off=get_char_offset(FD_STRING_DATA(string),max);
	    FD_ADD_TO_CHOICE(answers,FD_LISPFIX(char_off));}
	  else {FD_ADD_TO_CHOICE(answers,FD_LISPFIX(max));}}}}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;
  fd_decref(pat); fd_decref(strings);
  return answers;
}

static fd_lisp lisp_text_match_handler(fd_lisp expr,fd_lispenv env)
{
  int match=0;
  fd_lisp pat=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp strings=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_lisp offsets=fd_eval_in_env(fd_get_arg(expr,3,FD_LISPFIX(0)),env);
  FD_DO_CHOICES(offset,offsets) {
    FD_DO_CHOICES(string,strings) {
      int byte_off=get_byte_offset(fd_strdata(string),fd_fixlisp(offset),fd_strlen(string));
      if (byte_off < 0) {}
      else if (fd_text_match(pat,env,fd_strdata(string),byte_off,
			(MATCH_IGNORE_CASE|MATCH_DO_BINDINGS))) {
	match=1; break;}}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;
  fd_decref(pat); fd_decref(strings); fd_decref(offsets);
  if (match) return FD_TRUE; else return FD_FALSE;
}

static fd_lisp lisp_text_search_lexpr(fd_lisp args)
{
  fd_lisp pat, string, offset_arg; int byte_offset, pos;
  fd_get_args("TX-SEARCH",args,&pat,FD_VOID,&string,FD_VOID,&offset_arg,FD_LISPFIX(0),NULL);
  byte_offset=get_byte_offset(fd_strdata(string),fd_fixlisp(offset_arg),fd_strlen(string));
  if (byte_offset < 0) return FD_EMPTY_CHOICE;
  pos=fd_text_search
    (pat,NULL,fd_strdata(string),byte_offset,
     fd_strlen(string),MATCH_IGNORE_CASE);
  if (pos < 0) return FD_EMPTY_CHOICE;
  else return FD_LISPFIX(get_char_offset(FD_STRING_DATA(string),pos));
}

static fd_lisp lisp_text_extract_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp pat=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp strings=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_lisp offset=fd_eval_in_env(fd_get_arg(expr,3,FD_LISPFIX(0)),env);
  fd_lisp answers=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(each_offset,offset) {
    FD_DO_CHOICES(string,strings) {
      int byte_off=get_byte_offset(fd_strdata(string),fd_fixlisp(each_offset),fd_strlen(string));
      if (byte_off >= 0) {
	int size=fd_strlen(string);
	fd_lisp answer=fd_text_extract
	  (pat,env,fd_strdata(string),byte_off,size,
	   (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
	FD_DO_CHOICES(each,answer) {
	  if (!(FD_PAIRP(each)))
	    fd_type_error(_("invalid matcher result"),each);
	  else if (fd_lisp2int(FD_CAR(each))==size) {
	    FD_ADD_TO_CHOICE(answers,(fd_cdr(each)));}}
	END_FD_DO_CHOICES;
	fd_decref(answer);}}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;
  fd_decref(pat); fd_decref(strings); fd_decref(offset);
  return answers;
}

static fd_lisp tx_closure_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp pattern=fd_get_arg(expr,1,FD_VOID);
  return fd_tx_closure(pattern,env);
}

/** Matcher substitution **/

static void do_substitution(fd_lisp extraction,fd_string_stream ss,lispenv env)
{
  if (FD_STRINGP(extraction)) fd_sputs(ss,FD_STRING_DATA(extraction));
  else if (FD_VECTORP(extraction)) {
    int i=0, l=FD_VECTOR_LENGTH(extraction);
    while (i < l) {
      do_substitution(FD_VECTOR_REF(extraction,i),ss,env); i++;}}
  else if (FD_CHARACTERP(extraction)) fd_sputc(ss,FD_CHAR_CODE(extraction));
  else if (FD_SYMBOLP(extraction)) {}
  else if (!(FD_PAIRP(extraction)))
    fd_type_error(_("not a pair"),extraction);
  else if (EQ(FD_CAR(extraction),subst_symbol)) {
    fd_lisp original=fd_get_arg(extraction,1,FD_VOID);
    fd_lisp replacement=fd_get_arg(extraction,2,FD_VOID);
    if (FD_STRINGP(replacement)) fd_sputs(ss,FD_STRING_DATA(replacement));
    else {
      FD_WITH_LEXICAL_ENV(text_subst_env,env,1) {
	fd_lisp result;
	fd_bind_value(source_symbol,original,text_subst_env);
	result=fd_eval_in_env(replacement,text_subst_env);
	if (FD_STRINGP(result)) fd_sputs(ss,FD_STRING_DATA(result));
	else fd_print_lisp_to_string(result,ss);
	fd_decref(result);}
      FD_END_WITH_LEXICAL_ENV_NOVALUE();}}
  else {
    FD_DOLIST(elt,FD_CDR(extraction)) do_substitution(elt,ss,env);}
}

static fd_lisp lisp_text_subst_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp pat=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp strings=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_lisp offset=fd_eval_in_env(fd_get_arg(expr,3,FD_LISPFIX(0)),env);
  fd_lisp answers=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(each_offset,offset) {
    int off=FD_FIXLISP(each_offset);
    FD_DO_CHOICES(string,strings) {
      int size=fd_strlen(string);
      fd_lisp answer=fd_text_extract
	(pat,env,fd_strdata(string),off,size,
	 (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
      FD_DO_CHOICES(each,answer) {
	if (!(FD_PAIRP(each)))
	  fd_type_error(_("invalid matcher result"),each);
	else if (fd_lisp2int(FD_CAR(each))==size) {
	  struct FD_STRING_STREAM ss; 
	  FD_INITIALIZE_STRING_STREAM(&ss,size*2);
	  do_substitution(FD_CDR(each),&ss,env);
	  FD_ADD_TO_CHOICE(answers,fd_init_string(ss.ptr,ss.size));}}
      END_FD_DO_CHOICES;
      fd_decref(answer);}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;
  fd_decref(pat); fd_decref(strings); fd_decref(offset);
  return answers;
}

FDTEXT_EXPORT
/* fd_text_subst:
    Arguments: a pattern (a lisp pointer) and a string (also a lisp pointer)
    Returns: another lisp string
    Applies the substitutions specified in the pattern to the string,
returning the result.
*/
fd_lisp fd_text_subst(fd_lisp pat,fd_lisp string)
{
  fd_lisp answers=FD_EMPTY_CHOICE;
  int size=fd_strlen(string);
  fd_lisp answer=fd_text_extract
    (pat,NULL,fd_strdata(string),0,size,
     (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
  FD_DO_CHOICES(each,answer) {
      if (!(FD_PAIRP(each)))
	fd_type_error(_("invalid matcher result"),each);
      else if (fd_lisp2int(FD_CAR(each))==size) {
	struct FD_STRING_STREAM ss; 
	FD_INITIALIZE_STRING_STREAM(&ss,size*2);
	do_substitution(FD_CDR(each),&ss,NULL);
	FD_ADD_TO_CHOICE(answers,fd_init_string(ss.ptr,ss.size));}}
  END_FD_DO_CHOICES;
  fd_decref(answer);
  return answers;
}

/** Matcher Binding **/

static void do_bind(fd_lisp expr,fd_lispenv env)
{
  if ((FD_PAIRP(expr)) && (EQ(FD_CAR(expr),label_symbol))) {
    fd_lisp sym=fd_get_arg(expr,1,FD_VOID);
    fd_lisp value=fd_get_arg(expr,2,FD_VOID);
    fd_lisp current=fd_lexical_symeval(sym,env);
    if (FD_VOIDP(current))
      fd_bind_value(sym,value,env);
    else {
      FD_ADD_TO_CHOICE(current,fd_incref(value));
      fd_bind_value(sym,current,env);
      fd_decref(current);}}
  else if (FD_PAIRP(expr)) {
    FD_DOLIST(remainder,FD_CDR(expr))
      do_bind(remainder,env);}
  else if (FD_VECTORP(expr)) {
    int i=0, l=FD_VECTOR_LENGTH(expr);
    while (i < l) {
      do_bind(FD_VECTOR_REF(expr,i),env); i++;}}
  else return;
}

static fd_lisp lisp_text_bind_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp pats=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp strings=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_lisp body=fd_get_body(expr,3);
  fd_lisp answers=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(string,strings) {
    int len=fd_strlen(string);
    fd_lisp extractions=fd_text_extract
      (pats,env,fd_strdata(string),0,fd_strlen(string),
       (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
    FD_DO_CHOICES(extraction,extractions)
      if (!(FD_PAIRP(extraction)))
	fd_type_error(_("Invalid extraction result"),extraction);
      else {
	fd_lisp value=FD_VOID;
	fd_lisp size=fd_car_noref(extraction), data=fd_cdr_noref(extraction);
	if ((FD_FIXLISP(size)) == len) {
	  FD_WITH_LEXICAL_ENV(text_env,env,8) {
	    do_bind(data,text_env);
	    {FD_DOLIST(expr,body) {
	      fd_decref(value); value=fd_evaluate(expr,text_env);}}}
	  FD_END_WITH_LEXICAL_ENV(value);
	  FD_ADD_TO_CHOICE(answers,value);}}
    END_FD_DO_CHOICES;
    fd_decref(extractions);}
  END_FD_DO_CHOICES;
  fd_decref(pats); fd_decref(strings);
  return answers;
}

static void do_fadd(fd_lisp expr,fd_lisp frame)
{
  if ((FD_PAIRP(expr)) && (EQ(FD_CAR(expr),label_symbol))) {
    fd_lisp sym=fd_get_arg(expr,1,FD_VOID);
    fd_lisp value=fd_get_arg(expr,2,FD_VOID);
    fd_frame_add(frame,sym,value);}
  else if (FD_PAIRP(expr)) {
    FD_DOLIST(remainder,FD_CDR(expr)) do_fadd(remainder,frame);}
  else if (FD_VECTORP(expr)) {
    int i=0, l=FD_VECTOR_LENGTH(expr);
    while (i < l) {do_fadd(FD_VECTOR_REF(expr,i),frame); i++;}}
  else return;
    
}

static fd_pool interpret_pool(fd_lisp arg) 
{
  if (FD_FALSEP(arg)) return NULL;
  else return fd_interpret_pool(arg);
}

static fd_lisp lisp_text_frame_cproc(fd_lisp pool,fd_lisp pattern,fd_lisp string)
{
  fd_pool p=interpret_pool(pool);
  fd_lisp frames=FD_EMPTY_CHOICE;
  int len=fd_strlen(string);
  fd_lisp extractions=fd_text_extract
    (pattern,NULL,fd_strdata(string),0,len,
     (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
  FD_DO_CHOICES(extraction,extractions)
    if (!(FD_PAIRP(extraction)))
      fd_type_error(_("Invalid extraction result"),extraction);
    else if ((FD_FIXLISP(FD_CAR(extraction))) == len) {
      fd_lisp frame=
	((p) ? (fd_frame_create(p)) : (fd_make_slotmap(8)));
      fd_lisp data=fd_cdr_noref(extraction);
      do_fadd(data,frame);
      FD_ADD_TO_CHOICE(frames,frame);}
  END_FD_DO_CHOICES;
  fd_decref(extractions);
  return frames;
}


/** Standard Segmentation **/

static fd_lisp extract_substring(fd_lisp string,int start,int end);
static int text_matcher(fd_lisp pat,char *string,int pos,int lim);

static fd_lisp lisp_standard_segment(fd_lisp arg)
{
  if (FD_STRINGP(arg)) {
    fd_lisp result=FD_EMPTY_LIST, end=FD_EMPTY_LIST;
    fd_lisp seg, lst;
    fd_u8char *scan=FD_STRING_DATA(arg), *start=scan, *last=start;
    int c=fd_sgetc(&scan);
    while (c >= 0)
      if (fd_isalnum(c)) {last=scan; c=fd_sgetc(&scan);}
      else {
	seg=fd_make_substring(start,last); lst=FD_MAKE_LIST1(seg);
	if (FD_EMPTY_LISTP(end)) result=end=lst;
	else {FD_RPLACD(end,lst); end=lst;}
	start=scan; c=fd_sgetc(&scan);
	while ((c>0) && (!(fd_isalnum(c)))) {
	  start=scan; c=fd_sgetc(&scan);}
	last=start;}
    seg=fd_make_substring(start,scan); lst=FD_MAKE_LIST1(seg);
    if (FD_EMPTY_LISTP(end)) result=end=lst;
    else {FD_RPLACD(end,lst); end=lst;}
    return result;}
  else fd_type_error(_("not a string"),arg);
}

#define compound_charp(c) ((fd_isalnum(c)) || (c == '-') || (c == '/') || (c == '_'))

static fd_lisp lisp_compound_segment_cproc(fd_lisp arg)
{
  if (FD_STRINGP(arg)) {
    fd_lisp result=FD_EMPTY_LIST, end=FD_EMPTY_LIST;
    fd_lisp seg, lst;
    fd_u8char *scan=FD_STRING_DATA(arg), *start=scan, *last=start;
    int c=fd_sgetc(&scan);
    while (c >= 0)
      if (compound_charp(c)) {
	last=scan; c=fd_sgetc(&scan);}
      else {
	seg=fd_make_substring(start,last); lst=FD_MAKE_LIST1(seg);
	if (FD_EMPTY_LISTP(end)) result=end=lst;
	else {FD_RPLACD(end,lst); end=lst;}
	start=scan; c=fd_sgetc(&scan);
	while ((c>0) && (!(compound_charp(c)))) {
	  start=scan; c=fd_sgetc(&scan);}
	last=start;}
    seg=fd_make_substring(start,scan); lst=FD_MAKE_LIST1(seg);
    if (FD_EMPTY_LISTP(end)) result=end=lst;
    else {FD_RPLACD(end,lst); end=lst;}
    return result;}
  else fd_type_error(_("not a string"),arg);
}

static fd_lisp extract_substring(fd_lisp string,int start,int end)
{
  /*  int end=((end_arg<0) ? (fd_strlen(string)) : (end_arg)); */
  char *data=fd_xmalloc((end-start)+1);
  strncpy(data,fd_strdata(string)+start,end-start);
  data[end-start]='\0';
  return fd_init_string(data,end-start);
}

/* Returns the length of the substring matched at POS */
static int text_matcher(fd_lisp pat,char *string,int pos,int lim)
{
  fd_lisp answer=fd_text_matcher(pat,NULL,string,pos,lim,0);
  if (FD_FIXNUMP(answer))
    if (FD_FIXLISP(answer) < 0)
      fd_pigs_fly("fd_text_matcher returns negative offset");
    else return FD_FIXLISP(answer)-pos;
  else {
    int max=-1; FD_DO_CHOICES(a,answer) {
      int n=fd_lisp2int(a);
      if (n < 0)
	fd_pigs_fly("fd_text_matcher returns negative offset");
      if (n>max) max=n;}
    END_FD_DO_CHOICES;
    fd_decref(answer);
    if (max == -1) return 0;
    else return max-pos;}
}


/** Gathering Matches **/

static fd_lisp text_gather(fd_lisp pat,fd_lisp string)
{
  struct FD_HASHSET seen; fd_lisp answers=FD_EMPTY_CHOICE;
  fd_u8char *data=fd_strdata(string);
  int answer, start=0, slength=FD_STRING_LENGTH(string); 
  
  fd_init_hashset(&seen,1024);

  while ((start < slength) &&
	 ((answer=fd_text_search(pat,NULL,data,start,slength,0)) >= 0)) {
    int size=text_matcher(pat,data,answer,slength);
    if (size == 0) start++;
    else if (size > slength)
      fd_pigs_fly("text matcher returned size beyond limit in gather");
    else {
      fd_lisp substring=extract_substring(string,answer,answer+size);
      fd_hashset_add(&seen,substring);
      fd_decref(substring);
      start=answer+size;}}
  
  answers=fd_hashset_elts(&seen);
  fd_free_hashset(&seen);
  return answers;
}

static fd_lisp lisp_gather_lexpr(fd_lisp args)
{
  fd_lisp pattern, strings, answers=FD_EMPTY_CHOICE;
  fd_get_args("TX-GATHER",args,&pattern,FD_VOID,&strings,FD_VOID,NULL);
  {FD_DO_CHOICES(string,strings) {
    fd_lisp gathered=text_gather(pattern,string);
    FD_ADD_TO_CHOICE(answers,gathered);}
  END_FD_DO_CHOICES;}
  return fd_return_proper_choice(answers);
}


/** Matcher Segmentation **/

static fd_lisp text_segment(fd_lisp pat,fd_lisp string)
{
  int answer, start=0;
  fd_u8char *data=fd_strdata(string);
  int slength=FD_STRING_LENGTH(string);
  fd_lisp head=FD_EMPTY_LIST, *tail=&head;
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  while ((start < slength) &&
	 ((answer=fd_text_search(pat,NULL,data,start,slength,0)) >= 0)) {
    int size=text_matcher(pat,data,answer,slength);
    *tail=FD_MAKE_LIST1(extract_substring(string,start,answer));
    tail=&(FD_CDR(*tail));
    if ((size == 0) && (answer == start)) start=start+1; 
    else start=answer+size;}

  if (start < slength)
    *tail=FD_MAKE_LIST1
      (extract_substring(string,start,FD_STRING_LENGTH(string)));
  return head;
}

static fd_lisp lisp_segment_lexpr(fd_lisp args)
{
  fd_lisp answer=FD_EMPTY_CHOICE, strings, break_pat;
  fd_get_args("TX-SEGMENT",args,&strings,FD_VOID,&break_pat,FD_FALSE,NULL);
  {FD_DO_CHOICES(string,strings) {
    fd_lisp segmented;
    if (FD_FALSEP(break_pat)) segmented=lisp_standard_segment(string);
    else segmented=text_segment(break_pat,string);
    FD_ADD_TO_CHOICE(answer,segmented);}
  END_FD_DO_CHOICES;}
  return fd_return_proper_choice(answer);
}

static fd_lisp text_fragment(fd_lisp pat,fd_lisp string)
{
  int answer, start=0;
  fd_u8char *data=fd_strdata(string);
  int slength=FD_STRING_LENGTH(string);
  fd_lisp head=FD_EMPTY_LIST, *tail=&head;
  if (!(FD_STRINGP(string)))
    fd_type_error(_("not a string"),string);
  while ((start < slength) &&
	 ((answer=fd_text_search(pat,NULL,data,start,slength,0)) >= 0)) {
    int size=text_matcher(pat,data,answer,slength);
    if ((size == 0) && (answer == start)) { 
      /* If the fragmenter here has zero width, skip one character */
      fd_u8char *next=fd_utf8_substring(data+start,1);
      size=next-(data+start);}
    if (start == 0)
      if (answer != 0) {
	head=FD_MAKE_LIST1(fd_make_string("")); tail=&(FD_CDR(head));
	*tail=FD_MAKE_LIST1(extract_substring(string,start,answer));
	tail=&(FD_CDR(*tail));}
      else {}
    else {
      *tail=FD_MAKE_LIST1(extract_substring(string,start,answer));
      tail=&(FD_CDR(*tail));}
    *tail=FD_MAKE_LIST1(extract_substring(string,answer,answer+size));
    tail=&(FD_CDR(*tail));
    start=answer+size;}

  if (start < slength)
    *tail=FD_MAKE_LIST1
      (extract_substring(string,start,FD_STRING_LENGTH(string)));
  else
    *tail=FD_MAKE_LIST1(fd_make_string(""));
  return head;
}

static fd_lisp lisp_fragment_lexpr(fd_lisp args)
{
  fd_lisp answer=FD_EMPTY_CHOICE, strings, break_pat;
  fd_get_args("TX-FRAGMENT",args,&strings,FD_VOID,&break_pat,FD_FALSE,NULL);
  {FD_DO_CHOICES(string,strings) {
    fd_lisp segmented=text_fragment(break_pat,string);
    FD_ADD_TO_CHOICE(answer,segmented);}
   END_FD_DO_CHOICES;}
  return fd_return_proper_choice(answer);
}


/** Miscellaneous text analysis **/

#define isnot_mailidp(c) \
   ((isspace(c)) || (c == ',') || (c == '(') || (c == '>') || (c == '<'))

/* mailid extraction:
   Returns all of the mail ids (email addresses or message references) in a string. */
static fd_lisp lisp_extract_mail_ids_cproc(fd_lisp string)
{
  if (FD_STRINGP(string)) {
    fd_lisp answer=FD_EMPTY_CHOICE;
    char *scan=FD_STRING_DATA(string), *begin=NULL, *limit=scan+FD_STRING_LENGTH(string);
    int seek_start=1, seek_end=0;
    while (scan<limit)
      if (seek_start)
	if (isnot_mailidp(*scan)) scan++;
	else {begin=scan++; seek_start=0;}
      else if ((seek_end == 0) && (*scan == '@')) {
	scan++; seek_end=1; seek_start=0;}
      else if (seek_end)
	if (isnot_mailidp(*scan)) {
	  fd_lisp ss=fd_make_substring(begin,scan);
	  FD_ADD_TO_CHOICE(answer,ss); scan++;
	  seek_end=0; seek_start=1;}
	else scan++;
      else if (isnot_mailidp(*scan)) {
	seek_start=1; seek_end=0;}
      else scan++;
    if ((seek_end) && (begin)) {
      fd_lisp last=fd_make_string(begin);
      FD_ADD_TO_CHOICE(answer,last);}
    return answer;}
  else fd_type_error(_("not a string"),string);
}

/* stemming:
   calls the Porter stemmer; this is a no-op for non-ascii strings */
static fd_lisp lisp_stem_english_word_cproc(fd_lisp string)
{
  if (FD_ASCII_STRINGP(string)) {
    char *stemmed=fd_stem_english_word(FD_STRING_DATA(string));
    fd_lisp result=fd_make_string(stemmed);
    fd_xfree(stemmed);
    return result;}
  else if (FD_STRINGP(string)) return fd_incref(string);
  else fd_type_error(_("not a string"),string);
}

/* Margin stripping:
    This finds the common initial substring for a multi-line string and removes it. */
static fd_lisp lisp_strip_margin_cproc(fd_lisp string)
{
  if (FD_STRINGP(string)) {
    fd_lisp answer; int c=-1, beginning_of_line=1;
    fd_u8char *scan=FD_STRING_DATA(string);
    struct FD_STRING_STREAM ss;
    FD_INITIALIZE_STRING_STREAM
      (&ss,fd_roundup(FD_STRING_LENGTH(string),FD_STRING_CHUNK));
    while ((c=fd_sgetc(&scan))>0) {
      if (fd_isalnum(c)) {fd_sputc(&ss,c); beginning_of_line=0;}
      else if (beginning_of_line) {}
      else {fd_sputc(&ss,c); beginning_of_line=1;}}
    answer=fd_copy_string(ss.ptr); fd_xfree(ss.ptr);
    return answer;}
  else fd_type_error(_("not a string"),string);
}

/* Trigrams and bigrams */

static fd_lisp make_bigram(int c1,int c2)
{
  struct FD_STRING_STREAM os;
  FD_INITIALIZE_STRING_STREAM(&os,4);
  fd_sputc(&os,c1); fd_sputc(&os,c2);
  return fd_init_string(os.ptr,os.size);
}

static fd_lisp make_trigram(int c1,int c2,int c3)
{
  struct FD_STRING_STREAM os;
  FD_INITIALIZE_STRING_STREAM(&os,8);
  fd_sputc(&os,c1); fd_sputc(&os,c2); fd_sputc(&os,c3);
  return fd_init_string(os.ptr,os.size);
}

FDTEXT_EXPORT
/* fd_get_bigrams:
     Arguments: a string pointer (UTF8) and a length
     Returns: a choice of two-character strings
 This returns the letter pairs from its string argument,
returning both upper and lowercase varieties but does not
do any unicode character normalization.
*/
fd_lisp fd_get_bigrams(fd_u8char *string,int len)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  fd_u8char *scan=string, *limit=scan+len;
  int c1=fd_sgetc(&scan), c2; 
  while (scan < limit) {
    fd_lisp bigram; c2=fd_sgetc(&scan);
    if ((fd_isupper(c1)) || (fd_isupper(c2))) {
      fd_lisp bigram=make_bigram(fd_tolower(c1),fd_tolower(c2));
      FD_ADD_TO_CHOICE(results,bigram);}
    else {
      bigram=make_bigram(c1,c2);
      FD_ADD_TO_CHOICE(results,bigram);}
    c1=c2;}
  return results;
}

FDTEXT_EXPORT
/* fd_get_trigrams:
     Arguments: a string pointer (UTF8) and a length
     Returns: a choice of three-character strings
 This returns the letter triples from its string argument,
returning both upper and lowercase varieties but does not
do any unicode character normalization.
*/
fd_lisp fd_get_trigrams(fd_u8char *string,int len)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  fd_u8char *scan=string, *limit=scan+len;
  int c1, c2, c3; 
  c1=fd_sgetc(&scan); c2=fd_sgetc(&scan);
  while (scan < limit) {
    fd_lisp trigram; c3=fd_sgetc(&scan);
    if ((fd_isupper(c1)) || (fd_isupper(c2)) || (fd_isupper(c3))) {
      fd_lisp trigram=
	make_trigram(fd_tolower(c1),fd_tolower(c2),fd_tolower(c3));
      FD_ADD_TO_CHOICE(results,trigram);}
    else {
      trigram=make_trigram(c1,c2,c3);
      FD_ADD_TO_CHOICE(results,trigram);}
    c1=c2; c2=c3;}
  return results;
}

static fd_lisp get_bigrams_cproc(fd_lisp string)
{
  return fd_get_bigrams(FD_STRING_DATA(string),FD_STRING_LENGTH(string));
}

static fd_lisp get_trigrams_cproc(fd_lisp string)
{
  return fd_get_trigrams(FD_STRING_DATA(string),FD_STRING_LENGTH(string));
}


/** Time point extraction **/

static fd_hashset time_suffixes, time_words;

static int time_wordp(fd_u8char *string,int comma)
{
  fd_u8char *scan=string, *last=scan; int c, weird_char=0;
  while ((c=fd_sgetc(&scan))>=0)
    if ((fd_isdigit(c)) || (c == ':') || (c == '/')) last=scan;
    else {weird_char=1; break;}
  if (!(weird_char)) return 1;
  else if (fd_hashset_strget(time_suffixes,last,-1)) return 1;
  else if (comma) return 0;
  else if (fd_hashset_strget(time_words,string,-1)) return 1;
  else return 0;
}

EXPORTED
fd_lisp fd_get_timepoints(fd_u8char *string)
{
  fd_lisp answers=FD_EMPTY_CHOICE;
  fd_u8char *start=string, *scan=string;
  fd_u8char *word_start=start, *word_end=start, *lword_end=start;
  int in_time=0, n_words=0, comma=0;
  struct FD_STRING_STREAM word_stream; int c;
  FD_INITIALIZE_STRING_STREAM(&word_stream,32);
  /* Read words sequentially, looking for cues
     At any point, word_stream contains the current word (so far)
                   n_words contains the number of words in the timepoint
		   start is where the current timepoint started
		   word_start is where the last word started
   */
  while ((c=fd_sgetc(&scan))>=0) 
    if (!(fd_isalnum(c))) { /* ((fd_isspace(c)) || (c == ',')) */
      if (in_time)
	if (time_wordp(word_stream.ptr,comma)) n_words++;
	else {
	  if (start < lword_end) {
	    fd_lisp timepoint=fd_make_substring(start,lword_end);
	    FD_ADD_TO_CHOICE(answers,timepoint);}
	  in_time=0; n_words=0; start=NULL;}
      else if (time_wordp(word_stream.ptr,comma)) {
	start=word_start; n_words=1; in_time=1;}
      /* Handle updating for the end of a word */
      if (c == ',') comma=1; else comma=0;
      lword_end=word_end;
      while ((c>=0) && (!(fd_isalnum(c)))) { /* Skip whitespace */
	word_start=scan; c=fd_sgetc(&scan);}
      /* Re-init word_stream */
      word_stream.ptr[0]=0; word_stream.size=0;
      /* Put the last character you read onto the word stream */
      if (c>=0) {fd_sputc(&word_stream,fd_toupper(c));}}
    else {word_end=scan; fd_sputc(&word_stream,fd_toupper(c));}
  if (in_time) {
    if (start < lword_end) {
      fd_lisp timepoint=fd_make_substring(start,lword_end);
      FD_ADD_TO_CHOICE(answers,timepoint);}}
  else if ((word_stream.size) && (time_wordp(word_stream.ptr,comma))) {
    fd_lisp timepoint=fd_copy_string(word_stream.ptr);
    FD_ADD_TO_CHOICE(answers,timepoint);}
  fd_xfree(word_stream.ptr);
  return answers;
}

static fd_lisp lisp_get_timepoints_cproc(fd_lisp string)
{
  return fd_get_timepoints(fd_strdata(string));
}


/** Simple morphological analysis **/

static fd_lisp apply_suffixrule
 (fd_u8char *string,int slen,
  fd_u8char *suffix,int sufflen,
  fd_u8char *rep,int replen,
  fd_hashset roots)
{
  if (slen <= sufflen) return FD_EMPTY_CHOICE;
  else if (slen-sufflen+replen > 256) return FD_EMPTY_CHOICE;
  else if (strcmp(string+(slen-sufflen),suffix) == 0) {
    fd_u8char buf[256]; strcpy(buf,string); strcpy(buf+(slen-sufflen),rep);
    if (fd_hashset_strget(roots,buf,slen-sufflen+replen))
      return fd_init_string(fd_strdup(buf),slen-sufflen+replen);
    else return FD_EMPTY_CHOICE;}
  else return FD_EMPTY_CHOICE;
}

static fd_lisp suffixrule_lexpr(fd_lisp args)
{
  fd_lisp strings, rules, roots, results=FD_EMPTY_CHOICE;
  fd_hashset roots_table; int consed_table=0;
  fd_get_args("SUFFIXRULE",args,&strings,FD_VOID,&rules,FD_VOID,&roots,FD_VOID,NULL);
  if (FD_PRIM_TYPEP(roots,hashset_type)) 
    roots_table=(fd_hashset)FD_CPTR_DATA(roots);
  else {
    fd_hashset table=fd_make_hashset(2*FD_CHOICE_SIZE(roots));
    FD_DO_CHOICES(root,roots) fd_hashset_add(table,root); END_FD_DO_CHOICES;
    roots_table=table; consed_table=1;}
  {
    FD_DO_CHOICES(string,strings)
      if (FD_STRINGP(string)) {
	FD_DO_CHOICES(rule,rules)
	  if (FD_VECTORP(rule)) {
	    fd_lisp suffix=FD_VECTOR_REF(rule,0);
	    fd_lisp replacement=FD_VECTOR_REF(rule,1);
	    fd_lisp tag=
	      ((FD_VECTOR_LENGTH(rule)==3) ?
	       (FD_VECTOR_REF(rule,2)) : (FD_VOID));
	    fd_lisp output=
	      apply_suffixrule
	      (FD_STRING_DATA(string),FD_STRING_LENGTH(string),
	       FD_STRING_DATA(suffix),FD_STRING_LENGTH(suffix),
	       FD_STRING_DATA(replacement),FD_STRING_LENGTH(replacement),
	       roots_table);
	    if (FD_EMPTYP(output)) {}
	    else if (FD_VOIDP(tag)) {FD_ADD_TO_CHOICE(results,output);}
	    else {
	      fd_lisp elt=FD_MAKE_LIST(2,output,fd_incref(tag));
	      FD_ADD_TO_CHOICE(results,elt);}}
	  else fd_type_error(_("Invalid morphology rule"),rule);
	END_FD_DO_CHOICES;}
      else fd_type_error(_("not a string"),string);
    END_FD_DO_CHOICES;}
  if (consed_table) {
    fd_free_hashset(roots_table);
    fd_free(roots_table,sizeof(struct FD_HASHSET));}
  return results;
}

static fd_lisp apply_subst_rule(fd_lisp pat,fd_lisp string,fd_lispenv env)
{
  fd_lisp answers=FD_EMPTY_CHOICE;
  int size=fd_strlen(string); fd_u8char *sdata=fd_strdata(string);
  if ((FD_PAIRP(pat)) && (FD_STRINGP(FD_CAR(pat)))) {
    fd_lisp suffix=fd_get_arg(pat,1,FD_VOID);
    fd_lisp replacement=fd_get_arg(pat,0,FD_VOID);
    int offset=size-fd_strlen(suffix);
    if (offset <= 0) return FD_EMPTY_CHOICE;
    else if (strcasecmp(fd_strdata(suffix),sdata+offset) == 0) {
      if ((FD_STRINGP(replacement)) && (FD_STRING_LENGTH(replacement)>0)) {
	int new_size=size-fd_strlen(suffix)+FD_STRING_LENGTH(replacement);
	fd_u8char *new_string=fd_malloc(new_size+1);
	strncpy(new_string,sdata,offset); new_string[offset]=0;
	strcat(new_string+offset,FD_STRING_DATA(replacement));
	return fd_init_string(new_string,new_size);}
      else return fd_make_substring(sdata,sdata+offset);}
    else return FD_EMPTY_CHOICE;}
  else {
    fd_lisp answer=fd_text_extract
      (pat,env,fd_strdata(string),0,size,
       (MATCH_IGNORE_CASE|MATCH_DO_BINDINGS));
    FD_DO_CHOICES(each,answer) {
      if (!(FD_PAIRP(each)))
	fd_type_error(_("invalid matcher result"),each);
      else if (fd_lisp2int(FD_CAR(each))==size) {
	struct FD_STRING_STREAM ss; 
	FD_INITIALIZE_STRING_STREAM(&ss,size*2);
	do_substitution(FD_CDR(each),&ss,env);
	FD_ADD_TO_CHOICE(answers,fd_init_string(ss.ptr,ss.size));}}
    END_FD_DO_CHOICES;
    fd_decref(answer);
    return answers;}
}

static fd_lisp test_root(fd_lisp string,fd_lisp roots)
{
  fd_lisp answer=FD_VOID;
  FD_DO_CHOICES(root,roots)
    if (FD_STRINGP(root)) {
      if (FD_LISP_EQUAL(root,string)) {answer=root; break;}}
    else if (FD_PRIM_TYPEP(root,hashset_type)) {
      if (fd_hashset_get((fd_hashset)FD_CPTR_DATA(root),string)) {
	answer=string; break;}}
    else if (FD_PRIM_TYPEP(root,hashtable_type)) {
      fd_lisp entry=
	fd_hashtable_get((fd_hashtable)FD_CPTR_DATA(root),string,FD_VOID);
      if (FD_PAIRP(entry)) {
	answer=fd_incref(FD_CAR(entry)); break;}
      else if (FD_STRINGP(entry)) {
	answer=fd_incref(entry); break;}
      fd_decref(entry);}
    else if (FD_PRIM_TYPEP(root,index_type)) {
      fd_lisp entry=
	fd_index_get((fd_index)FD_CPTR_DATA(root),string,FD_VOID);
      if (FD_PAIRP(entry)) {
	answer=fd_incref(FD_CAR(entry)); break;}
      else if (FD_STRINGP(entry)) {
	answer=fd_incref(entry); break;}
      fd_decref(entry);}
    else if (FD_PRIM_TYPEP(root,sproc_type)) {
      fd_lisp argl=FD_MAKE_LIST1(fd_incref(string));
      fd_lisp v=fd_apply(root,argl);
      fd_decref(argl);
      if (FD_STRINGP(v)) {answer=v; break;}
      else if (FD_PAIRP(v)) {
	answer=fd_incref(FD_CAR(v)); fd_decref(v); break;}
      else fd_raise_exception(_("Invalid MORPHRULE root"));}
  END_FD_DO_CHOICES;
  return answer;
}

static fd_lisp morphrule_lexpr(fd_lisp args)
{
  fd_lisp strings, tag, rules, roots, results=FD_EMPTY_CHOICE;
  fd_get_args("MORPHRULE",args,&strings,FD_VOID,&tag,FD_VOID,
	      &rules,FD_VOID,&roots,FD_VOID,
	      NULL);
  {FD_DO_CHOICES(string,strings)
     if (FD_STRINGP(string)) {
       FD_DO_CHOICES(rule,rules) {
	 fd_lisp rewrites, result;
	 rewrites=apply_subst_rule(rule,string,NULL);
	 {FD_DO_CHOICES(rewrite,rewrites) {
	   if (FD_STRINGP(rewrite)) {
	     fd_lisp root=test_root(rewrite,roots);
	     if (!(FD_VOIDP(root))) {
	       if (FD_SYMBOLP(tag))
		 result=FD_MAKE_LIST(2,fd_incref(root),fd_incref(tag));
	       else result=fd_incref(root);
	       FD_ADD_TO_CHOICE(results,result);}}}
	 END_FD_DO_CHOICES;
	 fd_decref(rewrites);}}
       END_FD_DO_CHOICES;}
     else fd_type_error("MORPHRULE: arg must be string",string);
  END_FD_DO_CHOICES;}
  return results;
}


/** Support for date and time parsing **/

fd_hashtable month_map;

static char *initial_months[]={
  "jan","january",NULL,
  "feb","february",NULL,
  "mar","march",NULL,
  "apr","april",NULL,
  "may",NULL,
  "jun","june",NULL,
  "jul","july",NULL,
  "aug","august",NULL,
  "sep","sept","september",NULL,
  "oct","october",NULL,
  "nov","november",NULL,
  "dec","december",NULL,
  NULL};

static int lookup_month_name(fd_u8char *string)
{
  fd_lisp as_string=fd_lower_string(string);
  fd_lisp answer=fd_hashtable_get(month_map,as_string,FD_VOID);
  fd_decref(as_string);
  if (FD_FIXNUMP(answer)) return FD_FIXLISP(answer);
  fd_decref(answer);
  return -1;
}

static struct FD_TEXT_ENCODING *ascii_encoding;

static fd_lisp lowered_string(char *string)
{
  struct FD_TEXT_ENCODING *enc=fd_get_default_encoding();
  fd_u8char *converted; fd_lisp lowered;
  if (enc != ascii_encoding)
    converted=fd_make_utf8(string,NULL,enc);
  else converted=fd_make_utf8(string,NULL,fd_get_encoding("LATIN-1"));
  lowered=fd_lower_string(converted); fd_xfree(converted);
  return lowered;
}

static void init_month_map()
{
  struct tm _now, *now=&_now;
  char **scan=initial_months; int i=1;
  month_map=fd_make_hashtable(32);
  while (*scan) {
    fd_lisp str=fd_make_string(*scan);
    fd_lisp value=FD_LISPFIX(i);
    fd_hashtable_set(month_map,str,value); scan++;
    if ((*scan) == NULL)
      if (scan[1] != NULL) {i++; scan++;}}
  /* This gets locale specific names */
  fd_localtime(now,time(NULL));
  i=0; while (i < 12) {
    char buf[64]; 
    /* We set the year to 80 since that's what mktime usually likes */
    now->tm_mon=i; now->tm_mday=2; now->tm_year=80;
    strftime(buf,64,"%b",now);
    fd_hashtable_set(month_map,lowered_string(buf),FD_LISPFIX(i+1));
    strftime(buf,64,"%B",now);
    fd_hashtable_set(month_map,lowered_string(buf),FD_LISPFIX(i+1));
    i++;}
}

static int american_date_format=0;

static void determine_date_style()
{
  char buf[64]; struct tm tmp;
  memset(&tmp,0,sizeof(struct tm));
  tmp.tm_mon=10; tmp.tm_mday=3; tmp.tm_year=75; mktime(&tmp);
  strftime(buf,64,"%x",&tmp);
  if ((strcmp(buf,"11/03/75") == 0) || (strcmp(buf,"11/3/75") == 0) ||
      (strcmp(buf,"11/03/1975") == 0) || (strcmp(buf,"11/3/1975") == 0))
    american_date_format=1; else american_date_format=0;
}

/* Determining time zone */

void parse_tz_token(fd_u8char *s,int *tzoff)
{
  /* Handle time zone offsets */
  if ((strchr(s,'+')) || (strchr(s,'-'))) {
    char *off=strchr(s,'+');
    if (off == NULL) off=strchr(s,'-');
    if ((*off == '+') || (*off == '-')) {
      int bound, hroff=0, moff=0; 
      bound=sscanf(off+1,"%d:%d",&hroff,&moff);
      *tzoff=((*off == '-') ? -1 : 1)*((hroff*3600)+moff*60);
      return;}}
  else { /* Handle literal timezones */
    int t1=fd_parse_tzspec(s,100);
    if (t1 != 100) *tzoff=t1;}
}

void determine_timezone(fd_u8char *string,int *tzoff)
{
  fd_u8char *space;
  while ((space=token_end(string)) != NULL) {
    if ((space-string) == 0) {}
    else if ((space-string) > 31) {}
    else {
      char buf[32]; strncpy(buf,string,space-string);
      buf[space-string]=0; parse_tz_token(buf,tzoff);}
    string=token_start(space);}
}

/* Parsing time tokens */

void parse_time_token(char *s,struct FD_XTIME *xtime,int american)
{
  enum date_type { slash, dash, dot, none } sep=none;
  int hrs=0, mins=0, secs=0, time_bound=0, just_num=0, month=0;
  int date_first=0, date_second=0, date_third=0, date_length;
  struct tm *tptr=&(xtime->tptr); int *tzoff=&(xtime->tzoff);
  /* Handle time of day references */
  time_bound=sscanf(s,"%d:%d:%d",&hrs,&mins,&secs);
  if (time_bound > 1) {
    tptr->tm_hour=hrs; tptr->tm_min=mins;
    if (time_bound == 3) tptr->tm_sec=secs;
    if (xtime->precision < day+time_bound)
      xtime->precision=day+time_bound;
    return;}
  /* Handle AM and PM */
  if ((strcasecmp(s,"AM")) == 0) return;
  else if ((strcasecmp(s,"PM")) == 0) {
    if (tptr->tm_hour < 12) tptr->tm_hour=tptr->tm_hour+12;
    return;}
  /* Handle month names */
  if ((month=lookup_month_name(s)) >= 0) {
    tptr->tm_mon=month-1;
    if (xtime->precision == year) xtime->precision=month;
    return;}
  /* Handle dates (some formats) */
  date_length=sscanf(s,"%d/%d/%d",&date_first,&date_second,&date_third);
  if (date_length > 1) sep=slash;
  if (sep == none) {
    date_length=sscanf(s,"%d-%d-%d",&date_first,&date_second,&date_third);
    if (date_length > 1) sep=dash;}
  if (sep == none) {
    date_length=sscanf(s,"%d.%d.%d",&date_first,&date_second,&date_third);
    if (date_length > 1) sep=dot;}
  if (sep != none) {
    if (xtime->precision < date_length) xtime->precision=date_length;
    if (date_length == 2) {
      /* Two formats: mon/year and mon/date */
      tptr->tm_mon=date_first-1;
      if (date_second > 31) tptr->tm_year=date_first;
      else tptr->tm_mday=date_second;
      return;}
    else if (date_first > 31) { /* Assume year/month/date */
      tptr->tm_year=date_first;
      tptr->tm_mon=date_second-1;
      tptr->tm_mday=date_third;
      return;}
    else if (date_third > 31) {
      tptr->tm_year=date_third;
      if ((american) || (date_second > 12)) {
	tptr->tm_mon=date_first-1;
	tptr->tm_mday=date_second;}
      else {
	tptr->tm_mon=date_second-1;
	tptr->tm_mday=date_first;}
      return;}
    else {}}
  /* Handle just numbers */
  if (strlen(s) == 8) {
    int year, month, date;
    int len=sscanf(s,"%4d%2d%2d",&year,&month,&date);
    if (year > 1900) {
      tptr->tm_year=year; tptr->tm_mon=month-1; tptr->tm_mday=date;
      return;}}
  {fd_u8char *scan=s; int c=fd_sgetc(&scan);
   while ((c>0) && (fd_isdigit(c))) c=fd_sgetc(&scan);
   if (c >= 0) return;}
  just_num=strtol(s,NULL,10);
  if (just_num > 31)
    if (just_num > 1900) tptr->tm_year=just_num;
    else if (just_num < 100) tptr->tm_year=just_num+1900;
    else tptr->tm_year=just_num;
  else {
    if (xtime->precision <= month) xtime->precision=day;
    tptr->tm_mday=just_num;}
}


/** Date and Time Parsing **/

EXPORTED
fd_lisp fd_parse_timestring(fd_u8char *string,int american,fd_lisp base)
{
  struct FD_XTIME xtime; fd_u8char *space; int tzoff;
  fd_init_xtime(&xtime); xtime.precision=year;
  if (strchr(string,' ') == NULL) {
    time_t t; /* May not be used */
    if (strlen(string) == 8) {
      int year, month, date; 
      int len=sscanf(string,"%4d%2d%2d",&year,&month,&date);
      if (len != 3) {}
      else if (year > 1900) {
	struct tm *tptr=&(xtime.tptr);
	xtime.precision=len; xtime.nsecs=0;
	tptr->tm_year=year; tptr->tm_mon=month-1; tptr->tm_mday=date;
	xtime.secs=fd_mktime(tptr,0);
	return fd_xtime_to_timestamp(&xtime);}}
    t=fd_iso8601_to_xtime(string,&xtime);
    if (t>=0) return fd_xtime_to_timestamp(&xtime);}
  /* Get the base for filling in fields */
  if (FD_LRECORD_TYPEP(base,timestamp_symbol))
    fd_timestamp_to_xtime(base,&xtime);
  else fd_get_now(&xtime);
  xtime.precision=year;
  determine_timezone(string,&(xtime.tzoff));
  while ((space=token_end(string)) != NULL) {
    if ((space-string) == 0) {}
    else if ((space-string) > 31) {}
    else {
      char buf[32]; strncpy(buf,string,space-string);
      buf[space-string]=0;
      parse_time_token(buf,&xtime,american);}
    string=token_start(space);}
  if (string)
    parse_time_token(string,&xtime,american);
  if (xtime.precision < month) xtime.tptr.tm_mon=0;
  if (xtime.precision < day) xtime.tptr.tm_mday=1;
  if (xtime.precision < hour) xtime.tptr.tm_hour=0;
  if (xtime.precision < minute) xtime.tptr.tm_min=0;
  if (xtime.precision < minute) xtime.tptr.tm_sec=0;
  xtime.secs=fd_mktime(&(xtime.tptr),xtime.tzoff);
  return fd_xtime_to_timestamp(&xtime);
}
      
static fd_lisp lisp_parse_timestring_lexpr(fd_lisp args)
{
  fd_lisp time, base;
  fd_get_args("PARSE-TIMESTRING",args,&time,FD_VOID,&base,FD_FALSE,NULL);
  return fd_parse_timestring(fd_strdata(time),american_date_format,base);
}


/** Customizing parsing from FDScript **/


/** Record Streams **/

struct FD_RECORD_STREAM {
  char *fname; FILE *in; fd_xfile xfile;
  struct FD_STRING_STREAM ss;
  int offset, block_size, at_eof, n_records;
  char *read_buf;
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif
  fd_lisp pattern;};

static fd_lisp open_record_stream(fd_lisp args)
{
  fd_lisp filename=fd_get_arg(args,0,FD_VOID);
  fd_lisp pattern=fd_get_arg(args,1,FD_FALSE);
  fd_lisp encoding=fd_get_arg(args,2,FD_FALSE);
  fd_lisp bsize=fd_get_arg(args,3,FD_LISPFIX(256));
  struct FD_RECORD_STREAM *r=fd_xmalloc(sizeof(struct FD_RECORD_STREAM));
  int block_size;
  if (!(FD_STRINGP(filename)))
    fd_type_error(_("not a filestring"),filename);
  r->in=fd_fopen(FD_STRING_DATA(filename),"rb"); 
  if (r->in == NULL)
    fd_raise_detailed_exception(fd_FileOpenFailed,FD_STRING_DATA(filename));
  if (FD_FALSEP(encoding)) fd_set_file_encoding(r->in,NULL);
  else if (FD_STRINGP(encoding))
    fd_set_file_encoding(r->in,FD_STRING_DATA(encoding));
  else if (FD_SYMBOLP(encoding))
    fd_set_file_encoding(r->in,FD_SYMBOL_NAME(encoding));
  if (FD_FALSEP(bsize)) block_size=128;
  else if (FD_FIXNUMP(bsize)) block_size=FD_FIXLISP(bsize);
  else fd_type_error(_("block size is not a fixnum"),bsize);
  r->xfile=fd_get_xfile(r->in); r->fname=fd_strdup(FD_STRING_DATA(filename));
  r->block_size=block_size; r->offset=0; r->at_eof=0; r->n_records=0;
  FD_INITIALIZE_STRING_STREAM(&(r->ss),block_size*2);
  r->read_buf=fd_malloc(block_size);
  r->pattern=fd_copy_lisp(pattern);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(r->lock));
#endif
  return fd_make_cptr(record_stream_type,(void *)r);
}

static void free_record_stream(fd_lisp x)
{
  struct FD_RECORD_STREAM *r=FD_CPTR_DATA(x);
  if (r) {
    fd_fclose(r->in); fd_decref(r->pattern);
    fd_xfree(r->ss.ptr); fd_free(r->read_buf,r->block_size);
    fd_xfree(r);}
}

static void print_record_stream(fd_lisp x,fd_string_stream s)
{
  struct FD_RECORD_STREAM *r=(struct FD_RECORD_STREAM *)FD_CPTR_DATA(x);
  fd_printf(s,"[#RECORD-STREAM %s by %q]",r->fname,r->pattern);
}

static void fill_record_stream(struct FD_RECORD_STREAM *r)
{
  int bytes_read=0, chars_read=0, block_size=r->block_size;
  struct FD_XFILE *xfile=r->xfile;
  struct FD_STRING_STREAM *ss=&(r->ss);
  /* Don't even try at EOF */ if (r->at_eof) return; 
  /* Discard the data you've already read: */
  memmove(ss->ptr,ss->ptr+r->offset,ss->size-r->offset);
  ss->size=ss->size-r->offset; ss->ptr[ss->size]=0; r->offset=0;
  /* Read block_size new characters */
  bytes_read=fread(r->read_buf,1,r->block_size,xfile->f);
  if ((bytes_read == 0) && (feof(xfile->f))) r->at_eof=1;
  chars_read=fd_write_utf8
    (ss,r->read_buf,r->read_buf+bytes_read,xfile->encoding);
}

static fd_lisp read_record_internal(struct FD_RECORD_STREAM *r)
{
  int next_record=
    fd_text_search(r->pattern,NULL,r->ss.ptr,r->offset,r->ss.size,0);
  if ((next_record < 0) || (next_record == r->ss.size))
    if (r->at_eof) return FD_EOF_OBJECT;
    else {
      fill_record_stream(r);
      return FD_EMPTY_CHOICE;}
  else {
    fd_lisp matches=
      fd_text_matcher(r->pattern,NULL,r->ss.ptr,next_record,r->ss.size,0);
    int lmatch=get_longest(matches);
    if (lmatch <= 0) {
      fill_record_stream(r);
      return FD_EMPTY_CHOICE;}
    if ((r->at_eof) || (lmatch < r->ss.size)) {
      r->offset=lmatch; 
      return fd_make_substring(r->ss.ptr+next_record,r->ss.ptr+lmatch);}
    else {
      fill_record_stream(r);
      return FD_EMPTY_CHOICE;}}
}

static fd_lisp read_record(struct FD_RECORD_STREAM *r)
{
  fd_lisp answer;
  fd_lock_mutex(&(r->lock));
  answer=read_record_internal(r);
  while (FD_EMPTYP(answer)) answer=read_record_internal(r);
  r->n_records++;
  fd_unlock_mutex(&(r->lock));
  return answer;
}


static fd_lisp read_spacing_internal(struct FD_RECORD_STREAM *r)
{
  if (r->offset < 0) return FD_EOF_OBJECT;
  else {
    int next_record=
      fd_text_search(r->pattern,NULL,r->ss.ptr,r->offset,r->ss.size,0);
    if (next_record < 0)
      if (r->at_eof) {
	fd_lisp answer=
	  fd_make_substring(r->ss.ptr+r->offset,r->ss.ptr+r->ss.size);
	r->offset=next_record;
	return answer;}
      else {
	fill_record_stream(r);
	return read_spacing_internal(r);}
    else {
      fd_lisp answer=
	fd_make_substring(r->ss.ptr+r->offset,r->ss.ptr+next_record);
      r->offset=next_record;
      return answer;}}
}

static fd_lisp read_spacing(struct FD_RECORD_STREAM *r)
{
  fd_lisp answer;
  fd_lock_mutex(&(r->lock));
  answer=read_spacing_internal(r);
  fd_unlock_mutex(&(r->lock));
  return answer;
}

static fd_lisp lisp_read_record_cproc(fd_lisp stream)
{
  if (FD_PRIM_TYPEP(stream,record_stream_type))
    return read_record((struct FD_RECORD_STREAM *)FD_CPTR_DATA(stream));
  else fd_type_error(_("not a record stream"),stream);
}

static fd_lisp lisp_read_spacing_cproc(fd_lisp stream)
{
  if (FD_PRIM_TYPEP(stream,record_stream_type))
    return read_spacing((struct FD_RECORD_STREAM *)FD_CPTR_DATA(stream));
  else fd_type_error(_("not a record stream"),stream);
}

static fd_lisp lisp_record_stream_data(fd_lisp stream)
{
  if (FD_PRIM_TYPEP(stream,record_stream_type)) {
    struct FD_RECORD_STREAM *rs=
      (struct FD_RECORD_STREAM *)FD_CPTR_DATA(stream);
    FILE *f=rs->in; int pos=ftell(f), end;
    struct stat status;
    int stat_result=fstat(fileno(f),&status);
    if (stat_result < 0) end=-1; else end=status.st_size;
    f=rs->in; pos=ftell(f);
    return FD_MAKE_LIST(3,FD_LISPFIX(pos),
			FD_LISPFIX(end),
			FD_LISPFIX(rs->n_records));}
  else fd_type_error(_("not a record stream"),stream);
}


/* record 2 frame */

static fd_lisp string_symbol;

static fd_lisp apply_parser(fd_lisp recval,fd_lisp parser)
{
  if (FD_FALSEP(parser)) return fd_incref(recval);
  else if (!(FD_STRINGP(recval))) return fd_incref(recval);
  else if (FD_LISP_EQ(parser,string_symbol))
    if (FD_STRING_LENGTH(recval) == 0) return FD_EMPTY_CHOICE;
    else return fd_incref(recval);
  else if (FD_TRUEP(parser))
    if (FD_STRING_LENGTH(recval) == 0) return FD_EMPTY_CHOICE;
    else return fd_parse_string(FD_STRING_DATA(recval));
  else if (FD_APPLICABLEP(parser)) 
    return fd_lisp_call(parser,recval);
  else return fd_incref(recval);
}

static fd_lisp lisp_record2frame_lexpr(fd_lisp args)
{
  fd_lisp pool_arg=fd_get_arg(args,0,FD_VOID);
  fd_lisp record=fd_get_arg(args,1,FD_VOID);
  fd_lisp slotids=fd_get_arg(args,2,FD_VOID);
  fd_lisp parsers=fd_get_arg(args,3,FD_FALSE);
  int record_len=fd_seq_length(record), slotids_len=fd_seq_length(record);
  int result_len=((record_len>slotids_len) ? slotids_len : record_len);
  int n_parsers=
    (((FD_PRIM_TYPEP(parsers,vector_type)) ||
      (FD_PRIM_TYPEP(parsers,pair_type))) ?
     (fd_seq_length(parsers)) : (-1));
  fd_lisp frame=fd_make_slotmap(result_len);
  int i=0; while (i < result_len) {
    fd_lisp slotid=fd_seq_elt(slotids,i);
    fd_lisp recval=fd_seq_elt(record,i), slotval;
    if (FD_PAIRP(slotid)) {
      fd_lisp parser=fd_car_noref(FD_CDR(slotid));
      slotval=apply_parser(recval,parser);}
    else if ((n_parsers > 0) && (i < n_parsers)) {
      fd_lisp parser=fd_seq_elt(parsers,i);
      slotval=apply_parser(recval,parser);
      fd_decref(parser);}
    else slotval=apply_parser(recval,parsers);
    if (!(FD_EMPTYP(slotval)))
      if (FD_PAIRP(slotid))
	fd_prim_set(frame,FD_CAR(slotid),slotval);
      else fd_prim_set(frame,slotid,slotval);
    fd_decref(slotid); fd_decref(recval); fd_decref(slotval);
    i++;}
  if (FD_FALSEP(pool_arg)) return frame;
  else {
    fd_pool p=fd_interpret_pool(pool_arg);
    fd_lisp oid=fd_new_oid(p);
    fd_set_oid_value(oid,frame);
    fd_decref(frame);
    return oid;}
}

static fd_lisp lisp_columnize_cproc(fd_lisp string,fd_lisp cols)
{
  int i=0, coln=0, len=fd_vector_length(cols), slen=fd_strlen(string), pos=0;
  fd_u8char *scan=fd_strdata(string), *start=NULL, *limit=scan+slen;
  fd_lisp vec=fd_make_vector(len);
  while ((i < len) && (scan < limit)) {
    int col=fd_fixlisp(FD_VECTOR_REF(cols,i));
    if (col >= slen) break;
    while ((pos < col-1) && (scan < limit)) {fd_sgetc(&scan); pos++;}
    if (start) {
      FD_VECTOR_SET(vec,coln,fd_make_substring(start,scan)); coln++;}
    start=scan; i++;}
  FD_VECTOR_SET(vec,coln,fd_make_substring(start,limit)); coln++;
  while (coln < len) {
    FD_VECTOR_SET(vec,i,FD_EMPTY_CHOICE); coln++;}
  return vec;
}

/** Interface to proper name extraction **/

static fd_lisp get_names_lexpr(fd_lisp args)
{
  fd_lisp result=FD_EMPTY_CHOICE, strings, title_abbrevs;
  fd_lisp stop_words, name_suffixes;
  fd_hashset stops, abbrevs, suffixes;
  int free_stops=0, free_abbrevs=0, free_suffixes=0;
  fd_get_args("GETNAMES",args,&strings,FD_VOID,
	      &title_abbrevs,FD_FALSE,&stop_words,FD_FALSE,
	      &name_suffixes,FD_FALSE,
	      NULL);
  if (FD_FALSEP(title_abbrevs)) abbrevs=fd_english_title_abbrevs;
  else if (FD_PRIM_TYPEP(title_abbrevs,hashset_type)) 
    abbrevs=(fd_hashset)FD_CPTR_DATA(title_abbrevs);
  else {
    fd_hashset temp=fd_make_hashset(128);
    FD_DO_CHOICES(abv,title_abbrevs) {fd_hashset_add(temp,abv);}
    END_FD_DO_CHOICES;
    abbrevs=temp; free_abbrevs=1;}
  if (FD_FALSEP(stop_words)) stops=fd_english_stop_words;
  else if (FD_PRIM_TYPEP(stop_words,hashset_type)) 
    stops=(fd_hashset)FD_CPTR_DATA(stop_words);
  else {
    fd_hashset temp=fd_make_hashset(128);
    FD_DO_CHOICES(sw,stop_words) {fd_hashset_add(temp,sw);}
    END_FD_DO_CHOICES;
    stops=temp; free_stops=1;}
  if (FD_FALSEP(name_suffixes)) suffixes=fd_name_suffixes;
  else if (FD_PRIM_TYPEP(name_suffixes,hashset_type)) 
    suffixes=(fd_hashset)FD_CPTR_DATA(name_suffixes);
  else {
    fd_hashset temp=fd_make_hashset(128);
    FD_DO_CHOICES(suff,name_suffixes) {fd_hashset_add(temp,suff);}
    END_FD_DO_CHOICES;
    suffixes=temp; free_suffixes=1;}
  {
    FD_DO_CHOICES(string,strings) {
      fd_extract_proper_names
	(fd_strdata(string),&result,abbrevs,stops,suffixes);}
    END_FD_DO_CHOICES;}
  if (free_stops) {
    fd_free_hashset(stops); fd_free(stops,sizeof(struct FD_HASHSET));}
  if (free_abbrevs) {
    fd_free_hashset(abbrevs); fd_free(abbrevs,sizeof(struct FD_HASHSET));}
  if (free_suffixes) {
    fd_free_hashset(suffixes); fd_free(suffixes,sizeof(struct FD_HASHSET));}
  return result;
}

static fd_lisp lisp_add_refstop_cproc(fd_lisp rstop)
{
  fd_hashset_add(fd_english_stop_words,rstop);
  return FD_VOID;
}

static fd_lisp lisp_add_abbrev_cproc(fd_lisp abb)
{
  fd_hashset_add(fd_english_title_abbrevs,abb);
  return FD_VOID;
}

/** String canonicalization (case, diacritics, etc)  **/

static fd_lisp stdstring_cproc(fd_lisp string)
{
  int len=fd_strlen(string), probe;
  if (len == 0) return fd_incref(string);
  else {
    fd_u8char *scan=fd_strdata(string), *limit=scan+len;
    struct FD_STRING_STREAM os; 
    FD_INITIALIZE_STRING_STREAM(&os,fd_strlen(string)+2);
    probe=fd_sgetc(&scan);
    /* We skip over any initial whitespace */
    if (fd_isspace(probe)) {
      fd_u8char *next=scan; int ch;
      scan=next; ch=fd_sgetc(&next);
      while ((next<limit) && (fd_isspace(ch))) {
	scan=next; ch=fd_sgetc(&next);}}
    else scan=fd_strdata(string);
    /* Now scan the string */
    while (scan < limit) {
      int ch=fd_sgetc(&scan);
      if ((fd_isspace(ch))) {
	/* If we run into whitespace, write one space and skip the rest. */
	fd_sputc(&os,' ');
	while ((scan<limit) && (fd_isspace(ch))) ch=fd_sgetc(&scan);
	/* If we go over the end while skipping the rest, erase the trailing space
	   and break. */
	if (scan >= limit) {
	  os.ptr[os.size-1]=NUL; os.size--;
	  break;}}
      /* Drop modifiers */
      if (fd_ismodifier(ch)) {}
      /* ASCII won't have decompositions */
      else if (ch < 0x80) fd_sputc(&os,fd_tolower(ch));
      else {
	/* Othwerwise, try decomposing and use the first
	   character of the decomposition. */
	fd_u8char *decomp=fd_decompose_char(ch);
	if (decomp) {
	  int c2=fd_sgetc(&decomp);
	  fd_sputc(&os,fd_tolower(c2));}
	else fd_sputc(&os,fd_tolower(ch));}}
    return fd_stream_string(&os);}
}

static fd_lisp stdspace_cproc(fd_lisp string)
{
  int len=fd_strlen(string), probe;
  if (len == 0) return fd_incref(string);
  else {
    fd_u8char *start=fd_strdata(string), *scan=start, *limit=scan+len;
    struct FD_STRING_STREAM os; int copying=0, probe=fd_sgetc(&scan);
    if (fd_isspace(probe)) {
      while (fd_isspace(probe)) {
	start=scan; probe=fd_sgetc(&scan);}
      FD_INITIALIZE_STRING_STREAM(&os,len+2); copying=1;}
    else scan=start;
    while (scan < limit) {
      fd_u8char *here=scan; int probe=fd_sgetc(&scan);
      if (fd_isspace(probe)) {
	if (copying == 0) {
	  FD_INITIALIZE_STRING_STREAM(&os,len+2); copying=1;}
	fd_sputn(&os,start,(here-start)); start=here;
	while ((scan<=limit) && (fd_isspace(probe))) {
	  here=scan; probe=fd_sgetc(&scan);}
	if (probe>=0) fd_sputc(&os,' ');
	if (probe > 0) fd_sputc(&os,probe); start=scan;}}
    if (copying) {
      fd_sputs(&os,start);
      return fd_stream_string(&os);}
    else return fd_incref(string);}
}


/** Initialization **/

char *time_suffixes_init[]={
  "PM","AM","EST","EDT","PST","PDT","CET","GMT",NULL};
char *time_words_init[]={
  "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
  "SEP","SEPT","OCT","NOV","DEC",
  "JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE",
  "JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
  "SUNDAY","MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY",
  "SUN","MON","TUE","TUES","WED","THR","THURS","FRI","SAT",
  NULL};

/* New stuff */

void initialize_match_c(void);
void initialize_getnames_c(void);

EXPORTED
void fd_initialize_fdtext()
{
  fd_lispenv menv=fd_make_module();
  fd_texttools_env=menv;
  
  ascii_encoding=fd_get_encoding("ASCII");

  timestamp_symbol=fd_make_symbol("TIMESTAMP0");

  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(record_stream_type);
   r->gc_fcn=free_record_stream; r->print_fcn=print_record_stream;}
  
  label_symbol=fd_make_symbol("LABEL");
  subst_symbol=fd_make_symbol("SUBST");
  source_symbol=fd_make_symbol("SOURCE");
  string_symbol=fd_make_symbol("STRING");

  fd_add_cproc(menv,"EMPTY-STRING?",1,lisp_string_emptyp_cproc);
  fd_add_cproc(menv,"LOWERCASE?",1,lisp_string_lowercasep_cproc);
  fd_add_cproc(menv,"UPPERCASE?",1,lisp_string_uppercasep_cproc);
  fd_add_cproc(menv,"NUMERIC?",1,lisp_string_numericp_cproc);
  fd_add_cproc(menv,"CAPITALIZED?",1,lisp_string_capitalizedp_cproc);
  fd_add_cproc(menv,"MULTI-LINE?",1,lisp_string_multilinep_cproc);
  fd_add_cproc(menv,"WHITESPACE%",1,lisp_string_whitespace_percentage_cproc);
  fd_add_cproc(menv,"ALPHABETIC%",1,lisp_string_alphabetic_percentage_cproc);
  fd_add_cproc(menv,"ALPHANUMERIC%",1,lisp_string_alphanumeric_percentage_cproc);

  fd_add_cproc(menv,"STRING-SUBST",3,lisp_string_subst_cproc);
  fd_add_cproc(menv,"STRING-CAPITALIZE",1,lisp_string_capitalize_cproc);

  fd_add_cproc(menv,"GET-BIGRAMS",1,get_bigrams_cproc);
  fd_add_cproc(menv,"GET-TRIGRAMS",1,get_trigrams_cproc);

  fd_add_lexpr(menv,"RECORD->FRAME",FD_NORMAL_LEXPR,lisp_record2frame_lexpr);
  fd_add_cproc(menv,"COLUMNIZE",2,lisp_columnize_cproc);

  initialize_match_c();
  initialize_getnames_c();
  
  fd_add_special_form(menv,"TEXTLET",lisp_text_bind_handler);
  fd_add_cproc(menv,"MATCH->FRAME",3,lisp_text_frame_cproc);

  fd_add_special_form(menv,"TX-EXTRACT",lisp_text_extract_handler);
  fd_add_special_form(menv,"TX-SUBST",lisp_text_subst_handler);
  fd_add_special_form(menv,"TX-MATCHER",lisp_text_matcher_handler);
  fd_add_special_form(menv,"TX-MATCH",lisp_text_match_handler);
  fd_add_lexpr(menv,"TX-SEARCH",FD_NORMAL_LEXPR,lisp_text_search_lexpr);

  fd_add_special_form(menv,"TX-CLOSURE",tx_closure_handler);

  fd_add_restricted_lexpr
    ("OPEN-RECORD-STREAM",FD_ND_LEXPR,open_record_stream);
  fd_add_cproc(menv,"READ-RECORD",1,lisp_read_record_cproc);
  fd_add_cproc(menv,"READ-SPACING",1,lisp_read_spacing_cproc);
  fd_add_cproc(menv,"RECORD-STREAM-DATA",1,lisp_record_stream_data);

  fd_add_lexpr(menv,"GATHER",FD_ND_LEXPR,lisp_gather_lexpr);
  fd_add_lexpr(menv,"SEGMENT",FD_ND_LEXPR,lisp_segment_lexpr);
  fd_add_cproc(menv,"CSEGMENT",1,lisp_compound_segment_cproc);
  fd_add_lexpr(menv,"FRAGMENT",FD_ND_LEXPR,lisp_fragment_lexpr);
  fd_add_alias(menv,"TX-GATHER","GATHER");
  fd_add_alias(menv,"TX-SEGMENT","SEGMENT");
  fd_add_alias(menv,"TX-FRAGMENT","FRAGMENT");

  fd_add_cproc(menv,"GET-MAILIDS",1,lisp_extract_mail_ids_cproc);

  fd_add_cproc(menv,"ENGLISH-STEM",1,lisp_stem_english_word_cproc);

  fd_add_lexpr(menv,"SUFFIXRULE",FD_ND_LEXPR,suffixrule_lexpr);
  fd_add_lexpr(menv,"MORPHRULE",FD_ND_LEXPR,morphrule_lexpr);

  fd_add_cproc(menv,"STRIP-MARGIN",1,lisp_strip_margin_cproc);

  fd_add_cproc(menv,"ADD-ABBREV!",1,lisp_add_abbrev_cproc);
  fd_add_cproc(menv,"ADD-REFSTOP!",1,lisp_add_refstop_cproc);

  fd_add_lexpr(menv,"GET-NAMES",FD_ND_LEXPR,get_names_lexpr);

  fd_add_cproc(menv,"GET-TIMES",1,lisp_get_timepoints_cproc);
  fd_add_alias(menv,"REFPOINTS","GET-NAMES");

  fd_add_lexpr(menv,"PARSE-TIMESTRING",FD_NORMAL_LEXPR,
	       lisp_parse_timestring_lexpr);

  fd_add_cproc(menv,"MD5",1,fd_md5);

  fd_add_cproc(menv,"STDSPACE",1,stdspace_cproc);

  fd_add_cproc(menv,"STDSTRING",1,stdstring_cproc);

  init_month_map();
  determine_date_style();

  {
    char **scan;
    time_suffixes=fd_make_hashset(100);
    time_words=fd_make_hashset(100);
    scan=time_suffixes_init; while (*scan) {
      fd_hashset_add(time_suffixes,fd_make_string(*scan)); scan++;}
    scan=time_words_init; while (*scan) {
      fd_hashset_add(time_words,fd_make_string(*scan)); scan++;}}

  /* !!! Note that the third arg shouldn't really be one, because of opening segmented files */
  fd_register_module("FDTEXT",menv,FD_SAFE_ENV,0);

  fd_register_source_file("text",__DATE__,vcid);
}







/* File specific stuff */

/* The CVS log for this file
   $Log: text.c,v $
   Revision 1.59  2005/01/14 16:48:50  haase
   Updated copyrights to 2005

   Revision 1.58  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.57  2004/07/19 16:57:15  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.56  2004/04/02 10:15:24  haase
   Record streams finally detect end of file

   Revision 1.55  2004/03/13 00:40:16  haase
   Added columnize primitive

   Revision 1.54  2004/03/08 10:36:50  haase
   Added type check to alpha% procedures

   Revision 1.53  2004/03/03 18:47:56  haase
   Added alphanumeric%

   Revision 1.52  2004/02/20 03:59:54  haase
   Further fix to attribute parsing and addition of STRING type for record->frame

   Revision 1.51  2004/02/18 20:43:51  haase
   Made new READLINE implementation return EOF

   Revision 1.50  2004/02/04 19:10:34  haase
   Fixes to record stream from recent UTF-8 repairs

   Revision 1.49  2004/02/03 16:20:42  haase
   Optimized readline for handling UTF-8, added fd_write_utf8 for transcoding buffers in memory

   Revision 1.48  2003/12/05 14:58:47  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.47  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.46  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.45  2003/09/07 18:23:23  haase
   Added src/text/xmleval.c for FDXML callouts

   Revision 1.44  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.43.2.5  2003/08/06 18:41:08  haase
   Updated copyright notices to 2003

   Revision 1.43.2.4  2003/08/05 21:16:22  haase
   Fixed stack growth on record reading

   Revision 1.43.2.3  2003/01/26 20:50:23  haase
   Misc. fixes, especially GC

   Revision 1.43.2.2  2002/09/26 02:03:38  haase
   Fixed collision between textlet bindings and global bindings by making textlet bindings always check and bind locally

   Revision 1.43.2.1  2002/09/24 19:09:25  haase
   Fixed error checking, nd-handling, etc for entity conversion

   Revision 1.43  2002/07/11 18:43:36  haase
   Added trigram/bigram extraction primitives

   Revision 1.42  2002/07/05 21:17:54  uid59704
   Made string-capitalize capitalize after . and dashes

   Revision 1.41  2002/07/05 11:20:03  haase
   Made string-capitalize capitalize after punctuation

   Revision 1.40  2002/07/03 02:30:07  haase
   Fixed stdspace to strip space prefix

   Revision 1.39  2002/07/01 14:50:48  haase
   More fixes to precision determination in timestring parsing

   Revision 1.38  2002/07/01 02:46:10  haase
   Fixed excess precision bug in fd_parse_timestring and fixed fencepost problem in STDSPACE

   Revision 1.37  2002/06/24 14:50:06  haase
   Added CSEGMENT to break at compound words

   Revision 1.36  2002/06/18 01:05:27  haase
   Fix hiccup in stdspace

   Revision 1.35  2002/06/02 20:54:14  haase
   Made STDSPACE imply string-trim

   Revision 1.34  2002/05/27 18:09:01  haase
   Fixed bug in entity conversion

   Revision 1.33  2002/05/07 08:03:40  haase
   Updated entity conversion to handle hashtable entity maps

   Revision 1.32  2002/05/01 21:46:32  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.31  2002/04/27 17:48:11  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.30  2002/04/20 20:46:23  haase
   Added STRING-CAPITALIZE

   Revision 1.29  2002/04/20 02:38:45  haase
   Fixed double GC problem with TEXTLET

   Revision 1.28  2002/04/20 01:52:33  haase
   Fixed textlet to bind non-deterministcally

   Revision 1.27  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.26  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.25  2002/04/17 00:30:23  haase
   src/text/text.c

   Revision 1.24  2002/04/08 16:39:45  haase
   Added more internal error checking to gather and related functions

   Revision 1.23  2002/04/04 04:38:13  haase
   Added STDSPACE

   Revision 1.22  2002/04/03 01:54:41  haase
   Fixed fd_get_args bugs

   Revision 1.21  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
