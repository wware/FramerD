/* txio.c
   This file implements reading and writing of Lisp data for FramerD.
   It is actually included several times in the file io.c, each
   time with different macro definitions to define different
   functions.  Admittedly, a kludge which would be easy with C++,
   but I'm trying to keep the cognitive overload in other places.

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

#ifdef SKIP_WHITESPACE
#undef SKIP_WHITESPACE
#endif

/*
  SKIP_WHITESPACE is DESIGNED to leave the first non-whitespace
  character in 'c', because it's inlined into the context of the
  parsers that use it, and those parsers use 'c' as their working
  current character store.
*/
#define SKIP_WHITESPACE(c,str) \
  while (((c=uni_getc(str)) > 0) && \
         ((fd_isspace(c)) || (c == ';'))) \
    if (c == ';') while (((c=uni_getc_raw(str)) >= 0) && (c != '\n')) {}
#define CHECK_EOF(c) if (c < 0) fd_raise_exception(fd_Unexpected_EOF)

static lisp parse_vector(TXSTREAM str);
static lisp parse_choice(TXSTREAM str);
static lisp parse_list(TXSTREAM str,u8char *prefix);
static lisp parse_slotmap(TXSTREAM str);

char *read_atom(TXSTREAM str,u8char *prefix)
{
  struct FD_STRING_STREAM ss;
  int c=uni_getc(str), protected=0;
  FD_INITIALIZE_STRING_STREAM(&ss,16);
  if (prefix) fd_sputs(&ss,prefix);
  while (!(c<=0)) {
    if (c == '\\') {
      int nextc=uni_getc(str);
      fd_sputc(&ss,nextc);  c=uni_getc(str);}
    else if (c == '|') {
      if (protected) protected=0; else protected=1;
      c=uni_getc(str);}
    else if ((protected == 0) && (issymbreak(c))) break;
    else if (protected) {
      fd_sputc(&ss,c); c=uni_getc(str);}
    else {
      fd_sputc(&ss,fd_toupper(c)); c=uni_getc(str);}}
  uni_ungetc(c,str);
  return ss.ptr;
}

DTYPES_EXPORT lisp lisp_parser(TXSTREAM str)
{
  int c; SKIP_WHITESPACE(c,str);
  if (c < 0) return (FD_EOF_OBJECT);
  else if (c == 0) fd_raise_exception(fd_Unexpected_NULL);
  else switch (c) {
  case '#': {
    switch ((c=uni_getc(str))) { 
    case EOF: fd_raise_exception(fd_Unexpected_EOF);
    case 0: fd_raise_exception(fd_Unexpected_NULL);
    case '(': return parse_vector(str);
    case '{': {
      lisp ch=parse_choice(str);
      if (FD_EMPTYP(ch)) return FD_QUOTED_EMPTY_CHOICE;
      else {
	RETURN_LISP(quoted_choice_type,choice,PTR_DATA(ch,choice));}}
    case '[': return parse_slotmap(str);
    case 'T': case 't': return (FD_TRUE);
    case 'F': case 'f': return (FD_FALSE);
    case 'Z': {
      fd_lisp string=lisp_parser(str);
      fd_lisp zstring=fd_zify_string(FD_PTR_DATA(string,string));
      fd_decref(string);
      return zstring;}
    case '@': {
      char *ref=read_atom(str,NULL); int c;
      lisp oid=parse_oid(ref); fd_xfree(ref);
      c=uni_getc(str); uni_ungetc(c,str);
      if (can_start_oid_tag(c)) {
	lisp ignored_tag=lisp_parser(str);
	decref(ignored_tag);}
      return oid;}
    case '?': return (FD_VOID);
    case '\\': {
      char name[32]; int c=uni_getc(str), i=0;
      if (c >= 0x80) return fd_make_character(c);
      else if (c == 'u') {
	int i=0; while (i < 4) {
	  int c=uni_getc(str);
	  if ((c<0x80) && (isxdigit(c))) name[i++]=c;
	  else {
	    memmove(name+1,name,i); name[0]='"';
	    name[i+1]=c; name[i+2]='"'; name[i+3]=NUL;
	    fd_raise_detailed_exception(fd_InvalidUnicodeEscape,name);}}
	name[4]='\0'; sscanf(name,"%4x",&c);
	return fd_make_character(c);}
      else if (c == 'U') {
	int i=0; while (i < 8) {
	  int c=uni_getc(str);
	  if ((c<0x80) && (isxdigit(c))) name[i++]=c;
	  else {
	    memmove(name+1,name,i); name[0]='"';
	    name[i+1]=c; name[i+2]='"'; name[i+3]=NUL;
	    fd_raise_detailed_exception(fd_InvalidUnicodeEscape,name);}}
	name[9]='\0'; c=strtol(name,NULL,16);
	return fd_make_character(c);}
      while ((!(issymbreak(c))) && (i < 32)) {name[i++]=c; c=uni_getc(str);}
      if (i == 0) return fd_make_character(c);
      else if (i == 1) {
	uni_ungetc(c,str);
	return fd_make_character(name[0]);}
      else if (i >= 32)
	fd_raise_detailed_exception(fd_ParseError,"invalid character constant");
      else {
	uni_ungetc(c,str); name[i]='\0';
	return interpret_character_name(name);}}
    case 'X': case 'x': {
      char *digits=read_atom(str,NULL);
      int num=strtol(digits,NULL,16);
      if (errno)
	if (bignum_parser != NULL) {
	  lisp answer=bignum_parser(digits,16);
	  fd_xfree(digits); CLEAR_ERR(); return answer;}
	else fd_raise_detailed_exception(fd_ParseError,_("Hex parsing error"));
      else {fd_xfree(digits); return LISPFIX(num);}}
    case 'D': case 'd': {
      char *digits=read_atom(str,NULL);
      int num=strtol(digits,NULL,10);
      if (errno)
	if (bignum_parser != NULL) {
	  lisp answer=bignum_parser(digits,10);
	  fd_xfree(digits); CLEAR_ERR(); return answer;}
	else fd_raise_detailed_exception(fd_ParseError,_("Invalid decimal number"));
      else {fd_xfree(digits); return LISPFIX(num);}}
    case 'B': case 'b': {
      char *digits=read_atom(str,NULL);
      int num=strtol(digits,NULL,2);
      if (errno)
	if (bignum_parser != NULL) {
	  lisp answer=bignum_parser(digits,2);
	  fd_xfree(digits); CLEAR_ERR(); return answer;}
	else fd_raise_detailed_exception(fd_ParseError,_("Binary parsing error"));
      else {fd_xfree(digits); return LISPFIX(num);}}
    case 'O': case 'o': {
      char *digits=read_atom(str,NULL);
      int num=strtol(digits,NULL,8);
      if (errno)
	if (bignum_parser != NULL) {
	  lisp answer=bignum_parser(digits,16);
	  fd_xfree(digits); CLEAR_ERR(); return answer;}
	else fd_raise_detailed_exception(fd_ParseError,_("Octal parsing error"));
      else {fd_xfree(digits); return LISPFIX(num);}}
    default:
      /* Special case kludge for people who do #\u.... to get a character
	 constant. */
      if (c > 0x80) return fd_make_character(c);
      else if (fd_isalnum(c)) {
	char *token_name; fd_lisp result;
	uni_ungetc(c,str); token_name=read_atom(str,NULL);
	if ((strcasecmp(token_name,"null")) == 0) result=FD_EMPTY_LIST;
	else if ((strcasecmp(token_name,"void")) == 0) result=FD_VOID;
	else if ((strcasecmp(token_name,"eof")) == 0) result=FD_EOF_OBJECT;
	else fd_raise_detailed_exception
	       ("Unrecognized read token",token_name);
	fd_xfree(token_name); return result;}
      else {
	char op[3];
	lisp arg=lisp_parser(str);
	op[0]='#'; op[1]=c; op[2]='\0';
	return FD_MAKE_LIST(2,fd_make_symbol(op),arg);}}}
  case '@': {
    char *ref=read_atom(str,NULL);
    if ((*ref=='?') && (ref[1]=='\0'))
      if (lookup_oid) {
	fd_lisp name, oid;
	uni_getc(str); /* Use up the ? */
	name=lisp_parser(str);
	oid=lookup_oid(name);
	fd_decref(name); return oid;}
      else {
	fd_lisp name=lisp_parser(str);
	fd_raise_lisp_exception(CantLookupOIDName,NULL,name);}
    else {
      lisp oid=parse_oid(ref); fd_xfree(ref);
      c=uni_getc(str); uni_ungetc(c,str);
      if (can_start_oid_tag(c)) {
	lisp ignored_tag=lisp_parser(str);
	decref(ignored_tag);}
      return oid;}}
  case '\'': {
    lisp expr=lisp_parser(str);
    lisp quoted=FD_MAKE_LIST(2,quote_symbol,expr);
    return quoted;}
  case '`': {
    lisp expr=lisp_parser(str);
    return FD_MAKE_PAIR(backquote_symbol,FD_MAKE_LIST1(expr));}
  case ',': {
    int nxt=uni_getc(str);
    if (nxt == '@') {
      lisp expr=lisp_parser(str);
      return FD_MAKE_LIST(2,unquote_splice_symbol,expr);}
    else {
      lisp expr; uni_ungetc(nxt,str);
      expr=lisp_parser(str);
      return FD_MAKE_LIST(2,unquote_symbol,expr);}}
  case '(': {
    SKIP_WHITESPACE(c,str); CHECK_EOF(c);
    if (c == ')') return (FD_EMPTY_LIST);
    else {uni_ungetc(c,str); return parse_list(str,NULL);}}
  case '"': {
    struct FD_STRING_STREAM ss; int c=uni_getc(str);
    FD_INITIALIZE_STRING_STREAM(&ss,256);
    while (c >= 0) {
      if (c == '\\') {
	int code=uni_getc(str);
	if (code == 'n') c='\n';
	else if (code == 't') c='\t';
	else if (code == 'f') c='\f';
	else if (code == 'b') c='\b';
	else if (code == 'r') c='\r';
	else if (code == 'n') c='\\';
	else if (code == 'u') {
	  char buf[5];
	  buf[0]=uni_getc(str); buf[1]=uni_getc(str);
	  buf[2]=uni_getc(str); buf[3]=uni_getc(str);
	  buf[4]='\0'; c=strtol(buf,NULL,16);}
	else c=code;
	fd_sputc(&ss,c);}
      else if (c == '"') return fd_stream_string(&ss);
      /* Allow NULLs inside strings */
      /* else if (c == '\0') fd_raise_exception("Unexpected NULL"); */
      else fd_sputc(&ss,c);
      c=uni_getc(str);}
    fd_raise_exception(fd_Unexpected_EOF);}
  case ';': {
    int c=uni_getc(str);
    while (!((c < 0) || (c == '\n'))) c=uni_getc(str);
    return lisp_parser(str);}
  case '{':
    return parse_choice(str);
  case '[':
    return parse_slotmap(str);
  case '}': case ')': case ']':
    fd_raise_detailed_exception(fd_ParseError,"Extra delimiter");
    return FD_VOID;
  default: {
      char *buf; lisp result; uni_ungetc(c,str);
      buf=read_atom(str,NULL);
      result=parse_atom(buf,1); fd_xfree(buf);
      return result;}}
}

static lisp parse_choice(TXSTREAM str)
{
  lisp accumulate=(FD_EMPTY_CHOICE);
  int c; SKIP_WHITESPACE(c,str); CHECK_EOF(c);
  while (c != '}') {
    lisp item; uni_ungetc(c,str); item=lisp_parser(str);
    ADD_TO_CHOICE(accumulate,item);
    SKIP_WHITESPACE(c,str); CHECK_EOF(c);}
  return fd_return_proper_choice(accumulate);
}

static lisp parse_list(TXSTREAM str,u8char *atomic_prefix)
{
  int c; lisp car, cdr;
  if (atomic_prefix) {
    u8char *atom_string=read_atom(str,atomic_prefix);
    fd_lisp atom=parse_atom(atom_string,0);
    fd_xfree(atom_string);
    car=atom;}
  else car=lisp_parser(str);
  SKIP_WHITESPACE(c,str);
  if (c < 0) fd_raise_exception(fd_Unexpected_EOF);
  else if (c == 0) fd_raise_exception(fd_Unexpected_NULL);
  else if (c == ')') {cdr=(FD_EMPTY_LIST);}
  else if (c == '.') {
    int next=uni_getc(str);
    if (fd_isspace(next)) {
      SKIP_WHITESPACE(next,str); CHECK_EOF(next);
      uni_ungetc(next,str); cdr=lisp_parser(str);
      SKIP_WHITESPACE(next,str); CHECK_EOF(next);
      if (next != ')')
	fd_raise_detailed_exception
	  ("Syntax Error","multiple dotted expressions");}
    else {
      struct FD_STRING_STREAM astr; u8char buf[32];
      FD_INITIALIZE_FIXED_STRING_STREAM(&astr,32,buf);
      fd_sputc(&astr,'.'); fd_sputc(&astr,next);
      cdr=parse_list(str,buf);}}
  else {
    int next; uni_ungetc(c,str);
    SKIP_WHITESPACE(next,str); CHECK_EOF(next);
    uni_ungetc(next,str);
    cdr=parse_list(str,NULL);}
  return FD_MAKE_PAIR(car,cdr);
}

static lisp parse_vector(TXSTREAM str)
{
  lisp result, buf[16], *ptr=buf;
  unsigned int i=0, j=0, limit=16; int c;
  SKIP_WHITESPACE(c,str); CHECK_EOF(c);
  while (c != ')') {
    uni_ungetc(c,str);
    if (i == limit) {
      lisp *nptr=fd_malloc(sizeof(lisp)*limit*2);
      memmove(nptr,ptr,limit*sizeof(lisp));
      if (ptr != buf) fd_free(ptr,sizeof(lisp)*limit);
      ptr=nptr; limit=limit*2;}
    ptr[i++]=lisp_parser(str);
    SKIP_WHITESPACE(c,str); CHECK_EOF(c);}
  result=fd_make_vector(i);
  j=0; while (j < i) {VECTOR_REF(result,j)=ptr[j]; j++;}
  if (ptr != buf) fd_free(ptr,(sizeof(lisp)*limit));
  return result;
}

static lisp parse_slotmap(TXSTREAM str)
{
  lisp smap, buf[256], *ptr=buf;
  unsigned int i=0, j=0, limit=256; int c;
  SKIP_WHITESPACE(c,str); CHECK_EOF(c);
  while (c != ']') {
    uni_ungetc(c,str);
    if (i == limit){
      lisp *nptr=fd_malloc(sizeof(lisp)*limit*2);
      memmove(ptr,nptr,sizeof(lisp)*limit);
      if (ptr != buf) fd_free(ptr,sizeof(lisp)*limit);
      ptr=nptr; limit=limit*2;}
    ptr[i++]=lisp_parser(str);
    SKIP_WHITESPACE(c,str); CHECK_EOF(c);}
  if (i%2 != 0) {
    if (ptr != buf) fd_free(ptr,sizeof(lisp)*limit);
    fd_raise_detailed_exception(fd_ParseError,_("invalid slotmap"));}
  smap=fd_make_slotmap(i/2);
  j=0; while (j < i) {
    SLOTMAP_KEY(smap,j/2)=ptr[j]; 
    SLOTMAP_VALUE(smap,j/2)=ptr[j+1]; j=j+2;}
  (PTR_DATA(smap,slotmap))->size=i/2;
  if (ptr != buf) fd_free(ptr,sizeof(lisp)*limit);
  return smap;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: txio.c,v $
   Revision 1.15  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.14  2004/09/29 00:32:02  haase
   Fixes to oid lookup parsing

   Revision 1.13  2004/09/28 23:38:40  haase
   Added non-symbol oid lookup

   Revision 1.12  2004/09/15 16:40:14  haase
   Fixes of some print escape code

   Revision 1.11  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.10  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8.2.1  2003/01/26 20:55:32  haase
   Added zstrings

   Revision 1.8  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.7  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
