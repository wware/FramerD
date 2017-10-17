/* C Mode */

#include <stdlib.h>
#include <string.h>
#if defined(WIN32)
#include <windows.h>
#endif

#include "fdwww.h"
#include "framerd/fdtext.h"

/* Handling weird stuff:
    a tag can have a set of `close tags' which opening it will
    automatically close.  For example, TH and TD close TH and TD.
    TR can close TR, TH, TD.  P can close P.  In addition, some
    tags may be empty tags.
 */

extern fd_lispenv fd_wwwtools_env;
fd_exception fd_XML_Parse_Error, fd_XML_Mismatch;
fd_exception fd_XML_Parse_Error=_("XML Parsing error");
fd_exception fd_XML_Mismatch=_("Start and end tags don't match");
static fd_exception Malformed_DOCTYPE=_("Malformed doctype");

static fd_lisp doctype_symbol, qmark_symbol, xmlpi_symbol, xmltag_tag, xmlfrag_tag;
static fd_lisp xmltag_name_slot, xmltag_namespace_slot;
static fd_lisp element_name_slot, element_attribs_slot, element_content_slot;

#define XML_LAX 0
#define XML_ATTENTIVE 1
#define XML_PARANOID 2

static void xml_warn(int err_level,fd_u8char *error,fd_u8char *point)
{
  if (err_level == XML_LAX) {}
  else if (err_level == XML_ATTENTIVE)
    fd_warn(_("XML Parsing Error: %s\n%s\n"),error,point);
  else if (err_level == XML_PARANOID)
    fd_raise_lisp_exception
      (fd_XML_Parse_Error,error,fd_copy_string(point));
}

static int xml_valid_eltp(fd_u8char *start,fd_u8char *end)
{
  fd_u8char *scan=start, *prev=start; int c=fd_sgetc(&scan);
  if ((fd_isalpha(c)) || (c == '_')) {
    prev=scan; c=fd_sgetc(&scan);
    while (prev < end)
      if ((fd_isalnum(c)) || (c == '.') || (c == '-') || (c == '_')) {
	prev=scan; c=fd_sgetc(&scan);}
      else return 0;
    return 1;}
  else return 0;
}

static int xml_parse_elt(fd_u8char *s,int len,
			 fd_u8char **ns_start,fd_u8char **ns_end,
			 fd_u8char **elt_start,fd_u8char **elt_end)
{
  fd_u8char *scan=s,*limit=s+len, *last=s, *estart=NULL;
  int c=fd_sgetc(&scan);
  if ((fd_isalpha(c)) || (c == '_')) {
    fd_u8char *colon=strchr(scan,':');
    if (colon)
      if ((colon+1 >= s+len) || (strchr(colon+1,':')))
	return 0;
      else if ((xml_valid_eltp(s,colon-1) &&
		xml_valid_eltp(colon+1,s+len))) {
	*ns_start=s; *ns_end=colon;
	*elt_start=colon+1; *elt_end=s+len;
	return 1;}
      else return 0;
    else {
      *ns_start=NULL; *ns_end=NULL;
      *elt_start=s; *elt_end=s+len;
      return (xml_valid_eltp(s,s+len));}}
  else if (c == '{') {
    fd_u8char *end=strchr(scan,'}');
    if (end == NULL) return 0;
    if (end+1 >= s+len) return 0;
    *ns_start=scan; *ns_end=end;
    *elt_start=end+1; *elt_end=s+len;
    return xml_valid_eltp(end+1,s+len);}
  else return 0;
}


/* XML tag stuff */

static struct FD_HASHSET xmltags;

FDTEXT_EXPORT
/* fd_make_xmltag:
     Arguments: a namespace (a string or symbol), and a type (a symbol)
     Returns: an lisp object describing a qualified xmltag

 If the namespace is false (#f), this just returns the base arg.  Otherwise,
 it conses a lisp record. */
fd_lisp fd_make_xmltag(fd_lisp ns,fd_lisp base)
{
  fd_lisp probe=
    fd_make_lrecord(xmltag_tag,FD_MAKE_PAIR(fd_incref(ns),fd_incref(base)));
  fd_lisp interned=fd_hashset_intern(&xmltags,probe);
  fd_decref(probe);
  return interned;
}
static fd_lisp cons_xmltag(fd_lisp ns,fd_lisp base)
{
  fd_lisp probe=fd_make_lrecord(xmltag_tag,FD_MAKE_PAIR(ns,base));
  fd_lisp interned=fd_hashset_intern(&xmltags,probe);
  fd_decref(probe);
  return interned;
}

static fd_lisp make_xmltag_lexpr(fd_lisp args)
{
  fd_lisp base, ns;
  fd_get_args("MAKE-XMLTAG",args,&base,FD_VOID,&ns,FD_FALSE,NULL);
  return fd_make_xmltag(ns,base);
}

static fd_lisp xmltagp_cproc(fd_lisp x)
{
  if ((FD_SYMBOLP(x)) || (FD_RECORD_TYPEP(x,xmltag_tag)))
    return FD_TRUE;
  else if (FD_STRINGP(x)) {
    fd_u8char *nstart, *nend, *estart, *eend;
    if (xml_parse_elt(FD_STRING_DATA(x),FD_STRING_LENGTH(x),
		      &nstart,&nend,&estart,&eend))
      return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

FDTEXT_EXPORT
/* fd_xmltagp:
     Arguments: a lisp object 
     Returns: 1 or 0

  Returns 1 if the object is a valid XML tag, including either
an XML tag object, a symbol whose pname is a valid xml element name,
or a string which is a valid xml element name (including namespace). */
int fd_xmltagp(fd_lisp x)
{
  if (FD_RECORD_TYPEP(x,xmltag_tag)) return 1;
  else if (FD_SYMBOLP(x)) {
    fd_u8char *s=FD_SYMBOL_NAME(x); int len=strlen(s);
    return xml_valid_eltp(s,s+len);}
  else if (FD_STRINGP(x)) {
    fd_u8char *nstart, *nend, *estart, *eend;
    return xml_parse_elt(FD_STRING_DATA(x),FD_STRING_LENGTH(x),
			 &nstart,&nend,&estart,&eend);}
  else return 1;
}

STATIC_INLINE fd_lisp xmltag_name(fd_lisp tag)
{
  if (FD_SYMBOLP(tag)) return tag;
  else if (FD_LRECORD_TYPEP(tag,xmltag_tag))
    return (fd_cdr(FD_LRECORD_DATA(tag)));
  else if (FD_OIDP(tag))
    return fd_prim_get(tag,xmltag_name_slot);
  else if (FD_STRINGP(tag)) {
    fd_u8char *nstart, *nend, *estart, *eend;
    if (xml_parse_elt(FD_STRING_DATA(tag),FD_STRING_LENGTH(tag),
		      &nstart,&nend,&estart,&eend))
      return fd_make_substring(estart,eend);
    else fd_type_error(_("Not an XML tag"),tag);}
  else fd_type_error(_("Not an XML tag"),tag);
}
FDTEXT_EXPORT
/* fd_xmltag_name:
     Arguments: a lisp object (symbol or xml tag)
     Returns: the tag's name

 For symbols, this is just the identity; for tags,
 it returns whatever the base of the xmltag is. */
fd_lisp fd_xmltag_name(fd_lisp tag)
{
  return xmltag_name(tag);
}

STATIC_INLINE fd_lisp xmltag_namespace(fd_lisp tag)
{
  if (FD_SYMBOLP(tag)) return FD_FALSE;
  else if (FD_LRECORD_TYPEP(tag,xmltag_tag))
    return fd_car(FD_LRECORD_DATA(tag));
  else if (FD_OIDP(tag))
    return fd_prim_get(tag,xmltag_namespace_slot);
  else if (FD_STRINGP(tag)) {
    fd_u8char *nstart, *nend, *estart, *eend;
    if (xml_parse_elt(FD_STRING_DATA(tag),FD_STRING_LENGTH(tag),
		      &nstart,&nend,&estart,&eend))
      return fd_make_substring(nstart,nend);
    else fd_type_error(_("Not an XML tag"),tag);}
  else fd_type_error(_("Not an XML tag"),tag);
}
FDTEXT_EXPORT
/* fd_xmltag_namespace:
     Arguments: a lisp object (symbol or xml tag)
     Returns: the tag's namespace (possibly #f)

 For symbols, this just returns #F; for tags,
 it returns the tag's namespace. */
fd_lisp fd_xmltag_namespace(fd_lisp tag)
{
  return xmltag_namespace(tag);
}

static unsigned int hash_xmltag(fd_lisp tag,fd_hashfn hf)
{
  return hf(FD_LRECORD_DATA(tag));
}

static void print_xmltag(fd_lisp tag,fd_string_stream ss)
{
  fd_lisp ldata=FD_LRECORD_DATA(tag);
  if (FD_PAIRP(ldata)) {
    fd_lisp ns=FD_CAR(ldata);
    fd_lisp name=FD_CDR(ldata);
    if ((FD_FALSEP(ns)) && (FD_STRINGP(name))) 
      fd_printf(ss,"#<XML %s>",FD_STRING_DATA(name));
    else if ((FD_SYMBOLP(ns)) && (FD_STRINGP(name))) 
      fd_printf(ss,"#<XML %s:%s>",FD_SYMBOL_NAME(ns),FD_STRING_DATA(name));
    else if ((FD_STRINGP(ns)) && (FD_STRINGP(name))) 
      fd_printf(ss,"#<XML {%s}%s>",FD_STRING_DATA(ns),FD_STRING_DATA(name));
    else fd_printf(ss,"#<XML %q:%q>",ns,name);}
  else fd_printf(ss,"#<XML %q>",ldata);
}

static fd_lisp xmltag(fd_u8char *start,fd_u8char *end,fd_lisp ns)
{
  fd_u8char *scan=start;
  while (scan < end) if (*scan == ':') break; else scan++;
  if (scan >= end) 
    return cons_xmltag(FD_FALSE,fd_make_substring(start,end));
  else if (scan-start < 64) {
    fd_u8char buf[64]; strncpy(buf,start,scan-start); buf[scan-start]=NUL;
    return cons_xmltag(fd_make_symbol(buf),fd_make_substring(scan+1,end));}
  else fd_raise_detailed_exception(_("invalid XML tag"),start);
}

/* getting stuff from XML elements */

FDTEXT_EXPORT
/* fd_xml_tag:
     Arguments: a lisp pointer
     Returns: a lisp pointer

Gets the tag (or element type) of an pair or OID describing
an XML element. */
fd_lisp fd_xml_tag(fd_lisp x)
{
  if (FD_PAIRP(x)) return fd_incref(fd_get_arg(x,0,FD_VOID));
  else if (FD_OIDP(x)) return fd_prim_get(x,element_name_slot);
  else fd_type_error(_("Not an XML fragment or OID"),x);
}

FDTEXT_EXPORT
/* fd_xml_attributes:
     Arguments: a lisp pointer
     Returns: a lisp pointer

Gets the attributes of a pair or OID describing
an XML element.  Note that the attributes are a property list of
 lists. */
fd_lisp fd_xml_attributes(fd_lisp x)
{
  if (FD_PAIRP(x)) return fd_incref(fd_get_arg(x,1,FD_VOID));
  else if (FD_OIDP(x)) return fd_prim_get(x,element_attribs_slot);
  else fd_type_error(_("Not an XML fragment or OID"),x);
}

FDTEXT_EXPORT
/* fd_xml_content:
     Arguments: a lisp pointer
     Returns: a lisp pointer

Gets the content of a pair or OID describing
an XML element. Note that this should always be a list of
either strings or other elements. */
fd_lisp fd_xml_content(fd_lisp x)
{
  if (FD_PAIRP(x)) return fd_incref(fd_get_arg(x,2,FD_EMPTY_CHOICE));
  else if (FD_OIDP(x)) return fd_prim_get(x,element_content_slot);
  else fd_type_error(_("Not an XML fragment or OID"),x);
}

static int xml_emptyp(fd_lisp x)
{
  fd_lisp content=fd_xml_content(x);
  int empty = !(FD_PAIRP(content));
  fd_decref(content);
  return empty;
}

/* Searching for markup */

static fd_u8char *markup_start(fd_u8char *string)
{
  fd_u8char *candidate=strchr(string,'<');
  if (candidate) {
    fd_u8char *scan=candidate+1; int c=fd_sgetc(&scan);
    if (fd_isspace(c)) return markup_start(scan);}
  return candidate;
}

static fd_u8char *markup_end(fd_u8char *string)
{
  if (*string == '<') return strchr(string,'>');
  else fd_raise_exception(_("Not at markup start"));
}

/* Managing XML parse states */

struct XML_STATE {
  fd_lisp tag, head, *tail;
  int closed;
  struct XML_STATE *up;};

static void add_text(fd_u8char *start,fd_u8char *end,struct XML_STATE *s)
{
  fd_lisp string, pair;
  if (start == end) return;
  if (end) string=fd_make_substring(start,end);
  else string=fd_copy_string(start);
  pair=FD_MAKE_PAIR(string,FD_EMPTY_LIST);
  *(s->tail)=pair; s->tail=&(FD_CDR(pair));
}
static void add_markup(fd_lisp markup,struct XML_STATE *s)
{
  fd_lisp pair=FD_MAKE_PAIR(markup,FD_EMPTY_LIST);
  *(s->tail)=pair; s->tail=&(FD_CDR(pair));
}

static void skip_whitespace(fd_u8char **scan,fd_u8char *end)
{
  if (*scan >= end) return;
  else {
    fd_u8char *prev=*scan; int c=fd_sgetc(scan);
    while (*scan<end)
      if (fd_isspace(c)) {prev=*scan; c=fd_sgetc(scan);}
      else {*scan=prev; return;}
    *scan=prev;}
}

static fd_u8char *skip_token(fd_u8char **scan,fd_u8char *end)
{
  fd_u8char *prev=*scan; int c;
  if (*scan >= end) return NULL;
  else c=fd_sgetc(scan);
  if ((c == '\'') || (c == '"')) {
    int end_quote=c; prev=*scan; c=fd_sgetc(scan);
    while (*scan<end) {
      if (c == end_quote) return prev;
      prev=*scan; c=fd_sgetc(scan);}
    if (c == end_quote) return prev; else return *scan;}
  else while (*scan<end) 
    if ((fd_isspace(c)) || (c == '=')) {*scan=prev; return prev;}
    else {prev=*scan; c=fd_sgetc(scan);}
  return end;
}

#define name_charp(x) (!((c == '=') || (fd_isspacep(x))))

/* An attribute is part of an element tag and because we may be working with
    messy HTML, we need to be complicated.
    Cases to handle:
     "foo"
     foo
     foo=bar
     foo=bar=baz (for Water, etc)
     foo= bar
     foo='bar'
     "foo=bar"
*/
static fd_lisp parse_attribute(fd_u8char **scan,fd_u8char *end,fd_lisp ns)
{
  fd_lisp tokens[256]; int n_tokens=0;
  fd_u8char *start, *token_start, *token_end;
  skip_whitespace(scan,end); token_start=*scan;
  while (token_end=skip_token(scan,end)) {
    skip_whitespace(scan,end);
    if ((*token_start == '\'') || (*token_start == '"')) token_start++;
    skip_whitespace(scan,end);
    if (n_tokens>254) {
      tokens[n_tokens++]=fd_make_substring(token_start,end);
      break;}
    else if (n_tokens)
      tokens[n_tokens++]=fd_make_substring(token_start,token_end);
    else if (**scan == '=') 
      tokens[n_tokens++]=xmltag(token_start,token_end,ns);
    else tokens[n_tokens++]=fd_make_substring(token_start,token_end);
    if (*scan>=end) break;
    else if (**scan == '=') (*scan)++;
    else break;
    skip_whitespace(scan,end);
    token_start=*scan;}
  if (n_tokens == 1) return tokens[0];
  else {
    fd_lisp result=FD_EMPTY_LIST;
    int i=n_tokens-1; while (i >= 0) {
      result=FD_MAKE_PAIR(tokens[i],result); i--;}
    return result;}
}

static fd_lisp parse_pi(fd_u8char *start,fd_u8char *end)
{
  fd_lisp attributes=FD_EMPTY_LIST, *tail=&attributes;
  fd_u8char *scan=start;
  while (scan < end) {
    fd_lisp attr=parse_attribute(&scan,end,FD_FALSE), lst=FD_MAKE_LIST1(attr);
    fd_u8char *oscan=scan; int c=fd_sgetc(&scan);
    while (fd_isspace(c)) {oscan=scan; c=fd_sgetc(&scan);} scan=oscan;
    *tail=lst; tail=&(FD_CDR(lst));}
  return FD_MAKE_LIST(2,qmark_symbol,attributes);
}
    
static void swallow_whitespace(fd_u8char **start,fd_u8char *end)
{
  fd_u8char *result=*start; int c=fd_sgetc(start);
  if (*start >= end) {
    *start=result; return;}
  else while (fd_isspace(c))
    if (*start >= end) return;
    else {result=*start; c=fd_sgetc(start);}
  *start=result;
}

static fd_lisp parse_doctype(fd_u8char **start,fd_u8char *end)
{
  fd_lisp root, system, name, uri, internal_dtd;
  root=parse_attribute(start,end,FD_FALSE); swallow_whitespace(start,end);
  system=parse_attribute(start,end,FD_FALSE); swallow_whitespace(start,end);
  if ((FD_STRINGP(system)) &&
      (strcmp(FD_STRING_DATA(system),"PUBLIC") == 0)) {
    name=parse_attribute(start,end,FD_FALSE); swallow_whitespace(start,end);
    uri=parse_attribute(start,end,FD_FALSE); swallow_whitespace(start,end);}
  else {
    uri=parse_attribute(start,end,FD_FALSE); swallow_whitespace(start,end);
    name=FD_FALSE;}
  if (**start == '[') {
    fd_u8char *idtd_end=strchr(*start+1,']');
    if (idtd_end) internal_dtd=fd_make_substring(*start+1,idtd_end);
    else fd_raise_exception(Malformed_DOCTYPE);
    *start=idtd_end+1;}
  else internal_dtd=FD_EMPTY_LIST;
  swallow_whitespace(start,end);
  if (FD_VOIDP(uri)) uri=FD_FALSE;
  if (**start != '>') fd_raise_exception(Malformed_DOCTYPE);
  else {
    fd_lisp result=
      FD_MAKE_LIST(6,doctype_symbol,root,
		   system,name,uri,internal_dtd);
    (*start)++;
    return result;}
}
    
/* Parsing a single tag */
static fd_lisp parse_markup
  (fd_u8char *start,fd_u8char *end,fd_lisp ns,int empty,int err_level)
{
  fd_u8char *scan=start, *oscan=start;
  fd_lisp tag, attributes=FD_EMPTY_LIST, *tail=&attributes;
  int c=fd_sgetc(&scan);
  while ((scan <= end) && (!(fd_isspace(c)))) {
    oscan=scan; c=fd_sgetc(&scan);}
  tag=xmltag(start,oscan,ns);
  /* Skip whitespace */
  while ((scan <= end) && (fd_isspace(c))) {
    oscan=scan; c=fd_sgetc(&scan);}
  if (scan <= end) scan=oscan;
  /* Read attributes */
  while (scan < end) {
    fd_lisp attr=parse_attribute(&scan,end,ns);
    fd_lisp new_cons=FD_MAKE_LIST1(attr);
    fd_u8char *oscan=scan; int c=fd_sgetc(&scan);
    while (fd_isspace(c)) {oscan=scan; c=fd_sgetc(&scan);} scan=oscan;
    *tail=new_cons; tail=&(FD_CDR(new_cons));}
  if (empty)
    return FD_MAKE_LIST(2,tag,attributes);
  else return FD_MAKE_LIST(3,tag,attributes,FD_EMPTY_LIST);
}

/* The central parse loop, which updates s to include a single
   XML expression parsed from string.  closers and empty_tags, if non
   NULL allow violations to strict XML to support HTML parsing.
   err_level is passed to xml_warn when syntax problems are found.  */
static fd_u8char *parse_xml
  (fd_u8char *string,fd_lisp ns,struct XML_STATE *s,
   fd_hashtable closers,fd_hashset empty_tags,fd_hashset pre_tags,
   int err_level)
{
  while (*string) {
    fd_u8char *mstart=markup_start(string);
    if (mstart == NULL) {
      add_text(string,NULL,s); return NULL;}
    else {
      fd_u8char *mend=markup_end(mstart);
      if (mend == NULL) {
	xml_warn(err_level,_("Incomplete markup"),mstart);
	add_text(string,NULL,s); return NULL;}
      else {
	add_text(string,mstart,s); string=mend+1;
	if (mstart[1] == '[') /* CDATA */
	  if ((strncmp(mstart,"<[CDATA[",8)) == 0) {
	    fd_u8char *cdata_end=strstr(mstart+8,"]]>");
	    if (cdata_end) {
	      add_text(mstart,cdata_end+3,s);
	      string=cdata_end+3;}
	    else {
	      xml_warn(err_level,_("Unterminated CDATA"),mstart);
	      add_text(mstart,mend,s);
	      string=mend+1;}}
	  else {
	    xml_warn(err_level,_("Invalid <[]> markup"),mstart);
	    add_text(string,mend,s);
	    string=mend+1;}
	else if (mstart[1] == '?') { /* Processing instructions */
	  if (mend[-1] != '?') add_text(string,mend,s);
	  else add_markup(parse_pi(mstart+2,mend-1),s);
	  string=mend+1;}
	else if (mstart[1] == '!') /* Handle comments */
	  if (strncmp(mstart,"<!--",4) == 0) {
	    fd_u8char *comment_end=strstr(mstart+4,"-->");
	    if (comment_end) {
	      add_text(mstart,comment_end+3,s);
	      string=comment_end+3;}
	    else {
	      xml_warn(err_level,_("Unterminated comment"),mstart);
	      add_text(mstart,mend+1,s);
	      string=mend+1;}}
	  else if (strncasecmp(mstart,"<!DOCTYPE ",10) == 0) {
	    fd_u8char *scan=mstart+10;
	    add_markup(parse_doctype(&scan,mend),s);
	    string=scan;}
	  else {
	    xml_warn(err_level,_("Odd DTD tag"),mstart);
	    add_text(mstart,mend,s);
	    string=mend+1;}
	else if (mstart[1] == '/') {
	  fd_lisp markup=parse_markup(mstart+2,mend,ns,1,err_level);
	  if (!(FD_PAIRP(markup)))
	    fd_type_error(_("Not parsed markup"),markup);
	  if (!(FD_EMPTY_LISTP(FD_CAR(FD_CDR(markup)))))
	    xml_warn(err_level,_("Close tag has attributes"),mstart);
	  if (FD_LISP_EQUAL(FD_CAR(markup),s->tag)) {
	    fd_decref(markup); return string;}
	  else if ((empty_tags) &&
		   (fd_hashset_get(empty_tags,FD_CAR(markup)))) {
	    fd_decref(markup); return string;}
	  else if (closers) {
	    fd_lisp markup2=parse_markup(mstart+1,mend,ns,1,err_level);
	    if (!(FD_PAIRP(markup2)))
	      fd_type_error(_("Not parsed markup"),markup2);
	    /* If this tag automatically closes the current context,
	       we pop out and do the parsing outside of this context. */
	    if ((closers) && (fd_hashtable_test(closers,FD_CAR(markup2),s->tag))) {
	      s->closed=1; fd_decref(markup); fd_decref(markup2);
	      return mstart;}
	    else {fd_decref(markup2);}}
	  if (err_level == XML_LAX) {}
	  else if (err_level == XML_ATTENTIVE)
	    fd_warn(_("%q entity closed with %q"),s->tag,markup);
	  else 
	    fd_raise_lisp_exception
	      (fd_XML_Mismatch,fd_symbol_name(s->tag),
	       fd_copy_string(mstart));
	  if (((s->up) && ((FD_LISP_EQUAL(FD_CAR(markup),s->up->tag)))) ||
	      ((s->up) && (s->up->up) &&
	       ((FD_LISP_EQUAL(FD_CAR(markup),s->up->up->tag))))) {
	    fd_decref(markup); return mstart;}
	  else {fd_decref(markup); return string;}}
	else if (mend[-1] == '/') { /* Empty tag */
	  fd_lisp markup=parse_markup(mstart+1,mend-1,ns,1,err_level);
	  add_markup(markup,s); string=mend+1;}
	else { /* Regular tag */
	  fd_lisp markup=parse_markup(mstart+1,mend,ns,0,err_level);
	  if ((empty_tags) && (fd_hashset_get(empty_tags,FD_CAR(markup)))) {
	    fd_lisp stripped_markup=
	      FD_MAKE_LIST(2,fd_incref(FD_CAR(markup)),
			   fd_incref(FD_CAR(FD_CDR(markup))));
	    fd_decref(markup);
	    add_markup(stripped_markup,s); string=mend+1;}
	  else if ((pre_tags) && (fd_hashset_get(pre_tags,FD_CAR(markup)))) {
	    fd_lisp tag=xmltag_name(FD_CAR(markup));
	    fd_u8char *tag_name=FD_SYMBOL_NAME(tag);
	    fd_u8char *tag_end=strstr(mend+1,"</");
	    int tag_len=strlen(tag_name); 
	    while (tag_end)
	      if (strncasecmp(tag_end+2,tag_name,tag_len) == 0) {
		fd_u8char *end_tag_end=strstr(tag_end,">");
		fd_lisp markup_tail=FD_CDR(FD_CDR(markup));
		fd_lisp str, str_list;
		if (end_tag_end == NULL) {
		  fd_warn("Malformed HTML"); end_tag_end=tag_end+2+tag_len;}
		str=fd_make_substring(mend+1,tag_end);
		str_list=FD_MAKE_LIST1(str);
		FD_RPLACA(markup_tail,str_list);
		add_markup(markup,s); string=end_tag_end+1;
		tag_end=NULL;}
	      else tag_end=strstr(tag_end+1,"</");
	    fd_decref(tag);}
	  else {
	    struct XML_STATE xs;
	    /* If this tag automatically closes the current context,
	       we pop out and do the parsing outside of this context. */
	    if ((closers) && (fd_hashtable_test(closers,FD_CAR(markup),s->tag))) {
	      fd_decref(markup); s->closed=1; return mstart;}
	    xs.tag=FD_CAR(markup); xs.head=markup; xs.closed=0;
	    xs.tail=&(FD_CAR(FD_CDR(FD_CDR(markup)))); xs.up=s;
	    string=parse_xml(string,ns,&xs,
			     closers,empty_tags,pre_tags,err_level);
	    add_markup(xs.head,s);
	    if (s->closed) return string;}}}}
    if (string == NULL) return string;}
  if (*string) return string;
  else return NULL;
}

/* Reversing parsing */

static fd_lisp reverse_html_parse(fd_lisp x);

static fd_lisp xml_reverse_list(fd_lisp scan)
{
  fd_lisp head, *tail=&head;
  FD_DOLIST(elt,scan) {
    fd_lisp reversed=reverse_html_parse(elt);
    fd_lisp pair=FD_MAKE_PAIR(reversed,FD_EMPTY_LIST);
    *tail=pair; tail=&(FD_CDR(pair));}
  return head;
}

static fd_lisp xml_reversed_content(fd_lisp x)
{
  fd_lisp content=fd_xml_content(x);
  fd_lisp reversed=xml_reverse_list(content);
  fd_decref(content);
  return reversed;
}

static fd_lisp convert_attr_list(fd_lisp input)
{
  if (FD_PAIRP(input))
    if (FD_PAIRP(FD_CAR(input))) { /* Alist style, so convert it */
      fd_lisp entry=FD_CAR(input), var=FD_CAR(entry), val=fd_car_noref(FD_CDR(entry));
      return FD_MAKE_PAIR
	(var,FD_MAKE_PAIR(fd_incref(val),convert_attr_list(FD_CDR(input))));}
    else return FD_MAKE_PAIR(fd_incref(FD_CAR(input)),
			     convert_attr_list(FD_CDR(input)));
  else return fd_incref(input);
}

static fd_lisp reverse_html_parse(fd_lisp x)
{
  if (FD_PAIRP(x)) {
    fd_lisp tag=fd_xml_tag(x);
    fd_lisp attributes=fd_xml_attributes(x);
    if (!(FD_SYMBOLP(tag))) {
      fd_decref(tag); fd_decref(attributes);
      return FD_MAKE_PAIR(fd_make_symbol("HTTPDOC"),xml_reverse_list(x));}
    else if (FD_LISP_EQ(tag,qmark_symbol)) 
      return FD_MAKE_PAIR(xmlpi_symbol,attributes);
    else if (FD_LISP_EQ(tag,doctype_symbol)) {
      struct FD_STRING_STREAM ss;
      fd_decref(attributes);
      FD_INITIALIZE_STRING_STREAM(&ss,256);
      fd_printf(&ss,"<!DOCTYPE %q SYSTEM %q>",
		fd_get_arg(x,1,FD_VOID),fd_get_arg(x,2,FD_VOID));
      return fd_stream_string(&ss);}
    else if (xml_emptyp(x)) /* Empty tag */
      return FD_MAKE_PAIR(tag,attributes);
    else if (FD_EMPTY_LISTP(attributes)) /* no attributes */
      return FD_MAKE_PAIR(tag,xml_reversed_content(x));
    else { /* Need to generate *-form */
      fd_lisp star_symbol, v;
      fd_u8char *tag_name=fd_symbol_name(tag);
      fd_u8char *star_name=fd_xmalloc(strlen(tag_name)+2);
      sprintf(star_name,"%s*",tag_name);
      star_symbol=fd_parse_string(star_name); fd_xfree(star_name);
      v=FD_MAKE_PAIR(convert_attr_list(attributes), xml_reversed_content(x));
      fd_decref(attributes);
      return FD_MAKE_PAIR(star_symbol,v);}}
  else return fd_incref(x);
}

FDTEXT_EXPORT
/* fd_parse_xml:
     Arguments: a UTF8-string, an error level, and a namespace (a lisp pointer)
     Returns: A nested pair structure based on the XML structure of the given string

  Unqualified element and attribute names will be created in the given namespace,
unless it is false (#f,FD_FALSE), in which case they will just be created as symbols.
The error level is an int in the range [0,2]:
	0	XML_LAX		Try and fix XML mismatches and other syntax errors
	1	XML_ATTENTIVE	Warn about XML mismatches and other syntax errors (and try to fix them)
	2	XML_PARANOID	Raise an exception for XML mismatches and other syntax errors
*/
fd_lisp fd_parse_xml(fd_u8char *string,int err_level,fd_lisp ns)
{
  fd_lisp answer=FD_EMPTY_LIST;
  struct XML_STATE xs; fd_u8char *scan=string;
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL;
  while (scan) scan=parse_xml(scan,ns,&xs,NULL,NULL,NULL,err_level);
  return answer;
}

/* Primitives */

static fd_lisp lisp_xml_parser_lexpr(fd_lisp args)
{
  struct XML_STATE xs; fd_u8char *scan;
  fd_lisp answer=FD_EMPTY_LIST, input, ns;
  fd_get_args("PARSE-XML",args,&input,FD_VOID,&ns,FD_FALSE,NULL);
  scan=fd_strdata(input);
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL;
  while (scan) scan=parse_xml(scan,ns,&xs,NULL,NULL,NULL,XML_ATTENTIVE);
  return FD_MAKE_LIST(3,FD_FALSE,FD_EMPTY_LIST,answer);
}

static fd_lisp lisp_xml_strict_parser_lexpr(fd_lisp args)
{
  struct XML_STATE xs; fd_u8char *scan;
  fd_lisp answer=FD_EMPTY_LIST, input, ns;
  fd_get_args("PARSE-XML-STRICT",args,&input,FD_VOID,&ns,FD_FALSE,NULL);
  scan=fd_strdata(input);
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL;
  while (scan) scan=parse_xml(scan,ns,&xs,NULL,NULL,NULL,XML_PARANOID);
  return FD_MAKE_LIST(3,FD_FALSE,FD_EMPTY_LIST,answer);
}

static fd_hashtable html_closers;
static fd_hashset html_empty_tags;
static fd_hashset html_pre_tags;

static fd_lisp lisp_html_parser(fd_lisp input)
{
  struct XML_STATE xs; fd_u8char *scan=fd_strdata(input);
  fd_lisp answer=FD_EMPTY_LIST;
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL; 
  while (scan)
    scan=parse_xml(scan,FD_FALSE,&xs,html_closers,html_empty_tags,html_pre_tags,XML_LAX);
  return answer;
}

FDTEXT_EXPORT
/* fd_parse_html:
     Arguments: a UTF8-string
     Returns: A nested pair structure based on the HTML structure of the given string

This knows about the empty HTML tags and will handle them appropriately.  It also automatically
terminates paragraphs and list items.
*/

fd_lisp fd_parse_html(fd_u8char *string)
{
  fd_lisp answer=FD_EMPTY_LIST;
  struct XML_STATE xs; fd_u8char *scan=string;
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL;
  while (scan)
    scan=parse_xml(scan,FD_FALSE,&xs,html_closers,html_empty_tags,html_pre_tags,XML_LAX);
  return answer;
}

static fd_lisp lisp_htmlr_parser(fd_lisp input)
{
  fd_lisp reversed_answer;
  struct XML_STATE xs; fd_u8char *scan=fd_strdata(input);
  fd_lisp answer=FD_EMPTY_LIST;
  xs.tag=FD_VOID; xs.head=FD_EMPTY_LIST;
  xs.tail=&answer; xs.closed=0; xs.up=NULL;
  while (scan)
    scan=parse_xml(scan,FD_FALSE,&xs,html_closers,html_empty_tags,html_pre_tags,XML_LAX);
  reversed_answer=reverse_html_parse(answer); fd_decref(answer);
  return reversed_answer;
}

/* tags are either symbols, lrecords, or OIDs */

static int tag_matchp(fd_lisp key,fd_lisp y)
{
  if (FD_LISP_EQ(key,y)) return 1;
  else {
    fd_lisp tagname=fd_xmltag_name(y);
    if ((FD_SYMBOLP(key)) && (FD_SYMBOLP(tagname)))
      return (FD_LISP_EQ(key,tagname));
    else {
      fd_u8char *kstring, *tstring; int result;
      if (FD_SYMBOLP(key)) kstring=FD_SYMBOL_NAME(key);
      else if (FD_STRINGP(key)) kstring=FD_STRING_DATA(key);
      else fd_type_error("XML tag key",key);
      if (FD_SYMBOLP(tagname)) tstring=FD_SYMBOL_NAME(tagname);
      else if (FD_STRINGP(tagname)) tstring=FD_STRING_DATA(tagname);
      else fd_type_error("XML tag key",tagname);
      result=((strcasecmp(tstring,kstring) == 0));
      fd_decref(tagname);
      return result;}}
}

static fd_lisp lisp_xmltag_matchp_cproc(fd_lisp tag1,fd_lisp tag2)
{
  if (tag_matchp(tag1,tag2)) return FD_TRUE; else return FD_FALSE;
}

FDTEXT_EXPORT 
fd_lisp fd_xml_get(fd_lisp arg,fd_lisp slot_id)
{
  if ((FD_OIDP(arg)) && (FD_OIDP(slot_id)))
    return fd_frame_get(arg,slot_id);
  else {
    fd_lisp tag=fd_xml_tag(arg);
    fd_lisp attribs=fd_xml_attributes(arg);
    fd_lisp content=fd_xml_content(arg);
    if (FD_EMPTYP(tag)) {
      fd_decref(tag); fd_decref(attribs); fd_decref(content);
      fd_type_error(_("not an XML fragment"),arg);}
    { /* First check for attrib matchings */
      FD_DOLIST(pair,attribs) {
	if (FD_PAIRP(pair)) 
	  if (tag_matchp(slot_id,FD_CAR(pair))) {
	    fd_decref(tag); fd_decref(attribs); fd_decref(content);
	    return fd_car(FD_CDR(pair));}}}
    { /* Then look for content */
      fd_lisp answer=FD_EMPTY_CHOICE;
      FD_DOLIST(item,content)
	if (FD_PAIRP(item)) {
	  if (tag_matchp(slot_id,FD_CAR(item))) {
	    FD_ADD_TO_CHOICE(answer,fd_incref(item));}}
	else if (FD_OIDP(item)) {
	  fd_lisp tag=fd_xml_tag(item); int match=0;
	  if (tag_matchp(slot_id,tag)) match=1;
	  fd_decref(tag);
	  if (match) {
	    FD_ADD_TO_CHOICE(answer,fd_incref(item));}}
	else {}
      fd_decref(tag); fd_decref(attribs); fd_decref(content);
      return answer;}}
}

static fd_lisp lisp_xmlget_cproc(fd_lisp arg,fd_lisp slot_id)
{
  if ((FD_OIDP(arg)) && (FD_OIDP(slot_id)))
    return fd_frame_get(arg,slot_id);
  else {
    fd_lisp tag=fd_xml_tag(arg);
    fd_lisp attribs=fd_xml_attributes(arg);
    fd_lisp content=fd_xml_content(arg);
    if (FD_EMPTYP(tag)) {
      fd_decref(tag); fd_decref(attribs); fd_decref(content);
      fd_type_error(_("not an XML fragment"),arg);}
    { /* First check for attrib matchings */
      FD_DOLIST(pair,attribs) {
	if (FD_PAIRP(pair)) 
	  if (tag_matchp(slot_id,FD_CAR(pair))) {
	    fd_decref(tag); fd_decref(attribs); fd_decref(content);
	    return fd_car(FD_CDR(pair));}}}
    { /* Then look for content */
      fd_lisp answer=FD_EMPTY_CHOICE;
      FD_DOLIST(item,content)
	if (FD_PAIRP(item)) {
	  if (tag_matchp(slot_id,FD_CAR(item))) {
	    FD_ADD_TO_CHOICE(answer,fd_incref(item));}}
	else if (FD_OIDP(item)) {
	  fd_lisp tag=fd_xml_tag(item); int match=0;
	  if (tag_matchp(slot_id,tag)) match=1;
	  fd_decref(tag);
	  if (match) {
	    FD_ADD_TO_CHOICE(answer,fd_incref(item));}}
	else {}
      fd_decref(tag); fd_decref(attribs); fd_decref(content);
      return answer;}}
}

static fd_lisp lisp_xml_getall_cproc(fd_lisp arg)
{
  fd_lisp content=fd_xml_content(arg);
  fd_lisp answer=FD_EMPTY_CHOICE;
  FD_DOLIST(item,content)
    if (FD_PAIRP(item)) {
      FD_ADD_TO_CHOICE(answer,fd_incref(item));}
    else if (FD_OIDP(item)) {
      FD_ADD_TO_CHOICE(answer,fd_incref(item));}
  fd_decref(content);
  return answer;
}

/* URI decoding */

static fd_lisp uri_decode_cproc(fd_lisp string)
{
  struct FD_STRING_STREAM ss;
  fd_u8char *scan=fd_strdata(string), *limit=scan+fd_strlen(string);
  FD_INITIALIZE_STRING_STREAM(&ss,fd_strlen(string));
  while (scan < limit) {
    int c=fd_sgetc(&scan);
    if (c == '+') fd_sputc(&ss,' ');
    else if (c == '%') {
      char hexcode[3]; int c1, c2, b;
      c1=fd_sgetc(&scan);
      if (!(isxdigit(c1))) {
	fd_sputc(&ss,'%'); fd_sputc(&ss,c1); continue;}
      c2=fd_sgetc(&scan);
      if (!(isxdigit(c2))) {
	fd_sputc(&ss,'%'); fd_sputc(&ss,c1);
	fd_sputc(&ss,c2); continue;}
      hexcode[0]=c1; hexcode[1]=c2; hexcode[2]=NUL;
      b=strtol(hexcode,NULL,16);
      fd_sputc(&ss,b);}
    else fd_sputc(&ss,c);}
  return fd_stream_string(&ss);
}


/* Generalization initialize */

#define _SYM(x) fd_make_symbol(x)

void initialize_xml_c()
{
  struct FD_TYPE_REGISTRY *xml_tag_record;

  fd_init_hashset(&xmltags,128);

  qmark_symbol=fd_make_symbol("?");
  xmlpi_symbol=fd_make_symbol("XMLPI");
  doctype_symbol=fd_make_symbol("!DOCTYPE");

  xmltag_name_slot=fd_make_symbol("%NAME");
  xmltag_namespace_slot=fd_make_symbol("%NAMESPACE");

  element_name_slot=fd_make_symbol("%ELT_TYPE");
  element_attribs_slot=fd_make_symbol("%ELT_ATTRIBS");
  element_content_slot=fd_make_symbol("%ELT_CONTENT");

  xmltag_tag=fd_make_symbol("XMLTAG");
  xml_tag_record=fd_register_record(xmltag_tag);
  xml_tag_record->hash_fcn=hash_xmltag;
  xml_tag_record->print_fcn=print_xmltag;

  xmlfrag_tag=fd_make_symbol("XMLFRAG");

  /* The semantics of this table are that the key (first item) 
     can close the value (second item).
     E.G. /TABLE can close TR, TH, or TD; H2 can close P, etc. */
  html_closers=fd_make_hashtable(64);
  /* Paragraphs, including paragraphs used (by some people and
     programs) to force spacing or line breaks. */
  fd_hashtable_add(html_closers,_SYM("P"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("H1"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("H2"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("H3"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("H4"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("H5"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("UL"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("TR"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("TD"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("TH"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("TABLE"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("PRE"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("BLOCKQUOTE"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("DL"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("DT"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("DD"),_SYM("P"));
  fd_hashtable_add(html_closers,_SYM("LI"),_SYM("P"));
  /* Tables in their various shapes and forms */
  fd_hashtable_add(html_closers,_SYM("TH"),_SYM("TH"));
  fd_hashtable_add(html_closers,_SYM("TH"),_SYM("TD"));
  fd_hashtable_add(html_closers,_SYM("TD"),_SYM("TH"));
  fd_hashtable_add(html_closers,_SYM("TD"),_SYM("TD"));
  fd_hashtable_add(html_closers,_SYM("/TR"),_SYM("TH"));
  fd_hashtable_add(html_closers,_SYM("/TR"),_SYM("TD"));
  fd_hashtable_add(html_closers,_SYM("/TABLE"),_SYM("TH"));
  fd_hashtable_add(html_closers,_SYM("/TABLE"),_SYM("TD"));
  fd_hashtable_add(html_closers,_SYM("/TABLE"),_SYM("TR"));
  fd_hashtable_add(html_closers,_SYM("TR"),_SYM("TR"));
  fd_hashtable_add(html_closers,_SYM("TR"),_SYM("TH"));
  fd_hashtable_add(html_closers,_SYM("TR"),_SYM("TD"));
  /* Lists, ordered and unordered */
  fd_hashtable_add(html_closers,_SYM("/OL"),_SYM("LI"));
  fd_hashtable_add(html_closers,_SYM("/UL"),_SYM("LI"));
  fd_hashtable_add(html_closers,_SYM("LI"),_SYM("LI"));
  fd_hashtable_add(html_closers,_SYM("P"),_SYM("LI"));
  /* Description lists */
  fd_hashtable_add(html_closers,_SYM("DD"),_SYM("DT"));
  fd_hashtable_add(html_closers,_SYM("DD"),_SYM("DD"));
  fd_hashtable_add(html_closers,_SYM("DT"),_SYM("DD"));
  fd_hashtable_add(html_closers,_SYM("DT"),_SYM("DT"));
  fd_hashtable_add(html_closers,_SYM("/DL"),_SYM("DD"));
  fd_hashtable_add(html_closers,_SYM("/DL"),_SYM("DT"));

  fd_hashtable_add(html_closers,_SYM("COLGROUP"),_SYM("COLGROUP"));
  fd_hashtable_add(html_closers,_SYM("COL"),_SYM("COL"));

  html_empty_tags=fd_make_hashset(16);
  fd_hashset_add(html_empty_tags,_SYM("BASE"));
  fd_hashset_add(html_empty_tags,_SYM("LINK"));
  fd_hashset_add(html_empty_tags,_SYM("IMG"));
  fd_hashset_add(html_empty_tags,_SYM("META"));
  fd_hashset_add(html_empty_tags,_SYM("BR"));
  fd_hashset_add(html_empty_tags,_SYM("BREAK")); /* Not really */
  fd_hashset_add(html_empty_tags,_SYM("HR"));

  html_pre_tags=fd_make_hashset(16);
  /* fd_hashset_add(html_pre_tags,_SYM("PRE")); */
  fd_hashset_add(html_pre_tags,_SYM("SCRIPT"));

  fd_add_lexpr(fd_wwwtools_env,"PARSE-XML",FD_NORMAL_LEXPR,
	       lisp_xml_parser_lexpr);
  fd_add_lexpr(fd_wwwtools_env,"PARSE-XML-STRICT",FD_NORMAL_LEXPR,
	       lisp_xml_strict_parser_lexpr);
  fd_add_cproc(fd_wwwtools_env,"PARSE-HTML",1,lisp_html_parser);
  fd_add_cproc(fd_wwwtools_env,"PARSE-HTMLR",1,lisp_htmlr_parser);

  fd_add_lexpr(fd_wwwtools_env,"MAKE-XMLTAG",FD_NORMAL_LEXPR,
	       make_xmltag_lexpr);
  fd_add_cproc(fd_wwwtools_env,"XMLTAG?",1,xmltagp_cproc);
  fd_add_cproc(fd_wwwtools_env,"XMLTAG-NAMESPACE",1,fd_xmltag_namespace);
  fd_add_cproc(fd_wwwtools_env,"XMLTAG-NAME",1,fd_xmltag_name);

  fd_add_cproc(fd_wwwtools_env,"XML-TAG",1,fd_xml_tag);
  fd_add_cproc(fd_wwwtools_env,"XML-ATTRIBUTES",1,fd_xml_attributes);
  fd_add_cproc(fd_wwwtools_env,"XML-CONTENT",1,fd_xml_content);

  fd_add_cproc(fd_wwwtools_env,"XMLTAG-MATCH?",2,lisp_xmltag_matchp_cproc);
  fd_add_cproc(fd_wwwtools_env,"XML-GET",2,lisp_xmlget_cproc);
  fd_add_cproc(fd_wwwtools_env,"XML-GETALL",1,lisp_xml_getall_cproc);

  fd_add_cproc(fd_wwwtools_env,"URI-DECODE",1,uri_decode_cproc);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: xml.c,v $
   Revision 1.22  2004/04/12 05:48:53  haase
   Added URI-DECODE

   Revision 1.21  2004/04/06 09:55:52  haase
   Fixed leak in xml-get

   Revision 1.20  2004/03/11 15:21:46  haase
   Fixes to XML attribute parsing

   Revision 1.19  2004/02/20 19:05:12  haase
   Fixes to attribute parsing to let non assignments be parsed as simple strings rather than XML names

   Revision 1.18  2004/02/20 03:59:55  haase
   Further fix to attribute parsing and addition of STRING type for record->frame

   Revision 1.17  2004/02/19 20:57:25  haase
   Simplified and fixed bug with attribute parsing

   Revision 1.16  2003/12/18 03:41:34  haase
   Cleaned up separation of fdtext and fdwww

   Revision 1.15  2003/11/18 16:34:37  haase
   Fixed bug in doctype parsing

   Revision 1.14  2003/09/07 18:23:40  haase
   Added API access to XML get

   Revision 1.13  2003/09/05 10:10:03  haase
   Fixed XML attribute parsing error, simplified tag_matchp

   Revision 1.12  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.7  2003/02/17 12:25:35  haase
   Added xmltag-match?

   Revision 1.11.2.6  2003/01/26 20:50:25  haase
   Misc. fixes, especially GC

   Revision 1.11.2.5  2002/10/28 08:11:44  haase
   Made parse-xml return a well-formed XML node

   Revision 1.11.2.4  2002/09/20 22:05:46  haase
   Fixed gc bug in XML access functions

   Revision 1.11.2.3  2002/08/25 01:39:20  haase
   Fixed DOCTYPE parsing and generation work correctly

   Revision 1.11.2.2  2002/08/21 01:53:21  haase
   Recoded XML/HTML generation to be more general and allow strings to be used as element names; first steps towards better namespace integration into XML generation

   Revision 1.11.2.1  2002/08/09 16:55:14  haase
   Initialize xmltags hashtable

   Revision 1.11  2002/07/05 21:20:08  uid59704
   Fixed GC contract of fd_hashset_intern and added fd_hashset_intern string

   Revision 1.10  2002/06/24 17:31:33  haase
   Fixes to fdxml and fdservlet to handle new XML tags

   Revision 1.9  2002/06/24 16:29:42  haase
   Made xml parsing do case preservation, complicating the implementation xmltags

   Revision 1.8  2002/05/29 18:41:40  haase
   Fixed documentation for build-api-doc

   Revision 1.7  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.6  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
