/* C Mode */

/* htmlgen.c
   Implements html output for FDScript
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

static char vcid[] = "$Id: htmlgen.c,v 1.68 2005/08/04 23:37:21 haase Exp $";

#define FD_INLINE_STRING_OPS 1
#define FD_INLINE_CHARACTER_OPS 1
#define FD_SOURCE 1
#define HTMLGEN_EXPORT EXPORTED
#include "fdwww.h"
#include "framerd/fdtext.h"

static char *default_doctype=
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\">\n";

static enum display_level oid_display_level=get_value;
HTMLGEN_EXPORT fd_lispenv fd_html_env, fd_xml_env;
fd_lispenv fd_html_env, fd_xml_env;

#define EMPTY_TAG_CLOSE(x) (((x != NULL) && (x->is_xml)) ? " />" : ">")

static fd_lisp a_symbol, href_symbol;
static fd_lisp tag_slotid, textarea_symbol;
static fd_lisp name_symbol, value_symbol, size_symbol;
static fd_lisp cols_symbol, rows_symbol, content_symbol;

static void html_puts(fd_u8char *string);
static void html_puts_noescape(fd_u8char *string);

static int is_asciip(fd_u8char *string)
{
  while (*string)
    if (*string < 0x80) return 0;
    else string++;
  return 1;
}

#define handle_empty_tag(arg) if (FD_EMPTYP(arg)) arg=fd_make_symbol("faketag");
#define handle_empty_string(arg) if (FD_EMPTYP(arg)) arg=fd_make_string("fakestring");
#define handle_empty_fixnum(arg) if (FD_EMPTYP(arg)) arg=FD_LISPFIX(42);

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

static char *get_ascii(fd_u8char *string)
{
  struct FD_STRING_STREAM out; 
  int c=fd_sgetc(&string);
  FD_INITIALIZE_STRING_STREAM(&out,64);
  while (c >= 0) {
    if (c == '"') fd_sputs(&out,"&quot;");
    else if (c == '&') fd_sputs(&out,"&amp;");
    else if (c < 0x80) fd_sputc(&out,c);
    else {
      char buf[8]; sprintf(buf,"\\u%04x",c); fd_sputs(&out,buf);}
    c=fd_sgetc(&string);}
  return out.ptr;
}

/* Generic CGI stream operations */

static void (*_http_puts)(char *s,void *f)=NULL;
static void (*_http_putc)(int c,void *f)=NULL;
static void (*_http_putn)(char *data,int n,void *f)=NULL;

static void http_puts(char *s,fd_htstream *f)
{
  if (f == NULL) fd_fputs_encoded(s,strlen(s),stdout);
  else if (f->stream_type == stdio)
    fd_xputs_encoded(s,strlen(s),&(f->stream.xfile));
  else if (f->stream_type == sstream) fd_sputs(f->stream.sstream,s);
  else if (f->stream_type == nullstream) {}
  else if (_http_puts) _http_puts(s,f);
  else fd_raise_exception("Weird HTTP stream");
}
static void http_putc(int ch,fd_htstream *f)
{
  if (f == NULL) fd_fputc(ch,stdout);
  else if (f->stream_type == stdio) fd_xputc(ch,&(f->stream.xfile));
  else if (f->stream_type == sstream) fd_sputc(f->stream.sstream,ch);
  else if (f->stream_type == nullstream) {}
  else if (_http_putc) _http_putc(ch,f);
  else fd_raise_exception("Weird HTTP stream");
}
static void http_printf1s(fd_htstream *f,char *format,char *arg)
{
  if (f == NULL) printf(format,arg);
  else if (f->stream_type == stdio)
    fd_xprintf(&(f->stream.xfile),format,arg);
  else if (f->stream_type == sstream)
    fd_printf(f->stream.sstream,format,arg);
  else if (f->stream_type == nullstream) {}
  else if (_http_puts) {
    char tmp_buf[512]; sprintf(tmp_buf,format,arg);
    _http_puts(tmp_buf,f);}
  else fd_raise_exception("Weird HTTP stream");
}
static void http_printf1i(fd_htstream *f,char *format,int i)
{
  if (f == NULL) printf(format,i);
  else if (f->stream_type == nullstream) {}
  else if ((f->stream_type == stdio) || (f->stream_type == sstream)) {
    char tmp_buf[512]; sprintf(tmp_buf,format,i);
    http_puts(tmp_buf,f);}
  else if (_http_puts) {
    char tmp_buf[512]; sprintf(tmp_buf,format,i);
    _http_puts(tmp_buf,f);}
  else fd_raise_exception("Weird HTTP stream");
}
static void http_printf2s(fd_htstream *f,char *format,char *arg1,char *arg2)
{
  if (f == NULL) printf(format,arg1,arg2);
  else if (f->stream_type == stdio)
    fd_xprintf(&(f->stream.xfile),format,arg1,arg2);
  else if (f->stream_type == sstream)
    fd_printf(f->stream.sstream,format,arg1,arg2);
  else if (f->stream_type == nullstream) {}
  else if (_http_puts) {
    char tmp_buf[512]; sprintf(tmp_buf,format,arg1,arg2);
    _http_puts(tmp_buf,f);}
  else fd_raise_exception("Weird HTTP stream");
}
static void http_write_bytes(char *s,int n,fd_htstream *f)
{
  if (f == NULL)
    fwrite(s,sizeof(char),n,stdout);
  else if (f->stream_type == stdio)
    fwrite(s,sizeof(char),n,f->stream.xfile.f);
  else if (f->stream_type == sstream)
    fd_sputn(f->stream.sstream,s,n);
  else if (f->stream_type == nullstream) {}
  else if (_http_putn) _http_putn(s,n,f);
  else fd_raise_exception("Weird HTTP stream");
}

static void http_puts_quoted(char *string,fd_htstream *f)
{
  char *next=strchr(string,'"');
  while (next) {
    http_write_bytes(string,next-string,f);
    http_puts("&quot;",f);
    string=next+1; next=strchr(string,'"');}
  http_puts(string,f);
}

static void http_puts_ascii(fd_u8char *string,fd_htstream *h,int quoted)
{
  fd_u8char *scan=string, *last=string;
  int c=fd_sgetc(&string);
  while (c >= 0) {
    if (quoted)
      if (c == '"') http_puts("&quot;",h);
      else if (c == '<') http_puts("&lt;",h); 
      else if (c == '>') http_puts("&gt;",h); 
      else if (c == '&') http_puts("&amp;",h);
      else if (c < 0x80) http_putc(c,h);
      else {
	char buf[8]; sprintf(buf,"\\u%04x",c); http_puts(buf,h);}
    else if (c < 0x80) http_putc(c,h);
    else {
      char buf[8]; sprintf(buf,"\\u%04x",c); http_puts(buf,h);}
    c=fd_sgetc(&string);}
}

static void html_write_param
  (fd_lisp tag,lisp value,char *postfix,fd_htstream *f)
{
  fd_lisp lname=fd_xmltag_name(tag), lnamespace=fd_xmltag_namespace(tag);
  fd_u8char *name=((FD_SYMBOLP(lname)) ? (FD_SYMBOL_NAME(lname)) :
		   (FD_STRINGP(lname)) ? (FD_STRING_DATA(lname)) :
		   (fd_type_error(_("bad tag name"),lname),(fd_u8char *)NULL));
  fd_u8char *namespace=
    ((FD_FALSEP(lnamespace)) ? (NULL) :
     (FD_SYMBOLP(lnamespace)) ? (FD_SYMBOL_NAME(lnamespace)) :
     (FD_STRINGP(lnamespace)) ? (FD_STRING_DATA(lnamespace)) : (NULL));
  if (f->stream_type == nullstream) return;
  if (namespace) {
    http_puts_ascii(namespace,f,0);
    http_puts(":",f);}
  http_puts_ascii(name,f,0);
  http_puts("=\"",f);
  if (STRINGP(value))
    http_puts_ascii(FD_STRING_DATA(value),f,1);
  else if (SYMBOLP(value))
    http_puts_ascii(FD_SYMBOL_NAME(value),f,1);
  else {
    struct FD_STRING_STREAM ss;
    FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
    fd_print_lisp_to_string(value,&ss);
    if (is_asciip(ss.ptr)) http_puts_quoted(ss.ptr,f);
    else {
      char *astring=get_ascii(ss.ptr);
      http_puts_quoted(astring,f); fd_xfree(astring);}
    fd_xfree(ss.ptr);}
  http_puts("\"",f); if (postfix) http_puts(postfix,f);
}

static void uri_encode(fd_u8char *input,struct FD_STRING_STREAM *ss);
static void uri_encode_local(fd_u8char *input,struct FD_STRING_STREAM *ss);

HTMLGEN_EXPORT
/* fd_set_http_output_methods:
     Arguments: three functions 
     Returns: void

  Sets the methods used for HTTP output.  The first function
is for outputting null-terminated strings in their entirety; the
second function is for outputing single characters; and the third function
is for outputing substrings given a start and a length.
*/
void fd_set_http_output_methods
  (void (*_puts)(char *,void *),void (*_putc)(int,void *),
   void (*_putn)(char *,int,void *))
{
  _http_puts=_puts; _http_putc=_putc; _http_putn=_putn;
}

/** State utility functions **/

#define FRAMERD_URL \
  "<A HREF=\"http://www.framerd.org/\">Framer<sub><em><strong>D</strong></em></sub></A>"
#define FRAMERD_CREDIT \
   "<P>This page was dynamically generated by FDScript, the scripting \
language for <A HREF=\"http://www.framerd.org/\">Framer<sub><strong><em>D</em></strong></sub></A> databases and applications."

static lisp html_name_symbol, timestamp_symbol, qmark_symbol, doctype_symbol, html_symbol;
static lisp xmltag_tag, img_symbol, src_symbol, p_symbol;

static lisp obj_name_symbol, anonymous_symbol;
static lisp html_methods_symbol, frame_symbol;
static lisp quote_symbol, current_file_symbol;
static lisp body_style_symbol, head_symbol, body_symbol;

FDSCRIPT_EXPORT lisp fd_get_method(lisp obj,lisp slot);
FDSCRIPT_EXPORT fd_exception fd_SyntaxError;

/** HTTP generation infrastructure **/

#if (FD_THREADS_ENABLED)
#define thget(var,key) fd_tld_get(key)
#define thset(var,key,val) fd_tld_set(key,(void *)val)
static fd_tld_key doctype_key;
static fd_tld_key local_frames_key;
static fd_tld_key http_output_key;
static fd_tld_key cookie_key;
#else
#define thget(var,key) (var)
#define thset(var,key,val) var=val
static char *doctype=NULL;
static fd_hashset local_frames=NULL;
static struct FD_HTTP_STREAM *http_output=NULL;
static char *cookie=NULL;
#endif

/** Looking up browse URLS */

static struct FDWWW_BROWSE_URLS {
  fd_pool pool; fd_lisp url;
  struct FDWWW_BROWSE_URLS *next;} *browse_urls;

#if (FD_THREADS_ENABLED)
static fd_mutex browse_url_lock;
static fd_lisp browse_url;
#else
static fd_lisp browse_url;
#endif

STATIC_INLINE fd_lisp get_browse_method(fd_pool p)
{
  struct FDWWW_BROWSE_URLS *scan;
  fd_lock_mutex(&browse_url_lock);
  if (p) scan=browse_urls; else scan=NULL;
  while (scan)
    if (scan->pool == p) break; else scan=scan->next;
  fd_unlock_mutex(&browse_url_lock);
  if (scan) return fd_incref(scan->url);
  else return fd_incref(browse_url);
}

HTMLGEN_EXPORT
void fd_set_browse_url(fd_lisp url,fd_pool p)
{
  struct FDWWW_BROWSE_URLS *scan;
  fd_lock_mutex(&browse_url_lock);
  /* A NULL pool means set the default entry */
  if (p == NULL) {
    fd_decref(browse_url);
    browse_url=fd_incref(url);
    fd_unlock_mutex(&browse_url_lock);
    return;}
  else scan=browse_urls;
  /* Find any existing entry */
  while (scan)
    if (scan->pool == p) break; else scan=scan->next;
  /* Change the existing entry or make a new one. */
  if (scan) {
    fd_decref(scan->url); scan->url=fd_incref(url);}
  else {
    scan=fd_xmalloc(sizeof(struct FDWWW_BROWSE_URLS));
    scan->url=fd_incref(url); scan->pool=p;
    scan->next=browse_urls; browse_urls=scan;}
  fd_unlock_mutex(&browse_url_lock);
}

static void generate_browse_url(fd_lisp browser,fd_lisp oid,fd_string_stream s)
{
  if (FD_STRINGP(browser)) {
    char buf[64];
    fd_sputs(s,FD_STRING_DATA(browser)); fd_sputc(s,'?');
    sprintf(buf,"@%x/%x",FD_OID_ADDR_HIGH(oid),FD_OID_ADDR_LOW(oid));
    fd_sputs(s,buf);}
  else if (FD_PAIRP(browser)) {
    fd_lisp scan=FD_CDR(browser); char buf[64];
    fd_sputs(s,fd_strdata(FD_CAR(browser))); fd_sputc(s,'?');
    while ((FD_PAIRP(scan)) && (FD_PAIRP(FD_CDR(scan)))) {
      fd_lisp var=FD_CAR(scan), val=FD_CAR(FD_CDR(scan));
      uri_encode(fd_symbol_name(var),s); fd_sputs(s,"=");
      if (FD_XPROCP(val)) {
	fd_lisp real_val=fd_lisp_call(val,oid);
	if (FD_STRINGP(real_val)) 
	  uri_encode(fd_strdata(real_val),s);
	else {
	  struct FD_STRING_STREAM vs;
	  FD_INITIALIZE_STRING_STREAM(&vs,256);
	  vs.fancy_oids=0; vs.escape=1;
	  fd_print_lisp_to_string(real_val,&vs);
	  uri_encode(vs.ptr,s); fd_xfree(vs.ptr);}
	fd_decref(real_val);}
      else {
	struct FD_STRING_STREAM vs;
	FD_INITIALIZE_STRING_STREAM(&vs,256);
	vs.fancy_oids=0; vs.escape=1;
	fd_print_lisp_to_string(val,&vs);
	uri_encode(vs.ptr,s); fd_xfree(vs.ptr);}
      fd_sputs(s,"&");
      scan=FD_CDR(FD_CDR(scan));}
    if (FD_EMPTY_LISTP(scan))
      fd_sputs(s,"FRAME=");
    else {uri_encode(fd_symbol_name(FD_CAR(scan)),s); fd_sputs(s,"=");}
    sprintf(buf,"@%x/%x",FD_OID_ADDR_HIGH(oid),FD_OID_ADDR_LOW(oid));
    fd_sputs(s,buf); fd_sputs(s,"&");}
  else if (FD_XPROCP(browser)) {
    fd_lisp result=fd_lisp_call(browser,oid);
    if (FD_STRINGP(result)) fd_sputs(s,FD_STRING_DATA(result));
    else fd_raise_lisp_exception(fd_Type_Error,"Base browse URL returned",result);
    fd_decref(result);}
  else fd_type_error(_("not a good browse URL"),browser);
}

static void http_browse_url(fd_lisp oid,fd_htstream *hs)
{
  fd_pool p=fd_get_pool(oid);
  fd_lisp browser=get_browse_method(p);
  if (hs->stream_type == sstream) generate_browse_url(browser,oid,hs->stream.sstream);
  else {
    struct FD_STRING_STREAM url_stream;
    FD_INITIALIZE_STRING_STREAM(&url_stream,128);
    generate_browse_url(browser,oid,&url_stream);
    http_puts(url_stream.ptr,hs); fd_xfree(url_stream.ptr);}
  fd_decref(browser);
}

/* Getting HTTP state variables */

struct FD_HTTP_STREAM stdout_htstream;

static fd_htstream *get_http_output()
{
  fd_htstream *tfile=thget(http_output,http_output_key);
  if (tfile) return tfile;
  else return &(stdout_htstream);
}
HTMLGEN_EXPORT
/* fd_get_http_output:
    Arguments: none
    Returns: a pointer to an fd_htstream
Returns the current HTTP output stream. */
fd_htstream *fd_get_http_output()
{
  return get_http_output();
}

static enum http_generation_phase get_http_phase() 
{
  fd_htstream *tfile=thget(http_output,http_output_key);
  if (tfile) return tfile->phase;
  else return stdout_htstream.phase;
}
static char *get_cookie() 
{
  return (char *) (thget(cookie,cookie_key));
}

static fd_hashset get_local_frames()
{
  fd_hashset h=thget(local_frames,local_frames_key);
  if (h) return h; else return NULL;
}

static char *get_doctype() 
{
  return (char *) (thget(doctype,doctype_key));
}

/* Setting HTTP state variables */

void set_http_output(fd_htstream *f)
{
  thset(http_output,http_output_key,f);
}

static void set_http_phase(enum http_generation_phase p)
{
  fd_htstream *tfile=thget(http_output,http_output_key);
  if (tfile) tfile->phase=p;
  else stdout_htstream.phase=p;
}
HTMLGEN_EXPORT void fd_set_http_phase(enum http_generation_phase p) { set_http_phase(p); }


HTMLGEN_EXPORT
/* fd_start_http_output:
     Arguments: a pointer to an fd_htstream
     Returns: void
  Begins output to the designated htstream, setting it
as the thread-local HTTP output stream and setting the initial
phasse of the HTTP output process.
*/
void fd_start_http_output(fd_htstream *s)
{
  set_http_output(s); s->phase=http_head;
}

HTMLGEN_EXPORT
/* fd_http_puts:
     Arguments: a pointer to a null-terminated string and an FD_HTTP_STREAM struct
     Returns: void
  Writes the string to the stream.
*/
void fd_http_puts(char *s,fd_htstream *stream)
{
  http_puts(s,stream);
}

HTMLGEN_EXPORT
/* fd_http_puts:
     Arguments: a pointer to a null-terminated string and an FD_HTTP_STREAM struct
     Returns: void
  Writes the string to the stream.
*/
void fd_http_write_bytes(char *s,int n,fd_htstream *stream)
{
  http_write_bytes(s,n,stream);
}

static fd_lisp lisp_http_flush_cproc()
{
  struct FD_HTTP_STREAM *hts=get_http_output();
  if ((hts) && (hts->stream_type == stdio)) fflush(hts->stream.xfile.f);
  return FD_VOID;
}

void fd_set_cookie(char *cookie)
{
  enum http_generation_phase ph=get_http_phase();
  if (ph == http_any) {
    set_http_phase(http_head); ph=http_head;}
  if (ph == http_head) {
    fd_htstream *out=get_http_output(); 
    http_printf1s(out,"Set-Cookie: %s\n",cookie);}
  else fd_raise_exception("Too late to set cookie");
}

static lisp lisp_set_doctype_cproc(lisp name,lisp location)
{
  enum http_generation_phase ph=get_http_phase();
  if (ph == http_any) {ph=http_head; set_http_phase(ph);}
  if (!(FD_XMLTAGP(name))) fd_type_error("Doctype ID not an XML tag",name);
  else if (!(STRINGP(location)))
    fd_type_error("Doctype location not a string",location);
  else if ((ph == http_head) || (ph == http_body)) {
    char *buf=fd_malloc(64+strlen(SYMBOL_NAME(name))+STRING_LENGTH(location));
    char first_char=STRING_DATA(location)[0];
    if ((first_char == '+') || (first_char == '-'))
      sprintf(buf,"<!DOCTYPE %s PUBLIC \"%s\">\n",
	      SYMBOL_NAME(name),STRING_DATA(location));
    else sprintf(buf,"<!DOCTYPE %s SYSTEM \"%s\">\n",
		 SYMBOL_NAME(name),STRING_DATA(location));
    thset(doctype,doctype_key,buf);}
  else fd_raise_exception("Too late to set document type");
  return FD_VOID;
}

static int html_eltp(fd_lisp arg)
{
  fd_lisp name=fd_xmltag_name(arg);
  if (FD_SYMBOLP(name))
    if (strcmp(FD_SYMBOL_NAME(name),"HTML") == 0) return 1;
    else return 0;
  else if (FD_STRINGP(name))
    if ((strcmp(FD_STRING_DATA(name),"html") == 0) ||
	(strcmp(FD_STRING_DATA(name),"HTML") == 0))
      return 1;
    else return 0;
  else return 0;
}

static void output_elt_name(fd_lisp elt_name,fd_htstream *out);

static lisp lisp_doctype_lexpr(fd_lisp args)
{
  fd_lisp root_elt=fd_get_arg(args,0,FD_VOID);
  fd_lisp location=fd_get_arg(args,1,FD_VOID);
  fd_lisp mime_type=fd_get_arg(args,2,FD_FALSE);
  struct FD_TEXT_ENCODING *enc=fd_get_default_encoding(); 
  fd_htstream *out=get_http_output();
  enum http_generation_phase ph=out->phase;
  int is_html=html_eltp(root_elt); 
  if (!(FD_XMLTAGP(root_elt))) fd_type_error(_("Doctype root elemen not an XML tag"),root_elt);
  else if (!(STRINGP(location)))
    fd_type_error(_("DTD specification is not a string"),location);
  else if ((ph == http_any) || (ph == http_head) || (ph == http_body))
    if (FD_FALSEP(mime_type))
      if (is_html)
	fd_start_http("text/html");
      else fd_start_http("text/xml");
    else fd_start_http(fd_strdata(mime_type));
  else fd_raise_exception(_("Too late to set mime type"));
  {
    http_printf1s(out,"<?xml version='1.0' encoding='%s' standalone='yes' ?>\n",enc->names[0]);
    http_puts("<!DOCTYPE ",out); output_elt_name(root_elt,out);
    if ((fd_strdata(location)[0] == '+') || (fd_strdata(location)[0] == '-'))
      http_printf1s(out," PUBLIC \"%s\">\n",STRING_DATA(location));
    else http_printf1s(out," SYSTEM \"%s\">\n",STRING_DATA(location));
    if (is_html) out->phase=http_body;
    else out->phase=xml_content;}
  out->is_xml=3;
  return FD_VOID;
}

/* Adds a frame to the local frames of the current document */
lisp fd_declare_local_frame(lisp frame)
{
  fd_hashset local=get_local_frames();
  if (local == NULL) {
    local=fd_make_hashset(128);
    thset(local_frames,local_frames_key,local);}
  fd_hashset_add(local,frame);
  return FD_TRUE;
}

/** HTTP generation functions **/

static void http_header(char *s)
{
  fd_htstream *out=get_http_output();
  if (out->phase == http_head) http_puts(s,out);
  else if (out->phase == http_any) {
    out->phase=http_head;
    http_puts(s,out);}
  else fd_raise_exception("Too late to specify http headers");
}

static void finish_http_header()
{
  fd_htstream *out=get_http_output();
  if (out->phase == http_any) return;
  if (out->phase == http_body) return;
  http_puts("\r\n",out);
  out->phase=http_body;
}

static void html_header(char *s)
{
  fd_htstream *out=get_http_output();
  enum http_generation_phase ph=out->phase;
  if (ph == http_any) return;
  if ((ph == http_head) || (ph == http_body)) {
    char *doctype=get_doctype();
    if (ph == http_head) finish_http_header();
    if (doctype) http_puts(doctype,out);
    else http_puts(default_doctype,out);
    http_puts("<html><head>\n",out);
    out->phase=html_head;
    ph=html_head;}
  if (ph == html_head) html_puts_noescape(s);
  else fd_raise_exception("Too late to generate HTML head");
}

static void start_body()
{
  fd_htstream *out=get_http_output();
  enum http_generation_phase ph=out->phase;
  if (ph == http_any) return;
  if (ph == html_body) return;
  if (ph == html_frameset) return;
  if (ph == xml_content) return;
  if ((ph == http_head) || (ph == http_body)) {
    char *doctype=get_doctype();
    finish_http_header();
    if (doctype) http_puts(doctype,out);
    else http_puts(default_doctype,out);
    http_puts("<html><head>\n<title>FramerD Generated Page</title>\n",out);
    out->phase=html_head;}
  if (out->phase == html_head) {
    lisp style_info=fd_thread_symeval(body_style_symbol); char *sinfo;
    if (STRINGP(style_info)) sinfo=STRING_DATA(style_info); else sinfo=NULL;
    if (sinfo) http_printf1s(out,"</head>\n<body style=\"%s\">\n",sinfo);
    else http_puts("</head>\n<body>\n",out);
    decref(style_info);
    out->phase=html_body;}    
}

/* This really just encodes in latin1, making a parorchial assumption
   which may be neccessary (at this point in history) for URL paths. */
static void uri_encode_local(fd_u8char *input,struct FD_STRING_STREAM *ss)
{
  fd_u8char *scan=input;
  while (*scan)
    if (*scan == ' ') {scan++; fd_sputc(ss,'+');}
    else if ((*scan == '"') ||
	     (*scan == '&') || (*scan == '=') ||
	     (*scan == '+') || (*scan == '#')) {
      char buf[4]; sprintf(buf,"%%%x",*scan++); fd_sputs(ss,buf);}
    else if (*scan >= 0x80) {
      char buf[8]; int c=fd_sgetc(&scan);
      if (c < 0x100) {
	sprintf(buf,"%%%02x",c); fd_sputs(ss,buf);}
      else {
	sprintf(buf,"\\u%04x",c);
	fd_raise_detailed_exception(fd_NoLocalChar,buf);}}
    else {int c=*scan++; fd_sputc(ss,c);}
}

static void uri_encode(fd_u8char *input,struct FD_STRING_STREAM *ss)
{
  fd_u8char *scan=input;
  while (*scan)
    if (*scan == ' ') {scan++; fd_sputc(ss,'+');}
    else if ((*scan == '"') ||
	     (*scan == '&') || (*scan == '=') ||
	     (*scan == '+') || (*scan == '#')) {
      char buf[4]; sprintf(buf,"%%%x",*scan++); fd_sputs(ss,buf);}
    else if (*scan >= 0x80) {
      char buf[8]; int c=fd_sgetc(&scan);
      if (c < 0x100) {
	sprintf(buf,"%%%02x",c); fd_sputs(ss,buf);}
      else {sprintf(buf,"\\u%04x",c); fd_sputs(ss,buf);}}
    else {int c=*scan++; fd_sputc(ss,c);}
}

/* HTML Generation */

#define html_special_char(c) \
   (((c)>=0x80) || (c == '<') || (c == '>') || (c == '&'))
#define oid_start(string) \
   (((*string) == '@') && ((string[1] == '/') | (isxdigit(string[1]))))


static lisp parse_literal_oid_ref(fd_u8char **string);
static void html_display_oid(lisp oid,fd_htstream *out);

/* html_display_string_internal:
     Arguments: a UTF-8 string, an htstream, and two flags
     Returns: void
  This outputs the string to the specified ports, doing 3 special
operations:
    1. HTML special characters (<>&) are emitted as &; escapes
    2. Non-ASCII characters are also emitted as &; escapes
    3. OID references are turned into ANCHORS if possible
 If the first flag argument is non-zero, the conversion to anchors
is not done; if the second flag argument is non zero, spaces will
be output as non-breakable spaces.
*/
static void html_display_string_internal
   (fd_u8char *string,fd_htstream *out,int no_oids,int nbsp)
{
  fd_u8char *scan=string, *start=scan;
  while (*scan) {
    int c;
    if (nbsp)
      while ((*scan) && (*scan != ' ') &&
	     (!(html_special_char(*scan))) &&
	     ((no_oids) || (!(oid_start(scan)))))
	scan++;
    else while ((*scan) && 
		(!(html_special_char(*scan))) &&
		((no_oids) || (!(oid_start(scan)))))
      scan++;
    if (scan > start) http_write_bytes(start,scan-start,out);
    if (*scan == '\0') return;
    if (*scan>=0x80) c=fd_sgetc(&scan); else c=*scan++;
    start=scan;
    switch (c) {
    case '<': http_puts("&lt;",out); break;
    case '>': http_puts("&gt;",out); break;
    case '&': http_puts("&amp;",out); break;
    case '"': http_puts("&quot;",out); break;
    case ' ':
      if (nbsp) http_puts("&nbsp;",out);
      else http_puts(" ",out);
      break;
    case '@': {
      lisp oid=parse_literal_oid_ref(&scan);
      if (OIDP(oid)) html_display_oid(oid,out);
      else {http_puts("@",out); break;}
      start=scan;
      break;}
    default: http_printf1i(out,"&#%d;",c); break;}}
}

static lisp parse_literal_oid_ref(fd_u8char **string)
{
  FD_OID id;
  fd_u8char *start=*string, tmp_buf[32];
  while (isxdigit(**string)) (*string)++;
  if ((*string == start) || (**string != '/')) {
    *string=start; return FD_VOID;}
  /* Don't run out of space on malformed OIDS */
  if (((*string)-start) >= 32) {*string=start; return FD_VOID;}
  /* Copy the high part and parse it */
  strncpy(tmp_buf,start,(*string)-start); tmp_buf[(*string)-start]=0;
  FD_SET_OID_HIGH(id,strtoul(tmp_buf,NULL,16));
  start=(*string)+1; (*string)++; while (isxdigit(**string)) (*string)++;
  if (*string == start) {*string=start; return FD_VOID;}
  /* Don't run out of space on malformed OIDS */
  if (((*string)-start) >= 32) {*string=start; return FD_VOID;}
  /* Copy the low part and parse it */
  strncpy(tmp_buf,start,(*string)-start); tmp_buf[(*string)-start]=0;
  FD_SET_OID_LOW(id,strtoul(tmp_buf,NULL,16));
  /* Return the OID */
  return fd_make_oid(id);
}

static void html_display_oid(lisp oid,fd_htstream *out)
{
  FD_OID id=FD_OID_ADDR(oid); lisp value; char buf[128];
  fd_hashset local_frames=get_local_frames();
  if ((local_frames) && (fd_hashset_get(local_frames,oid))) {
    sprintf(buf,"<a href=\"#%x/%x\" class='oidref'>",FD_OID_HIGH(id),FD_OID_LOW(id));
    http_puts(buf,out);}
  else {
    http_puts("<a href='",out);
    http_browse_url(oid,out);
    http_puts("' class='oidref'>",out);}
  if (oid_display_level == hide_value) value=FD_VOID;
  else value=fd_oid_value(oid);
  if (SLOTMAPP(value)) {
    lisp name=fd_prim_get(value,html_name_symbol);
    if (FD_EMPTYP(name))
      name=fd_prim_get(value,obj_name_symbol);      
    if (FD_EMPTYP(name)) {
      sprintf(buf,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
      http_puts(buf,out);}
    else if (FD_STRINGP(name)) {
      if (FD_STRING_LENGTH(name) < 32)
	html_display_string_internal(FD_STRING_DATA(name),out,1,1);
      else html_display_string_internal(FD_STRING_DATA(name),out,1,0);}
    else {
      fd_u8char *string=fd_object_to_string(name); 
      if (strlen(string) < 32)
	html_display_string_internal(string,out,1,1);
      else html_display_string_internal(string,out,1,0);
      fd_xfree(string);}
    fd_decref(name);}
  else {
      sprintf(buf,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
      http_puts(buf,out);}
  fd_decref(value);
  http_puts("</A>",out);
}

static void html_puts(fd_u8char *string)
{
  fd_htstream *out=get_http_output();
  enum http_generation_phase phase=out->phase;
  if (phase == html_body)
    html_display_string_internal(string,out,0,0);
  else html_display_string_internal(string,out,1,0);
}

static void html_puts_noescape(fd_u8char *string)
{
  int c; fd_htstream *out=get_http_output();
  while ((c=fd_sgetc(&string))>=0) {
    if (c < 0x80) http_putc(c,out);
    else http_printf1i(out,"&#%d;",c);}
}

static void html_puts_noescape_nobreak(fd_u8char *string)
{
  int c; fd_htstream *out=get_http_output();
  while ((c=fd_sgetc(&string))>=0) {
    if (c == ' ') http_printf1i(out,"&nbsp;",c);
    else if (c < 0x100) http_putc(c,out);
    else http_printf1i(out,"&#%d;",c);}
}

static void html_puts_param(fd_u8char *string)
{
  int c; fd_htstream *out=get_http_output();
  while ((c=fd_sgetc(&string))>=0) {
    if (c == '"') http_puts("&quot;",out);
    else if (c == '<') http_puts("&lt;",out);
    else if (c == '>') http_puts("&gt;",out);
    else if (c < 0x80) http_putc(c,out);
    else http_printf1i(out,"\\u%04x",c);}
}

static void html_printout(lisp args,lispenv env)
{
  int eval=1;
  DOLIST(arg,args) {
    lisp value=((eval) ? (fd_eval_in_env(arg,env)) : (incref(arg)));
    if (FD_VOIDP(value)) {}
    else if (STRINGP(value))
      html_puts_noescape(STRING_DATA(value));
    else {
      struct FD_STRING_STREAM ss;
      FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
      fd_print_lisp_to_string(value,&ss);
      html_puts(ss.ptr); fd_xfree(ss.ptr);}
    decref(value);}
}

static void html_write(lisp value)
{
  struct FD_STRING_STREAM ss;
  FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
  fd_print_lisp_to_string(value,&ss);
  html_puts(ss.ptr); fd_xfree(ss.ptr);
}

static void html_url_puts(fd_u8char *s,fd_htstream *out)
{
  while (*s) {
    int c=*s++;
    if (c < 0x80) http_putc(c,out);
    else http_printf1i(out,"%%%x",c);}
}

HTMLGEN_EXPORT
void fd_html_write(fd_lisp x) { html_write(x); }

/* Generating HTML commands */

static void output_elt_name(fd_lisp elt_name,fd_htstream *out)
{
  if (FD_SYMBOLP(elt_name))
    if (out->is_xml) {
      fd_u8char *scan=FD_SYMBOL_NAME(elt_name), *limit=scan+strlen(scan);
      while (scan < limit) {
	int c=fd_sgetc(&scan);
	http_putc(fd_tolower(c),out);}}
    else http_printf1s(out,"%s",FD_SYMBOL_NAME(elt_name));
  else if (FD_LRECORD_TYPEP(elt_name,xmltag_tag)) {
    fd_lisp ns=fd_xmltag_namespace(elt_name);
    fd_lisp name=fd_xmltag_name(elt_name);
    if (FD_FALSEP(ns))
      http_printf1s(out,"%s",FD_STRING_DATA(name));
    else if (FD_SYMBOLP(ns))
      http_printf2s(out,"%s:%s",FD_SYMBOL_NAME(ns),FD_STRING_DATA(name));
    else if (FD_STRINGP(ns))
      http_printf2s(out,"{%s}%s",FD_STRING_DATA(ns),FD_STRING_DATA(name));
    else fd_type_error("Bad XML tag",elt_name);}
  else if (FD_STRINGP(elt_name)) 
    if (fd_xmltagp(elt_name))
      http_printf1s(out,"%s",FD_STRING_DATA(elt_name));
    else fd_type_error("Not a valid element name",elt_name);
  else fd_type_error("Not a valid element name",elt_name);
}

/* Outputs an HTML environment start tag, with particular arguments,
   returning the string for the actual name, converting a name FOO*
   into simply FOO.  The name returned in this way can be used to
   close the environment. */
static void open_element(fd_lisp elt_name,lisp params,lispenv params_env,
			 int eval_params,fd_u8char *pre,fd_u8char *post)
{
  fd_htstream *out=get_http_output();
  start_body();
  http_puts(pre,out); output_elt_name(elt_name,out);
  if (FD_FALSEP(params)) http_puts(post,out);
  else if (FD_EMPTY_LISTP(params)) http_puts(post,out);
  else if (STRINGP(params)) {
    http_putc(' ',out); html_url_puts(STRING_DATA(params),out);
    http_puts(post,out);}
  else if (PAIRP(params)) {
    while ((PAIRP(params)) && (PAIRP(CDR(params)))) {
      lisp var=fd_get_arg(params,0,FD_VOID);
      lisp val_expr=fd_get_arg(params,1,FD_VOID);
      lisp val=((eval_params) ? (fd_eval_in_env(val_expr,params_env)) :
		(fd_incref(val_expr)));
      params=FD_CDR(FD_CDR(params));
      /* If the value is the empty choice, we skip the parameter entirely */
      if (FD_EMPTYP(val)) continue;
      /* We output the variable name and open quote here. */
      http_putc(' ',out); output_elt_name(var,out); http_puts("=\"",out);
      if (OIDP(val)) {
	char buf[64];
	sprintf(buf,"@%x/%x\"",OID_ADDR_HIGH(val),OID_ADDR_LOW(val));
	http_puts(buf,out);}
      else if (STRINGP(val)) {
	html_puts_param(STRING_DATA(val)); http_putc('"',out);}
      else {
	fd_u8char *string=fd_object_to_string(val);
	html_puts_param(string); http_putc('"',out);
	free(string);}
      fd_decref(val);}
    if (FD_PAIRP(params)) {
      fd_lisp tail=FD_CAR(params);
      if (FD_STRINGP(tail))
	http_printf1s(out," %s",FD_STRING_DATA(tail));
      else if (FD_SYMBOLP(tail))
	http_printf1s(out," %s",FD_SYMBOL_NAME(tail));
      else {
	fd_u8char *string=fd_object_to_string(tail);
	html_puts_param(string); fd_xfree(string);}}
    http_puts(post,out);}
  else fd_raise_exception("Weird parameter list");
}

static void close_element(fd_lisp elt_name)
{
  fd_htstream *out=get_http_output();
  http_puts("</",out); output_elt_name(elt_name,out);
  http_puts(">",out);
}

static void output_element
  (fd_lisp elt_name,fd_lisp elt_attribs,fd_lisp contents,
   fd_lispenv env,int newline,int eval_params)
{
  fd_htstream *out=get_http_output();
  if (FD_EMPTY_LISTP(contents))
    open_element(elt_name,elt_attribs,env,eval_params,"<"," />");
  else {
    UNWIND_PROTECT {
      open_element(elt_name,elt_attribs,env,eval_params,"<",">");
      if (newline) http_putc('\n',out);
      html_printout(contents,env);
      if (newline) http_putc('\n',out);}
    ON_UNWIND {
      close_element(elt_name);
      if (newline) http_putc('\n',out);}
    END_UNWIND;}
}

/* Generic output */

static int valid_env_specp(lisp expr)
{
  if ((FD_PAIRP(expr)) && (FD_PAIRP(FD_CDR(expr))) &&
      (FD_XMLTAGP(FD_CAR(expr)))) {
    fd_lisp params=FD_CDR(expr);
    while ((FD_PAIRP(params)) && (FD_PAIRP(FD_CDR(params))))
      if (FD_XMLTAGP(FD_CAR(params))) {
	params=FD_CDR(FD_CDR(params));}
      else return 0;
    return 1;}
  else if ((FD_PAIRP(expr)) && (FD_EMPTY_LISTP(FD_CDR(expr))) &&
	   (FD_XMLTAGP(FD_CAR(expr))))
    return 1;
  else return 0;
}

static lisp markup_handler(lisp expr,lispenv env)
{
  lisp envspec=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp env_name, env_params;
  handle_empty_tag(envspec);
  if (FD_XMLTAGP(envspec))  {
    env_name=envspec; env_params=FD_EMPTY_LIST;}
  else if (valid_env_specp(envspec)) {
    env_name=fd_get_arg(envspec,0,FD_VOID);
    env_params=fd_get_body(envspec,1);}
  else fd_type_error(_("Invalid env spec"),envspec);
  output_element(env_name,env_params,fd_get_body(expr,2),env,1,0);
  decref(envspec);
  return FD_VOID;
}

static lisp imarkup_handler(lisp expr,lispenv env)
{
  lisp envspec=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp env_name, env_params;
  handle_empty_tag(envspec);
  if (FD_XMLTAGP(envspec))  {
    env_name=envspec; env_params=FD_EMPTY_LIST;}
  else if (valid_env_specp(envspec)) {
    env_name=fd_get_arg(envspec,0,FD_VOID);
    env_params=fd_get_body(envspec,1);}
  else fd_type_error(_("Invalid env spec"),envspec);
  output_element(env_name,env_params,fd_get_body(expr,2),env,0,0);
  decref(envspec);
  return FD_VOID;
}

static lisp nl_handler(lisp expr,lispenv env)
{
  fd_htstream *out=get_http_output();
  http_putc('\n',out);
  return FD_VOID;
}

/* HTMLENV is like MARKUP but doesn't evaluate the element spec.
   However parameters in the markup spec are evaluated.  */
static lisp htmlenv_handler(lisp expr,lispenv env)
{
  lisp envspec=fd_get_arg(expr,1,FD_VOID);
  lisp env_name, env_params;
  if (FD_SYMBOLP(envspec)) {
    env_name=envspec; env_params=FD_EMPTY_LIST;}
  if (valid_env_specp(envspec)) {
    env_name=fd_get_arg(envspec,0,FD_VOID);
    env_params=fd_get_body(envspec,1);}
  else fd_type_error(_("Invalid env spec"),envspec);
  output_element(env_name,env_params,fd_get_body(expr,2),env,1,1);
  return FD_VOID;
}

/** Top level handlers **/

static struct FD_HASHTABLE normalized_elt_names; 

static fd_lisp get_normalized_elt_name(fd_lisp elt_name)
{
  fd_lisp normalized=
    fd_hashtable_get(&normalized_elt_names,elt_name,FD_FALSE);
  if (FD_FALSEP(normalized)) return elt_name;
  else return normalized;
}

static lisp html_inline_handler(lisp expr,lispenv env)
{
  lisp elt_name=fd_get_arg(expr,0,FD_FALSE);
  lisp contents=fd_get_body(expr,1);
  output_element(elt_name,FD_EMPTY_LIST,contents,env,0,0);
  return FD_VOID;
}

static lisp html_block_handler(lisp expr,lispenv env)
{
  lisp elt_name=fd_get_arg(expr,0,FD_FALSE);
  lisp contents=fd_get_body(expr,1);
  output_element(elt_name,FD_EMPTY_LIST,contents,env,1,0);
  return FD_VOID;
}

static lisp htmlattr_inline_handler(lisp expr,lispenv env)
{
  lisp elt_name=fd_get_arg(expr,0,FD_FALSE);
  lisp elt_attrs=fd_get_arg(expr,1,FD_FALSE);
  lisp contents=fd_get_body(expr,2);
  if (FD_SYMBOLP(elt_name)) {
    fd_lisp normalized=get_normalized_elt_name(elt_name);
    output_element(normalized,elt_attrs,contents,env,0,1);}
  else output_element(elt_name,elt_attrs,contents,env,0,1);
  return FD_VOID;
}

static lisp htmlattr_block_handler(lisp expr,lispenv env)
{
  lisp elt_name=fd_get_arg(expr,0,FD_FALSE);
  lisp elt_attrs=fd_get_arg(expr,1,FD_FALSE);
  lisp contents=fd_get_body(expr,2);
  if (FD_SYMBOLP(elt_name)) {
    fd_lisp normalized=get_normalized_elt_name(elt_name);
    output_element(normalized,elt_attrs,contents,env,1,1);}
  else output_element(elt_name,elt_attrs,contents,env,1,1);
  return FD_VOID;
}

/* We have a special case for the P tag which is a block level
   element where we don't put a newline after the start tag
   and before the end tag. */

static lisp p_handler(lisp expr,lispenv env)
{
  lisp elt_name=p_symbol;
  lisp elt_attrs=FD_EMPTY_LIST;
  lisp contents=fd_get_body(expr,1);
  output_element(elt_name,elt_attrs,contents,env,0,0);
  html_puts("\n");
  return FD_VOID;
}

static lisp pattr_handler(lisp expr,lispenv env)
{
  lisp elt_name=p_symbol;
  lisp elt_attrs=fd_get_arg(expr,1,FD_FALSE);
  lisp contents=fd_get_body(expr,2);
  output_element(elt_name,elt_attrs,contents,env,0,1);
  html_puts("\n");
  return FD_VOID;
}

/* Generating empty tags */

static lisp output_empty_element
   (lisp args,fd_lispenv env,fd_u8char *pre,fd_u8char *post)
{
  lisp elt_name=CAR(args), elt_attribs=CDR(args);
  open_element(elt_name,elt_attribs,env,(env != NULL),pre,post);
  return FD_VOID;
}

static lisp htmlempty_lexpr(lisp args)
{
  return output_empty_element(args,NULL,"<",">");
}

static lisp xmlempty_lexpr(lisp args)
{
  return output_empty_element(args,NULL,"<"," />");

}

static lisp xmlpi_lexpr(lisp args)
{
  return output_empty_element(args,NULL,"<?","?>");
}

static lisp html_empty_handler(lisp expr,lispenv env)
{
  fd_lisp elt_name=fd_get_arg(expr,0,FD_FALSE);
  fd_lisp attrs=fd_get_body(expr,1);
  fd_lisp element_spec;
  fd_htstream *out=get_http_output();
  if (FD_SYMBOLP(elt_name))
    element_spec=FD_MAKE_PAIR(get_normalized_elt_name(elt_name),fd_incref(attrs));
  else element_spec=FD_MAKE_PAIR(fd_incref(elt_name),attrs);
  output_empty_element(element_spec,env,"<",((out->is_xml) ? " />" : ">"));
  fd_decref(element_spec);
  return FD_VOID;
}


/* Declaring HTML functions */

void fd_add_html_inline_tag(char *name)
{
  char *primary=fd_xmalloc(strlen(name)+2+5);
  char *alternate=fd_xmalloc(strlen(name)+2+5);
  sprintf(primary,"%s",name);
  sprintf(alternate,"%s*",name);
  fd_hashtable_set
    (&normalized_elt_names,fd_make_symbol(alternate),fd_make_symbol(primary));
  fd_add_special_form(fd_html_env,primary,html_inline_handler);
  fd_add_special_form(fd_html_env,alternate,htmlattr_inline_handler);
}

void fd_add_html_block_tag(char *name)
{
  char *primary=fd_xmalloc(strlen(name)+2+5);
  char *alternate=fd_xmalloc(strlen(name)+2+5);
  sprintf(primary,"%s",name);
  sprintf(alternate,"%s*",name);
  fd_hashtable_set
    (&normalized_elt_names,fd_make_symbol(alternate),fd_make_symbol(primary));
  fd_add_special_form(fd_html_env,primary,html_block_handler);
  fd_add_special_form(fd_html_env,alternate,htmlattr_block_handler);
}

void fd_add_htmlattr_inline_tag(char *name)
{
  char *primary=fd_xmalloc(strlen(name)+2+5);
  sprintf(primary,"%s",name);
  fd_add_special_form(fd_html_env,primary,htmlattr_inline_handler);
}

void fd_add_htmlattr_block_tag(char *name)
{
  char *primary=fd_xmalloc(strlen(name)+2+5);
  sprintf(primary,"%s",name);
  fd_add_special_form(fd_html_env,primary,htmlattr_block_handler);
}

void fd_add_html_empty_tag(char *name)
{
  char *primary=fd_xmalloc(strlen(name)+2+5);
  sprintf(primary,"%s",name);
  fd_add_special_form(fd_html_env,primary,html_empty_handler);
}

/* Outputting FORM elements */

static lisp interpret_variable(lisp expr,lispenv env)
{
  if (STRINGP(expr)) return fd_make_symbol(STRING_DATA(expr));
  else {
    fd_lisp v=fd_eval_in_env(expr,env);
    handle_empty_tag(v);
    return v;}
}

static lisp html_submit_handler(lisp expr,lispenv env)
{
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env);
  lisp value=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  fd_htstream *out=get_http_output();
  if (!(SYMBOLP(variable)))
    fd_raise_exception("CGI-SUBMIT needs symbol for var name");
  http_puts("<INPUT TYPE=SUBMIT ",out); 
  if (FD_FALSEP(value)) {
    html_write_param(name_symbol,variable,"",out);
    http_puts(EMPTY_TAG_CLOSE(out),out);}
  else {
    html_write_param(name_symbol,variable," ",out);
    html_write_param(value_symbol,value,EMPTY_TAG_CLOSE(out),out);}
  decref(value);
  return FD_VOID;
}

static lisp html_checkbox_handler(lisp expr,lispenv env)
{
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env);
  lisp value=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp checked=fd_eval_in_env(fd_get_arg(expr,3,FD_VOID),env);
  lisp body=fd_get_body(expr,4);
  fd_htstream *out=get_http_output();
  if (!((SYMBOLP(variable)) || (STRINGP(variable))))
    fd_raise_exception("CGI-CHECKBOX needs symbol for var name");
  http_puts("<INPUT TYPE=CHECKBOX ",out);
  html_write_param(name_symbol,variable," ",out);
  html_write_param(value_symbol,value,"",out);
  if (!(FD_FALSEP(checked))) http_puts(" CHECKED>",out);
  else http_puts(EMPTY_TAG_CLOSE(out),out);
  decref(variable); decref(value); decref(checked);
  html_printout(body,env);
  return FD_VOID;
}

static lisp html_radiobutton_handler(lisp expr,lispenv env)
{
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env);
  lisp value=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp checked=fd_eval_in_env(fd_get_arg(expr,3,FD_VOID),env);
  lisp body=fd_get_body(expr,4);
  fd_htstream *out=get_http_output();
  if (!(SYMBOLP(variable)))
    fd_raise_exception("CGI-RADIOBUTTON needs symbol for var name");
  http_puts("<INPUT TYPE=RADIO ",out);
  html_write_param(name_symbol,variable," ",out);
  html_write_param(value_symbol,value,"",out);
  if (!(FD_FALSEP(checked))) http_puts(" CHECKED>",out);
  else http_puts(EMPTY_TAG_CLOSE(out),out);
  decref(value); decref(checked);
  html_printout(body,env);
  return FD_VOID;
}

static lisp html_cgipass_handler(lisp expr,lispenv env)
{
  int l=fd_list_length(expr);
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env), value;
  fd_htstream *out=get_http_output();
  if (!(SYMBOLP(variable))) fd_raise_exception("CGIPASS needs symbol for var name");
  if (l == 2) value=fd_symeval(variable,env);
  else if (l == 3) value=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  else fd_raise_exception("CGIPASS Syntax error");
  http_puts("<INPUT TYPE=HIDDEN ",out);
  html_write_param(name_symbol,variable," ",out);
  html_write_param(value_symbol,value,EMPTY_TAG_CLOSE(out),out);
  decref(value);
  return FD_VOID;
}

static lisp html_textfield_handler(lisp expr,lispenv env)
{
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env);
  lisp size=fd_eval_in_env(fd_get_arg(expr,2,LISPFIX(40)),env);
  lisp initial_contents=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  fd_htstream *out=get_http_output();
  if (STRINGP(variable)) {
    fd_lisp v=variable;
    variable=fd_make_symbol(STRING_DATA(variable));
    fd_decref(v);}
  else if (!(SYMBOLP(variable))) fd_raise_exception("CGI-TEXTINPUT needs symbol for var name");
  http_puts("<INPUT TYPE=TEXT ",out); 
  html_write_param(name_symbol,variable," ",out);
  html_write_param(size_symbol,size," ",out);
  if (FD_FALSEP(initial_contents)) http_puts(EMPTY_TAG_CLOSE(out),out);
  else html_write_param
	 (value_symbol,initial_contents,EMPTY_TAG_CLOSE(out),out);
  decref(size);
  decref(initial_contents);
  return FD_VOID;
}

static lisp html_textarea_handler(lisp expr,lispenv env)
{
  lisp variable=interpret_variable(fd_get_arg(expr,1,FD_VOID),env);
  lisp cols=fd_eval_in_env(fd_get_arg(expr,2,LISPFIX(40)),env);
  lisp rows=fd_eval_in_env(fd_get_arg(expr,3,LISPFIX(8)),env);
  lisp initial_contents=fd_eval_in_env(fd_get_arg(expr,4,FD_FALSE),env);
  fd_htstream *out=get_http_output();
  if (STRINGP(variable)) {
    fd_lisp v=variable;
    variable=fd_make_symbol(STRING_DATA(variable));
    fd_decref(v);}
  else if (!(SYMBOLP(variable))) fd_raise_exception("CGI-TEXTINPUT needs symbol for var name");
  http_puts("<TEXTAREA ",out); 
  html_write_param(name_symbol,variable," ",out);
  html_write_param(cols_symbol,cols," ",out);
  html_write_param(rows_symbol,rows," ",out);
  http_puts(">",out);
  if (!(FD_FALSEP(initial_contents))) {
    http_puts(fd_strdata(initial_contents),out);
    http_puts("</TEXTAREA>\n",out);}
  else http_puts("</TEXTAREA>\n",out);
  decref(cols);
  decref(rows);
  decref(initial_contents);
  return FD_VOID;
}

static lisp html_selection_handler(lisp expr,lispenv env)
{
  lisp head=fd_get_arg(expr,1,FD_VOID);
  lisp body=fd_get_body(expr,2);
  output_element(fd_make_symbol("SELECT"),head,body,env,1,1);
  return FD_VOID;
}

static lisp html_option_handler(lisp expr,lispenv env)
{
  lisp arg=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp body=fd_get_body(expr,2);
  fd_htstream *out=get_http_output();
  http_puts("<OPTION ",out);
  html_write_param(value_symbol,arg,"",out);
  http_puts(EMPTY_TAG_CLOSE(out),out);
  html_printout(body,env);
  return FD_VOID;
}


/* Anchor expressions */

static void output_anchor(fd_lisp ref,fd_lisp attribs,fd_lisp body,fd_lispenv env)
{
  fd_htstream *out=get_http_output(); fd_lisp href;
  if (FD_STRINGP(ref)) href=fd_incref(ref);
  else if (FD_SYMBOLP(ref)) {
    int new_len=strlen(FD_SYMBOL_NAME(ref))+2;
    fd_u8char *with_hash=fd_xmalloc(new_len+1);
    sprintf(with_hash,"#%s",FD_SYMBOL_NAME(ref));
    href=fd_init_string(with_hash,new_len);}
  else if (FD_OIDP(ref)) {
    struct FD_STRING_STREAM url_stream;
    fd_pool p=fd_get_pool(ref);
    fd_lisp browser=get_browse_method(p);
    FD_INITIALIZE_STRING_STREAM(&url_stream,128);
    generate_browse_url(browser,ref,&url_stream);
    fd_decref(browser);
    href=fd_stream_string(&url_stream);}
  else if (FD_EMPTYP(ref)) href=ref;
  else fd_type_error(_("not an anchor object"),ref);
  attribs=FD_MAKE_PAIR(href,fd_incref(attribs));
  attribs=FD_MAKE_PAIR(href_symbol,attribs);
  output_element(a_symbol,attribs,body,env,0,1);
  fd_decref(attribs);
}

static lisp htmlanchorplus(lisp expr,lispenv env)
{
  lisp ref=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp attribs=fd_get_arg(expr,2,FD_VOID);
  lisp body=fd_get_body(expr,3);
  start_body();
  if (FD_EMPTYP(ref)) html_printout(body,env);
  else output_anchor(ref,attribs,body,env);
  fd_decref(ref);
  return FD_VOID;
}

/* For simple anchor expressions, the anchor argument can be either:
     + a string (indicating a URL)
     + a symbol (indicating an internal tag)
     + an OID (indicating a recursive browse)
*/
static void generate_anchor(lisp ref,u8char *target,lisp body,fd_lispenv env)
{
  fd_htstream *out=get_http_output();
  if (target) {
    if (FD_OIDP(ref)) {
      http_puts("<a href=\'",out);
      http_browse_url(ref,out);
      http_printf1s(out,"' target='%s' class='oidref'>",target);}
    else if (STRINGP(ref)) {
      http_printf1s(out,"<a href='%s'",STRING_DATA(ref));
      http_printf1s(out," target='%s'>",target);}
    else if (SYMBOLP(ref)) {
      http_printf1s(out,"<a href='#%s'",SYMBOL_NAME(ref));
      http_printf1s(out," target='%s'>",target);}
    else if (FD_EMPTYP(ref)) {
      http_printf1s(out,"<a href='s'","{}");
      http_printf1s(out," target='%s' class='oidref'>",target);}
    else fd_type_error("not an anchor object",ref);}
  else {
    if (FD_OIDP(ref)) {
      http_puts("<a href=\'",out);
      http_browse_url(ref,out);
      http_printf1s(out,"' class='oidref'>",target);}
    else if (STRINGP(ref))
      http_printf1s(out,"<a href='%s' class='oidref'>",STRING_DATA(ref));
    else if (SYMBOLP(ref))
      http_printf1s(out,"<a href='#%s' class='oidref'>",SYMBOL_NAME(ref));
    else if (FD_EMPTYP(ref))
      http_printf1s(out,"<a href='%s' class='oidref'>","{}");
    else fd_type_error("not an anchor object",ref);}
  html_printout(body,env);
  http_puts("</A>",out);
}

static lisp htmlanchor(lisp expr,lispenv env)
{
  lisp ref=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp body=fd_get_body(expr,2);
  start_body();
  generate_anchor(ref,NULL,body,env);
  decref(ref);
  return FD_VOID;
}

static lisp htmltanchor(lisp expr,lispenv env)
{
  lisp target=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp ref=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp body=fd_get_body(expr,3);
  start_body();
  if (FD_EMPTYP(ref))
    generate_anchor(ref,"{}",body,env);
  else if (FD_STRINGP(target))
    generate_anchor(ref,FD_STRING_DATA(target),body,env);
  else if (FD_SYMBOLP(target))
    generate_anchor(ref,FD_SYMBOL_NAME(target),body,env);
  else fd_type_error(_("target is not a string or a symbol"),target);
  decref(ref); decref(target);
  return FD_VOID;
}


/* Forms for tags, images, inclusion, and code */

static lisp htmltag(lisp expr,lispenv env)
{
  fd_lisp head=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp tag, params;
  handle_empty_tag(head);
  if (FD_SYMBOLP(head)) tag=fd_copy_string(FD_SYMBOL_NAME(head));
  else if (FD_STRINGP(head)) tag=fd_incref(head);
  else if (FD_OIDP(head)) {
    fd_u8char buf[32];
    sprintf(buf,"%x/%x",OID_ADDR_HIGH(head),OID_ADDR_LOW(head));
    tag=fd_copy_string(buf);}
  else fd_type_error("Not a valid internal tag id",head);
  params=FD_MAKE_LIST(2,fd_make_symbol("NAME"),tag);
  output_element(fd_make_symbol("A"),params,fd_get_body(expr,2),env,1,1);
  decref(params); decref(head);
  return FD_VOID;
}

static lisp htmlimage(lisp expr,lispenv env)
{
  fd_htstream *out=get_http_output();  
  lisp src=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp body=fd_get_body(expr,2);
  handle_empty_string(src);
  if (FD_STRINGP(src)) {
    lisp params=FD_MAKE_PAIR
      (src_symbol,FD_MAKE_PAIR(src,incref(body)));
    lisp element_spec=FD_MAKE_PAIR(img_symbol,params);
    output_empty_element(element_spec,env,"<",((out->is_xml) ? " />" : ">"));
    fd_decref(element_spec);
    return FD_VOID;}
  else fd_type_error(_("Image SRC must be string"),src);
}

static lisp htmlinclude(lisp expr,lispenv env)
{
  FILE *f;
  lisp filename=fd_get_arg(expr,1,FD_VOID);
  char *fname=fd_strdata(filename), *data;
  if (strchr(fname,':')) {
    fd_lisp retrieved=fd_urlget(fname);
    if (FD_STRINGP(retrieved))
      data=fd_strdup(FD_STRING_DATA(retrieved));
    else if (FD_SLOTMAPP(retrieved)) {
      fd_lisp content=fd_prim_get(retrieved,fd_make_symbol("CONTENT"));
      if (FD_STRINGP(content))
	data=fd_strdup(FD_STRING_DATA(retrieved));
      else
        data=NULL;
      fd_decref(content);}
    else return retrieved;
    fd_decref(retrieved);}
  else {
    if (!((*fname == '/') || (*fname == '\\'))) {
      lisp root=fd_thread_symeval(current_file_symbol); 
      char *new=NULL;
      if (STRINGP(root)) {
	int new_size=STRING_LENGTH(filename)+STRING_LENGTH(root)+1;
	char *read=STRING_DATA(root);
	char *new=fd_xmalloc(sizeof(char)*new_size), *write=new, *sep=new;
	/* Copy the string, but track the last separator, because that's where you'll start
	   writing the relative part of the pathname. */
	while (*read) {
	  if ((*read == '/') || (*read == '\\')) sep=write+1;
	  *write++=*read++;}
	write=sep; read=fname;
	while (*read) *write++=*read++; *write++='\0';
	fname=new;}
      decref(root);
      fd_xfree(new);}
    f=fd_fopen(fname,"r");
    if (f == NULL)
      fd_raise_lisp_exception(fd_Cant_Read_File,fname,filename);
    else {
      char buf[1024];
      struct FD_STRING_STREAM ss;
      FD_INITIALIZE_STRING_STREAM(&ss,1024);
      while ((fgets(buf,1024,f)) != NULL) fd_sputs(&ss,buf);
      data=ss.ptr;}}
  http_puts(data,get_http_output());
  fd_xfree(data);
  return FD_VOID;
}

static lisp htmlcodeblock(lisp expr,lispenv env)
{
  lisp lwidth=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp contents=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  fd_htstream *out=get_http_output();
  struct FD_STRING_STREAM ss; 
  int width;
  handle_empty_fixnum(lwidth); width=fd_fixlisp(lwidth);
  FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_pprint_lisp_to_string(contents,&ss,0,0,width); 
  http_puts("<pre>\n",out);
  html_puts(ss.ptr);
  http_puts("\n</pre>\n",out);
  fd_xfree(ss.ptr); decref(contents); decref(lwidth);
  return FD_VOID;
}


/** Describing OIDs and frames in HTML **/

/*
  table.frame_details { border: 3; }
  table.frame_details th.title:
   { font-size: +2; }
  table.frame_details th.slotid: {
    font-family: sans-serif; font-weight: bold;
    valign: top; align: right;} 
  table.frame_details td.novalue: {
    font-style: italic;
    valign: top; align: right; content: 'No values';} 
  table.frame_details td.value: {
  font-family: fixed;
  valign: top; align: left;} 
*/

static void html_slot_row(lisp frame,lisp slot,lisp value)
{
  fd_htstream *out=get_http_output();
  http_puts
    ("<tr><th class='slotid'>",out);
  if (SYMBOLP(slot)) 
    html_puts(SYMBOL_NAME(slot));
  else html_write(slot);
  if (FD_EMPTYP(value)) 
    http_puts("</th><td class='novalue'></td></tr>\n",out);
  else {
    int first_value=1;
    http_puts
      ("</th><td class='slotvalue'>",out);
    {DO_CHOICES(v,value) {
      if (first_value) first_value=0; 
      else if ((OIDP(v)) || (FD_PAIRP(v)) || (FD_VECTORP(v)))
	http_puts("<br/>",out); 
      else http_puts(" . ",out);
      if ((FD_PAIRP(v)) || (FD_VECTORP(v))) {
	struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
	fd_pprint_lisp_to_string(v,&ss,0,0,60);
	html_puts(ss.ptr); fd_xfree(ss.ptr);}
      else if (FD_STRINGP(v)) {
	http_puts("<span class='string'>",out); html_write(v); http_puts("</span>",out);}
      else html_write(v);}
    END_DO_CHOICES;}}
  http_puts("</td></tr>\n",out);
}

static void html_frame_description(lisp frame)
{
  fd_htstream *out=get_http_output();
  lisp slotmap=fd_oid_value(frame);
  fd_pool p=FD_GET_POOL(frame);
  char buf[256];
  http_puts("<table class='frame_details'>\n",out);

  /* Output the caption */
  
  sprintf(buf,"<tr><th colspan=2 class='title'><a name=\"%x/%x\">The frame ",
	  OID_ADDR_HIGH(frame),OID_ADDR_LOW(frame));
  http_puts(buf,out);
  sprintf(buf,"@%x/%x",OID_ADDR_HIGH(frame),OID_ADDR_LOW(frame));
  http_puts(buf,out);

  sprintf(buf," has %d slots</a></th></tr>\n",
	  (SLOTMAP_PTR(slotmap))->size);
  http_puts(buf,out);

  fd_for_slots(html_slot_row,frame);

  sprintf(buf,"<tr><th class='pseudo_slotid'>OID&nbsp;Address</th><td class='oid_addr'>@%x/%x</td></tr>\n",
	  OID_ADDR_HIGH(frame),OID_ADDR_LOW(frame));
  http_puts(buf,out);

  if ((p) && (p->id)) {
    sprintf(buf,"<tr><th class='pseudo_slotid'>From&nbsp;Pool</th><td class='poolid'>%s</td></tr>\n",
	    p->id);
    http_puts(buf,out);}

  http_puts("</table>\n",out);

  fd_decref(slotmap);
}

void fd_describe_oid_in_html(lisp frame)
{
  fd_htstream *out=get_http_output();
  lisp value=fd_oid_value(frame);
  if (out->phase != html_body) {
    html_header("<title>\nDescription of frame ");
    html_write(frame);
    html_header("</title>");
    start_body();}
  if (SLOTMAPP(value)) {
    lisp html_methods=fd_prim_get(frame,html_methods_symbol);
    FD_WITH_LEXICAL_ENV(html_env,NULL,2) {
      fd_bind_value(frame_symbol,frame,html_env);
      if (!(FD_EMPTYP(html_methods))) {
	DO_CHOICES(script,html_methods) {
	  DOLIST(method,script) {
	    lisp value=fd_eval_in_env(method,html_env); decref(value);}
	  http_puts("<HR>\n",out);;}
	END_DO_CHOICES;}
      else html_frame_description(frame);}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    fd_decref(html_methods);}
  else {
    char buf[128];
    sprintf
      (buf,"<P><A NAME=\"%x/%x\">The value of %x/%x</A> is</P></A>\n",
       OID_ADDR_HIGH(frame),OID_ADDR_LOW(frame),
       OID_ADDR_HIGH(frame),OID_ADDR_LOW(frame));
    http_puts(buf,out); html_write(value);}
  fd_decref(value);
}

static lisp describe_oid_in_html(lisp obj)
{
  fd_describe_oid_in_html(obj);
  return FD_VOID;
}

static fd_lisp output_slots_lexpr(fd_lisp args)
{
  int n_values=fd_list_length(args);
  switch (n_values) {
  case 1:
    fd_for_slots(html_slot_row,fd_get_arg(args,0,FD_VOID)); break;
  case 2: {
    fd_lisp frame=fd_get_arg(args,0,FD_VOID);
    fd_lisp slotid=fd_get_arg(args,1,FD_VOID);
    fd_lisp values=fd_frame_get(frame,slotid);
    if (!(FD_EMPTYP(values)))
      html_slot_row(frame,slotid,values);
    fd_decref(values);
    break;}
  case 3: {
    fd_lisp frame=fd_get_arg(args,0,FD_VOID);
    fd_lisp slotid=fd_get_arg(args,1,FD_VOID);
    fd_lisp values=fd_get_arg(args,2,FD_VOID);
    html_slot_row(frame,slotid,values);
    break;}}
  return FD_VOID;
}

/** Top level stuff **/

static lisp htmlexpr(lisp expr,lispenv env)
{
  DOLIST(e,fd_get_body(expr,1)) {
    lisp val=fd_eval_in_env(e,env);
    if (STRINGP(val)) html_puts_noescape(STRING_DATA(val));
    else if (FD_VOIDP(val)) {}
    else {
      fd_htstream *out=get_http_output();
      enum http_generation_phase phase=out->phase;
      struct FD_STRING_STREAM ss;
      FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
      fd_print_lisp_to_string(val,&ss);
      if (phase == html_body)
	html_display_string_internal(ss.ptr,out,0,0);
      else html_display_string_internal(ss.ptr,out,1,0);
      fd_xfree(ss.ptr);}
    decref(val);}
  return FD_VOID;
}

static lisp htmlexpr_nobreak(lisp expr,lispenv env)
{
  DOLIST(e,fd_get_body(expr,1)) {
    lisp val=fd_eval_in_env(e,env);
    if (STRINGP(val))
      html_puts_noescape_nobreak(STRING_DATA(val));
    else if (FD_VOIDP(val)) {}
    else {
      fd_htstream *out=get_http_output();
      enum http_generation_phase phase=out->phase;
      struct FD_STRING_STREAM ss;
      FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.fancy_oids=0;
      fd_print_lisp_to_string(val,&ss);
      if (phase == html_body)
	html_display_string_internal(ss.ptr,out,0,1);
      else html_display_string_internal(ss.ptr,out,1,1);
      fd_xfree(ss.ptr);}
    decref(val);}
  return FD_VOID;
}
    
static lisp lisp_http_header_cproc(lisp name,lisp content)
{
  fd_htstream *out=get_http_output();  
  if (out->phase == http_any) set_http_phase(http_head);
  if (out->phase != http_head)
    fd_raise_exception("HTTP-HEADER: too late to specify headers");
  if (SYMBOLP(name)) http_puts(SYMBOL_NAME(name),out);
  else if (ASCII_STRINGP(name)) http_puts(STRING_DATA(name),out);
  else fd_type_error("HTTP-HEADER: Not a valid header",name);
  http_puts(": ",out); 
  if (STRINGP(content)) http_puts(STRING_DATA(content),out);
  else fd_type_error("HTTP-HEADER: Not valid header content",content);
  http_puts("\n",out);
  return FD_VOID;
}

static lisp lisp_html_insert_head_cproc(lisp string)
{
  html_header(fd_strdata(string));
  return FD_VOID;
}

static lisp lisp_html_start_body_cproc(lisp string)
{
  set_http_phase(html_body);
  html_puts_noescape(fd_strdata(string));
  return FD_VOID;
}

static lisp html_title_cproc(lisp expr,lispenv env)
{
  html_header("<TITLE>");
  html_printout(fd_get_body(expr,1),env);
  html_header("</TITLE>\n");
  return FD_VOID;
}

static lisp html_meta_cproc(lisp name,lisp content)
{
  fd_htstream *out=get_http_output();  
  http_puts("<META NAME=\"",out);
  if (STRINGP(name)) http_puts(STRING_DATA(name),out);
  else if (SYMBOLP(name)) http_puts(SYMBOL_NAME(name),out);
  else fd_type_error("HTML META name not a string or symbol",name);
  http_puts("\" ",out);
  html_write_param(content_symbol,content," />\n",out);
  return FD_VOID;
}

static lisp html_stylesheet_cproc(lisp arg)
{
  char *buf=fd_xmalloc(fd_strlen(arg)+128);
  sprintf(buf,"<LINK REL='stylesheet' TYPE='text/css' HREF='%s' />\n",
	  fd_strdata(arg));
  html_header(buf); fd_xfree(buf);
  return FD_VOID;
}

HTMLGEN_EXPORT
/* fd_start_http:
     Arguments: a string containing a mime type specification
     Returns: void
  Outputs a content-type header field, including a charset specifier
which refers to the current default character encoding.
*/
void fd_start_http(char *mime)
{
  fd_htstream *out=get_http_output();
  struct FD_TEXT_ENCODING *enc; char buf[128];
  if (out->stream_type == stdio)
    enc=out->stream.xfile.encoding;
  else enc=fd_get_default_encoding();
  set_http_phase(http_head);
  sprintf(buf,"Content-type: %s; charset=%s;\r\n\r\n",mime,enc->names[0]);
  http_header(buf);
}

static lisp httpdoc(lisp expr,lispenv env)
{
  fd_htstream *out=get_http_output();
  struct FD_TEXT_ENCODING *enc; char buf[128];
  if (out->stream_type == stdio)
    enc=out->stream.xfile.encoding;
  else enc=fd_get_default_encoding();
  set_http_phase(http_head);
  sprintf(buf,"Content-type: text/html; charset=%s;\r\n",enc->names[0]);
  http_header(buf);
  html_printout(fd_get_body(expr,1),env);
  if (get_http_phase() == html_end) {}
  else if (get_http_phase() == xml_content) {}
  else if (get_http_phase() == html_frameset) {
    html_puts_noescape("\n</html>");
    set_http_phase(html_end);}
  else {
    html_puts_noescape("\n</body>\n</html>");
    set_http_phase(html_end);}
  return FD_VOID;
}

static lisp write_html_file_handler(lisp expr,lispenv env)
{
  lisp destination=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  char *filename=fd_localize_utf8
    (fd_strdata(destination),fd_get_default_encoding());
  struct FD_HTTP_STREAM hts;
  FILE *f=fd_fopen(filename,"w");
  if (f == NULL) fd_raise_detailed_exception(fd_FileOpenWFailed, filename);
  hts.stream_type=stdio; hts.is_xml=0; hts.phase=http_body;
  fd_init_xfile(&(hts.stream.xfile),f,fd_get_default_encoding()); 
  fd_associate_xfile(f,&(hts.stream.xfile)); 
  hts.namespaces=NULL; hts.free_namespaces=0; 
  set_http_output(&hts);
  html_printout(fd_get_body(expr,2),env);
  if (!((hts.phase == html_end) || (hts.phase == xml_content))) {
    html_puts_noescape("\n</body></html>\n");
    hts.phase=html_end;}
  fclose(f); set_http_output(NULL);
  fd_xfree(filename); decref(destination);
  return FD_VOID;
}

static lisp write_xml_file_handler(lisp expr,lispenv env)
{
  lisp destination=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  char *filename=fd_localize_utf8
    (fd_strdata(destination),fd_get_default_encoding());
  struct FD_HTTP_STREAM hts;
  FILE *f=fd_fopen(filename,"w");
  if (f == NULL) fd_raise_detailed_exception(fd_FileOpenWFailed, filename);
  hts.stream_type=stdio; hts.is_xml=1; hts.phase=http_body;
  fd_init_xfile(&(hts.stream.xfile),f,fd_get_default_encoding()); 
  fd_associate_xfile(f,&(hts.stream.xfile)); 
  hts.namespaces=NULL; hts.free_namespaces=0;
  set_http_output(&hts);
  html_printout(fd_get_body(expr,2),env);
  fclose(f); set_http_output(NULL);
  fd_xfree(filename); decref(destination);
  return FD_VOID;
}

static lisp htmlstring_handler(lisp expr,lispenv env)
{
  struct FD_STRING_STREAM ss; struct FD_HTTP_STREAM hts;
  FD_INITIALIZE_STRING_STREAM(&ss,8192);
  hts.stream_type=sstream; hts.is_xml=0; hts.stream.sstream=&ss;
  hts.namespaces=NULL; hts.free_namespaces=0; hts.phase=http_any;
  set_http_output(&hts);
  html_printout(fd_get_body(expr,1),env);
  set_http_output(NULL);
  return fd_init_string(ss.ptr,ss.size);
}

static lisp htmlfragment_handler(lisp expr,lispenv env)
{
  struct FD_STRING_STREAM ss; struct FD_HTTP_STREAM hts;
  FD_INITIALIZE_STRING_STREAM(&ss,8192);
  hts.stream_type=sstream; hts.is_xml=0; hts.stream.sstream=&ss;
  hts.namespaces=NULL; hts.free_namespaces=0; hts.phase=html_body;
  set_http_output(&hts); 
  html_printout(fd_get_body(expr,1),env);
  set_http_phase(html_end);
  set_http_output(NULL);
  return fd_init_string(ss.ptr,ss.size);
}

static lisp xmlstring_handler(lisp expr,lispenv env)
{
  struct FD_STRING_STREAM ss; struct FD_HTTP_STREAM hts;
  FD_INITIALIZE_STRING_STREAM(&ss,8192);
  hts.stream_type=sstream; hts.is_xml=1; hts.stream.sstream=&ss;
  hts.namespaces=NULL; hts.free_namespaces=0; hts.phase=http_any;
  set_http_output(&hts);
  html_printout(fd_get_body(expr,1),env);
  set_http_output(NULL);
  return fd_init_string(ss.ptr,ss.size);
}

static lisp xmlfragment_handler(lisp expr,lispenv env)
{
  struct FD_STRING_STREAM ss; struct FD_HTTP_STREAM hts;
  FD_INITIALIZE_STRING_STREAM(&ss,8192);
  hts.stream_type=sstream; hts.is_xml=1; hts.stream.sstream=&ss;
  hts.namespaces=NULL; hts.free_namespaces=0; hts.phase=xml_content;
  set_http_output(&hts); 
  html_printout(fd_get_body(expr,1),env);
  set_http_output(NULL);
  return fd_init_string(ss.ptr,ss.size);
}

static lisp http_redirect_cproc(lisp url)
{
  if (STRINGP(url)) {
    http_header("Status: 302\n");
    http_header("Location: ");
    http_header(STRING_DATA(url));
    http_header("\n");
    return FD_VOID;}
  else fd_type_error("HTTP-REDIRECT: URL not a string",url);
}

static lisp html_redirect_handler(lisp expr,lispenv env)
{
  struct FD_TEXT_ENCODING *enc=fd_get_default_encoding();
  lisp destination=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp mime_type=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);  
  char *buf=fd_strdata(destination);
  set_http_phase(http_head);
  if (STRINGP(mime_type)) {
    char buf[512];
    sprintf(buf,"Content-type: %s\n",STRING_DATA(mime_type));
    http_header(buf);}
  else {
    char tbuf[128];
    sprintf(tbuf,"Content-type: text/html; charset=%s;\n",enc->names[0]);
    http_header(tbuf);}
  http_header("Status: 302\n");
  http_header("Location: "); http_header(buf); http_header("\n");
  html_printout(fd_get_body(expr,3),env);
  decref(destination); decref(mime_type);
  if (!((get_http_phase() == html_end) || (get_http_phase() == xml_content))) {
    html_puts_noescape("\n</body>\n</html>\n");
    set_http_phase(html_end);}
  return FD_VOID;
}
    
static lisp http_splash_lexpr(lisp args)
{
  fd_lisp url, wait;
  fd_get_args("HTTP-SPLASH",args,&url,FD_VOID,&wait,FD_LISPFIX(10),NULL);
  handle_empty_fixnum(wait); handle_empty_string(url);
  if (!(FIXNUMP(wait)))
    fd_type_error("HTTP-SPLASH: WAIT not a fixnum",wait);
  else if (!(STRINGP(url)))
    fd_type_error("HTTP-SPLASH: URL not a string",url);
  else {
    char buf[64]; sprintf(buf,"Refresh: #%d; URL=",FIXLISP(wait));
    http_header(buf);
    http_header(STRING_DATA(url));
    http_header("\n");
    return FD_VOID;}
}

static lisp html_splash_handler(lisp expr,lispenv env)
{
  struct FD_TEXT_ENCODING *enc=fd_get_default_encoding(); char tbuf[128];
  char buf[64];
  lisp interval=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp destination=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  handle_empty_fixnum(interval); handle_empty_string(destination);
  set_http_phase(http_head);
  sprintf(buf,"Refresh: #%d; URL=",fd_lisp2int(interval));
  sprintf(tbuf,"Content-type: text/html; charset=%s;\n",enc->names[0]); http_header(tbuf);
  http_header(buf); http_header(fd_strdata(destination));
  http_header("\n");
  html_printout(fd_get_body(expr,3),env);
  decref(interval); decref(destination);
  if (!((get_http_phase() == html_end) || (get_http_phase() == xml_content))) {
    html_puts_noescape("\n</body>\n</html>\n");
    set_http_phase(html_end);}
  return FD_VOID;
}
    
static lisp http_return_file(lisp mime_type,lisp filename)
{
  if (!(STRINGP(mime_type)))
    fd_type_error("mime-type is not a string",mime_type);
  else {
    fd_htstream *out=get_http_output();
    FILE *f=fd_fopen(STRING_DATA(filename),"rb");
    int bytes_read, bytes_total=0;
    char buf[1024];
    set_http_phase(http_head);
    if (f == NULL)
      fd_raise_lisp_exception
	("No Such File","for HTTP return file",filename);
    fseek(f,0,SEEK_END); bytes_total=ftell(f); fseek(f,0,SEEK_SET);
    http_printf1s(out,"Content-type: %s\r\n",STRING_DATA(mime_type));
    http_printf1i(out,"Content-length: %d\r\n",bytes_total);
    finish_http_header();
    while (((bytes_read=fread(buf,sizeof(unsigned char),1024,f)) > 0) ||
	   (!(feof(f))))
      http_write_bytes(buf,bytes_read,out);
    fclose(f); 
    return FD_VOID;}
}

static lisp xmldoc(lisp mime_type,lisp filename)
{
  if (!(STRINGP(mime_type)))
    fd_type_error("mime-type is not a string",mime_type);
  else {
    fd_htstream *out=get_http_output();
    FILE *f=fd_fopen(STRING_DATA(filename),"rb");
    int bytes_read, bytes_total=0;
    char buf[1024];
    set_http_phase(http_head);
    if (f == NULL)
      fd_raise_lisp_exception
	("No Such File","for HTTP return file",filename);
    fseek(f,0,SEEK_END); bytes_total=ftell(f); fseek(f,0,SEEK_SET);
    http_printf1s(out,"Content-type: %s\r\n",STRING_DATA(mime_type));
    http_printf1i(out,"Content-length: %d\r\n",bytes_total);
    finish_http_header();
    while (((bytes_read=fread(buf,sizeof(unsigned char),1024,f)) > 0) ||
	   (!(feof(f))))
      http_write_bytes(buf,bytes_read,out);
    fclose(f); 
    return FD_VOID;}
}

static lisp http_return_data(lisp mime_type,lisp data)
{
  if (!(STRINGP(mime_type)))
    fd_type_error("mime-type is not a string",mime_type);
  else {
    fd_htstream *out=get_http_output();
    int size;
    if (FD_STRINGP(data)) size=FD_STRING_LENGTH(data);
    else if (FD_PACKETP(data)) size=FD_PACKET_LENGTH(data);
    else fd_type_error(_("not a string or packet"),data);
    set_http_phase(http_head);
    http_printf1s(out,"Content-type: %s\r\n",STRING_DATA(mime_type));
    http_printf1i(out,"Content-length: %d\r\n",size);
    finish_http_header();
    if (FD_STRINGP(data))
      http_write_bytes(FD_STRING_DATA(data),size,out);
    else http_write_bytes(FD_PACKET_DATA(data),size,out);
    return FD_VOID;}
}

static lisp http_return_handler(lisp expr,fd_lispenv env)
{
  fd_lisp http_header=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  if (!(STRINGP(http_header))) 
    fd_type_error("HTTP header not a string",http_header);
  else {
    fd_htstream *out=get_http_output();
    set_http_phase(http_head);
    http_puts(FD_STRING_DATA(http_header),out);
    finish_http_header();
    set_http_phase(xml_content);
    html_printout(fd_get_body(expr,2),env);
    return FD_VOID;}
}


/* Invoking remote scripts from FDScript */

static lisp index_url(lisp index_name,lisp index_key)
{
  if (!(STRINGP(index_name)))
    fd_raise_exception("index name isn't string");
  else if (STRINGP(index_key)) {
    struct FD_STRING_STREAM ks;
    FD_INITIALIZE_STRING_STREAM(&ks,256);
    uri_encode_local(STRING_DATA(index_name),&ks); fd_sputs(&ks,"?");
    uri_encode(STRING_DATA(index_key),&ks);
    return fd_init_string(ks.ptr,ks.size);}
  else if (OIDP(index_key)) {
    FD_OID id=FD_OID_ADDR(index_key); char buf[32];
    struct FD_STRING_STREAM ks;
    FD_INITIALIZE_STRING_STREAM(&ks,256);
    uri_encode_local(STRING_DATA(index_name),&ks); 
    sprintf(buf,"?@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id)); fd_sputs(&ks,buf);
    return fd_init_string(ks.ptr,ks.size);}
  else fd_raise_exception("Invalid index arg");
}

static lisp scripturl(lisp args)
{
  lisp script_name=fd_get_arg(args,0,FD_VOID);
  lisp script_args=fd_get_body(args,1);
  struct FD_STRING_STREAM qs;
  if (FD_EMPTYP(script_name)) return FD_EMPTY_CHOICE; 
  else if (!(STRINGP(script_name))) 
    fd_raise_exception("Script name should be string");
  else {
    FD_INITIALIZE_STRING_STREAM(&qs,256);
    qs.fancy_oids=0; qs.escape=0;
    uri_encode_local(STRING_DATA(script_name),&qs);
    fd_sputs(&qs,"?");}
  if (FD_EMPTY_LISTP(script_args))
    return fd_init_string(qs.ptr,qs.size);
  else if (!(PAIRP(script_args)))
    fd_raise_exception("Weird args to SCRIPTURL");
  else if (FD_EMPTY_LISTP(CDR(script_args)))
    if (OIDP(CAR(script_args))) {
      FD_OID id=FD_OID_ADDR(CAR(script_args));
      char buf[32]; sprintf(buf,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
      uri_encode(buf,&qs);}
    else if (STRINGP(CAR(script_args))) {
      uri_encode_local(STRING_DATA(CAR(script_args)),&qs);}
    else  {
      struct FD_STRING_STREAM vs;
      FD_INITIALIZE_STRING_STREAM(&vs,256);
      vs.fancy_oids=0; vs.escape=1;
      fd_print_lisp_to_string(CAR(script_args),&vs);
      uri_encode(vs.ptr,&qs); fd_xfree(vs.ptr);}
  else while (PAIRP(script_args)) {
    lisp query_var=fd_get_arg(script_args,0,FD_VOID);
    lisp query_val=fd_get_arg(script_args,1,FD_VOID);
    uri_encode(fd_symbol_name(query_var),&qs);
    fd_sputs(&qs,"=");
    if (OIDP(query_val)) {
      char buf[32];
      sprintf(buf,"@%x/%x",OID_ADDR_HIGH(query_val),OID_ADDR_LOW(query_val));
      fd_sputs(&qs,buf);}
    else if (STRINGP(query_val)) {
      uri_encode(STRING_DATA(query_val),&qs);}
    else {
      struct FD_STRING_STREAM vs;
      FD_INITIALIZE_STRING_STREAM(&vs,256);
      vs.fancy_oids=0; vs.escape=1;
      fd_print_lisp_to_string(query_val,&vs);
      uri_encode(vs.ptr,&qs); fd_xfree(vs.ptr);}
    fd_sputs(&qs,"&");
    script_args=CDR(CDR(script_args));}
  return fd_init_string(qs.ptr,qs.size);
}

static lisp uri_encode_string(lisp string)
{
  struct FD_STRING_STREAM ss;
  FD_INITIALIZE_STRING_STREAM(&ss,64);
  uri_encode(fd_strdata(string),&ss);
  return fd_init_string(ss.ptr,ss.size);
}

/* Accessing parsed markup */

static int xml_emptyp(fd_lisp x)
{
  lisp content=fd_xml_content(x); int result;
  if (FD_PAIRP(content))
    if (FD_EMPTY_LISTP(FD_CAR(content))) result=1;
    else result=0;
  else result=1;
  fd_decref(content);
  return result;
}

static int html_emptyp(fd_lisp x)
{
  lisp content=fd_xml_content(x);
  if (FD_PAIRP(content)) {
    fd_decref(content); return 0;}
  else {
    fd_decref(content); return 1;}
}

/* Generating from parses */

static void output_tag
  (fd_u8char *prefix,fd_lisp tag_arg,fd_lisp attributes,fd_u8char *suffix,
   fd_htstream *hs)
{
  fd_lisp tag;
  http_puts(prefix,hs);
  if (FD_OIDP(tag_arg)) tag=fd_prim_get(tag_arg,tag_slotid);
  else tag=fd_incref(tag_arg);
  if (FD_SYMBOLP(tag)) 
    http_puts(FD_SYMBOL_NAME(tag),hs);
  else if (FD_STRINGP(tag))
    http_puts(FD_STRING_DATA(tag),hs);
  else if (FD_LRECORD_TYPEP(tag,xmltag_tag)) {
    fd_lisp ns=fd_xmltag_namespace(tag);
    fd_lisp name=fd_xmltag_name(tag);
    if (FD_SYMBOLP(ns)) {
      http_puts(FD_SYMBOL_NAME(ns),hs);
      http_puts(":",hs);}
    if (FD_STRINGP(name))
      http_puts(FD_STRING_DATA(name),hs);
    else if (FD_SYMBOLP(name))
      http_puts(FD_SYMBOL_NAME(name),hs);}
  else fd_type_error(_("Invalid XML tag"),tag_arg);
  {DOLIST(attr,attributes)
     if (STRINGP(attr)) {
       http_puts(" ",hs); http_puts(FD_STRING_DATA(attr),hs);}
     else if (PAIRP(attr)) {
       lisp var=FD_CAR(attr), val=fd_car_noref(FD_CDR(attr));
       http_puts(" ",hs); 
       html_write_param(var,val,"",hs);}
     else fd_raise_exception("Weird attribute");}
  http_puts(suffix,hs);       
}

static void unparse_xml_oid(fd_lisp oid,fd_lispenv env,fd_htstream *hs)
{
  fd_lisp tag=fd_xml_tag(oid);
  fd_lisp attributes=fd_xml_attributes(oid);
  if ((hs->is_xml == 0) && (html_emptyp(oid)))
    output_tag("<",tag,attributes,">",hs);
  else if (xml_emptyp(oid)) 
    output_tag("<",tag,attributes,EMPTY_TAG_CLOSE(hs),hs);
  else {
    fd_lisp content=fd_xml_content(oid);
    output_tag("<",tag,attributes,">",hs);
    {DOLIST(elt,content) fd_unparse_xml(elt,env,hs);}
    if (FD_SYMBOLP(tag)) 
      http_printf1s(hs,"</%s>",fd_symbol_name(tag));
    else output_tag("</",tag,FD_EMPTY_LIST,">",hs);}  
  fd_decref(tag); fd_decref(attributes);
}

HTMLGEN_EXPORT
/* fd_unparse_xml:
     Arguments: an xml element rep, an environment, and an http stream
     Returns: void

 Outputs a text representation of the xml/html structure represented by the first
  argument.  Any tags which have bindings in the environment are interpreted as function
  calls.  If the html flag is true, the generation process will try and generate
  HTML rather than XML. */
void fd_unparse_xml(fd_lisp expr,fd_lispenv env,fd_htstream *hs)
{
  if (hs == NULL) hs=get_http_output();
  if (STRINGP(expr)) http_puts(STRING_DATA(expr),hs);
  else if (PAIRP(expr)) {
    fd_lisp tag=fd_xml_tag(expr);
    if (FD_LISP_EQ(tag,qmark_symbol)) {
      fd_lisp attributes=fd_xml_attributes(expr);
      output_tag("<?",FD_CAR(attributes),FD_CDR(attributes),"?>",hs);
      fd_decref(attributes);}
    else if (FD_LISP_EQ(tag,doctype_symbol)) {
      lisp root=fd_get_arg(expr,1,FD_VOID);
      lisp sysarg=fd_get_arg(expr,2,FD_VOID);
      lisp sysref=fd_get_arg(expr,3,FD_VOID);
      lisp uri=fd_get_arg(expr,4,FD_VOID);
      lisp idtd=fd_get_arg(expr,5,FD_VOID);
      http_puts("<!DOCTYPE ",hs);
      output_elt_name(root,hs);
      http_puts(" ",hs); http_puts(fd_strdata(sysarg),hs); 
      if (FD_STRINGP(sysref)) 
	http_printf1s(hs," \"%s\"",fd_strdata(sysref));
      if (FD_STRINGP(uri)) 
	http_printf1s(hs," \"%s\"",fd_strdata(uri));
      if (!(FD_EMPTY_LISTP(idtd)))
	http_printf1s(hs," [%s] ",fd_strdata(uri));
      http_puts(">",hs);}
    else if ((SYMBOLP(tag)) ||
	     (FD_LRECORD_TYPEP(tag,xmltag_tag))) {
      fd_lisp handler=FD_VOID;
      fd_lisp attributes=fd_xml_attributes(expr);
      if (fd_get_xml_handler(tag,env,&handler)) {
	fd_lisp value=fd_xml_callout(handler,expr,env);
	if (FD_VOIDP(value)) {}
	else if (FD_STRINGP(value)) {
	  http_puts(STRING_DATA(value),hs);}
	else {
	  fd_u8char *string=fd_object_to_string(value);
	  http_puts(string,hs); fd_xfree(string);}
	fd_decref(handler);
	fd_decref(value);
	/* If we have a binding and run it, we just return; otherwise,
	   we go on and otuput the XML in the normal way. */
	return;}
      if (tag_matchp(body_symbol,tag)) hs->phase=html_body;
      else if (tag_matchp(head_symbol,tag)) hs->phase=html_head;
      else if (tag_matchp(html_symbol,tag)) hs->phase=http_body;
      if (((hs == NULL) || (hs->is_xml == 0)) && (html_emptyp(expr)))
	output_tag("<",tag,attributes,EMPTY_TAG_CLOSE(hs),hs);
      /* Should probably be a table */
      else if ((xml_emptyp(expr)) && (!(tag_matchp(textarea_symbol,tag)))) 
	output_tag("<",tag,attributes,EMPTY_TAG_CLOSE(hs),hs);
      else {
	fd_lisp content=fd_xml_content(expr);
	output_tag("<",tag,attributes,">",hs);
	{DOLIST(elt,content) fd_unparse_xml(elt,env,hs);}
	fd_decref(content);
	if (FD_SYMBOLP(tag)) 
	  http_printf1s(hs,"</%s>",fd_symbol_name(tag));
	else output_tag("</",tag,FD_EMPTY_LIST,">",hs);}
      if (FD_LISP_EQ(tag,body_symbol)) hs->phase=html_end;
      fd_decref(attributes);}
    else {
      DOLIST(item,expr)	fd_unparse_xml(item,env,hs);}}
  else if (FD_OIDP(expr)) {
    fd_lisp tag=fd_xml_tag(expr);
    if (FD_EMPTYP(tag)) {
      fd_u8char *rep=fd_object_to_string(expr);
      http_puts(rep,hs); fd_xfree(rep);}
    else unparse_xml_oid(expr,env,hs);
    fd_decref(tag);}
  else {
    fd_u8char *rep=fd_object_to_string(expr);
    http_puts(rep,hs); fd_xfree(rep);}
}

static lisp unparse_xml_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp xml_expr=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp tag_env=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  fd_htstream *hs=get_http_output();
  int was_xml=hs->is_xml;
  fd_lispenv use_env;
  hs->is_xml=1;
  if (FD_FALSEP(tag_env)) use_env=env;
  else if (FD_PRIM_TYPEP(tag_env,env_type)) use_env=FD_CPTR_DATA(tag_env);
  else if (FD_SYMBOLP(tag_env)) {
    fd_lispenv mod_env=fd_get_module(FD_SYMBOL_NAME(tag_env));
    if (mod_env) use_env=mod_env;}
  else fd_type_error(_("Not a module"),tag_env);
  if ((FD_PAIRP(xml_expr)) && (!(FD_XMLTAGP(FD_CAR(xml_expr))))) {
    FD_DOLIST(xml_item,xml_expr)
      fd_unparse_xml(xml_item,use_env,hs);}
  else fd_unparse_xml(xml_expr,use_env,hs);
  hs->is_xml=was_xml;
  fd_decref(xml_expr); fd_decref(tag_env);
  return FD_VOID;
}

static lisp unparse_html(fd_lisp expr)
{
  fd_unparse_xml(expr,NULL,get_http_output());
  return FD_VOID;
}

/* Access from Lisp */

static lisp use_browse_url_lexpr(lisp args)
{
  fd_pool p; fd_lisp url, method;
  fd_lisp arg1=fd_get_arg(args,0,FD_VOID), arg2=fd_get_arg(args,1,FD_FALSE),
    body=fd_get_body(args,2);
  if (FD_PRIM_TYPEP(arg1,pool_type)) {
    p=FD_CPTR_DATA(arg1); url=arg2;}
  else {url=arg1; p=NULL; body=fd_get_body(args,1);}
  if (FD_EMPTY_LISTP(body))
    method=fd_incref(url);
  else method=FD_MAKE_PAIR(fd_incref(url),fd_incref(body));
  fd_set_browse_url(method,p);
  fd_decref(method);
  return FD_VOID;
}

static fd_u8char *value2string(fd_lisp object)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  ss.escape=1; ss.fancy_oids=0;
  fd_print_lisp_to_string(object,&ss);
  return ss.ptr;
}

static lisp lisp_set_cookie_lexpr(lisp args)
{
  struct FD_STRING_STREAM cstream;
  fd_lisp name, value, path, expires;
  fd_u8char *valuestring;
  fd_get_args("SET-COOKIE!",args,&name,FD_VOID,&value,FD_VOID,
	      &path,FD_FALSE,&expires,FD_FALSE,
	      NULL);
  if (!(FD_SYMBOLP(name)))
    fd_type_error(_("Cookie name is not a symbol"),name);
  if (FD_STRINGP(value))
    valuestring=fd_strdup(STRING_DATA(value));
  else valuestring=value2string(value);
  FD_INITIALIZE_STRING_STREAM(&cstream,256);
  uri_encode(SYMBOL_NAME(name),&cstream); fd_sputc(&cstream,'=');
  uri_encode(valuestring,&cstream); fd_sputs(&cstream,"; "); fd_xfree(valuestring);
  if (FD_FALSEP(expires)) {}
  else if (STRINGP(expires))
    fd_printf(&cstream,"expires=%s ",fd_strdata(expires));
  else if (LRECORD_TYPEP(expires,timestamp_symbol)) {
    time_t now=fd_timestamp_time(expires); struct tm expiration;
    char buf[128];
    fd_breakup_time(&expiration,now,0);
    strftime(buf,128,"%A, %d-%b-%Y %H:%M:%S GMT",&expiration);
    fd_printf(&cstream,"expires=%s;",buf);}
  else fd_type_error("Invalid expiration date",expires);
  if (STRINGP(path)) {
    fd_u8char *sdata=FD_STRING_DATA(path);
    if (strchr(sdata,':')) {
      fd_u8char *colon=strchr(sdata,':');
      fd_u8char *copy=fd_xmalloc((colon-sdata)+1);
      strncpy(copy,sdata,colon-sdata); copy[colon-sdata]=NUL;
      fd_sputs(&cstream,"domain="); uri_encode(copy,&cstream);
      fd_sputs(&cstream,"; "); fd_xfree(copy);
      fd_sputs(&cstream,"path="); uri_encode(colon+1,&cstream);
      fd_sputs(&cstream,"; ");}
    else {
      fd_sputs(&cstream,"path="); uri_encode(sdata,&cstream);
      fd_sputs(&cstream,"; ");}}
  fd_set_cookie(cstream.ptr); fd_xfree(cstream.ptr);
  return FD_VOID;
}

static lisp lisp_http_body_cproc()
{
  set_http_phase(html_body);
}


/* Generating frames */

static void start_env
  (fd_htstream *out,u8char *namestring,fd_lisp params,fd_lispenv params_env)
{
  http_printf1s(out,"<%s",namestring);
  if (FD_FALSEP(params)) http_puts(">",out);
  if (FD_EMPTY_LISTP(params)) http_puts(">",out);
  else if (STRINGP(params)) {
    http_putc(' ',out); html_url_puts(STRING_DATA(params),out);
    http_putc('>',out);}
  else if (PAIRP(params)) {
    while (PAIRP(params)) {
      lisp var=fd_get_arg(params,0,FD_VOID);
      if (STRINGP(var)) {
	http_putc(' ',out);
	html_puts_param(STRING_DATA(var));
	http_putc(' ',out);
      	params=CDR(params);}
      else if (FD_SYMBOLP(var)) {
	lisp val_expr=fd_get_arg(params,1,FD_VOID);
	lisp val=fd_eval_in_env(val_expr,params_env);
	params=CDR(CDR(params));
	http_printf1s(out," %s=\"",SYMBOL_NAME(var));
	if (OIDP(val)) {
	  char buf[64];
	  sprintf(buf,"@%x/%x\"",OID_ADDR_HIGH(val),OID_ADDR_LOW(val));
	  http_puts(buf,out);}
	else if (STRINGP(val)) {
	  html_puts_param(STRING_DATA(val)); http_putc('"',out);}
	else {
	  fd_u8char *string=fd_object_to_string(val);
	  html_puts_param(string); http_putc('"',out);
	  fd_xfree(string);}
	decref(val);}
      else fd_type_error("not a valid HTML/XML param",var);}
    http_puts(">\n",out);}
  else fd_raise_exception("Weird parameter list");
}

static fd_lisp html_frameset_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp frameset_attribs=fd_get_arg(expr,1,FD_VOID);
  fd_htstream *out=get_http_output();
  enum http_generation_phase ph=out->phase;
  if (ph == html_body)
    fd_raise_exception("Can't generate frameset within body");
  if ((ph == http_head) || (ph == http_body)) {
    char *doctype=get_doctype();
    finish_http_header();
    if (doctype) http_puts(doctype,out);
    else http_puts(default_doctype,out);}
  if (out->phase == html_head) http_puts("\n</head>\n",out);
  out->phase=html_frameset;
  start_env(out,"frameset",frameset_attribs,env);
  html_printout(fd_get_body(expr,2),env);
  http_puts("</frameset>\n",out);
  return FD_VOID;
}

#if 0
static fd_lisp html_frameset_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp frameset_attribs=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_htstream *out=get_http_output();
  enum http_generation_phase ph=out->phase;
  if (ph == html_body)
    fd_raise_exception("Can't generate frameset within body");
  out->phase=html_frameset;
  if ((ph == http_head) || (ph == http_body)) {
    char *doctype=get_doctype();
    finish_http_header();
    if (doctype) http_puts(doctype,out);
    else http_puts(default_doctype,out);
    http_puts("<html><head>\n<title>FramerD Generated Page</title>\n",out);
    out->phase=html_head;}
  http_puts("</head>\n",out);
  start_env(out,"frameset",frameset_attribs,env);
  {DOLIST(frame_spec,fd_get_body(expr,2))
     start_env(out,"frame",frame_spec,env);}
  http_puts("</frameset>\n",out);
  return FD_VOID;
}
#endif

/* Initializing the module */

void initialize_htmlgen_c()
{
  lisp framerd_url_symbol=fd_make_symbol("FRAMERD-URL");
  lisp framerd_credit_symbol=fd_make_symbol("FRAMERD-CREDIT");
  
  fd_lispenv menv=fd_make_module();
  fd_xml_env=fd_make_module();
  fd_html_env=menv;

#if (FD_THREADS_ENABLED)
  fd_init_mutex(&browse_url_lock);
  fd_new_tld_key(&local_frames_key,NULL);
  fd_new_tld_key(&http_output_key,NULL);
  fd_new_tld_key(&cookie_key,NULL);
  fd_new_tld_key(&doctype_key,NULL);
#endif
  
  stdout_htstream.stream_type=stdio;
  stdout_htstream.is_xml=(FD_IS_XML|FD_TRANSITIONAL_XHTML);
  fd_init_xfile(&stdout_htstream.stream.xfile,stdout,fd_get_default_encoding());

  browse_url=fd_make_string("browse.fdcgi");

  tag_slotid=fd_make_symbol("%TAG");
  xmltag_tag=fd_make_symbol("XMLTAG");

  qmark_symbol=fd_make_symbol("?");
  doctype_symbol=fd_make_symbol("!DOCTYPE");
  html_symbol=fd_make_symbol("HTML");

  html_name_symbol=fd_make_symbol("HTML-NAME");
  obj_name_symbol=fd_make_symbol("OBJ-NAME");
  body_style_symbol=fd_make_symbol("$BODY-STYLE$");
  timestamp_symbol=fd_make_symbol("TIMESTAMP0");
  
  a_symbol=fd_make_symbol("A");
  href_symbol=fd_make_symbol("HREF");

  textarea_symbol=fd_make_symbol("TEXTAREA");
  name_symbol=fd_make_symbol("NAME");
  value_symbol=fd_make_symbol("VALUE");
  size_symbol=fd_make_symbol("SIZE");
  cols_symbol=fd_make_symbol("COLS");
  rows_symbol=fd_make_symbol("ROWS");
  content_symbol=fd_make_symbol("CONTENT");

  FD_SET_SYMBOL_VALUE(framerd_url_symbol,fd_make_string(FRAMERD_URL));
  FD_SET_SYMBOL_VALUE(framerd_credit_symbol,fd_make_string(FRAMERD_CREDIT));
  obj_name_symbol=fd_make_symbol("%ID"); 
  frame_symbol=fd_make_symbol("FRAME"); 
  anonymous_symbol=fd_make_symbol("Anonymous");
  html_methods_symbol=fd_make_symbol("HTML-SCRIPT");
  head_symbol=fd_make_symbol("HEAD");
  body_symbol=fd_make_symbol("BODY");
  quote_symbol=fd_make_symbol("QUOTE");
  current_file_symbol=fd_make_symbol("*CURRENT-FILE*");
  p_symbol=fd_make_symbol("P");
  img_symbol=fd_make_symbol("IMG");
  src_symbol=fd_make_symbol("SRC");
  
  fd_init_hashtable(&normalized_elt_names,128);

  fd_add_cproc(menv,"HTTP-BODY!",0,lisp_http_body_cproc);
  fd_add_cproc(menv,"HTTP-FLUSH!",0,lisp_http_flush_cproc);
  fd_add_lexpr(menv,"SET-COOKIE!",FD_ND_LEXPR,lisp_set_cookie_lexpr);
  fd_add_cproc(menv,"SET-DOCTYPE!",2,lisp_set_doctype_cproc);
  fd_add_lexpr(menv,"!DOCTYPE",FD_NORMAL_LEXPR,lisp_doctype_lexpr);
  fd_add_lexpr(fd_xml_env,"!DOCTYPE",FD_NORMAL_LEXPR,lisp_doctype_lexpr);
  fd_add_lexpr(menv,"USE-BROWSE-SCRIPT!",FD_NORMAL_LEXPR,use_browse_url_lexpr);
  fd_add_cproc(menv,"DECLARE-LOCAL-FRAME!",1,fd_declare_local_frame);

  fd_add_cproc(menv,"HEADER",2,lisp_http_header_cproc);
  fd_add_cproc(menv,"HTML-INSERT-HEAD",1,lisp_html_insert_head_cproc);
  fd_add_cproc(menv,"HTML-START-BODY",1,lisp_html_start_body_cproc);

  fd_add_cproc(menv,"HTTP-REDIRECT",1,http_redirect_cproc);
  fd_add_lexpr(menv,"HTTP-SPLASH",FD_NORMAL_LEXPR,http_splash_lexpr);
  fd_add_cproc(menv,"STYLESHEET!",1,html_stylesheet_cproc);
  fd_add_cproc(menv,"META",2,html_meta_cproc);
  fd_add_special_form(menv,"TITLE",html_title_cproc);

  fd_add_special_form(menv,"WRITE-HTML-FILE",write_html_file_handler);
  fd_add_special_form(menv,"WRITE-XML-FILE",write_xml_file_handler);
  fd_add_special_form(menv,"HTMLFRAGMENT",htmlfragment_handler);
  fd_add_special_form(menv,"HTMLSTRING",htmlstring_handler);
  fd_add_special_form(fd_xml_env,"XMLFRAGMENT",xmlfragment_handler);
  fd_add_special_form(fd_xml_env,"XMLSTRING",xmlstring_handler);
  fd_add_special_form(menv,"HTTPDOC",httpdoc);
  /* Obsolete */
  fd_add_special_form(menv,"HTMLDOC",httpdoc);

  fd_add_special_form(menv,"REDIRECT",html_redirect_handler);
  fd_add_special_form(menv,"SPLASH",html_splash_handler);
  fd_add_cproc(menv,"MIME-RETURN-FILE",2,http_return_file);
  fd_add_cproc(menv,"MIME-RETURN-DATA",2,http_return_data);
  fd_add_special_form(menv,"MIME-RETURN",http_return_handler);

  fd_add_special_form(fd_xml_env,"BLOCK-MARKUP",markup_handler);
  fd_add_special_form(fd_xml_env,"MARKUP",imarkup_handler);
  fd_add_special_form(fd_xml_env,"NL",nl_handler);
  fd_add_special_form(menv,"BLOCK-MARKUP",imarkup_handler);
  fd_add_special_form(menv,"MARKUP",imarkup_handler);
  fd_add_special_form(menv,"NL",nl_handler);
  fd_add_lexpr(fd_xml_env,"XMLTAG",FD_NORMAL_LEXPR,xmlempty_lexpr);
  fd_add_lexpr(fd_xml_env,"XMLPI",FD_NORMAL_LEXPR,xmlpi_lexpr);
  fd_add_lexpr(menv,"XMLTAG",FD_NORMAL_LEXPR,xmlempty_lexpr);
  fd_add_special_form(fd_xml_env,"XMLENV",htmlenv_handler);
  fd_add_special_form(menv,"XMLENV",htmlenv_handler);

  fd_add_lexpr(menv,"HTMLTAG",FD_NORMAL_LEXPR,htmlempty_lexpr);
  fd_add_special_form(menv,"HTMLENV",htmlenv_handler);

  fd_add_special_form(menv,"HTML",htmlexpr);
  fd_add_special_form(menv,"XMLOUT",htmlexpr);
  fd_add_special_form(fd_xml_env,"XMLOUT",htmlexpr);
  fd_add_special_form(menv,"NOBREAK",htmlexpr_nobreak);

  /* Structural formatting directives */
  fd_add_html_block_tag("H1"); fd_add_html_block_tag("H2");
  fd_add_html_block_tag("H3"); fd_add_html_block_tag("H4");
  fd_add_html_block_tag("CENTER"); fd_add_html_block_tag("TABLE"); 
  fd_add_html_block_tag("BLOCKQUOTE");
  
  fd_add_special_form(fd_html_env,"P",p_handler);
  fd_add_special_form(fd_html_env,"P*",pattr_handler); 

  fd_add_html_block_tag("OL"); fd_add_html_block_tag("UL"); 
  fd_add_html_block_tag("LI");

  fd_add_html_empty_tag("HR"); fd_add_html_empty_tag("BR");
  fd_add_html_empty_tag("IMG");

  fd_add_special_form(menv,"CODEBLOCK",htmlcodeblock);

  /* Typeface formatting directives */
  fd_add_html_inline_tag("EM"); fd_add_html_inline_tag("STRONG");
  fd_add_html_inline_tag("DEFN"); fd_add_html_inline_tag("TT");
  fd_add_html_inline_tag("SUB"); fd_add_html_inline_tag("SUP"); 

  fd_add_htmlattr_inline_tag("A"); 
  fd_add_htmlattr_inline_tag("A*"); 
  fd_add_htmlattr_inline_tag("FONT"); 
  fd_add_htmlattr_inline_tag("FONT*"); 
  fd_add_htmlattr_inline_tag("SPAN"); 

  /* Form stuff */
  fd_add_htmlattr_block_tag("FORM");
  fd_add_htmlattr_block_tag("DIV");

  fd_add_html_empty_tag("INPUT");
  /* fd_add_htmlattr_inline_tag("TEXTAREA"); */

  fd_add_special_form(menv,"CHECKBOX",html_checkbox_handler);
  fd_add_special_form(menv,"RADIOBUTTON",html_radiobutton_handler);
  fd_add_special_form(menv,"TEXTFIELD",html_textfield_handler);
  fd_add_special_form(menv,"TEXTAREA",html_textarea_handler);
  fd_add_special_form(menv,"SELECTION",html_selection_handler);
  fd_add_special_form(menv,"OPTION",html_option_handler);
  fd_add_special_form(menv,"SUBMIT",html_submit_handler);
  fd_add_special_form(menv,"CGIPASS",html_cgipass_handler);
  fd_add_alias(menv,"PASS","CGIPASS");
  fd_add_htmlattr_inline_tag("BUTTON"); 
  
  fd_add_special_form(fd_html_env,"OPTION*",htmlattr_inline_handler);

  /* Tabular output */
  fd_add_html_block_tag("TR");
  fd_add_html_inline_tag("TD");
  fd_add_html_inline_tag("TH");

  /* Some special forms */
  fd_add_special_form(menv,"ANCHOR",htmlanchor);
  fd_add_special_form(menv,"ANCHOR@",htmltanchor);
  fd_add_special_form(menv,"ANCHOR+",htmlanchorplus);
  fd_add_special_form(menv,"TAGGED",htmltag);
  fd_add_alias(menv,"TAG","TAGGED");
  fd_add_special_form(menv,"IMAGE",htmlimage);
  fd_add_special_form(menv,"IMG*",htmlattr_inline_handler);
  fd_add_special_form(menv,"HTMLINCLUDE",htmlinclude);
  fd_add_special_form(menv,"FRAMESET",html_frameset_handler);

  fd_add_cproc(menv,"FRAME->HTML",1,describe_oid_in_html);
  fd_add_alias(menv,"OID->HTML","FRAME->HTML");
  fd_add_lexpr(menv,"SLOTS->HTML",FD_NORMAL_LEXPR,output_slots_lexpr);

  /* Obsolete */
  fd_add_cproc(menv,"DESCRIBE-OID",1,describe_oid_in_html);
  
  /* Invoking remote scripts */
  fd_add_lexpr(fd_xml_env,"SCRIPTURL",FD_ND_LEXPR,scripturl);
  fd_add_lexpr(menv,"SCRIPTURL",FD_ND_LEXPR,scripturl);
  fd_add_cproc(menv,"URI-ENCODE",1,uri_encode_string);

  fd_add_cproc(menv,"INDEX-URL",2,index_url);
  fd_add_cproc(menv,"WEBINDEX",2,index_url);

  fd_add_special_form(fd_xml_env,"UNPARSE-XML",unparse_xml_handler);
  fd_add_cproc(fd_xml_env,"UNPARSE-HTML",1,unparse_html);
  fd_add_cproc(menv,"UNPARSE-HTML",1,unparse_html);

  fd_register_module("HTMLGEN",menv,FD_SAFE_ENV,0);
  fd_register_module("XMLGEN",fd_xml_env,FD_SAFE_ENV,0);

  fd_register_source_file("htmlgen",__DATE__,vcid);
}



/* File specific stuff */

/* The CVS log for this file
   $Log: htmlgen.c,v $
   Revision 1.68  2005/08/04 23:37:21  haase
   Changed obj-name to %id

   Revision 1.67  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.66  2004/12/16 22:08:07  haase
   Update for XML and FRAMESET output

   Revision 1.65  2004/11/17 16:37:28  haase
   Made redirection add a status code (so it works)

   Revision 1.64  2004/11/13 00:13:08  haase
   Fixed browse scripts to not escape string arguments

   Revision 1.63  2004/10/27 17:24:10  haase
   Fix to handle empty choice hrefs

   Revision 1.62  2004/08/26 19:16:22  haase
   Removed overlays

   Revision 1.61  2004/08/02 14:38:33  haase
   Added more HTML escaping

   Revision 1.60  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.59  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.58  2004/07/18 15:17:32  haase
   Addex XMLOUT to XML module

   Revision 1.57  2003/12/18 03:41:34  haase
   Cleaned up separation of fdtext and fdwww

   Revision 1.56  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.55  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.54  2003/12/02 12:31:54  haase
   Made slots->html accept explicit slotids and optional values

   Revision 1.53  2003/11/25 12:47:25  haase
   Made unparse-xml work on either top level elements or embedded content (lists of elements or strings)

   Revision 1.52  2003/11/20 15:55:29  haase
   Fixes to slotmap output

   Revision 1.51  2003/11/15 09:51:17  haase
   Additional leak fix for HTML generation

   Revision 1.50  2003/11/15 09:37:27  haase
   Fixed leak in empty element generation

   Revision 1.49  2003/11/07 19:57:45  haase
   Added init for browse_url

   Revision 1.48  2003/11/06 09:56:09  haase
   Various fixes to handle empty args to HTML generators; cleaned up ascii output code to avoid extra mallocs

   Revision 1.47  2003/11/04 19:43:05  haase
   Patch to HTMLGEN anchor failure case

   Revision 1.46  2003/11/04 19:38:38  haase
   Made some HTMLGEN procedures more forgiving of failures

   Revision 1.45  2003/11/03 00:08:42  haase
   Added more complex browse URL themes

   Revision 1.44  2003/10/22 22:52:47  haase
   Whitespace changes

   Revision 1.43  2003/10/20 17:00:42  haase
   Added some pseudo slotids to automatically generated frame descriptions

   Revision 1.42  2003/10/20 09:22:06  haase
   Updates to FDXML primitives

   Revision 1.41  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.40  2003/10/05 06:47:30  haase
   Made attributes with empty (fail) values not be displayed at all.

   Revision 1.39  2003/09/26 16:24:55  haase
   Fixed malloc bug in anchor generation

   Revision 1.38  2003/09/24 06:38:55  haase
   Added anchor+ to combine oid refs with attributes

   Revision 1.37  2003/09/23 12:39:58  haase
   Made OID description use CSS, removed bug with complex arg to fd_printf

   Revision 1.36  2003/09/20 18:04:42  haase
   Fixes and updates to fdxml and htmlgen

   Revision 1.35  2003/09/07 18:24:25  haase
   Added API access to http output and moved FDXML callouts to xmleval

   Revision 1.34  2003/09/05 13:00:45  haase
   Added improved XML callouts for XML unparsing

   Revision 1.33  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.32.2.12  2003/08/06 18:41:08  haase
   Updated copyright notices to 2003

   Revision 1.32.2.11  2003/08/02 13:47:45  haase
   Made HTTP streams use XFILEs, and added nullstream type for noop streams

   Revision 1.32.2.10  2003/05/22 01:39:07  haase
   Added HTTP-BODY\! primitive

   Revision 1.32.2.9  2003/05/20 22:43:50  haase
   Fix the double name display in OID generation

   Revision 1.32.2.8  2003/02/17 12:26:18  haase
   Changed html_start to http_body

   Revision 1.32.2.7  2003/01/26 21:26:20  haase
   Fixed IMAGE HTML tag to evaluate parameters in current environment

   Revision 1.32.2.6  2003/01/26 20:50:20  haase
   Misc. fixes, especially GC

   Revision 1.32.2.5  2002/09/29 11:35:39  haase
   Fixes to HTML generation and elisp indentations and installation

   Revision 1.32.2.4  2002/08/25 01:39:20  haase
   Fixed DOCTYPE parsing and generation work correctly

   Revision 1.32.2.3  2002/08/22 21:26:15  haase
   Made generated XHTML straight for legacy browsers

   Revision 1.32.2.2  2002/08/21 22:06:25  haase
   Generalizations to HTTP generation to make it work better with non-HTML XML

   Revision 1.32.2.1  2002/08/21 01:53:21  haase
   Recoded XML/HTML generation to be more general and allow strings to be used as element names; first steps towards better namespace integration into XML generation

   Revision 1.32  2002/07/16 15:26:34  haase
   Fixed TAGGED to evaluate its argument (in case its an OID, for instance)

   Revision 1.31  2002/07/01 02:53:38  haase
   Minor, inusufficient, changes to oid->html

   Revision 1.30  2002/06/24 16:29:42  haase
   Made xml parsing do case preservation, complicating the implementation xmltags

   Revision 1.29  2002/06/15 14:52:20  haase
   Made PASS into an alias for CGIPASS

   Revision 1.28  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.27  2002/05/27 13:06:06  haase
   Added external API for http output from C

   Revision 1.26  2002/05/27 00:57:24  haase
   Fixed SEGV problem in looking for XML handlers

   Revision 1.25  2002/05/26 04:53:16  haase
   Added fdservlet executable and mod_fdserv module

   Revision 1.24  2002/05/13 07:26:18  haase
   Fixes to argument checking in htmlgen primitives

   Revision 1.23  2002/05/11 13:41:47  haase
   Added MIME-RETURN-DATA for returning packets or strings directly

   Revision 1.22  2002/05/07 08:07:25  haase
   Made HTMLGEN write namespaces in generated XML

   Revision 1.21  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.20  2002/04/27 17:48:11  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.19  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.18  2002/04/19 00:14:40  haase
   Fix comment

   Revision 1.17  2002/04/03 13:27:05  haase
   Update new browse-script customization for non-threaded environments

   Revision 1.16  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
