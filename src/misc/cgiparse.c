/* C Mode */

/* cgiparse.c
   Implements CGI (and FastCGI intepretation) for FDScript
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

static char vcid[] = "$Id: cgiparse.c,v 1.39 2005/01/14 16:48:46 haase Exp $";

#define FD_SOURCE 1
#include "framerd/fdscript.h"
#include "framerd/fdtext.h"
#include "framerd/fdwww.h"

static fd_lisp cgi_data_symbol, content_slotid, query_slotid;
static fd_lisp disp_name_symbol, disp_filename_symbol;
static fd_lisp parsed_symbol, string_symbol;
EXPORTED fd_lispenv fd_cgiparse_env;
IMPORTED fd_lispenv fd_xmleval_env;
fd_lispenv fd_cgiparse_env;
static int cgi_init_debug=0;

static fd_lisp
  SERVER_SOFTWARE, SERVER_NAME, SERVER_PORT, SERVER_HOST,
  SCRIPT_NAME, SCRIPT_URI, PATH_INFO, HTTPS, HTTP_REFERER,
  HTTP_USER_AGENT, REMOTE_IDENT, REMOTE_HOST, REMOTE_ADDR,
  DOCUMENT_ROOT, PATH_TRANSLATED, AUTH_TYPE, REMOTE_USER, HTTP_COOKIE,
  HTTP_ACCEPT, HTTP_ACCEPT_CHARSET, HTTP_ACCEPT_ENCODING, HTTP_ACCEPT_LANGUAGE,
  REQUEST_METHOD, CONTENT_TYPE, CONTENT_LENGTH, QUERY_STRING, POST_DATA,
  HTTP_SECURE;


/* Retrieving CGI data */

#if FD_THREADS_ENABLED
static fd_tld_key cgi_data_key;
#else
static fd_lisp cgi_data;
#endif

static fd_lisp get_cgi_data()
{
#if FD_THREADS_ENABLED
  fd_lisp *v=fd_tld_get(cgi_data_key);
  if (v) return fd_incref(*v); else return FD_VOID;
#else
  return fd_incref(cgi_data);
#endif
}

static void set_cgi_data(fd_lisp nv)
{
#if FD_THREADS_ENABLED
  fd_lisp *v=fd_tld_get(cgi_data_key);
  if (v) {fd_decref(*v); *v=fd_incref(nv);}
  else {
    v=fd_xmalloc(sizeof(fd_lisp)); *v=fd_incref(nv);
    fd_tld_set(cgi_data_key,v);}
#else
  fd_decref(cgi_data);
  cgi_data=fd_incref(nv);
#endif
}

EXPORTED
/* fd_get_cgi_data:
     Arguments: none
     Returns: a lisp pointer
     The returned value is a slotmap describing the current CGI session.
*/
fd_lisp fd_get_cgi_data()
{
  return get_cgi_data();
}

EXPORTED
/* fd_get_cgi_data:
     Arguments: a lisp pointer
     Returns: nothing (void)
     Sets the state variable describing the current CGI session.
*/
void fd_set_cgi_data(fd_lisp new_value)
{
  set_cgi_data(new_value);
}

/* Parsing CGI args */

static int parse_hex(char *buf)
{
  char *scan=buf; int c;
  while (*scan)
    if (isxdigit(*scan)) scan++;
    else fd_raise_exception("Invalid unicode escape");
  c=strtol(buf,NULL,16);
  return c;
}

static fd_u8char *convert_string(char *start,char *end)
{
  unsigned char *string=start, *scan=string;
  unsigned char *string_end=((end == NULL) ? (start+strlen(start)) : (end));
  struct FD_STRING_STREAM os;
  if (*start == NUL) return fd_strdup(start);
  FD_INITIALIZE_STRING_STREAM(&os,(string_end-string)+3);
  while (scan < string_end)
    if (*scan == '+') {fd_sputc(&os,' '); scan++;}
    else if (*scan > 0x7f) {fd_sputc(&os,*scan); scan++;}  
    else if (*scan == '%') {
      char buf[3]; long charcode;
      scan++; buf[0]=*scan++; buf[1]=*scan++; buf[2]='\0';
      charcode=strtol(buf,NULL,16);
      if ((charcode == '\\') && (scan[0] == 'u')) {
	char buf[5]; long c; scan=scan+1;
	buf[0]=*scan++; buf[1]=*scan++; buf[2]=*scan++; buf[3]=*scan++;
	buf[4]=0; c=parse_hex(buf);
	fd_sputc(&os,c);}
      else if ((charcode == '\\') && (scan[0] == 'U')) {
	char buf[9]; long c; scan=scan+1;
	buf[0]=*scan++; buf[1]=*scan++; buf[2]=*scan++; buf[3]=*scan++;
	buf[4]=*scan++; buf[5]=*scan++; buf[6]=*scan++; buf[7]=*scan++;	
	buf[8]=0; c=parse_hex(buf);
	fd_sputc(&os,c);}
      else fd_sputc(&os,charcode);}
    else if ((*scan == '\\') && (scan[1] == 'u')) {
      char buf[5]; long c; scan=scan+2;
      buf[0]=*scan++; buf[1]=*scan++; buf[2]=*scan++; buf[3]=*scan++;
      buf[4]=0; c=parse_hex(buf);
      fd_sputc(&os,c);}
    else if ((*scan == '\\') && (scan[1] == 'U')) {
      char buf[9]; long c; scan=scan+1;
      buf[0]=*scan++; buf[1]=*scan++; buf[2]=*scan++; buf[3]=*scan++;
      buf[4]=*scan++; buf[5]=*scan++; buf[6]=*scan++; buf[7]=*scan++;	
      buf[8]=0; c=parse_hex(buf);
      fd_sputc(&os,c);}
    else {int c=*scan++; fd_sputc(&os,c);}
  return os.ptr;
}

static void init_cgi_var(fd_slotmap smap,char *var,char *string)
{
  lisp symbol=fd_parse_string(var);
  if (cgi_init_debug) fd_warn("init_cgi_var(%s,%s)",var,string);
  if (!(SYMBOLP(symbol))) symbol=fd_make_symbol(var);
  if (string == NULL) fd_slotmap_add(smap,symbol,FD_EMPTY_CHOICE);
  else {
    lisp current=fd_slotmap_get(smap,symbol,FD_VOID);
    lisp new_string=fd_copy_string(string);
    if (FD_VOIDP(current)) {
      fd_slotmap_set(smap,symbol,new_string);
      fd_decref(new_string);}
    else if (FD_STRINGP(current)) {
      lisp new_list=FD_MAKE_LIST(2,new_string,incref(current));
      fd_slotmap_set(smap,symbol,new_list);
      fd_decref(new_list);}
    else if (FD_PAIRP(current)) {
      lisp new_list=FD_MAKE_PAIR(new_string,current);
      fd_slotmap_set(smap,symbol,new_list);
      fd_decref(new_list);}
    else fd_type_error("not a string or pair",current);}
}

static void init_cgi_slotmap(fd_slotmap smap,fd_lisp symbol,char *string)
{
  if (cgi_init_debug)
    fd_warn("init_cgi_var(%s,%s)",fd_symbol_name(symbol),string);
  if (string) {
    lisp current=fd_slotmap_get(smap,symbol,FD_VOID);
    lisp new_string=fd_copy_string(string);
    if (FD_VOIDP(current)) {
      fd_slotmap_set(smap,symbol,new_string);
      fd_decref(new_string);}
    else if (FD_STRINGP(current)) {
      lisp new_list=FD_MAKE_LIST(2,new_string,incref(current));
      fd_slotmap_set(smap,symbol,new_list);
      fd_decref(new_list);}
    else if (FD_PAIRP(current)) {
      lisp new_list=FD_MAKE_PAIR(new_string,current);
      fd_slotmap_set(smap,symbol,new_list);
      fd_decref(new_list);}
    else fd_type_error("not a string or pair",current);}
}

static void init_uri_encoded_form_data(fd_slotmap cgi_frame,char *data,int len)
{
  /* Initialize any variables bound in the query string */
  char *scan=data, *limit=scan+len; 
  if (cgi_init_debug)
    fd_warn("processing uri encoded form data: %s",data);
  while (scan && (scan < limit)) {
    char *equals=strchr(scan,'='), *end;
    fd_u8char *varname, *value;
    if (equals) end=strchr(equals,'&'); else break;
    varname=convert_string(scan,equals);
    if (equals+1 >= limit) init_cgi_var(cgi_frame,varname,"");
    else {
      value=convert_string(equals+1,end);
      init_cgi_var(cgi_frame,varname,value);
      fd_xfree(value);}
    fd_xfree(varname); 
    if (end) scan=end+1; else scan=end;}
}

static char *assemble_mime_packet(int len,char *ctype,char *data,int *msize)
{
  char *buf=fd_xmalloc(1024+strlen(ctype)+len);
  int head_len=sprintf(buf,"Content-Length: %d\r\nContent-Type: %s\r\n\r\n",
		       len,((ctype == NULL) ? "text" : (ctype)));
  memcpy(buf+head_len,data,len); *msize=head_len+len;
  return buf;
}

static void bind_from_mime(fd_slotmap cgi_frame,fd_lisp item)
{
  if (FD_SLOTMAPP(item)) {
    fd_lisp disp_name=fd_prim_get(item,disp_name_symbol);
    fd_lisp disp_filename=fd_prim_get(item,disp_filename_symbol);
    if (FD_STRINGP(disp_name)) 
      fd_slotmap_add(cgi_frame,fd_make_symbol(FD_STRING_DATA(disp_name)),item);
    if (FD_STRINGP(disp_filename)) 
      fd_prim_add(item,fd_make_symbol("FILENAME"),disp_filename);}
}

/* Handling some special fields */

static void parse_cookie(fd_slotmap cgi_frame,char *cookie)
{
  char *binding=cookie;
  while (isspace(*binding)) binding++;
  while (1) {
    char *equals=strchr(binding,'='), *varname, *varval, *scan, *end=NULL;
    if (equals == NULL) {
      fd_warn("Bad cookie spec: `%s'",cookie);
      return;}
    else end=strchr(equals,';');
    /* Extract the varname and valname */
    varname=convert_string(binding,equals);
    varval=convert_string(equals+1,end);
    /* Uppercase the variable name */
    scan=varname; while (*scan) {*scan=toupper(*scan); scan++;}
    /* Do the init and then free the strings you created */
    init_cgi_var(cgi_frame,varname,varval);
    fd_xfree(varname); fd_xfree(varval);
    /* Advance to the next binding */
    if (end) binding=end+1; else break;
    while (isspace(*binding)) binding++;}
}

static int check_secure_connection(fd_slotmap cgi_frame,char *env_val)
{
  if (env_val == NULL) {
    fd_slotmap_add(cgi_frame,HTTP_SECURE,FD_FALSE);
    return 0;}
  else if ((strcasecmp(env_val,"NO") == 0) || (strcasecmp(env_val,"OFF") == 0)) {
    fd_slotmap_add(cgi_frame,HTTP_SECURE,FD_FALSE);
    return 0;}
  else fd_slotmap_add(cgi_frame,HTTP_SECURE,FD_TRUE);
  return 1;
}

/* Handling client preferences */

struct _FD_CLIENT_PREF {fd_lisp pref; float weight; int init_pos;};

static int compare_client_pref(const void *v1,const void *v2)
{
  const struct _FD_CLIENT_PREF *p1=v1, *p2=v2;
  if (p1->weight > p2->weight) return 1;
  else if (p2->weight > p1->weight) return -1;
  else if (p1->init_pos < p2->init_pos) return -1;
  else if (p1->init_pos > p2->init_pos) return 1;
  else return 0;
}

static void bind_client_preferences
  (fd_slotmap cgi_data,char *symbol_name,fd_lisp slotid)
{
  fd_lisp val=fd_slotmap_get(cgi_data,slotid,FD_VOID);
  if (FD_VOIDP(val)) {
    /* fprintf(stderr,"Binding for %s is NULL\n",symbol_name); */
    return;}
  else {
    fd_u8char *env_val=FD_STRING_DATA(val);
    struct _FD_CLIENT_PREF *prefs=
      fd_xmalloc(sizeof(struct _FD_CLIENT_PREF)*64);
    char *scan=env_val;
    fd_lisp results=FD_EMPTY_LIST;
    int i=0, n_prefs=0, max_prefs=64;
    /* fprintf(stderr,"BINDING %s based on %s\n",symbol_name,env_val); */
    while ((*scan) && (isspace(*scan))) scan++;
    while ((scan) && (*scan)) {
      char *equals=strstr(scan,";q="), *end=strchr(scan,',');
      int pref_len, weight_len;
      if (equals == NULL) equals=strstr(scan," q=");
      /* fprintf(stderr,"scan=%s\nequals=%s\nend=%s\n",scan,equals,end); */
      if (n_prefs >= max_prefs) {
	prefs=fd_xrealloc(prefs,sizeof(struct _FD_CLIENT_PREF)*(64+max_prefs));
	max_prefs=max_prefs+64;}
      if ((end == NULL) && (equals == NULL)) {
	pref_len=strlen(scan); weight_len=0;}
      else if ((end == NULL) && (equals)) {
	pref_len=equals-scan; weight_len=strlen(equals+3);}
      else if ((equals) && (equals < end)) {
	pref_len=equals-scan; weight_len=end-(equals+3);}
      else {pref_len=end-scan; weight_len=0;}
      /* fprintf(stderr,"weight_len=%d\npref_len=%d\n",weight_len,pref_len); */
      if (weight_len > 64) {
	fd_warn(_("Strange value to init %s: %s"),symbol_name,env_val);
	if (end) scan=end+1; else scan=end;}
      else if (weight_len == 0) {
	prefs[n_prefs].pref=fd_make_substring(scan,scan+pref_len);
	prefs[n_prefs].weight=1.0; prefs[n_prefs].init_pos=n_prefs;
	n_prefs++;}
      else {
	char buf[64];
	prefs[n_prefs].pref=fd_make_substring(scan,scan+pref_len);
	strncpy(buf,equals+3,weight_len); buf[weight_len]=NUL;
	sscanf(buf,"%f",&(prefs[n_prefs].weight));
	prefs[n_prefs].init_pos=n_prefs;
	n_prefs++;}
      if (end) {
	scan=end+1; while ((*scan) && (isspace(*scan))) scan++;}
      else scan=end;}
    /* fprintf(stderr,"About to start sorting %d prefs\n"); */
    qsort(prefs,n_prefs,sizeof(struct _FD_CLIENT_PREF),compare_client_pref);
    /* fprintf(stderr,"Done sorting %d prefs\n"); */
    i=0; while (i < n_prefs) {
      if (prefs[i].weight == 1.0)
	results=FD_MAKE_PAIR(FD_MAKE_PAIR(prefs[i].pref,FD_LISPFIX(1)),results);
      else results=
	     FD_MAKE_PAIR(FD_MAKE_PAIR(prefs[i].pref,
				       FD_LISPFLOAT(prefs[i].weight)),
			  results);
      i++;}
    /* fd_fprintf(stderr,"Results is %q\n",results); */
    fd_slotmap_set(cgi_data,fd_make_symbol(symbol_name),results);
    fd_decref(results); fd_decref(val);}
}

/* Actually doing the CGI init */

static char *fake_query_string=NULL, *fake_script_name=NULL;

EXPORTED void fd_fake_query(char *script_name,char *query_string)
{
  fake_script_name=fd_strdup(script_name);
  fake_query_string=fd_strdup(query_string);
}

static void setup_fake_query(fd_slotmap cgi_slotmap)
{
  init_cgi_slotmap(cgi_slotmap,QUERY_STRING,fake_query_string);
  if (fake_script_name)
    init_cgi_slotmap(cgi_slotmap,SCRIPT_NAME,fake_script_name);
  init_cgi_slotmap(cgi_slotmap,SERVER_NAME,"fakeserver");
  init_cgi_slotmap(cgi_slotmap,REQUEST_METHOD,"GET");
  fd_slotmap_add(cgi_slotmap,SERVER_PORT,FD_LISPFIX(80));
}

static void stdcgi_basic_init(fd_slotmap cgi_slotmap)
{
  char *rmethod;

  init_cgi_slotmap(cgi_slotmap,SERVER_SOFTWARE,getenv("SERVER_SOFTWARE"));
  init_cgi_slotmap(cgi_slotmap,SERVER_NAME,getenv("SERVER_NAME"));
  init_cgi_slotmap(cgi_slotmap,SERVER_PORT,getenv("SERVER_PORT"));
  init_cgi_slotmap(cgi_slotmap,SCRIPT_NAME,getenv("SCRIPT_NAME"));
  
  init_cgi_slotmap(cgi_slotmap,HTTP_REFERER,getenv("HTTP_REFERER"));
  init_cgi_slotmap(cgi_slotmap,HTTP_USER_AGENT,getenv("HTTP_USER_AGENT"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_IDENT,getenv("REMOTE_IDENT"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_HOST,getenv("REMOTE_HOST"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_ADDR,getenv("REMOTE_ADDR"));
  
  init_cgi_slotmap(cgi_slotmap,PATH_INFO,getenv("PATH_INFO"));
  init_cgi_slotmap(cgi_slotmap,DOCUMENT_ROOT,getenv("DOCUMENT_ROOT"));
  init_cgi_slotmap(cgi_slotmap,PATH_TRANSLATED,getenv("PATH_TRANSLATED"));
  
  init_cgi_slotmap(cgi_slotmap,AUTH_TYPE,getenv("AUTH_TYPE"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_USER,getenv("REMOTE_USER"));
  init_cgi_slotmap(cgi_slotmap,HTTP_COOKIE,getenv("HTTP_COOKIE"));
  init_cgi_slotmap(cgi_slotmap,SERVER_HOST,getenv("SERVER_HOST"));
  init_cgi_slotmap(cgi_slotmap,HTTP_COOKIE,getenv("HTTP_COOKIE"));
  init_cgi_slotmap(cgi_slotmap,HTTPS,getenv("HTTPS"));
  
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT,getenv("HTTP_ACCEPT"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_CHARSET,
		 getenv("HTTP_ACCEPT_CHARSET"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_ENCODING,
		 getenv("HTTP_ACCEPT_ENCODING"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_LANGUAGE,
		 getenv("HTTP_ACCEPT_LANGUAGE"));
  
  init_cgi_slotmap(cgi_slotmap,REQUEST_METHOD,getenv("REQUEST_METHOD"));
  init_cgi_slotmap(cgi_slotmap,CONTENT_TYPE,getenv("CONTENT_TYPE"));
  init_cgi_slotmap(cgi_slotmap,CONTENT_LENGTH,getenv("CONTENT_LENGTH"));
  if (fake_query_string) setup_fake_query(cgi_slotmap);
  else init_cgi_slotmap(cgi_slotmap,QUERY_STRING,getenv("QUERY_STRING"));

  rmethod=getenv("REQUEST_METHOD");
  if ((rmethod) && (strcmp(rmethod,"POST") == 0)) {
    char *size_string=getenv("CONTENT_LENGTH"), *post_data;
    int actual_size, post_size; fd_lisp packet;
    post_size=strtol(size_string,NULL,10);
    post_data=fd_xmalloc(post_size+1);
    actual_size=fread(post_data,sizeof(char),post_size,stdin);
    post_data[actual_size]='\0';
    if (actual_size != post_size)
      fd_warn(_("Promised and actual content-length don't match"));
    packet=fd_make_packet(actual_size,post_data);
    fd_slotmap_add(cgi_slotmap,POST_DATA,packet);
    fd_decref(packet);}
}

EXPORTED void fdcgi_extended_init(fd_slotmap cgi_slotmap)
{
  int secure=0;
  {
    fd_lisp cookie=fd_slotmap_get(cgi_slotmap,HTTP_COOKIE,FD_VOID);
    if (FD_STRINGP(cookie))
      parse_cookie(cgi_slotmap,FD_STRING_DATA(cookie));
    fd_decref(cookie);}
  {
    fd_lisp https=fd_slotmap_get(cgi_slotmap,HTTP_SECURE,FD_VOID);
    if (FD_STRINGP(https))
      if(check_secure_connection(cgi_slotmap,FD_STRING_DATA(https)))
	secure=1; else secure=0;
    fd_decref(https);}

  /* Handle preferences */
  bind_client_preferences(cgi_slotmap,"ACCEPTED-MIME-TYPES",HTTP_ACCEPT);
  bind_client_preferences(cgi_slotmap,"ACCEPTED-CHARSETS",HTTP_ACCEPT_CHARSET);
  bind_client_preferences(cgi_slotmap,"ACCEPTED-ENCODINGS",HTTP_ACCEPT_ENCODING);
  bind_client_preferences(cgi_slotmap,"ACCEPTED-LANGUAGES",HTTP_ACCEPT_LANGUAGE);

  {
    fd_lisp query=fd_slotmap_get(cgi_slotmap,QUERY_STRING,FD_VOID);
    if (!(FD_VOIDP(query))) {
      fd_slotmap_add(cgi_slotmap,query_slotid,query);
      fd_decref(query);}}
  {
    fd_lisp method=fd_slotmap_get(cgi_slotmap,REQUEST_METHOD,FD_VOID);
    fd_lisp ctype=fd_slotmap_get(cgi_slotmap,CONTENT_TYPE,FD_VOID);
    /* fd_warn("METHOD=%q",method); fd_warn("CTYPE=%q",ctype); */
    if ((FD_VOIDP(method)) ||
	((FD_STRINGP(method)) &&
	 (strcmp(FD_STRING_DATA(method),"GET") == 0))) {
      fd_lisp qstring=fd_slotmap_get(cgi_slotmap,QUERY_STRING,FD_VOID);
      if (FD_STRINGP(qstring)) 
	init_uri_encoded_form_data
	  (cgi_slotmap,FD_STRING_DATA(qstring),FD_STRING_LENGTH(qstring));
      fd_decref(qstring);}
    else if ((FD_STRINGP(method)) &&
	     (strcmp(FD_STRING_DATA(method),"POST") == 0))
      if ((STRINGP(ctype)) &&
	  (strncmp(FD_STRING_DATA(ctype),"multipart/form-data",19) == 0)) {
	fd_lisp pdata=fd_slotmap_get(cgi_slotmap,POST_DATA,FD_VOID);
	int mime_size;
	char *mdata=assemble_mime_packet
	  (fd_byte_length(pdata),FD_STRING_DATA(ctype),
	   fd_byte_data(pdata),&mime_size);
	fd_lisp entity=fd_parse_mime(mdata,mime_size);
	fd_lisp contents=fd_prim_get(entity,content_slotid);
	{DOLIST(item,contents) bind_from_mime(cgi_slotmap,item);}
	fd_slotmap_add(cgi_slotmap,fd_make_symbol("POST"),entity);
	fd_xfree(mdata); fd_decref(pdata); fd_decref(entity);
	fd_decref(contents);}
      else {
	fd_lisp pdata=fd_slotmap_get(cgi_slotmap,POST_DATA,FD_VOID);
	if (cgi_init_debug)
	  fd_fprintf(stderr,"Posted URI encoded data: %s",FD_STRING_DATA(pdata));
	if ((FD_PACKETP(pdata)) || (FD_STRINGP(pdata)))
	  init_uri_encoded_form_data
	    (cgi_slotmap,FD_STRING_DATA(pdata),FD_STRING_LENGTH(pdata));
	fd_decref(pdata);}
    fd_decref(ctype); fd_decref(method);}
  {
    fd_lisp server_name=fd_slotmap_get(cgi_slotmap,SERVER_NAME,FD_VOID);
    fd_lisp server_port=fd_slotmap_get(cgi_slotmap,SERVER_PORT,FD_VOID);
    fd_lisp script_name=fd_slotmap_get(cgi_slotmap,SCRIPT_NAME,FD_VOID);
    if ((FD_STRINGP(server_name)) &&
	((FD_STRINGP(server_port)) || (FD_FIXNUMP(server_port))) &&
	(FD_STRINGP(script_name))) {
      struct FD_STRING_STREAM out; fd_lisp suri;
      FD_INITIALIZE_STRING_STREAM(&out,128);
      if (secure) fd_sputs(&out,"https://"); else fd_sputs(&out,"http://");
      fd_sputs(&out,fd_strdata(server_name));
      if (FD_FIXNUMP(server_port)) fd_printf(&out,":%d",fd_lisp2int(server_port));
      else if (FD_STRINGP(server_port)) fd_printf(&out,":%s",fd_strdata(server_port));
      fd_sputs(&out,fd_strdata(script_name)); suri=fd_copy_string(out.ptr);
      fd_slotmap_add(cgi_slotmap,SCRIPT_URI,suri); fd_decref(suri);
      fd_xfree(out.ptr);}
    fd_decref(server_name); fd_decref(server_port); fd_decref(script_name);}
}

EXPORTED lisp fd_stdcgi_init(fd_lispenv env)
{
  char *cookie, *rmethod, *query_data, *ctype;
  lisp data=fd_make_slotmap(16); fd_slotmap cgi_slotmap;
  set_cgi_data(data); cgi_slotmap=SLOTMAP_PTR(data);
  stdcgi_basic_init(cgi_slotmap);
  fdcgi_extended_init(cgi_slotmap);
  return data;
}

#if (HAVE_FASTCGI)
#define fgetenv(x) FCGX_GetParam(x,fenv)

static void fastcgi_basic_init
  (fd_slotmap cgi_slotmap,FCGX_ParamArray fenv,FCGX_Stream *in)
{
  char *rmethod;

  init_cgi_slotmap(cgi_slotmap,SERVER_SOFTWARE,fgetenv("SERVER_SOFTWARE"));
  init_cgi_slotmap(cgi_slotmap,SERVER_NAME,fgetenv("SERVER_NAME"));
  init_cgi_slotmap(cgi_slotmap,SERVER_PORT,fgetenv("SERVER_PORT"));
  init_cgi_slotmap(cgi_slotmap,SCRIPT_NAME,fgetenv("SCRIPT_NAME"));
  
  init_cgi_slotmap(cgi_slotmap,HTTP_REFERER,fgetenv("HTTP_REFERER"));
  init_cgi_slotmap(cgi_slotmap,HTTP_USER_AGENT,fgetenv("HTTP_USER_AGENT"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_IDENT,fgetenv("REMOTE_IDENT"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_HOST,fgetenv("REMOTE_HOST"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_ADDR,fgetenv("REMOTE_ADDR"));
  
  init_cgi_slotmap(cgi_slotmap,PATH_INFO,fgetenv("PATH_INFO"));
  init_cgi_slotmap(cgi_slotmap,DOCUMENT_ROOT,fgetenv("DOCUMENT_ROOT"));
  init_cgi_slotmap(cgi_slotmap,PATH_TRANSLATED,fgetenv("PATH_TRANSLATED"));
  
  init_cgi_slotmap(cgi_slotmap,AUTH_TYPE,fgetenv("AUTH_TYPE"));
  init_cgi_slotmap(cgi_slotmap,REMOTE_USER,fgetenv("REMOTE_USER"));
  init_cgi_slotmap(cgi_slotmap,HTTP_COOKIE,fgetenv("HTTP_COOKIE"));
  init_cgi_slotmap(cgi_slotmap,SERVER_HOST,fgetenv("SERVER_HOST"));
  init_cgi_slotmap(cgi_slotmap,HTTP_COOKIE,fgetenv("HTTP_COOKIE"));
  init_cgi_slotmap(cgi_slotmap,HTTPS,fgetenv("HTTPS"));
  
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT,fgetenv("HTTP_ACCEPT"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_CHARSET,
		 fgetenv("HTTP_ACCEPT_CHARSET"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_ENCODING,
		 fgetenv("HTTP_ACCEPT_ENCODING"));
  init_cgi_slotmap(cgi_slotmap,HTTP_ACCEPT_LANGUAGE,
		 fgetenv("HTTP_ACCEPT_LANGUAGE"));
  
  init_cgi_slotmap(cgi_slotmap,REQUEST_METHOD,fgetenv("REQUEST_METHOD"));
  init_cgi_slotmap(cgi_slotmap,CONTENT_TYPE,fgetenv("CONTENT_TYPE"));
  init_cgi_slotmap(cgi_slotmap,CONTENT_LENGTH,fgetenv("CONTENT_LENGTH"));
  if (fake_query_string) setup_fake_query(cgi_slotmap);
  else init_cgi_slotmap(cgi_slotmap,QUERY_STRING,fgetenv("QUERY_STRING"));

  rmethod=fgetenv("REQUEST_METHOD");
  if ((rmethod) && (strcmp(rmethod,"POST") == 0)) {
    int post_size; fd_lisp packet;
    char *size_string=fgetenv("CONTENT_LENGTH"), *post_data;
    post_size=strtol(size_string,NULL,10);
    post_data=fd_malloc(post_size);
    FCGX_GetStr(post_data,post_size,in);
    packet=fd_make_packet(post_size,post_data);
    fd_slotmap_add(cgi_slotmap,fd_make_symbol("POST"),packet);
    fd_decref(packet);}
}

EXPORTED lisp fd_fastcgi_init
  (fd_lispenv env,FCGX_ParamArray fenv,FCGX_Stream *in)
{
  char *cookie, *rmethod, *ctype, *query_data;
  lisp data=fd_make_slotmap(16); fd_slotmap cgi_slotmap;
  set_cgi_data(data); cgi_slotmap=SLOTMAP_PTR(data);

  fastcgi_basic_init(cgi_slotmap,fenv,in);
  fd_warn("fastcgi_basic_init done");
  fdcgi_extended_init(cgi_slotmap);
  fd_warn("fdcgi_extended_init done");

  return data;
}
#endif

static lisp reverse_list(lisp lst)
{
  if (!(PAIRP(lst))) return incref(lst);
  else {
    lisp answer=FD_EMPTY_LIST;
    DOLIST(elt,lst)
      answer=FD_MAKE_PAIR(incref(elt),answer);
    return answer;}
}

static lisp fdxml_cgi_handler(lisp expr,fd_lispenv env)
{
  fd_lisp table=get_cgi_data();
  fd_lisp string_keys=fd_xml_get(expr,string_symbol);
  fd_lisp lisp_keys=fd_xml_get(expr,parsed_symbol);
  {FD_DO_CHOICES(skey,string_keys)
     if (FD_STRINGP(skey)) {
       fd_lisp var=fd_intern(FD_STRING_DATA(skey),FD_STRING_LENGTH(skey));
       fd_lisp val=fd_prim_get(table,var);
       fd_bind_value(var,val,env);
       fd_decref(val);}
   FD_END_DO_CHOICES;}
  {FD_DO_CHOICES(lkey,lisp_keys)
     if (FD_STRINGP(lkey)) {
       fd_lisp var=fd_intern(FD_STRING_DATA(lkey),FD_STRING_LENGTH(lkey));
       fd_lisp val=fd_prim_get(table,var);
       fd_lisp result=FD_EMPTY_CHOICE;
       if (FD_STRINGP(val)) result=fd_parse_string(fd_strdata(val));
       else if (FD_PAIRP(val)) {
	 FD_DOLIST(elt,val) {
	   fd_lisp pelt=
	     ((FD_STRINGP(elt)) ? (fd_parse_string(fd_strdata(elt))) : fd_incref(elt));
	   FD_ADD_TO_CHOICE(result,pelt);}}
       else result=fd_incref(val);
       fd_bind_value(var,result,env);
       fd_decref(val); fd_decref(result);}
   FD_END_DO_CHOICES;}
  fd_decref(string_keys); fd_decref(lisp_keys); fd_decref(table);
  return FD_VOID;
}

static lisp cgi_init_handler(lisp expr,fd_lispenv env)
{
  lisp vars=fd_get_body(expr,1);
  lisp table=get_cgi_data();
  {DOLIST(var,vars)
     if (SYMBOLP(var)) {
       lisp v=fd_prim_get(table,var);
       if (FD_PAIRP(v)) {
	 fd_lisp rv=reverse_list(v);
	 fd_bind_value(var,rv,env);
	 fd_decref(rv);}
       else if (FD_EMPTYP(v))
	 fd_bind_value(var,FD_EMPTY_LIST,env);
       else fd_bind_value(var,v,env);
       decref(v);}}
  fd_decref(table);
  return FD_VOID;
}

static lisp cgi_var_handler(lisp expr,fd_lispenv env)
{
  lisp vars=fd_get_body(expr,1);
  lisp table=get_cgi_data();
  {DOLIST(var,vars)
     if (SYMBOLP(var)) {
       lisp vals=fd_prim_get(table,var);
       if (FD_STRINGP(vals)) {
	 lisp nv=fd_parse_string(FD_STRING_DATA(vals));
	 fd_bind_value(var,nv,env);
	 fd_decref(nv);}
       else if (FD_PAIRP(vals)) {
	 lisp nv=FD_EMPTY_CHOICE;
	 DOLIST(v,vals) {
	   if (FD_STRINGP(v)) {
	     ADD_TO_CHOICE(nv,fd_parse_string(FD_STRING_DATA(v)));}
	   else {ADD_TO_CHOICE(nv,incref(v));}}
	 fd_bind_value(var,nv,env);
	 fd_decref(nv);}
       else fd_bind_value(var,vals,env);
       fd_decref(vals);}}
  fd_decref(table);
  return FD_VOID;
}

static lisp lisp_cgi_data_cproc()
{
  return get_cgi_data();
}

/* Fast CGI output extensions */

#if (HAVE_FASTCGI)
static void fcgi_puts(char *s,fd_htstream *f)
{
  if (f->stream_type == nullstream) {}
  else if (f->stream_type == fcgi) {
    if (FCGX_PutS(s,f->stream.fcgi) < 0) 
      fd_raise_exception("FCGI error");}
  else fd_raise_exception("Weird HTTP stream");
}
static void fcgi_putc(int ch,fd_htstream *f)
{
  if (f->stream_type == nullstream) {}
  else if (f->stream_type == fcgi) {
    if (FCGX_PutChar(ch,f->stream.fcgi) < 0)
      fd_raise_exception("FCGI error");}
  else fd_raise_exception("Weird HTTP stream");
}
static void fcgi_putn(char *s,int n,fd_htstream *f)
{
  if (f->stream_type == nullstream) {}
  else if (f->stream_type == fcgi) {
    if (FCGX_PutStr(s,n,f->stream.fcgi) < 0)
      fd_raise_exception("FCGI error");}
  else fd_raise_exception("Weird HTTP stream");
}
#endif

void init_symbols()
{
  cgi_data_symbol=fd_make_symbol("CGI-DATA");
  content_slotid=fd_make_symbol("CONTENT");
  disp_name_symbol=fd_make_symbol("DISPOSITION.NAME");
  disp_filename_symbol=fd_make_symbol("DISPOSITION.FILENAME");
  query_slotid=fd_make_symbol("QUERY");

  SERVER_SOFTWARE=fd_make_symbol("SERVER_SOFTWARE");
  SERVER_NAME=fd_make_symbol("SERVER_NAME");
  SERVER_PORT=fd_make_symbol("SERVER_PORT");
  SCRIPT_NAME=fd_make_symbol("SCRIPT_NAME");
  SCRIPT_URI=fd_make_symbol("SCRIPT_URI");
  
  HTTP_REFERER=fd_make_symbol("REFERER");
  HTTP_USER_AGENT=fd_make_symbol("USER_AGENT");
  REMOTE_IDENT=fd_make_symbol("REMOTE_IDENT");
  REMOTE_HOST=fd_make_symbol("REMOTE_HOST");
  REMOTE_ADDR=fd_make_symbol("REMOTE_ADDRESS");
  
  PATH_INFO=fd_make_symbol("PATH_INFO");
  DOCUMENT_ROOT=fd_make_symbol("DOCUMENT_ROOT");
  PATH_TRANSLATED=fd_make_symbol("PATH_TRANSLATED");
  
  AUTH_TYPE=fd_make_symbol("AUTH_TYPE");
  REMOTE_USER=fd_make_symbol("REMOTE_USER");
  HTTP_COOKIE=fd_make_symbol("HTTP_COOKIE");
  SERVER_HOST=fd_make_symbol("SERVER_HOST_NAME");
  HTTP_COOKIE=fd_make_symbol("HTTP_COOKIE");
  HTTPS=fd_make_symbol("HTTPS");
  
  HTTP_ACCEPT=fd_make_symbol("HTTP_ACCEPT");
  HTTP_ACCEPT_CHARSET=fd_make_symbol("HTTP_ACCEPT_CHARSET");
  HTTP_ACCEPT_ENCODING=fd_make_symbol("HTTP_ACCEPT_ENCODING");
  HTTP_ACCEPT_LANGUAGE=fd_make_symbol("HTTP_ACCEPT_LANGUAGE");
  
  REQUEST_METHOD=fd_make_symbol("REQUEST_METHOD");
  CONTENT_TYPE=fd_make_symbol("CONTENT_TYPE");
  CONTENT_LENGTH=fd_make_symbol("CONTENT_LENGTH");
  QUERY_STRING=fd_make_symbol("QUERY_STRING");
  POST_DATA=fd_make_symbol("POST_DATA");
  HTTP_SECURE=fd_make_symbol("HTTP-SECURE");

  parsed_symbol=fd_make_symbol("PARSED");
  string_symbol=fd_make_symbol("STRING");
}

void fd_init_cgiparse_c()
{
  fd_lispenv menv=fd_make_module();
  fd_cgiparse_env=menv;

#if FD_THREADS_ENABLED
  fd_new_tld_key(&cgi_data_key,NULL);
#else
  cgi_data=FD_VOID;
#endif

  init_symbols();

  fd_add_cproc(menv,"CGI-DATA",0,lisp_cgi_data_cproc);
  fd_add_special_form(menv,"CGI-INIT",cgi_init_handler);
  fd_add_special_form(menv,"CGI-VAR",cgi_var_handler);

  fd_add_special_form(fd_xmleval_env,"CGI",fdxml_cgi_handler);

#if (HAVE_FASTCGI)
  fd_set_http_output_methods
    ((void (*)(char *,void *))fcgi_puts,
     (void (*)(int,void *))fcgi_putc,
     (void (*)(char *,int,void *))fcgi_putn);
#endif

  fd_register_module("CGITOOLS",menv,FD_SAFE_ENV,0);

  fd_register_source_file("cgiparse",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: cgiparse.c,v $
   Revision 1.39  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.38  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.37  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.36  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.35  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.34  2003/12/03 11:15:00  haase
   Fixed bug with determining connection security and SCRIPT_URI

   Revision 1.33  2003/11/20 15:54:12  haase
   Fixes to fake query handling for fdcgi and fdxml

   Revision 1.32  2003/11/03 00:19:33  haase
   Add correct handling of parsed multiple CGI vars

   Revision 1.31  2003/10/22 22:51:45  haase
   Fixed bug in script URI generation

   Revision 1.30  2003/09/26 16:27:19  haase
   Fixed cgiparse to handle non-strings, typically already parsed

   Revision 1.29  2003/09/07 18:27:40  haase
   Fixed various CGI init bugs (in command-line testing) and added FDXML primitive for CGI binding

   Revision 1.28  2003/09/05 13:13:58  haase
   Fixed GC problems

   Revision 1.27  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.26.2.4  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.26.2.3  2003/08/02 13:47:45  haase
   Made HTTP streams use XFILEs, and added nullstream type for noop streams

   Revision 1.26.2.2  2003/06/29 19:38:54  haase
   Added additional CGI info

   Revision 1.26.2.1  2003/01/26 20:43:54  haase
   Misc. fixes especially some GC

   Revision 1.26  2002/06/18 20:43:10  haase
   Browsed slots now always show glosses

   Revision 1.25  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.24  2002/05/27 18:09:33  haase
   Made CGI-DATA be thread bound

   Revision 1.23  2002/05/26 22:05:41  haase
   Commented out debugging statement

   Revision 1.22  2002/05/26 06:16:34  haase
   Typo fix to fd_byte_data

   Revision 1.21  2002/05/26 05:56:42  haase
   Removed or conditionalized debugging statements

   Revision 1.20  2002/05/26 04:53:16  haase
   Added fdservlet executable and mod_fdserv module

   Revision 1.19  2002/05/19 13:21:18  haase
   Fix typo

   Revision 1.18  2002/05/14 08:54:45  haase
   Added debugging statements to CGI parsing

   Revision 1.17  2002/05/14 07:43:06  haase
   Fix to handle (as before) non mime-encoded posts

   Revision 1.16  2002/05/07 08:02:41  haase
   Updated cgi mime handling for new mime functionality

   Revision 1.15  2002/04/29 13:48:57  haase
   Fixed leak in port argument to PRINTOUT-TO

   Revision 1.14  2002/04/16 16:14:39  haase
   Fixed some inconsistent returns

   Revision 1.13  2002/04/11 00:23:21  haase
   Register the CGI module for external access

   Revision 1.12  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
