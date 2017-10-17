/* C Mode */

/* fdxml.c
   The top level FDScript interpreter for FramerD
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

static char vcid[] = "$Id: fdxml.c,v 1.31 2005/01/14 16:48:45 haase Exp $";

#include "framerd/fdscript.h"
#include "framerd/fdwww.h"
#undef INIT_FDTEXT
#define INIT_FDTEXT 0
#undef INIT_FDWWW
#define INIT_FDWWW 0
#include "framerd/plugins.h"
#include "sys/stat.h"
#include "time.h"

#if (HAVE_FASTCGI)
extern fd_lisp fd_fastcgi_init
  (fd_lispenv env,FCGX_ParamArray fenv,FCGX_Stream *in);
#endif
FDSCRIPT_EXPORT void fd_initialize_fdtext(void);
extern void fd_init_cgiparse_c(void);
extern fd_lisp fd_stdcgi_init(fd_lispenv env);
extern void fd_fake_query(char *script_name,char *query_string);
extern fd_lispenv fd_cgiparse_env;

static fd_lisp xmltag_tag;

IMPORTED fd_lispenv fd_html_env, fd_xml_env, fd_texttools_env, fd_wwwtools_env;
IMPORTED fd_lispenv fd_osprims_env, fd_fdinternals_env;

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000
#endif

#ifndef SPECIAL_INITS
#define SPECIAL_INITS
#endif


/* Loading files */

/* This is a simplified version of what is in fdservlet in order to
   keep the code the same. */

static fd_lispenv make_load_env()
{
  fd_lispenv load_env=fd_make_module();
  fd_module_uses(load_env,fd_enabled_env);
  fd_module_uses(load_env,fd_osprims_env);
  fd_module_uses(load_env,fd_fdinternals_env);
  fd_module_uses(load_env,fd_wwwtools_env);
  fd_module_uses(load_env,fd_texttools_env);
  fd_module_uses(load_env,fd_html_env);
  fd_module_uses(load_env,fd_xml_env);
  fd_module_uses(load_env,fd_cgiparse_env);

  return load_env;
}

static void load_for_env(char *loadpath,fd_lispenv env)
{
  fd_lispenv new_env=make_load_env();
  fd_load_file(loadpath,NULL,new_env);
  fd_module_uses(env,new_env);
}


/* Reading the file */

static fd_lisp module_symbol, load_symbol;
static fd_lisp pool_symbol, index_symbol, qmark_symbol;

static fd_lisp handle_module_pi(fd_lisp module,fd_lispenv env)
{
  fd_u8char *mname, *fname;
  fd_lispenv menv=NULL;
  if (FD_STRINGP(module)) mname=FD_STRING_DATA(module);
  else if (FD_SYMBOLP(module)) mname=FD_SYMBOL_NAME(module);
  else if (FD_PRIM_TYPEP(module,env_type)) menv=FD_CPTR_DATA(module);
  else fd_type_error("can't be a module specifier",module);
  if (menv == NULL) menv=fd_get_module(mname);
  if (menv) fd_module_uses(env,menv);
  else fd_type_error("not a module",module);
  return FD_VOID;
}

static void handle_pool_pi(char *sfile,fd_lisp val)
{
  fd_u8char *sval=fd_strdata(val);
  if (strchr(sval,'@')) fd_use_pool(sval);
  else {
    char *abspath=fd_make_absolute_pathname(sval,sfile);
    fd_use_pool(abspath); fd_xfree(abspath);}
}

static void handle_index_pi(char *sfile,fd_lisp val)
{
  fd_lisp lix; fd_u8char *sval=fd_strdata(val);
  fd_lisp bground_symbol=fd_make_symbol("%BACKGROUND");
  fd_lisp bground=fd_symbol_value(bground_symbol);
  if (strchr(sval,'@')) {
    fd_index ix=fd_open_index(sval);
    lix=fd_make_cptr(index_type,ix);}
  else {
    char *abspath=fd_make_absolute_pathname(sval,sfile);
    fd_index ix=fd_open_index(abspath);
    fd_xfree(abspath);
    lix=fd_make_cptr(index_type,ix);}
  if (!(fd_choice_containsp(lix,bground))) {
    FD_ADD_TO_CHOICE(bground,fd_incref(lix));
    fd_set_symbol_value(bground_symbol,bground);}
  fd_decref(bground);
}

static void handle_config_pi(char *sfile,fd_lisp val)
{
  fd_u8char *sval=fd_strdata(val);
  if (strchr(sval,'=')) {
    fd_u8char *equals=strchr(sval,'='); int eqpos=(equals-sval);
    fd_u8char *symbol_name=fd_malloc(1+eqpos);
    fd_lisp symbol, value;
    strncpy(symbol_name,sval,eqpos); symbol_name[eqpos]='\0';
    symbol=fd_make_symbol(symbol_name); value=fd_parse_string(equals+1);
    FD_SET_SYMBOL_VALUE(symbol,value);
    fd_decref(value); fd_free(symbol_name,1+eqpos);}
  else if (*sval == '/') {
    fd_load_config(sval);}
  else {
    char *config_path=fd_make_absolute_pathname(sval,sfile);
    fd_load_config(config_path);
    fd_xfree(config_path);}
  fd_do_preloads();
}

static void handle_load_pi(char *sfile,fd_lisp val,fd_lispenv env)
{
  fd_u8char *name=fd_strdata(val), *loadpath;
  if (*name == '/') loadpath=fd_strdup(name);
  else {
    char *loadpath=fd_make_absolute_pathname(name,sfile);
    load_for_env(loadpath,env);
    fd_xfree(loadpath);}
}

static int tag_matchp(fd_lisp tag,fd_u8char *name)
{
  if (FD_SYMBOLP(tag))
    return (strcmp(FD_SYMBOL_NAME(tag),name) == 0);
  else if (FD_LRECORD_TYPEP(tag,xmltag_tag)) {
    fd_lisp tagname=fd_xmltag_name(tag);
    if (FD_SYMBOLP(tagname))
      return (strcmp(FD_SYMBOL_NAME(tagname),name) == 0);
    else if (FD_STRINGP(tagname))
      return (strcmp(FD_STRING_DATA(tagname),name) == 0);
    else return 0;}
  else return 0;
}

static void handle_framerd_pi(char *filename,fd_lisp attributes,fd_lispenv env)
{
  FD_DOLIST(attr,attributes)
    if (!(FD_PAIRP(attr))) {}
    else {
      fd_lisp iname=FD_CAR(attr);
      fd_lisp arg=fd_get_arg(attr,1,FD_VOID), val=FD_VOID;
      if (FD_STRINGP(arg)) val=fd_getenv(FD_STRING_DATA(arg));
      if ((FD_VOIDP(val)) || (FD_EMPTYP(val))) val=arg;
      if (tag_matchp(iname,"pool")) handle_pool_pi(filename,val);
      else if (tag_matchp(iname,"index")) handle_index_pi(filename,val);
      else if (tag_matchp(iname,"load")) handle_load_pi(filename,val,env);
      else if (tag_matchp(iname,"module")) handle_module_pi(val,env);
      else if (tag_matchp(iname,"config")) handle_config_pi(filename,val);
      else if (tag_matchp(iname,"debug")) {
	fd_lisp sym=fd_make_symbol("%DEBUG");
	FD_SET_SYMBOL_VALUE(sym,FD_TRUE);}
      else fd_warn("Unknown framerd processing instruction %q",attr);}
}

static int framerd_pi_p(fd_lisp x)
{
  if ((FD_PAIRP(x)) && (FD_LISP_EQ(FD_CAR(x),qmark_symbol))) {
    fd_lisp cadr=fd_get_arg(x,1,FD_FALSE);
    fd_lisp caadr=fd_get_arg(cadr,0,FD_FALSE);
    if ((FD_STRINGP(caadr)) &&
	(strcasecmp(FD_STRING_DATA(caadr),"fdxml") == 0))
      return 1;
    else return 0;}
  else return 0;
}

static fd_lisp process_fdxml_pis(char *filename,fd_lisp content,fd_lispenv env)
{
  if (FD_PAIRP(content))
    if (framerd_pi_p(content)) {
      handle_framerd_pi(filename,FD_CAR(FD_CDR(content)),env);
      return FD_VOID;}
    else {
      fd_lisp copy, *tail=&copy;
      FD_DOLIST(elt,content) {
	fd_lisp copied_elt=process_fdxml_pis(filename,elt,env);
	if (FD_VOIDP(copied_elt)) {}
	else {
	  *tail=FD_MAKE_PAIR(copied_elt,FD_EMPTY_LIST);
	  tail=&(FD_CDR(*tail));}}
      return copy;}
  else return fd_incref(content);
}

static fd_lisp read_file(char *filename,fd_lispenv env)
{
  fd_lisp parsed_content, converted_content;
  fd_u8char *string=fd_filestring(filename), *scan=string;
  if ((string[0] == '#') && (string[1] == '!'))
    scan=strchr(string,'\n');
  /* Read parsed content */
  parsed_content=fd_parse_html(scan);
  /* Convert content, e.g. process and remove FDXML PIs */
  converted_content=process_fdxml_pis(filename,parsed_content,env);
  /* Clean up */
  fd_decref(parsed_content); fd_xfree(string);
  /* fd_pprint_lisp(converted_content,stderr,80); */
  return converted_content;
}

/* Standard CGI loop */

static void stdcgi_error
  (fd_u8char *string,fd_exception ex,char *details,fd_lisp obj);

static void do_stdcgi(char *filename,fd_lispenv env)
{
  struct FD_STRING_STREAM xio; fd_lisp content=FD_VOID;
  FD_WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&xio,1024);
    fd_direct_xio(&xio,NULL,NULL);
    fd_stdcgi_init(env);
    fd_autosync();
    content=read_file(filename,env);}
  FD_ON_EXCEPTION {
    stdcgi_error
      (xio.ptr,fd_theException(),fd_exception_details(),
       fd_exception_object());
    fd_xfree(xio.ptr);
    fd_reraise();}
  FD_END_HANDLING;
  fd_start_http("text/html");
  fd_set_http_phase(xml_content);
  fd_unparse_xml(content,env,NULL);
}

static void stdcgi_error
  (fd_u8char *xio,fd_exception ex,char *details,fd_lisp obj)
{
  char *objstring=NULL;
  if (!(FD_VOIDP(obj))) {
    objstring=fd_object_to_string(obj);}
  printf("Content-Type: text/html; charset=utf8;\n\n");
  printf("<HEAD><TITLE>%s (%s)</TITLE></HEAD>\n<BODY>\n",
	 fd_theException(),fd_exception_details());
  printf("<H1>%s (%s)</H1>\n",fd_theException(),fd_exception_details());
  if (objstring) printf("<H2>%s</H2>\n",objstring);
  printf("<H2>%s</H2>\n",fd_session_id());
  printf("<PRE>\n%s\n</PRE></BODY>\n",xio);
}


/* Fast CGI stuff */

#if (HAVE_FASTCGI)
static void fastcgi_error
  (FCGX_Stream *out,fd_u8char *xio,fd_exception ex,char *details,fd_lisp obj);

static void do_fastcgi(char *filename,fd_lispenv env)
{
  FCGX_Stream *in, *out, *err;
  FCGX_ParamArray envp;
  int last_mtime=0, file_read=0;
  fd_lisp content=FD_VOID;
  while (FCGX_Accept(&in, &out, &err, &envp) >= 0) {
    struct FD_HTTP_STREAM hts; struct FD_STRING_STREAM xio; 
    struct stat statb; 
    hts.stream_type=fcgi; hts.is_xml=1; hts.stream.fcgi=out;
    fd_fastcgi_init(env,envp,in);
    fd_autosync();
    fd_start_http_output(&hts);
    /* Get the last mod time for checking on each invocation */
    if ((stat(filename,&statb)) < 0)
      fd_warn("stat failed",filename);
    else if (statb.st_mtime > last_mtime) {
      last_mtime=statb.st_mtime; file_read=0;}
    else reload_changed_files(env);
    {FD_WITH_HANDLING {
      int need_eval=1;
      FD_INITIALIZE_STRING_STREAM(&xio,1024);
      fd_direct_xio(&xio,NULL,NULL);
      if (file_read == 0) {
	/* Read (or re-read) the file */
	fd_decref(content);
	content=read_file(filename,env);
	file_read=1;}
      fd_start_http("text/html");
      fd_unparse_xml(content,env,&hts);}
    FD_ON_EXCEPTION {
      fastcgi_error
	(out,xio.ptr,fd_theException(),fd_exception_details(),
	 fd_exception_object());
      fastcgi_error
	(err,xio.ptr,fd_theException(),fd_exception_details(),
	 fd_exception_object());
      fd_clear_exception();}
    FD_END_HANDLING;
    fd_direct_xio(NULL,NULL,NULL);
    fd_xfree(xio.ptr); FCGX_Finish();}}
}

static void fastcgi_error
  (FCGX_Stream *out,fd_u8char *xio,fd_exception ex,char *details,fd_lisp obj)
{
  char *objstring=NULL;
  if (!(FD_VOIDP(obj))) objstring=fd_object_to_string(obj);
  FCGX_FPrintF(out,"Content-Type: text/html; charset=utf8;\n\n");
  FCGX_FPrintF(out,"<HEAD><TITLE>%s (%s)</TITLE></HEAD>\n<BODY>\n",
	       fd_theException(),fd_exception_details());
  FCGX_FPrintF(out,"<H1>%s (%s)</H1>\n",
	       fd_theException(),fd_exception_details());
  if (objstring) FCGX_FPrintF(out,"<H2>%s</H2>\n",objstring);
  FCGX_FPrintF(out,"<H2>%s</H2>\n",fd_session_id());
  FCGX_FPrintF(out,"<PRE>\n%s\n</PRE></BODY>\n",xio);
}
#endif


/* The Main Event */

int main(int argc,char *argv[])
{
  fd_lispenv default_env;
  char *fdcgi_file;

  if (argc < 2)
    if (getenv("PATH_TRANSLATED"))
      fdcgi_file=fd_strdup(getenv("PATH_TRANSLATED"));
    else {
      fprintf(stderr,"Usage: fdxml <filename> [query]\n"); exit(1);}
  else fdcgi_file=argv[1];
  if (argc == 3) fd_fake_query(argv[1],argv[2]);
  fd_disable_notifications();
  fd_initialize_fdscript();
  fd_set_build_date(__DATE__);
  fd_set_session_mnemonic(fdcgi_file);

  xmltag_tag=fd_make_symbol("XMLTAG");
  load_symbol=fd_make_symbol("LOAD");
  module_symbol=fd_make_symbol("MODULE");
  pool_symbol=fd_make_symbol("POOL");
  index_symbol=fd_make_symbol("INDEX");
  qmark_symbol=fd_make_symbol("?");

  default_env=fd_make_module();

  fd_init_cgiparse_c();  
  fd_initialize_fdtext();
  fd_initialize_fdwww();

  fd_debug_fdscript(1);

  if (getenv("FDCGI_CONFIG")) {
    char *filename=getenv("FDCGI_CONFIG");
    fd_load_config(filename);}
  fd_do_preloads();

#if (HAVE_FASTCGI)
  if (FCGX_IsCGI()) { /* Sometimes this gets set */
    FD_CLEAR_ERR(); do_stdcgi(fdcgi_file,default_env,load_env);}
  else do_fastcgi(fdcgi_file,default_env);
#else
  do_stdcgi(fdcgi_file,default_env);
#endif

  fd_exit(0);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdxml.c,v $
   Revision 1.31  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.30  2004/07/31 14:13:59  haase
   Move around calls to fd_do_preload for greater coverage

   Revision 1.29  2004/07/31 14:08:04  haase
   Added fd_do_preloads after fd_load_configs

   Revision 1.28  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.27  2004/03/10 16:40:22  haase
   Made fdxml and fdservlet do relative interpretation of args to XML processing instructions

   Revision 1.26  2004/02/14 14:53:22  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.25  2003/12/22 03:20:49  haase
   Update modules for fdtext/fdwww division

   Revision 1.24  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.23  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.22  2003/11/23 16:54:55  haase
   Added config setting options to fdxml/fdservlet

   Revision 1.21  2003/11/20 15:54:14  haase
   Fixes to fake query handling for fdcgi and fdxml

   Revision 1.20  2003/11/03 00:22:55  haase
   Added fdxml config PI

   Revision 1.19  2003/10/28 18:25:16  haase
   Added debug option for fdxml processing instruction

   Revision 1.18  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.17  2003/09/26 16:28:40  haase
   Made the load processing instruction load files into their own environment and then use the environment

   Revision 1.16  2003/09/20 18:04:42  haase
   Fixes and updates to fdxml and htmlgen

   Revision 1.15  2003/09/09 01:12:28  haase
   Removed use of xmleval module by scripts, just used as default for xml callouts

   Revision 1.14  2003/09/09 00:33:17  haase
   Whitespace changes

   Revision 1.13  2003/09/07 18:28:37  haase
   Fixed some minor bugs, added access to xmleval environment

   Revision 1.12  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11  2002/06/24 17:31:30  haase
   Fixes to fdxml and fdservlet to handle new XML tags

   Revision 1.10  2002/05/27 00:57:48  haase
   Removed fdxml trace statement

   Revision 1.9  2002/04/16 16:14:35  haase
   Fixed some inconsistent returns

   Revision 1.8  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
