/* C Mode */

/* fdcgi.c
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

static char vcid[] = "$Id: fdcgi.c,v 1.20 2005/01/14 16:48:45 haase Exp $";

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
extern fd_lispenv fd_cgiparse_env;
extern void fd_fake_query(char *script_name,char *query_string);

IMPORTED fd_lispenv fd_html_env, fd_xml_env, fd_texttools_env, fd_wwwtools_env;
IMPORTED fd_lispenv fd_osprims_env, fd_fdinternals_env;

static char *request_method_error_message=
  _("The environment variable REQUEST_METHOD is set, so QUERY_STRING is ignored\n");

static int debugging_fastcgi_interaction=0;

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000
#endif

#ifndef SPECIAL_INITS
#define SPECIAL_INITS
#endif


/* Reading the file */

static fd_lisp cgi_exprs;
static fd_lisp cfile_symbol;
static fd_lispenv cgi_env;

static fd_lisp read_first_expr(FILE *f)
{
  char buf[256]; fgets(buf,256,f);
  if ((buf[0] == '#') && (buf[1] == '!'))
    return fd_parse_lisp_from_stream(f);
  else {
    fseek(f,0,SEEK_SET); return fd_parse_lisp_from_stream(f);}
}

static void read_file(char *filename,fd_lispenv env)
{
  FILE *f=fd_fopen(filename,"r");
  fd_lisp expr, exprs=FD_EMPTY_LIST, scan, last, old_fname;
  if (f) {
    FD_UNWIND_PROTECT {
      old_fname=fd_thread_symeval(cfile_symbol);
      expr=fd_make_string(filename);
      fd_thread_symbind(cfile_symbol,expr);
      fd_decref(expr);
      expr=read_first_expr(f);
      while (!(FD_EOF_OBJECTP(expr))) {
	fd_lisp value=fd_eval_in_env(expr,env);
	exprs=FD_MAKE_PAIR(expr,exprs); fd_decref(value);
	expr=fd_parse_lisp_from_stream(f);}}
    FD_ON_UNWIND {
      fd_fclose(f);
      fd_thread_symbind(cfile_symbol,old_fname);
      fd_decref(old_fname);}
    FD_END_UNWIND;
    scan=exprs; last=FD_EMPTY_LIST;
    while (FD_PAIRP(scan)) {
      fd_lisp next=FD_CDR(scan); FD_RPLACD(scan,last);
      last=scan; scan=next;
      if (FD_EMPTY_LISTP(scan)) {
	fd_decref(cgi_exprs); cgi_exprs=last;}}}
  else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
}

/* Standard CGI loop */

static void stdcgi_error
  (fd_u8char *string,fd_exception ex,char *details,fd_lisp obj);

static void do_stdcgi(char *filename,fd_lispenv env)
{
  fd_lisp main_symbol, main_proc;
  struct FD_STRING_STREAM xio;
  FD_WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&xio,1024);
    fd_direct_xio(&xio,NULL,NULL);
    fd_stdcgi_init(env);
    fd_autosync();
    read_file(filename,env);}
  FD_ON_EXCEPTION {
    stdcgi_error
      (xio.ptr,fd_theException(),fd_exception_details(),
       fd_exception_object());
    fd_xfree(xio.ptr);
    fd_reraise();}
  FD_END_HANDLING;
  main_symbol=fd_make_symbol("MAIN");
  main_proc=fd_symeval(main_symbol,env);
  if (FD_VOIDP(main_proc)) {}
  else if (FD_XPROCP(main_proc)) {
    fd_sproc s=FD_GET_SPROC(main_proc);
    FD_DOLIST(expr,fd_cdr_noref(fd_cdr_noref(s->lambda))) {
      fd_lisp value=fd_evaluate(expr,env);
      fd_decref(value);}}
  else {
    fd_lisp value=fd_apply(main_proc,FD_EMPTY_LIST);
    fd_decref(value);}
  fd_decref(main_proc);
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

static int file_read=0;

#if (HAVE_FASTCGI)
static void fastcgi_error
  (FCGX_Stream *out,fd_u8char *xio,fd_exception ex,char *details,fd_lisp obj);

static void do_fastcgi(char *filename,fd_lispenv env)
{
  FCGX_Stream *in, *out, *err;
  FCGX_ParamArray envp;
  int last_mtime=0, code;
  while ((code=FCGX_Accept(&in, &out, &err, &envp)) >= 0) {
    struct FD_HTTP_STREAM hts; struct FD_STRING_STREAM xio; 
    struct stat statb; 
    hts.stream_type=fcgi; hts.is_xml=0; hts.stream.fcgi=out;
    fd_fastcgi_init(env,envp,in);
    fd_autosync();
    fd_start_http_output(&hts);
    /* Get the last mod time for checking on each invocation */
    if ((stat(filename,&statb)) < 0)
      fd_raise_detailed_exception("stat failed",filename);
    if (statb.st_mtime > last_mtime) {
      last_mtime=statb.st_mtime; file_read=0;}
    {FD_WITH_HANDLING {
      int need_eval=1;
      FD_INITIALIZE_STRING_STREAM(&xio,1024);
      fd_direct_xio(&xio,NULL,NULL);
      if (file_read == 0) {
	fd_lisp main_symbol, main_proc;
	/* Read the file */
	read_file(filename,env); file_read=1;
	/* Get the main symbol and figure out the cgi_exprs */
	main_symbol=fd_make_symbol("MAIN");
	main_proc=fd_symeval(main_symbol,env);
	if (FD_XPROCP(main_proc)) {
	  fd_sproc sp=FD_GET_SPROC(main_proc); fd_decref(cgi_exprs);
	  cgi_exprs=fd_cdr_noref(fd_cdr_noref(sp->lambda));
	  fd_incref(cgi_exprs);}
	else need_eval=0;
	fd_decref(main_proc);}
      if (need_eval) {
	FD_WITH_LEXICAL_ENV(request_env,env,8) {
	  FD_DOLIST(expr,cgi_exprs) {
	    fd_lisp value=fd_eval_in_env(expr,env); fd_decref(value);}}
	FD_END_WITH_LEXICAL_ENV_NOVALUE();}}
    FD_ON_EXCEPTION {
      if (debugging_fastcgi_interaction)
	fprintf(stderr,"Exception %s raised\n",fd_theException());
      fastcgi_error
	(out,xio.ptr,fd_theException(),fd_exception_details(),
	 fd_exception_object());
      fastcgi_error
	(err,xio.ptr,fd_theException(),fd_exception_details(),
	 fd_exception_object());
      fd_clear_exception();}
    FD_END_HANDLING;
    fd_direct_xio(NULL,NULL,NULL);
    fd_xfree(xio.ptr);
    if (debugging_fastcgi_interaction)
      fprintf(stderr,"Before FCGX_Finish, pid=%d/%d\n",getpid(),getppid());
    FCGX_Finish();
    if (debugging_fastcgi_interaction)
      fprintf(stderr,"After FCGX_Finish pid=%d/%d\n",getpid(),getppid());}}
  if (debugging_fastcgi_interaction)
    fprintf(stderr,"FCGX_Accept returned %d\n",code);
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

/* Processing config assignments */

static void process_config_assignment(char *start)
{
  fd_lisp symbol, value; int add=0;
  char *equals=strchr(start,'=');
  char *buf=fd_xmalloc(equals-start+1);
  strncpy(buf,start,equals-start); buf[equals-start]=0;
  symbol=fd_make_symbol(buf);
  if (equals[1]=='+') {add=1; equals++;}
  value=fd_parse_arg(equals+1);
  if (add) {
    fd_lisp val=fd_symbol_value(symbol);
    if (FD_VOIDP(val))
      fd_set_symbol_value(symbol,value);
    else {
      FD_ADD_TO_CHOICE(val,fd_incref(value));
      fd_set_symbol_value(symbol,val);
      fd_decref(val);}}
  else fd_set_symbol_value(symbol,value);      
  fd_decref(value);
}


/* The Main Event */

int main(int argc,char *argv[])
{
  fd_lispenv default_env;
  char *fdcgi_file; int i=1;

  if (argc < 2)
    if (getenv("PATH_TRANSLATED"))
      fdcgi_file=fd_strdup(getenv("PATH_TRANSLATED"));
    else {
      fprintf(stderr,"Usage: fdcgi [var=val]+ <filename> [query]\n"); exit(1);}
  else {
    while (i < argc)
      if (strchr(argv[i],'='))
	process_config_assignment(argv[i++]);
      else break;
    fdcgi_file=argv[i];
    if (i+1 < argc) {
      if ((getenv("REQUEST_METHOD")) &&
	  (strcmp(getenv("REQUEST_METHOD"),"GET"))) {
	fprintf(stderr,request_method_error_message);
	exit(1);}
      else fd_fake_query(fdcgi_file,argv[i+1]);}}

  fd_disable_notifications();
  fd_initialize_fdscript();
  fd_set_build_date(__DATE__);
  fd_set_session_mnemonic(fdcgi_file);

  cfile_symbol=cfile_symbol;

  cgi_exprs=FD_EMPTY_LIST;

  default_env=fd_make_module();
  fd_module_uses(default_env,fd_enabled_env);
  fd_module_uses(default_env,fd_osprims_env);
  fd_module_uses(default_env,fd_fdinternals_env);
  fd_module_uses(default_env,fd_module_table);

  fd_initialize_fdtext();
  fd_initialize_fdwww();
  fd_module_uses(default_env,fd_texttools_env);
  fd_module_uses(default_env,fd_wwwtools_env);
  fd_module_uses(default_env,fd_html_env);
  fd_module_uses(default_env,fd_xml_env);

  fd_init_cgiparse_c();  
  fd_module_uses(default_env,fd_cgiparse_env);

  fd_debug_fdscript(1);

  if (getenv("FDCGI_CONFIG")) {
    char *filename=getenv("FDCGI_CONFIG");
    fd_load_config(filename);}
  fd_do_preloads();

#if (HAVE_FASTCGI)
  if (FCGX_IsCGI()) { /* Sometimes this gets set */
    FD_CLEAR_ERR(); do_stdcgi(fdcgi_file,default_env);}
  else do_fastcgi(fdcgi_file,default_env);
#else
  do_stdcgi(fdcgi_file,default_env);
#endif

  fd_exit(0);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdcgi.c,v $
   Revision 1.20  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.19  2004/07/31 14:13:59  haase
   Move around calls to fd_do_preload for greater coverage

   Revision 1.18  2004/07/31 14:08:04  haase
   Added fd_do_preloads after fd_load_configs

   Revision 1.17  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.16  2004/02/14 14:53:22  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.15  2003/12/22 03:20:49  haase
   Update modules for fdtext/fdwww division

   Revision 1.14  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.13  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.12  2003/11/20 15:54:14  haase
   Fixes to fake query handling for fdcgi and fdxml

   Revision 1.11  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.10  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.9.2.1  2003/01/26 20:40:27  haase
   Misc. fixes

   Revision 1.9  2002/05/26 04:53:16  haase
   Added fdservlet executable and mod_fdserv module

   Revision 1.8  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
