/* C Mode */

/* fdservlet.c
   The fdservlet implementation.  This is especialy designed to work
   with the Apache mod_fdserv module, which spawns a version of this program
   for each script it runs.

   Copyright (C) 2002, 2003, 2004 beingmeta, inc. (A Delaware Corporation)

   This program comes with absolutely NO WARRANTY, including implied
   warranties of merchantability or fitness for any particular purpose.

    Use, modification, and redistribution of this program is permitted
    under the terms of the GNU General Public License (GPL) Version 2.

*/

/* Design overview:
     The second argument to fdservlet is a filename to use for a Unix domain socket;
     it listens on this socket for new connection requests.  Whenever one comes through,
     it reads a slotmap off of the connection: this summarizes the request and is passed
      to the code or document fragment specified in the first argument.
*/

static char vcid[] = "$Id: fdservlet.c,v 1.44 2007/06/30 16:21:06 haase Exp $";

#include "framerd/fdscript.h"
#include "framerd/fdwww.h"
#include "framerd/plugins.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>

#ifndef KEEP_LAUNCH_LOG
#define KEEP_LAUNCH_LOG 0
#endif

#define DEBUGGING 0

IMPORTED fd_lispenv fd_html_env, fd_xml_env, fd_texttools_env, fd_wwwtools_env;
IMPORTED fd_lispenv fd_osprims_env, fd_fdinternals_env;
extern fd_lispenv fd_cgiparse_env;

static fd_lisp cgi_data_symbol, xmltag_tag, cfile_symbol, script_filename_slotid;

#define MAX_BACKLOG 10

#if ((defined(PF_UNIX)) && (!(defined(PF_LOCAL))))
#define PF_LOCAL PF_UNIX
#endif

#if ((defined(AF_UNIX)) && (!(defined(AF_LOCAL))))
#define AF_LOCAL AF_UNIX
#endif

#if FD_THREADS_ENABLED
fd_mutex load_lock;
#endif

static void check_main_file(char *filename);

/* Local socket functions */

static int open_file_socket(char *filename)
{
  struct sockaddr_un name;
  int socket_id=socket(PF_LOCAL,SOCK_STREAM,0);
  name.sun_family=AF_LOCAL; strcpy(name.sun_path,filename);
  if (socket_id < 0) {perror("Can't open socket"); return -1;}
  else if (bind(socket_id,(struct sockaddr *)&name,sizeof(struct sockaddr_un)) < 0) {
    perror("Can't bind socket");
    fd_warn("bind failed on %s",filename);
    return -1;}
  else if ((listen(socket_id,MAX_BACKLOG)) < 0) {
    perror("Can't listen on socket"); return -1;}
  else return socket_id;
}

/** Checking memory usage **/

static int memory_base=0;
static fd_lisp memory_headroom_symbol;
#if FD_THREADS_ENABLED
static fd_mutex memcheck_mutex;
#endif

FASTOP unsigned int memory_usage()
{
  return fd_malloc_usage()+fd_cons_usage();
}

#define SWAP_OUTP() \
   ((FD_FIXNUMP(FD_SYMBOL_VALUE(memory_headroom_symbol))) && \
    ((memory_usage()) > \
     ((unsigned int)\
      (memory_base+\
       (FD_FIXLISP(FD_SYMBOL_VALUE(memory_headroom_symbol)))))))


static void swap_out_index(fd_index ix,void *ignore)
{
  fd_swap_out_index(ix);
}

static void swap_everything_out()
{
  fd_swap_out_indices();
  fd_for_indices(swap_out_index,NULL);
  memory_base=memory_usage();
  fd_warn(_("Swapped out everything; memory usage = %d bytes, limit = %d bytes"),
     memory_base,memory_base+
     (FD_FIXLISP(FD_SYMBOL_VALUE(memory_headroom_symbol))));
  fd_report_framerd_stats(stderr);
}

static void check_memory()
{
  fd_lock_mutex(&memcheck_mutex);
  if (SWAP_OUTP()) swap_everything_out();
  fd_unlock_mutex(&memcheck_mutex);
}

/** Work queues **/

#if FD_THREADS_ENABLED

struct WORK_QUEUE {
  fd_mutex lock;
  fd_condvar empty, full;
  int n_tasks, max_tasks; void **tasks;};

static struct WORK_QUEUE *wq=NULL;

static struct WORK_QUEUE *make_work_queue(int max_tasks)
{
  struct WORK_QUEUE *wq=fd_malloc(sizeof(struct WORK_QUEUE));
  fd_init_mutex(&(wq->lock));
  pthread_cond_init(&(wq->empty),NULL);
  pthread_cond_init(&(wq->full),NULL);
  wq->n_tasks=0; wq->max_tasks=max_tasks;
  wq->tasks=fd_malloc(sizeof(void *)*max_tasks);
  return wq;
  
}

static void *pop_task(struct WORK_QUEUE *wq)
{
  void *task;
  check_memory();
  pthread_mutex_lock(&(wq->lock));
  while (wq->n_tasks == 0) 
    pthread_cond_wait(&(wq->empty),&(wq->lock));
  task=wq->tasks[0];
  memmove(&(wq->tasks[0]),&(wq->tasks[1]),sizeof(void *)*(wq->n_tasks-1));
  wq->n_tasks--;
  pthread_mutex_unlock(&(wq->lock));
  return task;
}

static void push_task(struct WORK_QUEUE *wq,void *task)
{
  pthread_mutex_lock(&(wq->lock));
  while (wq->n_tasks == wq->max_tasks) 
    pthread_cond_wait(&(wq->full),&(wq->lock));
  wq->tasks[wq->n_tasks++]=task;
  pthread_cond_signal(&(wq->empty));
  pthread_mutex_unlock(&(wq->lock));
}

#endif

/* Updating the load file */

static fd_lispenv default_env;
static fd_lisp lame_duck_proc;
static char *main_file;

static struct CGI_EXEC_DATA {
  fd_u8char *filename; time_t mod_time;
  fd_lisp cgi_doc, cgi_exprs, cgi_arglist;
  fd_lispenv cgi_env;
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif
  struct CGI_EXEC_DATA *next;} *cgi_exec_data;

static struct FILE_TIMES {
  char *filename; fd_lispenv load_env; time_t mtime;
  struct FILE_TIMES *next;} *filetimes=NULL;

static struct CGI_EXEC_DATA *exec_data;
static int n_cgi_files=0;

static void read_fdxml_file(struct CGI_EXEC_DATA *edata,char *filename,fd_lispenv env);
static void read_fdscript_file(struct CGI_EXEC_DATA *edata,char *filename,fd_lispenv env);

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

static void check_exec_data(struct CGI_EXEC_DATA *edata)
{
  struct stat finfo;
  FD_WITH_MUTEX_LOCKED(&edata->lock) {
    char *filename=edata->filename;
    if ((stat(filename,&finfo) == 0) &&
	(finfo.st_mtime > edata->mod_time)) {
      int len=strlen(filename);
      if (((len>6) && (strcmp(filename+(len-6),".fdcgi") == 0)) ||
	  ((len>4) && (strcmp(filename+(len-4),".fdx") == 0))) {
	default_env=make_load_env();
	read_fdscript_file(edata,filename,default_env);
	edata->mod_time=finfo.st_mtime;}
      else if  (((len>6) && (strcmp(filename+(len-6),".fdxml") == 0)) ||
		((len>7) && (strcmp(filename+(len-7),".fdhtml") == 0))) {
	default_env=fd_make_module();
	read_fdxml_file(edata,filename,default_env);
	edata->mod_time=finfo.st_mtime;}
      else {
	default_env=make_load_env();
	read_fdscript_file(edata,filename,default_env);
	edata->mod_time=finfo.st_mtime;}}}
  FD_END_WITH_MUTEX_LOCKED(&edata->lock);
}

static struct FILE2ENV {
  char *loadpath; fd_lispenv env;
  struct FILE2ENV *next;} *files2envs=NULL;

#if FD_THREADS_ENABLED
static fd_mutex files2envs_lock;
#endif

static void load_for_env(char *loadpath,fd_lispenv env)
{
  struct FILE2ENV *scan;
  fd_lock_mutex(&files2envs_lock);
  scan=files2envs; while (scan)
    if ((strcmp(loadpath,scan->loadpath)) == 0) break;
    else scan=scan->next;
  if (scan) {
    fd_unlock_mutex(&files2envs_lock);
    fd_module_uses(env,scan->env);}
  else {
    scan=fd_malloc(sizeof(struct FILE2ENV));
    scan->loadpath=fd_strdup(loadpath);
    scan->env=make_load_env();
    scan->next=files2envs; files2envs=scan;
    fd_unlock_mutex(&files2envs_lock);
    fd_file_reloader(loadpath,NULL,scan->env);
    fd_module_uses(env,scan->env);}
}

/* Lookup up exec data */

#if FD_THREADS_ENABLED
static fd_mutex exec_data_lock;
#endif

static struct CGI_EXEC_DATA *get_exec_data(char *filename)
{
  struct CGI_EXEC_DATA *result=NULL;
  FD_WITH_MUTEX_LOCKED(&exec_data_lock) {
    struct CGI_EXEC_DATA *scan=cgi_exec_data, **ptr=&cgi_exec_data;
    while (scan)
      if (strcmp(scan->filename,filename) == 0) break;
      else {ptr=&(scan->next); scan=scan->next;}
    if (scan) result=scan;
    else {
      result=fd_malloc(sizeof(struct CGI_EXEC_DATA));
      result->filename=fd_strdup(filename); result->mod_time=0;
      result->cgi_exprs=FD_VOID; result->cgi_arglist=FD_VOID;
      result->cgi_doc=FD_VOID; result->cgi_env=NULL;
      fd_init_mutex(&result->lock); result->next=NULL;
      *ptr=result;}}
  FD_END_WITH_MUTEX_LOCKED(&exec_data_lock);
  return result;
}


/* Reading FDXML */

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
  else if (*sval == '/') fd_load_config(sval); 
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

static fd_lisp process_fdxml_pi(char *filename,fd_lisp content,fd_lispenv env)
{
  if (FD_PAIRP(content))
    if (framerd_pi_p(content)) {
      handle_framerd_pi(filename,FD_CAR(FD_CDR(content)),env);
      return FD_VOID;}
    else {
      fd_lisp copy, *tail=&copy;
      FD_DOLIST(elt,content) {
	fd_lisp copied_elt=process_fdxml_pi(filename,elt,env);
	if (FD_VOIDP(copied_elt)) {}
	else {
	  *tail=FD_MAKE_PAIR(copied_elt,FD_EMPTY_LIST);
	  tail=&(FD_CDR(*tail));}}
      return copy;}
  else return fd_incref(content);
}

static void read_fdxml_file(struct CGI_EXEC_DATA *edata,char *filename,fd_lispenv env)
{
  fd_lisp parsed_content, converted_content;
  fd_u8char *string=fd_filestring(filename), *scan=string;
  fd_warn("Reading fdxml file %s",filename);
  if ((string[0] == '#') && (string[1] == '!'))
    scan=strchr(string,'\n');
  parsed_content=fd_parse_html(scan);
  converted_content=process_fdxml_pi(filename,parsed_content,env);
  fd_decref(parsed_content); fd_xfree(string);
  fd_decref(edata->cgi_doc); edata->cgi_doc=converted_content;
  fd_decref(edata->cgi_exprs); edata->cgi_exprs=FD_VOID;
  fd_decref(edata->cgi_arglist); edata->cgi_exprs=FD_VOID;
  fd_free_env(edata->cgi_env); edata->cgi_env=fd_mallocd_env(env);
}

/* Reading FDScript file */

static fd_lisp read_first_expr(FILE *f)
{
  char buf[256]; fgets(buf,256,f);
  if ((buf[0] == '#') && (buf[1] == '!'))
    return fd_parse_lisp_from_stream(f);
  else {
    fseek(f,0,SEEK_SET); return fd_parse_lisp_from_stream(f);}
}

static void read_fdscript_file(struct CGI_EXEC_DATA *edata,char *filename,fd_lispenv env)
{
  FILE *f=fd_fopen(filename,"r");
  fd_lisp expr, exprs=FD_EMPTY_LIST, scan, last, old_fname;
  fd_warn("Reading fdscript file %s",filename);
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
    /* Reverse the list in place */
    while (FD_PAIRP(scan)) {
      fd_lisp next=FD_CDR(scan); FD_RPLACD(scan,last);
      last=scan; scan=next;
      if (FD_EMPTY_LISTP(scan)) {
	fd_decref(edata->cgi_exprs); edata->cgi_exprs=last;}}}
  else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
  {
    fd_lisp main_proc=fd_symeval(fd_make_symbol("MAIN"),env);
    if (FD_VOIDP(main_proc)) {
      fd_free_env(edata->cgi_env); fd_decref(edata->cgi_arglist);
      edata->cgi_env=fd_mallocd_env(env); edata->cgi_arglist=FD_EMPTY_LIST;}
    else {
      fd_sproc s=FD_GET_SPROC(main_proc);
      fd_free_env(edata->cgi_env); edata->cgi_env=fd_mallocd_env(s->env);
      fd_decref(edata->cgi_arglist); edata->cgi_arglist=fd_car(FD_CDR(s->lambda));
      fd_decref(edata->cgi_exprs); edata->cgi_exprs=fd_cdr(FD_CDR(s->lambda));}
    lame_duck_proc=fd_symeval(fd_make_symbol("LAME-DUCK"),env);}
  fd_decref(edata->cgi_doc); edata->cgi_doc=FD_VOID;
}

/* Responding to requests */

static void send_error_report
  (struct FD_HTTP_STREAM *hts,fd_u8char *xio,
   fd_exception ex,fd_u8char *details,fd_lisp obj);
static void cgi_eval
   (struct FD_HTTP_STREAM *hts,fd_lisp cgi_data,
    fd_lisp arglist,fd_lisp exprs,fd_lispenv env);
static void cgi_fdxml
   (struct FD_HTTP_STREAM *hts,fd_lisp cgi_data,
    fd_lisp cgi_doc,fd_lispenv request_env);

#if DEBUGGING
#define SERVLET_STREAM_TYPE sstream
#else
#define SERVLET_STREAM_TYPE stdio
#endif

static fd_u8char *get_script_filename(fd_lisp cgi_data)
{
  fd_lisp v=fd_frame_get(cgi_data,script_filename_slotid);
  if (FD_STRINGP(v)) {
    fd_u8char *copy=fd_strdup(FD_STRING_DATA(v)); fd_decref(v);
    return copy;}
  else {fd_decref(v); return NULL;}
}

static void cgi_main(fd_lisp cgi_data,FILE *f)
{
  struct FD_HTTP_STREAM hts; 
  struct FD_STRING_STREAM xio, ss;
  fd_u8char *filename=get_script_filename(cgi_data);
#if DEBUGGING
  FD_INITIALIZE_STRING_STREAM(&ss,1024);
  hts.stream_type=sstream; hts.is_xml=1; hts.stream.sstream=&ss;
#else
  hts.stream_type=stdio; hts.is_xml=1;
  fd_init_xfile(&(hts.stream.xfile),f,fd_get_default_encoding());
#endif
  fd_start_http_output(&hts);
  {FD_WITH_HANDLING {
    struct CGI_EXEC_DATA *edata;
    FD_INITIALIZE_STRING_STREAM(&xio,1024);
    fd_direct_xio(&xio,NULL,NULL);
    edata=get_exec_data(filename);
    check_exec_data(edata);
    fd_reloader();
    fd_set_cgi_data(cgi_data);
    fd_set_http_phase(http_any);
    fd_autosync();
    if (FD_VOIDP(edata->cgi_doc))
      cgi_eval(&hts,cgi_data,edata->cgi_arglist,edata->cgi_exprs,edata->cgi_env);
    else cgi_fdxml(&hts,cgi_data,edata->cgi_doc,edata->cgi_env);
    fd_set_cgi_data(FD_VOID);
#if DEBUGGING
    fd_warn("Wrote %d bytes: %s",ss.size,ss.ptr);
    fwrite(ss.ptr,1,ss.size,f);
    fd_xfree(ss.ptr);
#endif
  }
  FD_ON_EXCEPTION {
    send_error_report
      (&hts,xio.ptr,fd_theException(),fd_exception_details(),fd_exception_object());
    fd_clear_exception();}
  FD_END_HANDLING;
  fd_xfree(filename);
  fd_xfree(xio.ptr);}
}

static void cgi_eval
  (struct FD_HTTP_STREAM *hts,fd_lisp cgi_data,fd_lisp arglist,fd_lisp exprs,fd_lispenv cgi_env)
{    
  FD_WITH_LEXICAL_ENV(env,cgi_env,8) {
    fd_lisp scan=arglist;
    while (FD_PAIRP(scan))
      if (FD_SYMBOLP(FD_CAR(scan))) {
	fd_lisp val=fd_prim_get(cgi_data,FD_CAR(scan));
	fd_bind_value(FD_CAR(scan),val,env);
	fd_decref(val); scan=FD_CDR(scan);}
      else if ((FD_PAIRP(FD_CAR(scan))) &&
	       (FD_SYMBOLP(FD_CAR(FD_CAR(scan)))) &&
	       (FD_PAIRP(FD_CDR(FD_CAR(scan))))) {
	fd_lisp val=fd_prim_get(cgi_data,FD_CAR(FD_CAR(scan)));
	if (FD_EMPTYP(val))
	  fd_bind_value(FD_CAR(FD_CAR(scan)),FD_CAR(FD_CDR(FD_CAR(scan))),env);
	else {
	  fd_bind_value(FD_CAR(FD_CAR(scan)),val,env); fd_decref(val);}
	scan=FD_CDR(scan);}
    {FD_DOLIST(expr,exprs) {
      fd_lisp result=fd_eval_in_env(expr,env); fd_decref(result);}}}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
}

static void cgi_fdxml(struct FD_HTTP_STREAM *hts,fd_lisp cgi_data,fd_lisp cgi_doc,fd_lispenv cgi_env)
{
  FD_WITH_LEXICAL_ENV(env,cgi_env,8) {
    fd_start_http("text/html");
    fd_unparse_xml(cgi_doc,env,hts);}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
}

static void send_error_report
  (struct FD_HTTP_STREAM *hts,fd_u8char *xio,
   fd_exception ex,fd_u8char *details,fd_lisp obj)
{
  struct FD_STRING_STREAM ss;
  FD_INITIALIZE_STRING_STREAM(&ss,8192);
  fd_printf(&ss,"Content-Type: text/html; charset=utf8;\r\n\r\n");
  fd_printf(&ss,"<HTML>\n<HEAD><TITLE>%s (%s)</TITLE></HEAD>\n<BODY>\n",
	    fd_theException(),fd_exception_details());
  fd_printf(&ss,"<H1>%s (%s)</H1>\n",fd_theException(),fd_exception_details());
  fd_printf(&ss,"<H2>%s</H2>\n",fd_session_id());
  if (!(FD_VOIDP(obj))) fd_printf(&ss,"<PRE>%Q</PRE>\n",obj);
  fd_http_puts(ss.ptr,hts);
  fd_http_puts("<PRE>\n",hts);
  fd_http_puts(xio,hts); fd_http_puts("\n</PRE>\n",hts); 
  fd_http_puts("</BODY>\n</HTML>",hts);
}

/* This function runs in each thread. */

#if FD_THREADS_ENABLED
static void *serve_client_loop(void *thread_arg)
{
  struct WORK_QUEUE *wq=thread_arg;
  while (1) {
    int socket=(long)pop_task(wq);
#if DEBUGGING
    fd_warn("Serving request on socket %d",socket);
#endif
    if (socket >= 0) {
      FILE *f=fdopen(socket,"r+b");
      fd_lisp cgi_data=fd_fread_dtype(f), args, result;
      errno=0;
#if DEBUGGING
      fd_fprintf(stderr,"CGI-DATA=%q\n",cgi_data);
#endif
      fdcgi_extended_init(FD_SLOTMAP_PTR(cgi_data));
#if DEBUGGING
      fd_fprintf(stderr,"XCGI-DATA=%q\n",cgi_data);
#endif
      /* fd_fprintf(stderr,"RELOAD-DONE\n",cgi_data); */
      cgi_main(cgi_data,f);
      fd_decref(cgi_data);
      fclose(f);
      close(socket);}}
}

static pthread_t *start_thread_pool(struct WORK_QUEUE *wq,int n)
{
  int i=0;
  pthread_t *thread_pool=fd_malloc(sizeof(pthread_t)*n);
  while (i < n) {
    pthread_create(&thread_pool[i],
		   pthread_attr_default,serve_client_loop,(void *)wq);
    i++;}
  return thread_pool;
}

#endif

static void non_threaded_serve_socket(int socket)
{
  FILE *f=fdopen(socket,"r+b");
  fd_lisp cgi_data=fd_fread_dtype(f), args, result;
  check_memory(); errno=0;
#if DEBUGGING
  fd_fprintf(stderr,"CGI-DATA=%q\n",cgi_data);
#endif
  fdcgi_extended_init(FD_SLOTMAP_PTR(cgi_data));
#if DEBUGGING
  fd_fprintf(stderr,"XCGI-DATA=%q\n",cgi_data);
#endif
  /* fd_fprintf(stderr,"RELOAD-DONE\n",cgi_data); */
  cgi_main(cgi_data,f);
  fd_decref(cgi_data);
  fclose(f);
  close(socket);
}

/* The main event */

static char *server_file;
static int server_socket;
static int quit_server=0;

static void close_server_socket()
{
  close(server_socket);
  remove(server_file);
}

static void quit_awkwardly(int sig)
{
  exit(1);
}

static void quit_gracefully(int sig)
{
  /* Tell the loop to quit and stop taking new requests */
  quit_server=1; close(server_socket); remove(server_file);
  if (!(FD_VOIDP(lame_duck_proc)))
    fd_apply(lame_duck_proc,FD_EMPTY_LIST);
  /* Take 60 seconds for pending requests to clear
     before terminating radically */
  alarm(60); signal(SIGALRM,quit_awkwardly);
}

int main(int argc,char *argv[])
{
#if FD_THREADS_ENABLED
  pthread_t *thread_pool; int n_threads;
#endif

  fd_disable_notifications();
  fd_initialize_fdscript();
  fd_set_build_date(__DATE__);
  
#if FD_THREADS_ENABLED
  n_threads=fd_int_getenv("FDSERVLET_THREADS",5);
  fd_init_mutex(&load_lock);
  fd_init_mutex(&files2envs_lock);
  fd_init_mutex(&memcheck_mutex);
#endif

#if KEEP_LAUNCH_LOG
 {
   FILE *f=fopen("/tmp/fdservlet.log","a"); time_t now=time(NULL);
   fprintf(f,"fdservlet %s %s %s",argv[0],argv[1],ctime(&now));
   fclose(f);
 }
#endif

 fd_do_preloads();

  lame_duck_proc=FD_VOID;
  main_file=fd_strdup(argv[1]);

  xmltag_tag=fd_make_symbol("XMLTAG");
  cgi_data_symbol=fd_make_symbol("CGI-DATA");
  load_symbol=fd_make_symbol("LOAD");
  module_symbol=fd_make_symbol("MODULE");
  pool_symbol=fd_make_symbol("POOL");
  index_symbol=fd_make_symbol("INDEX");
  qmark_symbol=fd_make_symbol("?");
  memory_headroom_symbol=fd_make_symbol("MEMORY-HEADROOM");
  cfile_symbol=fd_make_symbol("*CURRENT-FILE*");
  script_filename_slotid=fd_make_symbol("SCRIPT_FILENAME");

  fd_initialize_fdtext();
  fd_initialize_fdwww();
  fd_init_cgiparse_c();  

  fd_debug_fdscript(1);

#if FD_THREADS_ENABLED
  if (argc > 3) n_threads=strtol(argv[3],NULL,10);
  wq=make_work_queue(256);
  thread_pool=start_thread_pool(wq,n_threads);  
#endif

  if (argc == 3) server_file=fd_strdup(argv[2]);
  else server_file=fd_strdup(argv[1]);
  server_socket=open_file_socket(server_file);
  if (server_socket>=0)
    if (getenv("PROMISCUOUS_FDSERVLET"))
      if (chmod(server_file,0777))
	perror("chmod server socket");
  if (argc == 3) get_exec_data(argv[2]);
  atexit(close_server_socket);
#ifdef SIGPIPE
  signal(SIGPIPE,SIG_IGN);
#endif
#ifdef SIGHUP
  signal(SIGHUP,quit_gracefully);
#endif
#ifdef SIGTERM
  signal(SIGTERM,quit_gracefully);
#endif
#ifdef SIGQUIT
  signal(SIGQUIT,quit_gracefully);
#endif

  memory_base=memory_usage();

  fd_notify("server socket is %d for %s",server_socket,argv[2]);
  
  {
    int client_socket=-1, addr_len=sizeof(struct sockaddr);
    struct sockaddr addr; errno=0;
    while (((client_socket=accept(server_socket,NULL,&addr_len)) >= 0) &&
	   (quit_server == 0)) {
#if FD_THREADS_ENABLED
      push_task(wq,(void *)((fd_intptr)client_socket));
#else
      non_threaded_serve_socket(client_socket);
#endif
    }}
  
  return 0;
}
