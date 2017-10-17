/* C Mode */

/* load.c
   Support for loading Scheme files
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

static char vcid[] = "$Id: load.c,v 1.34 2005/01/14 16:48:44 haase Exp $";

/** Recording source files **/
/** File Loading **/
/** Evaluating things just once **/
/** Loading components **/
/** Initialization **/

#include "fdeval.h"
#include "sys/stat.h"

static fd_lisp current_file_symbol, safe_symbol;

static void set_encoding_from_header(FILE *fp,char *buf)
{
  char *header_start=strstr(buf,"-*-");
  if (header_start == NULL) return;
  else {
    char *coding_start=strstr(header_start+3,"coding:");
    char *header_end=NULL;
    if (coding_start) header_end=strstr(coding_start,"-*-");
    if ((header_end) && (header_end > coding_start)) {
      char buf[1024], *scan=coding_start+7, *write=buf;
      while ((scan < header_end) && (*scan != ';'))
	*write++=*scan++;
      *write++=0;
      fd_set_file_encoding(fp,buf);}}
}

#if FD_THREADS_ENABLED
static fd_tld_key current_file_key;
static fd_tld_key current_env_key;
#define get_cur_file() ((FILE *) fd_tld_get(current_file_key))
#define set_cur_file(x) fd_tld_set(current_file_key,x)
#define get_cur_env() ((fd_lispenv) fd_tld_get(current_env_key))
#define set_cur_env(x) fd_tld_set(current_env_key,x)
#else
static FILE *current_file;
static fd_lispenv current_env;
#define get_cur_file() (current_file)
#define set_cur_file(x) current_file=x
#define get_cur_env() (current_env)
#define set_cur_env(x) current_env=x
#endif

FRAMERD_EXPORT void fd_set_load_env(fd_lispenv env)
{
  set_cur_env(env);
}
FRAMERD_EXPORT fd_lispenv fd_get_load_env()
{
  return (fd_lispenv) get_cur_env();
}

FRAMERD_EXPORT void fd_set_load_file(FILE *file)
{
  set_cur_file(file);
}
FRAMERD_EXPORT FILE *fd_get_load_file()
{
  return (FILE *)get_cur_file();
}

static lisp lisp_set_encoding_cproc(lisp enc)
{
  FILE *f=get_cur_file();
  if (f) fd_set_file_encoding(f,fd_strdata(enc));
  else fd_set_default_encoding(fd_strdata(enc));
  return FD_VOID;
}

/** File Loading **/

FRAMERD_EXPORT
/* fd_process_file:
     Arguments: a filename, an encoding name, and an environment.
     Returns: a lisp object

  This is the core of all the FDScript/Scheme file loading functions.
  It maintains the load environment and load file thread variables,
  looks at the -*- header to get encoding information, and handles error
  conditions.

  It returns the result of evaluating the last expression in the file.
*/
lisp fd_process_file(char *fname,char *enc,fd_lispenv env)
{
  FILE *in=fd_fopen(fname,"r"), *cur; fd_lispenv saved_env;
  if (in) {
    lisp result=FD_EMPTY_CHOICE, last_form=FD_VOID;
    lisp old_fname;
    char buf[1024];
    UNWIND_PROTECT {
      char *absolute; size_t n;
      fd_lisp new_fname;
      
      /* Save current file */
      old_fname=fd_thread_symeval(current_file_symbol);
      
      /* Dynamically bind *CURRENT-FILE* */
      
      absolute=fd_absolute_pathname(fname);
      new_fname=fd_make_string(absolute); fd_xfree(absolute);
      fd_thread_symbind(current_file_symbol,new_fname);
      fd_decref(new_fname);
      cur=get_cur_file(); set_cur_file(in);
      saved_env=get_cur_env(); set_cur_env(env);

      n=fread(buf,1,1023,in);
      buf[n]='\0';
      
      if (enc) fd_set_file_encoding(in,enc);
      else set_encoding_from_header(in,buf);

      if ((buf[0] == '#') && (buf[1] == '!')) {
	char *eol=strchr(buf,'\n');
	if (eol) fseek(in,eol-buf,SEEK_SET); /* Skip the first line */
	else fseek(in,0,SEEK_SET);}
      else fseek(in,0,SEEK_SET);

      /* Evaluating expressions */
      while (1) {
	lisp form=fd_parse_lisp_from_stream(in);
	if (FD_EOF_OBJECTP(form)) break;
	else {
	  decref(result); result=fd_eval_in_env(form,get_cur_env());
	  decref(last_form); last_form=form;
	}}}
    ON_UNWIND {
      if (fd_theException()) {
	fd_u8char *details=fd_exception_details();
	if (details)
	  fd_warn(_("Error <%m> while loading \"%s\""),
		  fd_theException(),fname);
	else fd_warn(_("Error <%m:%s> while loading \"%s\""),
		     fd_theException(),details,fname);

	fd_warn(_("Last form was %q"),last_form);}
      set_cur_file(cur); set_cur_env(saved_env);
      fd_fclose(in);
      fd_thread_symbind(current_file_symbol,old_fname);
      fd_decref(old_fname);
      decref(last_form);}
    END_UNWIND;
    return result;}
  else fd_raise_detailed_exception(fd_FileOpenFailed,fname);
}

static lisp traced_process_file(char *fname,char *enc,fd_lispenv env)
{
  FILE *in=fd_fopen(fname,"r"), *cur; fd_lispenv saved_env;
  if (in) {
    lisp result=FD_EMPTY_CHOICE, last_form=FD_VOID;
    lisp old_fname;
    char buf[1024];
    UNWIND_PROTECT {
      char *absolute; size_t n;
      lisp new_fname;
      
      /* Save current file */
      old_fname=fd_thread_symeval(current_file_symbol);
      
      /* Dynamically bind *CURRENT-FILE* */
      
      absolute=fd_absolute_pathname(fname);
      new_fname=fd_make_string(absolute); fd_xfree(absolute);
      fd_thread_symbind(current_file_symbol,new_fname);
      fd_decref(new_fname);
      cur=get_cur_file(); set_cur_file(in);
      saved_env=get_cur_env(); set_cur_env(env);

      n=fread(buf,1,1023,in);
      buf[n]='\0';
      
      if (enc) fd_set_file_encoding(in,enc);
      else set_encoding_from_header(in,buf);

      if ((buf[0] == '#') && (buf[1] == '!')) {
	char *eol=strchr(buf,'\n');
	if (eol) fseek(in,eol-buf,SEEK_SET); /* Skip the first line */
	else fseek(in,0,SEEK_SET);}
      else fseek(in,0,SEEK_SET);
      

      /* Evaluating expressions */
      while (1) {
	lisp form=fd_parse_lisp_from_stream(in);
	if (FD_EOF_OBJECTP(form)) break;
	else {
	  fd_fprintf(stderr,"eval:  %q\n",form);
	  decref(result); result=fd_eval_in_env(form,get_cur_env());
	  fd_fprintf(stderr,"value: %q\n",result);
	  decref(last_form); last_form=form;
	}}}
    ON_UNWIND {
      if (fd_theException()) {
	fd_warn(_("Error <%m> while loading \"%s\""),fd_theException(),fname);
	fd_warn(_("Last form was %q"),last_form);}
      set_cur_file(cur); set_cur_env(saved_env);
      fd_fclose(in);
      fd_thread_symbind(current_file_symbol,old_fname);
      fd_decref(old_fname);
      decref(last_form);}
    END_UNWIND;
    return result;}
  else fd_raise_detailed_exception(fd_FileOpenFailed,fname);
}

FRAMERD_EXPORT
/* fd_load_file
     Arguments: a filename string, an encoding string, an environment
     Returns: a lisp pointer
 Loads a file into a particular environment with a particular encoding.
*/
lisp fd_load_file(char *filename,char *enc,fd_lispenv env)
{
  lisp result=fd_process_file(filename,enc,env);
  return result;
}

FRAMERD_EXPORT
/* fd_load_library
     Arguments: a filename string, an encoding string, an environment
     Returns: a lisp pointer
 Loads a file into a particular environment with a particular encoding.
This searches for the file along FDMYPATH and %FDPATH.
*/
lisp fd_load_library(char *filename,char *enc,fd_lispenv env)
{
  fd_lisp path=fd_getpath("FDMYPATH");
  fd_u8char *fname=fd_find_file(filename,path);
  if (fname == NULL) {
    fd_decref(path);
    path=fd_getpath("%FDPATH");
    fname=fd_find_file(filename,path);}
  fd_decref(path);
  if (fname) {
    lisp result=fd_process_file(fname,enc,env); fd_xfree(fname); 
    return result;}
  else fd_raise_detailed_exception(fd_CantFindFile,filename);
}

/** Loading files from LISP **/

static fd_lispenv get_load_env(fd_lispenv start_env)
{
  fd_lispenv env=start_env;
  while (env)
    if (env->module) return env;
    else env=env->parent;
  return env;
}

static char *interpret_encoding(lisp encoding)
{
  if (SYMBOLP(encoding)) return (SYMBOL_NAME(encoding));
  else if (STRINGP(encoding)) return (STRING_DATA(encoding));
  else fd_type_error(_("not a character encoding (string or symbol)"),
		     encoding);
}

static lisp lisp_load_file_handler(lisp expr,fd_lispenv env)
{
  char *abspath, *encoding; fd_lispenv load_env;
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  lisp result=FD_VOID;
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  else abspath=fd_absolute_pathname(FD_STRING_DATA(filename));
  if (FD_FALSEP(enc_var)) encoding=NULL;
  else encoding=interpret_encoding(enc_var);
  if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
  else if (PRIM_TYPEP(env_arg,env_type)) 
    load_env=CPTR_DATA(env_arg);
  else fd_type_error(_("not an environment"),env_arg);
  result=fd_load_file(abspath,encoding,load_env);
  decref(filename); decref(enc_var); decref(env_arg); fd_xfree(abspath);
  return result;
}

static lisp lisp_traced_load_handler(lisp expr,fd_lispenv env)
{
  char *abspath, *encoding; fd_lispenv load_env;
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  lisp result=FD_VOID;
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  else abspath=fd_absolute_pathname(FD_STRING_DATA(filename));
  if (FD_FALSEP(enc_var)) encoding=NULL;
  else encoding=interpret_encoding(enc_var);
  if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
  else if (PRIM_TYPEP(env_arg,env_type)) 
    load_env=CPTR_DATA(env_arg);
  else fd_type_error(_("not an environment"),env_arg);
  result=traced_process_file(abspath,encoding,load_env);
  decref(filename); decref(enc_var); decref(env_arg); fd_xfree(abspath);
  return result;
}

static lisp lisp_load_library_handler(lisp expr,fd_lispenv env)
{
  char *abspath, *encoding; fd_lispenv load_env;
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  lisp result=FD_VOID;
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  else abspath=fd_absolute_pathname(FD_STRING_DATA(filename));
  if (FD_FALSEP(enc_var)) encoding=NULL;
  else encoding=interpret_encoding(enc_var);
  if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
  else if (PRIM_TYPEP(env_arg,env_type)) 
    load_env=CPTR_DATA(env_arg);
  else fd_type_error(_("not an environment"),env_arg);
  result=fd_load_library(abspath,NULL,load_env);
  decref(filename); decref(enc_var); decref(env_arg); fd_xfree(abspath);
  return result;
}

static lisp lisp_load_once_handler(lisp expr,fd_lispenv env)
{
  char *abspath, *encoding; fd_lispenv load_env;
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  lisp result=FD_VOID, loaded_files;
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  else abspath=fd_absolute_pathname(FD_STRING_DATA(filename));
  if (FD_FALSEP(enc_var)) encoding=NULL;
  else encoding=interpret_encoding(enc_var);
  if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
  else if (PRIM_TYPEP(env_arg,env_type)) 
    load_env=CPTR_DATA(env_arg);
  else fd_type_error(_("not an environment"),env_arg);
  loaded_files=fd_symeval(fd_make_symbol("%FILES"),load_env);
  if ((FD_VOIDP(loaded_files)) ||
      (!(fd_choice_containsp(filename,loaded_files)))) {
    if (FD_VOIDP(loaded_files)) loaded_files=incref(filename);
    else {ADD_TO_CHOICE(loaded_files,incref(filename));}
    fd_set_value(fd_make_symbol("%FILES"),loaded_files,load_env);
    result=fd_load_file(abspath,NULL,load_env);}
  decref(filename); decref(enc_var); decref(env_arg); decref(loaded_files);
  fd_xfree(abspath);
  return result;
}

/** Loading 'components' (from the same directory as the current file) **/

FRAMERD_EXPORT char *fd_get_component_file(char *name)
{
  lisp root=fd_thread_symeval(current_file_symbol);
  if (STRINGP(root)) {
    char *dirname=fd_dirname(STRING_DATA(root));
    int dirlen=strlen(dirname), new_size=strlen(name)+dirlen+2;
    char *new=fd_xmalloc(sizeof(char)*new_size);
    strcpy(new,dirname);
    if ((*new) && (dirname[dirlen-1] != '/')) strcat(new,"/");
    strcat(new,name);
    decref(root); fd_xfree(dirname);
    return new;}
  else {decref(root); return fd_strdup(name);}
}

static lisp lisp_get_component(lisp filename)
{
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  else return fd_init_string(fd_get_component_file(STRING_DATA(filename)),-1);
}

static lisp lisp_load_component_handler(lisp expr,lispenv env)
{
  char *component_name, *encoding; fd_lispenv load_env;
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  lisp result;
  if (!(STRINGP(filename)))
    fd_type_error(_("filename must be string"),filename);
  if (FD_FALSEP(enc_var)) encoding=NULL;
  else encoding=interpret_encoding(enc_var);
  if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
  else if (PRIM_TYPEP(env_arg,env_type)) 
    load_env=CPTR_DATA(env_arg);
  else fd_type_error(_("not an environment"),env_arg);
  component_name=fd_get_component_file(STRING_DATA(filename));
  if (component_name == NULL) {
    fd_u8char *with_suffix=fd_malloc(STRING_LENGTH(filename)+10);
    strcpy(with_suffix,STRING_DATA(filename));
    strcat(with_suffix,".fdx");
    component_name=fd_get_component_file(with_suffix);
    fd_free(with_suffix,STRING_LENGTH(filename)+10);}
  if (component_name) 
    result=fd_load_file(component_name,NULL,load_env);
  else fd_raise_detailed_exception(fd_CantFindFile,component_name);
  decref(filename); decref(enc_var); decref(env_arg);
  fd_xfree(component_name);
  return result;
}

/** Evaluating things just once **/

static fd_hashset evaluated_once;
#if FD_THREADS_ENABLED
static fd_mutex eval_once_lock;
#endif

static lisp eval_once(lisp action,fd_lispenv env)
{
  lock_mutex(&eval_once_lock);
  if (fd_hashset_get(evaluated_once,action)) {
    unlock_mutex(&eval_once_lock); return FD_VOID;}
  else {
    lisp value;
    WITH_HANDLING {
      fd_hashset_add(evaluated_once,action);
      unlock_mutex(&eval_once_lock);
      value=fd_eval_in_env(action,env);}
    ON_EXCEPTION {
      fd_hashset_drop(evaluated_once,action);
      fd_reraise();}
    END_HANDLING;
    return value;}
  
}

static lisp eval_once_handler(lisp expr,lispenv env)
{
  lisp action=fd_get_arg(expr,1,FD_VOID);
  return eval_once(action,env);
}

/** Loading configurations **/

static lisp load_config_cproc(lisp filename_arg)
{
  char *filespec=NULL, *filename;
  fd_lisp mypath;
  fd_lisp path=FD_VOID;
  /* Convert lisp arg to string */
  if (STRINGP(filename_arg))
    filespec=fd_strdup(STRING_DATA(filename_arg));
  else if (SYMBOLP(filename_arg))
    filespec=fd_string_getenv(SYMBOL_NAME(filename_arg));
  else fd_type_error(_("filename must be string"),filename_arg);
  /* Try to find a file which matches */
  mypath=fd_getpath("MYFDPATH");
  filename=fd_find_file(filespec,mypath);
  if (filename == NULL) {
    path=fd_getpath("%FDPATH");
    filename=fd_find_file(filespec,path);}
  if (filename == NULL) { /* Try with .cfg suffix */
    char *buf=fd_malloc(strlen(filespec)+10);
    strcpy(buf,filespec); strcat(buf,".cfg");
    filename=fd_find_file(buf,mypath);
    if (filename == NULL)
      filename=fd_find_file(buf,path);
    fd_free(buf,strlen(filespec)+10);}
  fd_decref(mypath);
  fd_decref(path);
  if (filename) {
    fd_load_config(filename);
    fd_do_preloads();
    fd_xfree(filename); fd_xfree(filespec);
    return FD_VOID;}
  else fd_raise_detailed_exception(fd_CantFindFile,filespec);
}

static lisp lisp_load_user_profile_cproc()
{
  fd_load_user_profile();
  return FD_VOID;
}

static lisp lisp_get_config_file_cproc()
{
  char *file=fd_get_config_file();
  if (file)
    return fd_make_string(file);
  else return FD_EMPTY_CHOICE;
}

/** RELOADER **/

static struct FILE_TIME {
  fd_u8char *filename; char *encoding; fd_lispenv env;
  time_t mtime; fd_lisp load_result;
  struct FILE_TIME *next;} *filetimes=NULL;

#if FD_THREADS_ENABLED
static fd_mutex filetimes_lock;
#endif

static struct FILE_TIME *get_filetime(fd_u8char *name,fd_lispenv env)
{
  struct FILE_TIME *scan;
  fd_lock_mutex(&filetimes_lock);
  scan=filetimes; while (scan)
    if (((env) == (scan->env)) &&
	((strcmp(name,scan->filename) == 0))) break;
    else scan=scan->next;
  if (scan) {
    fd_unlock_mutex(&filetimes_lock);
    return scan;}
  scan=fd_malloc(sizeof(struct FILE_TIME));
  scan->filename=fd_strdup(name); scan->env=fd_mallocd_env(env); scan->encoding=NULL;
  scan->load_result=FD_VOID; scan->mtime=0; scan->next=filetimes; filetimes=scan;
  fd_unlock_mutex(&filetimes_lock);
  return scan;
}

FRAMERD_EXPORT
/* fd_reloader:
     Arguments: none
     Returns: void
 Reloads all files which are tagged for automatic reloading and have changed since
 last loaded. */
void fd_reloader()
{
  FD_WITH_MUTEX_LOCKED(&filetimes_lock) {
    struct FILE_TIME *scan=filetimes; struct stat statb;
    while (scan) 
      if ((stat(scan->filename,&statb)) < 0) {
	fd_warn("stat failed on %s",scan->filename);
	scan=scan->next;}
      else if (statb.st_mtime > scan->mtime) {
	fd_lisp new_result;
	scan->mtime=statb.st_mtime;
	new_result=fd_load_file(scan->filename,NULL,scan->env);
	fd_decref(scan->load_result); scan->load_result=new_result;}
      else scan=scan->next;}
  FD_END_WITH_MUTEX_LOCKED(&filetimes_lock);
}

FRAMERD_EXPORT
/* fd_file_reloader:
    Arguments: a filename, an encoding name, and an environment pointer
    Returns: the result of loading the file
 This records the file for automatic reloading by fd_reloader() whenever it changes.
*/
fd_lisp fd_file_reloader(char *pathname,char *enc,fd_lispenv env)
{
  struct FILE_TIME *entry=get_filetime(pathname,env); 
  struct stat statb;
  if ((stat(pathname,&statb)) < 0)
    fd_raise_detailed_exception("Cannot stat file",pathname);
  else if ((entry->mtime) && (entry->mtime >= statb.st_mtime))
    return fd_incref(entry->load_result);
  else {
    fd_lisp load_result=fd_load_file(pathname,enc,env);
    entry->mtime=statb.st_mtime; entry->load_result=fd_incref(load_result);
    if (enc) entry->encoding=fd_strdup(enc); else entry->encoding=enc;
    return load_result;}
}
    
static fd_lisp lisp_reloader_handler(fd_lisp expr,fd_lispenv env)
{
  if (FD_EMPTY_LISTP(FD_CDR(expr))) {
    fd_reloader();
    return FD_VOID;}
  else {
    char *abspath, *encoding; fd_lispenv load_env;
    lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
    lisp enc_var=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
    lisp env_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
    lisp result=FD_VOID;
    if (!(STRINGP(filename)))
      fd_type_error(_("filename must be string"),filename);
    else abspath=fd_absolute_pathname(FD_STRING_DATA(filename));
    if (FD_FALSEP(enc_var)) encoding=NULL;
    else encoding=interpret_encoding(enc_var);
    if (FD_FALSEP(env_arg)) load_env=get_load_env(env);
    else if (PRIM_TYPEP(env_arg,env_type)) 
      load_env=CPTR_DATA(env_arg);
    else fd_type_error(_("not an environment"),env_arg);
    result=fd_file_reloader(abspath,encoding,load_env);
    decref(filename); decref(enc_var); decref(env_arg);
    fd_xfree(abspath);
    return result;}
}

/** Initialization **/

void fd_initialize_load_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&eval_once_lock);
  fd_init_mutex(&filetimes_lock);
  fd_new_tld_key(&current_file_key,NULL);
  fd_new_tld_key(&current_env_key,NULL);
#endif
  
  evaluated_once=fd_make_hashset(15);

  current_file_symbol=fd_make_symbol("*CURRENT-FILE*");

  fd_add_restricted_special_form("LOAD",lisp_load_file_handler);
  fd_add_restricted_special_form("TRACED-LOAD",lisp_traced_load_handler);
  fd_add_restricted_special_form("LOAD-FILE",lisp_load_file_handler);
  fd_add_restricted_special_form("LOAD-LIBRARY",lisp_load_library_handler);
  fd_add_restricted_special_form("LOAD-ONCE",lisp_load_once_handler);
  fd_add_restricted_special_form("LOAD-COMPONENT",lisp_load_component_handler);
  fd_add_restricted_cproc("GET-COMPONENT",1,lisp_get_component);
  fd_add_restricted_special_form("EVAL-ONCE",eval_once_handler);

  fd_add_alias(fd_enabled_env,"TLOAD","TRACED-LOAD");

  fd_add_restricted_special_form("RELOADER",lisp_reloader_handler);

  fd_add_restricted_cproc("LOAD-CONFIG",1,load_config_cproc);
  fd_add_restricted_cproc("LOAD-USER-PROFILE",0,lisp_load_user_profile_cproc);
  fd_add_restricted_cproc("GET-CONFIG-FILE",0,lisp_get_config_file_cproc);

  fd_add_restricted_cproc("SET-ENCODING!",1,lisp_set_encoding_cproc);

  fd_register_source_file("load",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: load.c,v $
   Revision 1.34  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.33  2004/07/31 14:05:25  haase
   Fixed type signature of fd_set/get_load_file to be FILE *

   Revision 1.32  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.31  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.30  2004/03/10 14:37:56  haase
   Added a generic reloading facility

   Revision 1.29  2003/12/05 14:58:44  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.28  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.27  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.26  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.25.2.5  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.25.2.4  2003/02/16 21:41:23  haase
   Minor patches to ralphc patches

   Revision 1.25.2.3  2003/02/09 23:10:01  haase
   Fix module loading to do a search again

   Revision 1.25.2.2  2003/01/26 20:38:00  haase
   Misc. fixes, especially GC

   Revision 1.25.2.1  2002/08/13 01:26:23  haase
   Fixed some FD_ prefix problems

   Revision 1.25  2002/07/05 14:32:09  haase
   New in-module implementation

   Revision 1.24  2002/06/24 18:08:33  haase
   Fixed some source file registrations

   Revision 1.23  2002/06/15 20:42:31  haase
   Made IN-MODULE default to the same level of security as the current context

   Revision 1.22  2002/06/15 20:01:49  haase
   Fixes to risky arg to in-module

   Revision 1.21  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.20  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.19  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.18  2002/04/22 11:57:53  haase
   Added FD_USING_THREAD conditional where it was needed

   Revision 1.17  2002/04/20 19:47:48  haase
   Renamed fd_hashset_zap to fd_hashset_drop

   Revision 1.16  2002/04/19 00:16:13  haase
   Fixed sense-inverting typo on module locking

   Revision 1.15  2002/04/17 17:50:36  haase
   Fixed some inconsistent returns

   Revision 1.14  2002/04/17 13:16:26  haase
   Decrement number of loading modules when finished with a module; renamed FDPATH to %FDPATH

   Revision 1.13  2002/04/17 12:25:36  haase
   Made locks for module loading to avoid double loading

   Revision 1.12  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.11  2002/04/10 17:10:18  haase
   Made error messags passed out through LOAD be more informative

   Revision 1.10  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
