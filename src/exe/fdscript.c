/* C Mode */

/* fdscript.c
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

static char vcid[] = "$Id: fdscript.c,v 1.29 2005/01/14 16:48:45 haase Exp $";

#include "framerd/fdscript.h"
#include "framerd/plugins.h"
#include "time.h"
#include "signal.h"
#include "sys/types.h"

/* Readline stubs */

static pid_t this_pid;

FDSCRIPT_EXPORT void fd_initialize_fdtext(void);
IMPORTED fd_lispenv fd_texttools_env;
FDSCRIPT_EXPORT void fd_initialize_fdwww(void);
IMPORTED fd_lispenv fd_wwwtools_env;

FDSCRIPT_EXPORT fd_lispenv fd_osprims_env;
FRAMERD_EXPORT fd_lispenv fd_enabled_env;
IMPORTED fd_lispenv fd_internals_env;
IMPORTED fd_lispenv fd_fdmaint_env;

fd_lisp fd_interaction_loop
  (void *data,fd_lispenv env,
   fd_lisp (*get_expr)(void *,fd_lispenv),
   void (*display_result)(void *,fd_lispenv,fd_lisp),
   void (*show_status)(void *,fd_lispenv,char *),
   void (*report_error)(void *,fd_lispenv,fd_exception,char *,fd_lisp,
			struct FD_EXCEPTION_CONTEXT *));

fd_lisp fd_console_loop(fd_lispenv env);
fd_lisp fd_busy_console_loop(fd_lispenv env);
fd_lisp fd_promptless_loop(fd_lispenv env);

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000
#endif

#ifndef SPECIAL_INITS
#define SPECIAL_INITS
#endif

static void show_usage()
{
  fprintf(stderr,"Usage: fdscript [filename] [options] [arg1] [arg2] [arg...]\n");
  fprintf(stderr,"  Options: noherald=1|0, noconfig=1|0, noprofile=1|0\n");  
  fprintf(stderr,"  Options: config=<file>, charset=<charset>\n");  
  fprintf(stderr,"  Options: preload_files=[+]<filename> preload_modules=[+]<module name>\n");  
  fprintf(stderr,"  Options: <var>=<val>\n");  
}


/* Special input handling procedures */

static fd_u8char *read_stdin_as_string()
{
  struct FD_STRING_STREAM ss; int c;
  FD_INITIALIZE_STRING_STREAM(&ss,1024);
  while ((c=fd_fgetc(stdin)) >= 0) fd_sputc(&ss,c);
  return ss.ptr;
}

static fd_lisp read_stdin_as_lisp()
{
  fd_lisp answer=FD_EMPTY_CHOICE;
  while (1) {
    fd_lisp elt=fd_parse_lisp_from_stream(stdin);
    if (FD_EOF_OBJECTP(elt)) return answer;
    else {FD_ADD_TO_CHOICE(answer,elt);}}
}

static fd_lisp stdin_args;
static char *stdin_string;

fd_lisp listref(fd_lisp lst,int i)
{
  FD_DOLIST(elt,lst)
    if (i == 0) return elt; else i--;
  return FD_VOID;
}

void init_variable(char *name,fd_lisp value)
{
  fd_lisp symbol=fd_make_symbol(name);
  fd_set_symbol_value(symbol,value);
}

static void set_session_mnemonic(char *filename)
{
  /* Sets the session mnemonic to filename with its .x prefix stripped */
  char *copy=fd_strdup(filename), *scan=copy, *slash=NULL, *dot=NULL;
  while (*scan)
    if ((*scan == '/') || (*scan == '\\')) {dot=NULL; slash=++scan;}
    else if (*scan == '.') dot=scan++;
    else scan++;
  if (dot) *dot=0;
  if ((slash) && ((dot==NULL) | (slash<dot)))
    fd_set_session_mnemonic(slash);
  else fd_set_session_mnemonic(copy);
  fd_xfree(copy);
}

static fd_lisp string_arg(char *string)
{
  fd_u8char *s=fd_convert_os_string(string);
  fd_u8char *result=fd_interpret_unicode_escapes(s);
  fd_xfree(s); return fd_init_string(result,-1);
}


/* Starting new REPs */

static fd_lisp push_rep_lexpr(fd_lisp args)
{
  fd_lisp env=fd_get_arg(args,0,FD_FALSE);
  if (FD_PRIM_TYPEP(env,env_type)) {
    fd_lisp v=fd_console_loop(FD_CPTR_DATA(env));
    fd_decref(v);
    return fd_incref(env);}
  else {
    fd_lispenv m=fd_make_module(); fd_lisp v;
    fd_module_uses(m,fd_enabled_env);
    fd_module_uses(m,fd_osprims_env);
    fd_module_uses(m,fd_texttools_env);
    fd_module_uses(m,fd_module_table);
    fd_module_uses(m,fd_fdinternals_env);
    fd_module_uses(m,fd_fdmaint_env);
    v=fd_console_loop(m); fd_decref(v);
    return fd_make_cptr(env_type,m);}
}

static fd_lisp push_busy_rep_lexpr(fd_lisp args)
{
  fd_lisp env=fd_get_arg(args,0,FD_FALSE);
  if (FD_PRIM_TYPEP(env,env_type)) {
    fd_lisp v=fd_busy_console_loop(FD_CPTR_DATA(env));
    fd_decref(v);
    return fd_incref(env);}
  else {
    fd_lispenv m=fd_make_module(); fd_lisp v;
    fd_module_uses(m,fd_enabled_env);
    fd_module_uses(m,fd_module_table);
    fd_module_uses(m,fd_fdinternals_env);
    fd_module_uses(m,fd_osprims_env);
    fd_module_uses(m,fd_texttools_env);
    fd_module_uses(m,fd_fdmaint_env);
    v=fd_busy_console_loop(m); fd_decref(v);
    return fd_make_cptr(env_type,m);}
}

static void process_config_assignment(char *start)
{
  fd_lisp symbol, value; int add=0;
  char *equals=strchr(start,'=');
  char *buf=fd_xmalloc(equals-start+1);
  strncpy(buf,start,equals-start); buf[equals-start]=0;
  symbol=fd_make_symbol(buf);
  if (equals[1]=='+') {add=1; equals++;}
  if ((equals[1]==' ') || (equals[1]=='\0')) value=FD_VOID;
  else value=fd_parse_arg(equals+1);
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

static void stop_yourself(fd_exception ex,fd_u8char *details,fd_lisp obj)
{
#ifdef SIGSTOP
  killpg(this_pid,SIGSTOP); sleep(5);
#endif
}

static void preload_module(fd_lisp spec,fd_lispenv default_env)
{
  fd_u8char *mname=((FD_SYMBOLP(spec)) ? (FD_SYMBOL_NAME(spec)) :
		    (FD_STRINGP(spec)) ? (FD_STRING_DATA(spec)) : ((fd_u8char *) NULL));
  fd_lispenv module=((mname) ? (fd_get_module(mname)) : ((fd_lispenv) NULL));
  if (module == NULL) fd_warn("Couldn't locate module %q",spec);
  else fd_module_uses(default_env,module);
}


/* The Main Event */

int main(int argc,char *argv[])
{
  char *load_filename=NULL;
  int interactive=0, i=1, busy=0, promptless=0, no_profile=0, load_pos;
  fd_u8char **params; int param_count;
  fd_lisp args=FD_EMPTY_LIST, *tail=&args; fd_lispenv default_env;

  this_pid=getpid();

  {
    char *stop_arg=getenv("FD_STOP_ON_EXCEPTIONS");
    if ((stop_arg) && (*stop_arg != '\0') && (*stop_arg != 'n'))
      fd_set_exception_fn(stop_yourself);}

  params=fd_cmdline(&param_count,argv,argc);

  if ((param_count > 0) && (params[0][0] == '-') && (params[0][1] != NUL) &&
      (!(fd_file_existsp(params[0])))) {
    show_usage();
    exit(1);}

  if (fd_int_getenv("NOPROFILE",0)) no_profile=1;
  if (fd_int_getenv("NOCONFIG",0)) fd_suppress_config();
  if (fd_int_getenv("NOHERALD",0)) fd_inhibit_herald(1);

  fd_cmd_args(&argc,&argv);

  stdin_args=FD_VOID; stdin_string=NULL;

  /* Determine how you are running: interactive, promptless,
     interactive with args, etc.  Also find out the load filename. */
  if (param_count == 0) { /* No arguments at all */
    interactive=1;
    set_session_mnemonic(argv[0]); load_filename=NULL;
    if (no_profile == 0) fd_use_profile();
    fd_show_poolids(1); fd_control_frame_printing(2);}
  else if (strcmp(params[0],"-") == 0) { /* filename is `stdin' */
    /* Running promptless */
    interactive=1; promptless=1;
    fd_show_poolids(0); load_filename=NULL;
    fd_disable_notifications();}
  else if (strcmp(params[0],".") == 0) {
    /* Running interactive with args */
    interactive=1; promptless=0;
    fd_show_poolids(0); load_filename=NULL;}
  else {
    /* Running like a script */
    fd_disable_notifications();
    fd_show_poolids(0);
    set_session_mnemonic(params[0]);
    load_filename=fd_strdup(params[0]);}
  
  /* Library initializations */
  fd_set_build_date(__DATE__);
  fd_initialize_fdscript();

  /* Set up the default module */
  default_env=fd_make_module();
  fd_module_uses(default_env,fd_enabled_env);
  fd_module_uses(default_env,fd_osprims_env);
  fd_module_uses(default_env,fd_fdinternals_env);
  fd_module_uses(default_env,fd_fdmaint_env);
  fd_module_uses(default_env,fd_module_table);

  /* Initialize the console functions and access
     the fdtext module */
  fd_initialize_console_c();
  fd_initialize_fdtext();
  fd_initialize_fdwww();
  fd_module_uses(default_env,fd_texttools_env);
  fd_module_uses(default_env,fd_wwwtools_env);

  {
    char *charset=fd_string_getenv("CHARSET");
    if (charset) {
      fd_set_default_encoding(charset);
      fd_set_file_encoding(stdin,charset);
      fd_set_file_encoding(stdout,charset);
      fd_set_file_encoding(stderr,charset);}}

  fd_do_preloads();

  /* Now, process the arguments which fdscript handles */
  i=0; while (i < param_count) {
    fd_lisp arg=FD_VOID;
    if (strcmp(params[i],"-") == 0)
      /* Read STDIN as your argument */
      if (FD_VOIDP(stdin_args))
	if (stdin_string)
	  fd_warn(_("Can't use stdin as both lisp and string"));
	else arg=stdin_args=read_stdin_as_lisp();
      else arg=fd_incref(stdin_args);
    else if (strcmp(params[i],"$") == 0) {
      if (stdin_string == NULL)
	if (FD_VOIDP(stdin_args))
	  stdin_string=read_stdin_as_string();
	else fd_warn(_("Can't use stdin as both lisp and string"));
      else arg=fd_make_string(stdin_string);}
    else arg=string_arg(params[i]);
    if (!(FD_VOIDP(arg))) {
      /* Add it to the list of arguments */
      *tail=FD_MAKE_LIST1(arg); tail=&(FD_CDR(*tail));}
    i++;}
  
  /* Other inits */
  SPECIAL_INITS;

  fd_add_restricted_lexpr("%PUSH",FD_ND_LEXPR,push_rep_lexpr);
  fd_add_restricted_lexpr("%BUSY",FD_ND_LEXPR,push_busy_rep_lexpr);

  /* Initialize argument variables */
  init_variable("ARGS",args);
  init_variable("NARGS",FD_LISPFIX(param_count));
  init_variable("ARG0",listref(args,0));
  init_variable("ARG1",listref(args,1));
  init_variable("ARG2",listref(args,2));
  init_variable("ARG3",listref(args,3));
  init_variable("ARG4",listref(args,4));

  if (fd_string_getenv("ALWAYS_BUSY")) busy=1;

  if (interactive) {
    char *init_name=fd_string_getenv("FD_INTERACTIVE_INIT");
    fd_lisp value;
    if (init_name) {
      value=fd_load_file(init_name,NULL,default_env);
      fd_xfree(init_name);}
    if (promptless) value=fd_promptless_loop(default_env);
    else if (busy) value=fd_busy_console_loop(default_env);
    else value=fd_console_loop(default_env);
    fd_decref(value);}
  else {
    fd_lisp main_symbol=fd_make_symbol("MAIN"), main_proc, value, v=FD_VOID;
    char *absolute=fd_absolute_pathname(load_filename);
    FD_WITH_HANDLING {
      v=fd_load_file(absolute,NULL,default_env);
      fd_decref(v); v=FD_VOID;}
    FD_ON_EXCEPTION {
      fd_fprintf(stderr,_("Error while loading %s: %s (%s) %q\n"),
		 load_filename,fd_theException(),
		 fd_exception_details(),fd_exception_object());
      fd_reraise();}
    FD_END_HANDLING;
    fd_xfree(absolute);
    main_proc=fd_symeval(main_symbol,default_env);
    if (FD_VOIDP(main_proc)) {}
    else if (FD_XPROCP(main_proc)) {
      v=fd_apply(main_proc,FD_CDR(args));}
    else {
      v=fd_apply(main_proc,FD_EMPTY_LIST);}
    fd_decref(main_proc); fd_decref(v);}
  if (interactive | ((clock()/CLOCKS_PER_SEC) > 1))
    fd_report_framerd_stats(stderr);
  if (busy) fd_describe_mallocation();
#if WIN32 /* atexit doesn't seem to work under MINGW */
  fd_commit_pools();
  fd_commit_indices();
#endif
  fd_exit(0);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdscript.c,v $
   Revision 1.29  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.28  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.27  2004/07/31 14:13:59  haase
   Move around calls to fd_do_preload for greater coverage

   Revision 1.26  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.25  2004/04/05 11:05:12  haase
   Fixed params parsing for fdscript

   Revision 1.24  2004/04/04 17:37:59  haase
   Made fdscript use fd_cmdline

   Revision 1.23  2004/02/13 18:39:17  haase
   Fixed failure to reset GC'd variable in top level load

   Revision 1.22  2003/12/22 04:07:06  haase
   Fix header files for fdscript executable

   Revision 1.21  2003/12/22 04:01:23  haase
   Fix header files for fdscript executable

   Revision 1.20  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.19  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.18  2003/11/11 16:28:18  haase
   Made some configuration intializations happen before initializing libraries

   Revision 1.17  2003/10/20 12:04:36  haase
   Fixes to stopping on exceptions

   Revision 1.16  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.15  2003/10/01 09:02:18  haase
   Added exception preface function

   Revision 1.14  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.13.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.13.2.1  2003/01/26 20:40:27  haase
   Misc. fixes

   Revision 1.13  2002/06/01 21:11:50  haase
   Add handling of command line var configs which reset vars

   Revision 1.12  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
