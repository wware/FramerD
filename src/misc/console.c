/* C Mode */

/* console.c
   The read-eval-print loop
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

static char vcid[] = "$Id: console.c,v 1.19 2005/01/14 16:48:47 haase Exp $";

#include "fdscript.h"

#undef EXPORTED
#define EXPORTED

#if USING_READLINE
#if (WIN32)
#define USE_VARARGS 1
#define PREFER_STDARG 1
#define READLINE_DLL 1
#define __READLINE_IMPORT__ 1
#endif
#include <readline/readline.h>
static lisp possible_symbol_completions;
static enum { completing_symbol, completing_filename} completion_type;
static int completions_returned=0;
static const char *completion_prefix;
#if HAVE_RL_FILENAME_COMPLETION_FUNCTION
#define HAVE_FILENAME_COMPLETION 1
#define filename_completer rl_filename_completion_function
#elif HAVE_FILENAME_COMPLETION_FUNCTION
#define HAVE_FILENAME_COMPLETION 1
#define filename_completer filename_completion_function
#else
#define HAVE_FILENAME_COMPLETION 0
#endif
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif

#if WIN32
#define set_file_blocking(x) (0)
#define reset_file_flags(x,y) (0)
#else
static int set_file_blocking(FILE *f)
{
  int flags=fcntl(fileno(f),F_GETFL);
  fcntl(fileno(f),F_SETFL,(flags&(~(O_NONBLOCK))));
  return flags;
}
static void reset_file_flags(FILE *f,int flags)
{
  fcntl(fileno(f),F_SETFL,flags);
}
#endif

/* Utility list reversal */

static lisp reverse_list(lisp lst)
{
  if (!(PAIRP(lst))) return lst;
  else {
    lisp answer=FD_EMPTY_LIST;
    DOLIST(elt,lst)
      answer=FD_MAKE_PAIR(incref(elt),answer);
    decref(lst);
    return answer;}
}

#if USING_READLINE
/* If you're using GNU readline... */

/* Maybe evaluate on newline, maybe not... */
static int fdscript_newline(int count,int key)
{
  int result; char *buf=rl_line_buffer;
  fd_u8char *u8=NULL, *to_parse=NULL;
  WITH_HANDLING {
    lisp expr; fd_u8char *scan;
    u8=fd_make_utf8(buf,NULL,fd_get_default_encoding());
    to_parse=fd_interpret_unicode_escapes(u8); scan=to_parse;
    expr=fd_parse_lisp_from_string(&scan);
    decref(expr); rl_done=1; result=rl_newline(count,key);
    rl_redisplay();}
  ON_EXCEPTION {
    if (fd_theException() == fd_Unexpected_EOF) {
      fd_xfree(u8); fd_xfree(to_parse); u8=NULL; to_parse=NULL;
      result=rl_insert(count,'\n'); rl_redisplay();}
    else {
      rl_message("[Read error: %s (%s)]",
		 fd_theException(),fd_exception_details());
      rl_point=buf-rl_line_buffer; result=0;}}
  END_HANDLING;
  if (u8) fd_xfree(u8); if (to_parse) fd_xfree(to_parse);
  return result;
}

/* Check if symbol could complete completion_prefix */
static void completion_probe(lisp symbol)
{
  int len=strlen(completion_prefix);
  char *pname=SYMBOL_NAME(symbol);
  if ((strncasecmp(pname,completion_prefix,len)) == 0)
    ADD_TO_CHOICE(possible_symbol_completions,symbol);
}

static char *fdscript_complete(char *prefix,int state)
{
  if (state == 0) {
    int scan=rl_point;
    while ((scan > 0) && (!(isspace(rl_line_buffer[scan])))) scan--;
    if (rl_line_buffer[scan+1] == '"') 
      completion_type=completing_filename;
    else {
      decref(possible_symbol_completions);
      possible_symbol_completions=FD_EMPTY_CHOICE;
      completions_returned=0; completion_prefix=prefix;
      fd_for_all_symbols(completion_probe);}}
  if (completion_type == completing_symbol) {
    int n_completions=CHOICE_SIZE(possible_symbol_completions);
    if (completions_returned == n_completions) return NULL;
    else if (n_completions == 1) {
      completions_returned++;
      return fd_strdup(SYMBOL_NAME(possible_symbol_completions));}
    else {
      int i=completions_returned++; lisp symbol;
      fd_choice ch=PTR_DATA(possible_symbol_completions,choice);
      if (ch->elt_type) {
	symbol.type=symbol_type;
	symbol.data=ch->elements.data[i];}
      else symbol=ch->elements.lisp[i];
      return fd_strdup(SYMBOL_NAME(symbol));}}
  else if (completion_type == completing_filename) {
#if HAVE_FILENAME_COMPLETION
    return filename_completer(prefix,state);
#else
    return NULL;
#endif
  }
  else return NULL;
}

static void init_readline_for_fdscript()
{
  possible_symbol_completions=FD_EMPTY_CHOICE;
  rl_initialize();
  rl_completion_entry_function=(rl_compentry_func_t *)fdscript_complete;
  rl_readline_name="fdscript";
  rl_bind_key('\n',fdscript_newline);
  rl_bind_key('\r',fdscript_newline);
  if (errno) {CLEAR_ERR();} /* Sometimes gets set */
}

EXPORTED
char *fd_readline(char *prompt) 
{
  return readline(prompt);
}

EXPORTED
fd_lisp fd_read_exprs(char *prompt)
{
  lisp exprs=FD_EMPTY_LIST; int waiting=1;
  fd_u8char *temp_prompt=fd_xmalloc(strlen(prompt)+2);
  strcpy(temp_prompt,"*"); strcat(temp_prompt,prompt);
  while (waiting) {
    WITH_HANDLING {
      fd_u8char *input=readline(temp_prompt), *scan=input;
      if (input == NULL) exprs=FD_EOF_OBJECT;
      else if (*input == '\0') {free(input); exprs=FD_EMPTY_LIST;}
      else {
	lisp expr; fd_u8char *u8, *final;
	u8=fd_make_utf8(input,NULL,fd_get_default_encoding());
	final=fd_interpret_unicode_escapes(u8); scan=final;
	while (*scan) {
	  while ((*scan) && (isspace(*scan))) scan++;
	  expr=fd_parse_lisp_from_string(&scan);
	  exprs=FD_MAKE_PAIR(expr,exprs);
	  while ((*scan) && (isspace(*scan))) scan++;}
	fd_xfree(u8); fd_xfree(final);
	add_history(input);}
      waiting=0;}
    ON_EXCEPTION {
      if (fd_theException() == fd_Unexpected_EOF) {
	fd_clear_exceptions(); decref(exprs); exprs=FD_EMPTY_LIST;}
      else {
	fd_fprintf(stderr,_("[Read Error: %m (%s)]\n"),
		   fd_theException(),fd_exception_details());
	fd_fprintf(stderr,_("[Ignored %d characters of input]"),
		   fd_flush_input_buffer(stdin));}}
    END_HANDLING;}
  fd_xfree(temp_prompt);
  return reverse_list(exprs);
}
#else  /* USING_READLINE */
EXPORTED
char *fd_readline(char *prompt)
{
  char *buf=fd_xmalloc(2048);
  printf("%s",prompt); fflush(stdout);
  return fgets(buf,2048,stdin);
}
EXPORTED
fd_lisp fd_read_exprs(char *prompt)
{
  printf("%s",prompt);
  return FD_MAKE_LIST1(fd_parse_lisp_from_stream(stdin));
}
#endif /* not USING_READLINE */

/* Utility functions */

static lisp pop_symbol, ppwidth_symbol;

/* Gets the pretty print width */
static int get_ppwidth()
{
  lisp ppwidth=fd_thread_symeval(ppwidth_symbol);
  if (FIXNUMP(ppwidth)) return FIXLISP(ppwidth);
  else {decref(ppwidth); return 80;}
}

/** Special functions **/

static lisp lisp_break()
{ fd_raise_exception("Break"); return FD_TRUE;}
static lisp lisp_exit_lexpr(lisp args)
{
  lisp retcode=fd_get_arg(args,0,LISPFIX(0));
  if (FIXNUMP(retcode)) fd_exit(FIXLISP(retcode));
  else {
    fd_warn(_("Invalid return code %q"),retcode);
    fd_exit(1);}
  return FD_EMPTY_CHOICE;
}
static lisp lisp_abort_lexpr(lisp args)
{
  lisp retcode=fd_get_arg(args,0,LISPFIX(1));
  if (FIXNUMP(retcode)) fd_exit(FIXLISP(retcode));
  else {
    fd_warn(_("Invalid return code %q"),retcode);
    fd_exit(1);}
  return FD_EMPTY_CHOICE;
}

/* Console Listeners */

static int listener_init_done=0;
static void load_init(char *file)
{ lisp result=fd_load_file(file,NULL,NULL); decref(result);}
static void do_listener_init()
{
  WITH_HANDLING {
    lisp init_location=fd_getenv("FD_LISTENER_INIT");
#if FD_USING_DLLS
    lisp init_libs=fd_getenv("FD_LISTENER_LIBS");
    if (STRINGP(init_libs)) fd_load_dll(STRING_DATA(init_libs));
    else if (PAIRP(init_libs)) {
      DOLIST(elt,init_libs)
	if (STRINGP(elt)) fd_load_dll(STRING_DATA(elt));}
#endif
    if (STRINGP(init_location))
      load_init(STRING_DATA(init_location));
    else if (PAIRP(init_location)) {
      DOLIST(elt,init_location)
	if (STRINGP(elt)) load_init(STRING_DATA(elt));}}
  ON_EXCEPTION {
    fd_warn(_("Listener init error %m (%m): \n  %q"),
	    fd_theException(),fd_exception_details(),fd_exception_object());
    fd_clear_exception();}
  END_HANDLING;
  listener_init_done=1;
}

/* Looping */

static void fill_status_string(char *buf);

EXPORTED
lisp fd_interaction_loop
  (void *data,
   lispenv env,
   lisp (*get_exprs)(void *,lispenv),
   void (*display_result)(void *,lispenv,lisp),
   void (*show_status)(void *,lispenv,char *status),
   void (*report_error)(void *,fd_lispenv,fd_exception,char *,lisp,
			struct FD_EXCEPTION_CONTEXT *))
{
  lisp value=FD_VOID;
  int do_exit=0;
  if (listener_init_done == 0) do_listener_init();
  if (show_status) {
    char buf[256]; fill_status_string(buf);
    show_status(data,env,buf);}
  while (1) {
    WITH_HANDLING {
      lisp exprs=get_exprs(data,env); int i=0;
      CLEAR_ERR();
      if (FD_EOF_OBJECTP(exprs)) do_exit=1;
      else if (!(FD_PAIRP(exprs))) {} /* Ignore null input */
      else if (FD_EOF_OBJECTP(FD_CAR(exprs))) do_exit=1;
      else {
	DOLIST(expr,exprs) {
	  if (i)
	    if (display_result) display_result(data,env,value);
	    else {}
	  else i=1;
	  decref(value); value=FD_VOID;
	  if (LISP_EQ(expr,pop_symbol)) {do_exit=1; break;}
	  else value=fd_eval_in_env(expr,env);}
	decref(exprs); exprs=FD_EMPTY_LIST;}}
    ON_EXCEPTION
      if ((report_error) &&
	  (strcmp(fd_theException(),"CONTINUATION") != 0)) {
	struct FD_EXCEPTION_STACK *s=fd_exception_stack();
	report_error(data,env,fd_theException(),fd_exception_details(),
		     fd_exception_object(),
		     fd_exception_context(0));
	while (s) {
#if FD_THREADS_ENABLED
	  /* When threading, irritant is a C pointer to a lisp pointer,
	     since that's how we keep it thread local.  */
	  if (s->irritant) 
	    report_error(data,env,s->ex,s->details,*(s->irritant),
			 s->context);
	  else report_error(data,env,s->ex,s->details,FD_VOID,s->context);
#else /* However, when not threading, we just use the pointer directly.
	 A more general solution would be a macro to get the exception
	 stack's irritant, but we'll only do that if we need it in more
	 places than right here. */
	  report_error(data,env,s->ex,s->details,s->irritant,s->context);
#endif
	  s=s->next;}
	/* If there is an error, we clear them and punt */
	fd_clear_exceptions(); CLEAR_ERR();}
      else fd_reraise();
    END_HANDLING;
    if (do_exit) {fd_decref(value); return FD_VOID;}
    if (display_result) display_result(data,env,value);
    if (!((FD_VOIDP(value))||(FD_EMPTYP(value)))) {
      if (env->module) {
	fd_hashtable_set
	  (&(env->module->bindings),fd_make_symbol("THAT"),value);}
      else fd_bind_value(fd_make_symbol("THAT"),value,env);
      decref(value);}
    value=FD_VOID; /* Clear value for next go through */
    if (show_status) {
      char buf[256]; fill_status_string(buf);
      show_status(data,env,buf);}}
  return FD_VOID;
}

static void fill_status_string(char *buf)
{
  struct FD_STRING_STREAM s;
  FD_INITIALIZE_FIXED_STRING_STREAM((&s),256,buf);
#if (FD_LIGHTWEIGHT_OIDS)
  fd_printf
    (&s,"[%s] %f secs; %ld+%ld=%ld bytes; OIDs: %d %dP/%dI/%dS",
     fd_timestring(),((double)clock())/CLOCKS_PER_SEC,
     fd_cons_usage(),fd_malloc_usage(),
     fd_cons_usage()+fd_malloc_usage(),
     fd_loaded_oids,
     fd_get_pool_count(),fd_get_index_count(),fd_get_server_count());  
#else
  fd_printf
    (&s,"[%s] %f secs; %ld+%ld=%ld bytes; OIDs: %d/%d %dP/%dI/%dS",
     fd_timestring(),((double)clock())/CLOCKS_PER_SEC,
     fd_cons_usage(),fd_malloc_usage(),
     fd_cons_usage()+fd_malloc_usage(),
     fd_loaded_oids,fd_oid_table()->n_keys,
     fd_get_pool_count(),fd_get_index_count(),fd_get_server_count());  
#endif
}

static void report_symbols_changed(fd_lispenv env,FILE *out)
{
  if ((env->module) &&
      (!(FD_EMPTYP(env->module->changes)))) {
    lisp really=fd_return_proper_choice(env->module->changes);
    fd_fprintf(out,_(";; Values changed (%d):"),CHOICE_SIZE(really));
    {DO_CHOICES(elt,really) fd_fprintf(out," %q",elt); END_DO_CHOICES;}
    fd_fprintf(out,"\n"); decref(really); 
    env->module->changes=FD_EMPTY_CHOICE;}
}

/** Using the console **/

static lisp console_get_exprs(char *prompt,lispenv env)
{
  lisp exprs;
  char *tprompt=fd_xmalloc(strlen(prompt)+4);
  report_symbols_changed(env,stdout);
  sprintf(tprompt,"[%s] ",prompt);
  exprs=fd_read_exprs(tprompt); fd_xfree(tprompt);
  return exprs;
}

/** Using the console without readline **/

static lisp stdio_get_exprs(char *prompt,lispenv env)
{
  int c; lisp expr=FD_VOID;
  WITH_HANDLING {
    int flags=set_file_blocking(stdin);
    report_symbols_changed(env,stdout);
    if (prompt) fd_fprintf(stdout,"[%s] ",prompt);
    fflush(stdout);
    c=getc(stdin);
    while ((c == 0) || (isspace(c)) || (c == ';'))
      if (c == ';') {
	char buf[1024]; fgets(buf,1024,stdin); c=getc(stdin);}
      else c=getc(stdin);
    reset_file_flags(stdin,flags);
    if (c == EOF) expr=FD_EOF_OBJECT;
    else if (isspace(c)) expr=fd_parse_lisp_from_stream(stdin);
    else {
      ungetc(c,stdin); expr=fd_parse_lisp_from_stream(stdin);}}
  ON_EXCEPTION {
    fd_fprintf(stderr,_("[Read Error: %m (%s)]\n"),
	       fd_theException(),fd_exception_details());
    fd_fprintf(stderr,_("[Ignored %d characters of input]"),
	       fd_flush_input_buffer(stdin));}
  END_HANDLING;
  if (FD_EOF_OBJECTP(expr)) return expr;
  else return FD_MAKE_LIST1(expr);
}

static void stdio_display_value(lisp value,int indent,int width)
{
  if (!(PRIM_TYPEP(value,multiple_value_type))) {
    int i=0; while (i < indent) {fd_fprintf(stdout," "); i++;}
    fd_pprint_lisp_with_offset(value,stdout,width,indent);
    fd_fprintf(stdout,"\n");}
  else {
    int n=0, len=VECTOR_LENGTH(value);
    while (n < len) {
      lisp v=fd_mv_ref(value,n);
      int i=0; while (i < indent) {fd_fprintf(stdout," "); i++;}
      if (n == 0)
	fd_pprint_lisp_with_offset(v,stdout,width,indent);
      else fd_fprintf(stdout,";;+%d: %q",n,v);
      fd_fprintf(stdout,"\n"); n++;}}
}

static int multiple_valuep(lisp x)
{
  if (PRIM_TYPEP(x,multiple_value_type)) return 1;
  else if (CHOICEP(x)) {
    int mvp=0;
    DO_CHOICES(v,x) /* choice trouble */
      if (PRIM_TYPEP(v,multiple_value_type)) {mvp=1; break;}
    END_DO_CHOICES;
    return mvp;}
  else return 0;
}

static void stdio_display_result(char *prompt,lispenv env,lisp result)
{
  unsigned int width=get_ppwidth();
  if (FD_VOIDP(result)) 
    fd_fprintf(stdout,_(";; Nothing (void) was returned\n"));
  else {
    WITH_HANDLING {
      if (CHOICEP(result)) {
	char *string=fd_object_to_string(result);
	if ((strlen(string) > width) || (multiple_valuep(result))) {
	  fd_fprintf(stdout,_("{;; There are %d results\n"),
		     CHOICE_SIZE(result));
	  {DO_CHOICES(r,result) stdio_display_value(r,1,width);
	   END_DO_CHOICES;}
	  fd_fprintf(stdout,"}\n");}
	else {
	  fd_fprintf(stdout,_(";; There are %d results\n"),
		     CHOICE_SIZE(result),string);
	  stdio_display_value(result,0,width);}
	free(string);}
      else if (FD_EMPTYP(result))
	fd_fprintf(stdout,_(";; No choices were returned\n"));
      else {
	stdio_display_value(result,0,width);}}
    ON_EXCEPTION {
      fd_fprintf(stdout,_("\n!!!! !#@%%! -- Error (%m) printing return value\n"),
		 fd_theException());
      fd_clear_exception();}
    END_HANDLING}
}

static void stdio_show_status
   (char *prompt,lispenv env,char *s)
{
  fd_fprintf(stdout,";; %s\n",s);
}

static void stdio_report_error
   (char *prompt,fd_lispenv env,
    fd_exception ex,char *details,lisp irritant,
    struct FD_EXCEPTION_CONTEXT *ec)
{
  if (ec) {
    int i=0;
    fd_fprintf(stderr,";; ");
    while (i < ec->size) {
      if (i == ec->size-1)
	fd_fprintf(stderr,"%q\n",ec->stack[i]);
      else fd_fprintf(stderr,"%q < ",ec->stack[i]);
      i++;}}
  fd_fprintf(stderr,";; !%m! %m (%m) %q\n",prompt,ex,details,irritant);
  fd_fprintf(stderr,_(";; ! To see more details set %%DEBUG\n"));
  fflush(stderr);
}

/** External read-eval-print functions **/


EXPORTED
lisp fd_console_loop(fd_lispenv env)
{
  char *mnemonic=fd_get_session_mnemonic();
  lisp value;
  if (env->module) {
    decref(env->module->changes);
    env->module->changes=FD_EMPTY_CHOICE;}
  if (getenv("EMACS"))
    value=fd_interaction_loop
      ((void *) mnemonic,env,
       (lisp (*)(void *,lispenv))stdio_get_exprs,
       (void (*)(void *,lispenv,lisp))stdio_display_result,
       (void (*)(void *,lispenv,char *))(NULL),
       (void (*)(void *,lispenv,fd_exception,char *,lisp,
		 struct FD_EXCEPTION_CONTEXT *))stdio_report_error);
  else value=fd_interaction_loop
	 ((void *) mnemonic,env,
	  (lisp (*)(void *,lispenv))console_get_exprs,
	  (void (*)(void *,lispenv,lisp))stdio_display_result,
	  (void (*)(void *,lispenv,char *))NULL,
	  (void (*)(void *,fd_lispenv,fd_exception,char *,lisp,
		    struct FD_EXCEPTION_CONTEXT *))stdio_report_error);
  return value;
}

EXPORTED
lisp fd_busy_console_loop(fd_lispenv env)
{
  char *mnemonic=fd_get_session_mnemonic(); lisp value;
  if (env->module) {
    decref(env->module->changes);
    env->module->changes=FD_EMPTY_CHOICE;}
  if (getenv("EMACS"))
    value=fd_interaction_loop
      ((void *) mnemonic,env,
       (lisp (*)(void *,lispenv))stdio_get_exprs,
       (void (*)(void *,lispenv,lisp))stdio_display_result,
       (void (*)(void *,lispenv,char *))stdio_show_status,
       (void (*)(void *,lispenv,fd_exception,char *,lisp,
		    struct FD_EXCEPTION_CONTEXT *))stdio_report_error);
  else value=fd_interaction_loop
    ((void *) mnemonic,env,
     (lisp (*)(void *,lispenv))console_get_exprs,
     (void (*)(void *,lispenv,lisp))stdio_display_result,
     (void (*)(void *,lispenv,char *))stdio_show_status,
     (void (*)(void *,lispenv,fd_exception,char *,lisp,
	       struct FD_EXCEPTION_CONTEXT *))stdio_report_error);
  return value;
}

EXPORTED
lisp fd_promptless_loop(fd_lispenv env)
{
  lisp value;
  if (env->module) {
    decref(env->module->changes);
    env->module->changes=FD_EMPTY_CHOICE;}
  value=fd_interaction_loop
    ((void *) NULL,env,
     (lisp (*)(void *,lispenv))stdio_get_exprs,
     (void (*)(void *,lispenv,lisp))stdio_display_result,
     (void (*)(void *,lispenv,char *))(NULL),
     (void (*)(void *,lispenv,fd_exception,char *,lisp,
	       struct FD_EXCEPTION_CONTEXT *))stdio_report_error);
  return value;
}

/* User functions for console input */

static lisp lisp_readline_lexpr(lisp args)
{
  lisp prompt=fd_get_arg(args,0,FD_FALSE), result;
  char *string;
  if (FD_FALSEP(prompt)) string=fd_readline("[line] ");
  else string=fd_readline(fd_strdata(prompt));
  result=fd_make_string(string); fd_xfree(string);
  return result;
}

static lisp lisp_read_exprs_lexpr(lisp args)
{
  lisp prompt=fd_get_arg(args,0,FD_FALSE), result;
  if (FD_FALSEP(prompt)) result=fd_read_exprs("[exprs] ");
  else result=fd_read_exprs(fd_strdata(prompt));
  return result;
}

void fd_initialize_console_c()
{
#if USING_READLINE
  init_readline_for_fdscript();
#endif

  pop_symbol=fd_make_symbol("%POP");
  ppwidth_symbol=fd_make_symbol("PPWIDTH");

  fd_add_lexpr(NULL,"READ-EXPRS",FD_NORMAL_LEXPR,lisp_read_exprs_lexpr);
  fd_add_lexpr(NULL,"IREADLINE",FD_NORMAL_LEXPR,lisp_readline_lexpr);

  fd_add_restricted_cproc("BREAK",0,lisp_break);
  fd_add_restricted_lexpr("EXIT",FD_ND_LEXPR,lisp_exit_lexpr);
  fd_add_restricted_lexpr("ABORT",FD_ND_LEXPR,lisp_abort_lexpr);

  fd_register_source_file("console",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: console.c,v $
   Revision 1.19  2005/01/14 16:48:47  haase
   Updated copyrights to 2005

   Revision 1.18  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.17  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.16  2004/05/15 21:32:07  haase
   Fix end of input bug for consoles

   Revision 1.15  2004/04/23 12:52:21  haase
   Had the console clear thrown exceptions

   Revision 1.14  2004/03/03 18:57:18  haase
   Remove OLDER_READLINE conditional compilation

   Revision 1.13  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.12  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.11.2.1  2003/01/26 20:43:55  haase
   Misc. fixes especially some GC

   Revision 1.11  2002/05/11 13:42:37  haase
   fd_value_ref -> fd_mv_ref

   Revision 1.10  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
