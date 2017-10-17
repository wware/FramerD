/* C Mode */

/* threads.c
   Thread-specific primitives for FDScript
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

static char vcid[] = "$Id: threads.c,v 1.14 2005/01/14 16:48:45 haase Exp $";

/** Thread-local variables (for real and pretend) **/
/** Thread evaluation **/
/** Mutex data structures **/
/** Mutex special forms **/
/** Initialization **/

#include "fdeval.h"

extern void free_env(lispenv env);

/** Thread-local variables (for real and pretend) **/

/* A thread specific key contains an environment for looking up
    thread local variables.  Note that thread binding is shallow,
    so binding a variable frees whatever value was there before. */

#if FD_THREADS_ENABLED
static fd_tld_key threadenv_key;

FRAMERD_EXPORT
/* fd_threadenv:
     Arguments: none
     Returns: a C pointer to a hashtable
   Returns the hashtable used to store thread local variables */
fd_hashtable fd_threadenv()
{
  return fd_tld_get(threadenv_key);
}
FRAMERD_EXPORT
/* fd_thread_symeval:
     Arguments: a lisp symbol
     Returns: a lisp value
   Returns the thread-local value assigned to a symbol.  This
increfs the value it returns.  */
lisp fd_thread_symeval(lisp sym)
{
  fd_hashtable env=fd_threadenv();
  if (env) 
    return fd_hashtable_get(env,sym,FD_VOID);
  else return FD_VOID;
}
FRAMERD_EXPORT
/* fd_thread_symbind:
     Arguments: a lisp symbol and a lisp value
     Returns: void
   Assigns a thread-local value to a symbol.  The
value is `used up' in the assignment.  */
void fd_thread_symbind(lisp sym,lisp value)
{
  fd_hashtable env=fd_threadenv();
  if (env == NULL) {
    env=fd_make_hashtable(16);
    fd_tld_set(threadenv_key,env);}
  fd_hashtable_set(env,sym,value);
}
#else
static fd_hashtable threadenv;

FRAMERD_EXPORT
fd_hashtable fd_threadenv()
{
  return threadenv;
}
FRAMERD_EXPORT
lisp fd_thread_symeval(lisp symbol)
{
  return fd_hashtable_get(threadenv,symbol,FD_VOID);
}
FRAMERD_EXPORT
void fd_thread_symbind(lisp symbol,lisp value)
{
  fd_hashtable_set(threadenv,symbol,value);
}
#endif

/* Sets a symbol's value in the current thread to the result of evaluating
   an expression. */
static lisp tset_value_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value;
  var=fd_get_arg(expr,1,FD_VOID);
  value_expression=fd_get_arg(expr,2,FD_VOID);
  value=fd_eval_in_env(value_expression,env);
  if (SYMBOLP(var)) {
    fd_thread_symbind(var,value); return FD_VOID;}
  else fd_raise_lisp_exception(fd_SetRequiresSymbol,"not a symbol",var);
}

/** Thread evaluation **/

#if FD_THREADS_ENABLED
struct EVAL_STATE {lisp in, *out; lispenv env;};
struct APPLY_STATE {lisp fcn, args, *out;};

pthread_t fd_thread_eval(lisp expr,lispenv env,lisp *result);

static void *thread_eval(void *tstate)
{
  struct EVAL_STATE *state=tstate;
  int normal_exit=1;
  WITH_HANDLING {
    fd_use_threadlocal_malloc();
    if (state->out)
      *(state->out)=fd_eval_in_env(state->in,state->env);
    else {
      lisp tmp=fd_eval_in_env(state->in,state->env); decref(tmp);}}
  ON_EXCEPTION {
    fd_exception ex=fd_theException();
    fd_u8char *details=fd_exception_details();
    fd_lisp irritant=fd_exception_object();
    normal_exit=0; 
    if (FD_VOIDP(irritant))
      if (details)
	fd_warn("Thread exited with error %s (%s)",ex,details);
      else fd_warn("Thread exited with error %s",ex);
    else if (details)
	fd_warn("Thread exited with error %s (%s) [%q]",ex,details,irritant);
    else fd_warn("Thread exited with error %s [%q]",ex,irritant);
    fd_clear_exceptions(); CLEAR_ERR();} 
  END_HANDLING;
  fd_free_env(state->env); decref(state->in); fd_xfree(state);
  return NULL;
}

static void *thread_apply(void *tstate)
{
  struct APPLY_STATE *state=tstate;
  int normal_exit=1;
  WITH_HANDLING {
    fd_use_threadlocal_malloc();
    if (state->out)
      *(state->out)=fd_apply(state->fcn,state->args);
    else {
      lisp tmp=fd_apply(state->fcn,state->args); decref(tmp);}}
  ON_EXCEPTION {
    fd_exception ex=fd_theException();
    fd_u8char *details=fd_exception_details();
    fd_lisp irritant=fd_exception_object();
    normal_exit=0; 
    if (FD_VOIDP(irritant))
      if (details)
	fd_warn("Thread exited with error %s (%s)",ex,details);
      else fd_warn("Thread exited with error %s",ex);
    else if (details)
	fd_warn("Thread exited with error %s (%s) [%q]",ex,details,irritant);
    else fd_warn("Thread exited with error %s [%q]",ex,irritant);
    fd_clear_exceptions(); CLEAR_ERR();}
  END_HANDLING;
  decref(state->fcn); decref(state->args); fd_xfree(state);
  return NULL;
}

pthread_t fd_thread_eval(lisp expr,lispenv env,lisp *result)
{
  pthread_t tid;
  struct EVAL_STATE *state=fd_xmalloc(sizeof(struct EVAL_STATE));
  state->in=incref(expr); state->out=result;
  state->env=fd_mallocd_env(env); 
  pthread_create(&tid,pthread_attr_default,thread_eval,state);
  return tid;
}

pthread_t fd_thread_apply(lisp fcn,lisp args,lisp *result)
{
  pthread_t tid;
  struct APPLY_STATE *state=fd_xmalloc(sizeof(struct APPLY_STATE));
  state->fcn=incref(fcn); state->args=incref(args); state->out=result;
  pthread_create(&tid,pthread_attr_default,thread_apply,state);
  return tid;
}

static lisp in_parallel(lisp expr,lispenv env)
{
  int i=0, n_exprs=fd_list_length(expr)-1, status;
  lisp final_result=FD_EMPTY_CHOICE;
  lisp *answers=fd_malloc(sizeof(lisp)*n_exprs);
  pthread_t *threads=fd_malloc(sizeof(pthread_t)*n_exprs);
  while (i < n_exprs) {
    lisp subexpr=fd_get_arg(expr,i+1,FD_VOID);
	answers[i]=FD_VOID;
    threads[i]=fd_thread_eval(subexpr,env,&(answers[i]));
    i++;}
  i=0; while (i < n_exprs) {
    pthread_join(threads[i],(void * *)&status); i++;}
  i=0; while (i < n_exprs) {
    ADD_TO_CHOICE(final_result,answers[i]); i++;}
  fd_free(threads,sizeof(pthread_t)*n_exprs);
  fd_free(answers,sizeof(lisp)*n_exprs);
  return final_result;
}

static lisp expand_lexpr(lisp args)
{
  lisp result=FD_EMPTY_CHOICE;
  if (PAIRP(args)) {
    DO_CHOICES(elt,CAR(args)) {
      lisp expansions=expand_lexpr(CDR(args));
      DO_CHOICES(expansion,expansions) {
	lisp partial=FD_MAKE_PAIR(incref(elt),incref(expansion));
	ADD_TO_CHOICE(result,partial);}
      END_DO_CHOICES;
      decref(expansions);}
    END_DO_CHOICES;
    return result;}
  else return incref(args);
}

static lisp mpcall_lexpr(lisp args)
{
  lisp expansion=expand_lexpr(args);
  if (FD_EMPTY_LISTP(args))
    fd_raise_detailed_exception(fd_TooFewArgs,"MPCALL");
  else {
    int n_choices=CHOICE_SIZE(expansion);
    if (n_choices == 1) {
      lisp result=fd_apply(CAR(expansion),CDR(expansion));
      decref(expansion);
      return result;}
    else {
      int i=0, n_choices=CHOICE_SIZE(expansion), status;
      lisp final_result=FD_EMPTY_CHOICE;
      lisp *answers=fd_malloc(sizeof(fd_lisp)*n_choices);
      pthread_t *threads=fd_malloc(sizeof(pthread_t)*n_choices);
      DO_CHOICES(choice,expansion) {
	answers[i]=FD_VOID;
	threads[i]=fd_thread_apply(CAR(choice),CDR(choice),&(answers[i]));
	i++;}
      END_DO_CHOICES;
      i=0; while (i < n_choices) {
	pthread_join(threads[i],(void * *)&status); i++;}
      i=0; while (i < n_choices) {
	ADD_TO_CHOICE(final_result,answers[i]); i++;}
      decref(expansion);
      fd_free(threads,sizeof(pthread_t)*n_choices);
      fd_free(answers,sizeof(lisp)*n_choices);
      return final_result;}}
}

static lisp spawn_threads(lisp expr,lispenv env)
{
  int i=0, n_exprs=fd_list_length(expr)-1;
  while (i < n_exprs) {
    lisp subexpr=fd_get_arg(expr,i+1,FD_VOID);
    fd_thread_eval(subexpr,env,NULL);
    i++;}
  return FD_VOID;
}

/** Mutex data structures **/

static void print_mutex_ptr(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#MUTEX 0x%lx]",CPTR_DATA(x));
}

static void free_mutex_ptr(lisp x)
{
  fd_mutex *m=(fd_mutex *)CPTR_DATA(x);
  if (m) {fd_free(m,sizeof(fd_mutex));}
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

static lisp lisp_make_mutex()
{
  fd_mutex *m=fd_malloc(sizeof(fd_mutex));
  fd_init_mutex(m);
  return fd_make_cptr(mutex_type,m);
}

/** Mutex special forms **/

static lisp lisp_with_mutex_locked(lisp expr,lispenv env)
{
  lisp mutex_arg=fd_get_arg(expr,1,FD_VOID), 
       lm=fd_eval_in_env(mutex_arg,env);
  lisp body=fd_get_body(expr,2), value=FD_VOID;
  fd_mutex *m;
  if (PRIM_TYPEP(lm,mutex_type)) m=(fd_mutex *)CPTR_DATA(lm);
  else fd_type_error(_("Not a mutex"),mutex_arg);
  lock_mutex(m);
  {DOLIST(subexpr,body) {
    decref(value); value=fd_eval_in_env(subexpr,env);}}
  unlock_mutex(m);
  decref(lm);
  return value;
}
#else
static lisp lisp_make_mutex()
{
  return FD_FALSE;
}
static lisp lisp_with_mutex_locked(lisp expr,lispenv env)
{
  lisp body=fd_get_body(expr,2), value=FD_VOID;
  {DOLIST(subexpr,body)
     {decref(value); value=fd_eval_in_env(subexpr,env);}}
  return value;
}
#endif

/** Initialization **/

void fd_initialize_threads_c()
{
  
#if FD_THREADS_ENABLED
  fd_new_tld_key(&threadenv_key,NULL);
#else
  threadenv=fd_make_hashtable(16);
#endif
  
#if FD_THREADS_ENABLED
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(mutex_type);
   r->gc_fcn=free_mutex_ptr;
   r->print_fcn=print_mutex_ptr;}
#endif

  fd_add_restricted_special_form("TSET!",tset_value_handler); 

#if (FD_THREADS_ENABLED)
  fd_add_special_form(NULL,"PARALLEL",in_parallel);
  fd_add_special_form(NULL,"SPAWN",spawn_threads);
  fd_add_lexpr(NULL,"MPCALL",FD_ND_LEXPR,mpcall_lexpr);
#else
  fd_add_alias(NULL,"PARALLEL","CHOICE");
#endif
  fd_add_cproc(NULL,"MAKE-MUTEX",0,lisp_make_mutex);
  fd_add_special_form(NULL,"WITH-MUTEX-LOCKED",lisp_with_mutex_locked);

  fd_register_source_file("threads",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: threads.c,v $
   Revision 1.14  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.13  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.12  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.11  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.10  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.9  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.8  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.7  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.6  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
