/* C Mode */

/* eval.c
   The FDScript evaluator for FramerD
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

static char vcid[] = "$Id: eval.c,v 1.30 2005/01/14 16:48:44 haase Exp $";

/** Headers **/
/** Declarations and Prototypes **/
/** Safety/security definitions **/
/** Support for tail calls **/
/** Utility functions **/
/** Extended arguments **/
/** Tracking goals **/
/** Tracing the evaluator **/
/** Stack checking **/
/** Modifying symbol values **/
/** Error/trace reporting **/
/** Reporting applications for tracing and debugging **/
/** The Evaluator **/
/** FDScript primitive procedures (CPROCs) **/
/** Applying C Primitives **/
/** Applying interpreted procedures **/
/** Scheme Special forms **/
/** Conditional expressions **/
/** Sequencing and binding **/
/** Continuations **/
/** Unwind protect **/
/** Error handling special forms **/
/** Returning errors and exceptions **/
/** Remote evaluation **/
/** APPLY **/
/** LIST and backquote **/
/** vector->list and list->vector (used  by backquote) **/
/** Iteration: MAP and FOR-EACH **/
/** Iteration expressions: DO **/
/** Iteration expressions: DOTIMES, DOLIST, etc **/
/** Iteration expressions: WHILE and UNTIL **/
/** Using a symbol value as a cache **/
/** Read-Eval-Print loop and friends **/
/** Using the console **/
/** Using the console without readline **/
/** External read-eval-print functions **/
/** Watching the evaluation of particular forms **/
/** Autoload, Autolink, and Use-Server references **/
/** Using dynamic libraries **/
/** Getting the configuration file **/
/** Initializing the evaluator **/

#include "fdeval.h"
#include <stdarg.h>

#ifdef WIN32
#define DIRSEP "\\"
#else
#define DIRSEP "/"
#endif

/** Headers **/

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
# else
# include <time.h>
# endif
#endif

#if ((FD_USING_DLLS) && (HAVE_DLFCN_H))
#include <dlfcn.h>
#endif

#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

static int default_debug_fdscript=0;
static void load_dll(lisp module_name);

#if defined(NeXT)
#define CLOCKS_PER_SEC CLK_TCK
#endif

#ifndef PATH_MAX
#define PATH_MAX 512
#endif


/** Declarations and Prototypes **/

/* Exception definitions */

fd_exception fd_SyntaxShortExpr=_("Not enough subexpressions"),
  fd_SyntaxError=_("Special form syntax error"),
  fd_TooManyArgs=_("Too many arguments"),
  fd_TooFewArgs=_("Too few arguments"),
  fd_WeirdArgs=_("Weird args"),
  fd_UnboundVariable=_("Variable is unbound"),
  fd_UnboundFunction=_("Function variable is unbound"),
  fd_BadArgList=_("Malformed argument list"),
  fd_BadLambda=_("Malformed lambda expression"),
  fd_StackOverflow=_("Stack Overflow"),
  fd_DLLError=_("Error when loading DLL"),
  fd_NoSpecialFormApply=_("Can't apply a special form"),
  fd_IntOverflow=_("integer operation has overflowed"),
  fd_FlonumOverflow=_("floating point operation has overflowed");

fd_exception fd_NotAFunction=_("Cannot find function"),
             fd_FilenameMustBeString=_("The filename must be a string"),
             fd_NoSuchFile=_("No such file (couldn't open it)"),
             fd_SetRequiresSymbol=_("SET! needs a symbol");

/* Symbols used by the evaluator */

static lisp define_symbol, lambda_symbol, quote_symbol;
static lisp macro_symbol, ppwidth_symbol;
static lisp autolink_symbol, autoload_symbol, use_server_symbol;
static lisp debug_fdscript_symbol, toplevel_symbol;

#if FD_THREADS_ENABLED
fd_mutex symbol_change_mutex;
#endif

FRAMERD_EXPORT
/* fd_mv_arity:
     Arguments: a lisp pointer
     Returns: the number of values represented by the pointer
*/
int fd_mv_arity(lisp x)
{
  if (!(PRIM_TYPEP(x,multiple_value_type))) return 1;
  else {
    lisp_vector v=PTR_DATA(x,vector);
    return v->length;}
}

FRAMERD_EXPORT
/* fd_mv_ref:
     Arguments: a lisp pointer and an int
     Returns: the ith value represented by the pointer or FD_VOID otherwise
*/
fd_lisp fd_mv_ref(lisp x,unsigned int i)
{
  if (!(PRIM_TYPEP(x,multiple_value_type)))
    if (i == 0) return incref(x);
    else return FD_VOID;
  else {
    lisp_vector v=PTR_DATA(x,vector);
    if (i < v->length) return incref(v->elements[i]);
    else return FD_VOID;}
}

FRAMERD_EXPORT
/* fd_mv_return:
     Arguments: a pointer to an array of lisp pointers and a int length
     Returns: a multiple value object whose elements are the elements
      of the array (copied into a new array)
*/
fd_lisp fd_mv_return(lisp *x,int len)
{
  struct FD_VECTOR *vec=fd_malloca(struct FD_VECTOR); int i=0;
  fd_lisp *read=x, *limit=x+len;
  fd_lisp *write=vec->elements=fd_malloc(sizeof(fd_lisp)*len);
  vec->length=len; while (read < limit) {
    *write=fd_incref(*read); read++; write++;}
  {RETURN_LISP(multiple_value_type,vector,vec);}
}

STATIC_INLINE lisp sv(lisp x)
{
  if (PRIM_TYPEP(x,multiple_value_type)) {
    lisp v=fd_mv_ref(x,0); decref(x);
    return v;}
  else if (PRIM_TYPEP(x,quoted_choice_type)) {
    RETURN_LISP(choice_type,choice,PTR_DATA(x,choice));}
  else if (FD_QUOTED_EMPTY_CHOICEP(x)) return FD_EMPTY_CHOICE;
  else return x;
}

STATIC_INLINE lisp firstv(lisp x)
{
  if (PRIM_TYPEP(x,multiple_value_type))
    return fd_mv_ref(x,0);
  else if (PRIM_TYPEP(x,quoted_choice_type)) {
    incref(x);
    {RETURN_LISP(choice_type,choice,PTR_DATA(x,choice));}}
  else if (FD_QUOTED_EMPTY_CHOICEP(x)) return FD_EMPTY_CHOICE;
  else return incref(x);
}

/** Support for tail calls **/

static lisp start_eval(lisp expr,lispenv env);

#define finish_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x);
#define discard_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x); \
  decref(x);

/** Utility functions **/

/* Gets the tail of expr starting at start (where 0 gets the whole list) */
FRAMERD_EXPORT
lisp fd_get_body(lisp expr,int start)
{
  return get_body(expr,start);
}
/* Gets the element of expr at start (where 0 gets the first element) */
FRAMERD_EXPORT
lisp fd_get_arg(lisp expr,int start,lisp dflt)
{
  return get_arg(expr,start,dflt);
}
/* Gets the args from a lexpr */
FRAMERD_EXPORT
void fd_get_args(fd_u8char *name,fd_lisp arglist,...)
{
  int i=0, len=fd_list_length(arglist); va_list args; 
  va_start(args,arglist); 
  while (1) {
    fd_lisp *v=va_arg(args,fd_lisp *), dflt;
    if (v == NULL) break;
    else dflt=va_arg(args,fd_lisp);
    if ((FD_VOIDP(dflt)) && (i >= len)) {
      va_end(args);
      fd_raise_lisp_exception(fd_TooFewArgs,name,arglist);}
    else *v=get_arg(arglist,i,dflt); i++;}
  if (len > i)
    fd_raise_lisp_exception(fd_TooManyArgs,name,arglist);
}

/* Inline to reduce the hit for the standard (single value) case */
FASTOP lisp return_proper_choice(lisp values)
{
  if (PRIM_TYPEP(values,choice_type))
    return fd_return_proper_choice(values);
  else return values;
}

static lisp eval_body(lisp exprs,lispenv env)
{
  lisp final=FD_VOID;
  DOLIST(expr,exprs) {
    discard_value(final); final=start_eval(expr,env);}
  return final;
}

static int verbose_debuggingp()
{
  if (FD_VOIDP(SYMBOL_VALUE(debug_fdscript_symbol)))
    return default_debug_fdscript;
  else if (FD_FALSEP(SYMBOL_VALUE(debug_fdscript_symbol)))
    return 0;
  else return 1;
}

/* Gets the pretty print width */
static int get_ppwidth()
{
  lisp ppwidth=fd_thread_symeval(ppwidth_symbol);
  if (FIXNUMP(ppwidth)) return FIXLISP(ppwidth);
  else {decref(ppwidth); return 80;}
}

static fd_server get_connection(lisp host)
{
  fd_server s;
  if (PRIM_TYPEP(host,dtype_server_type))
    s=(fd_server)CPTR_DATA(host);
  else if (SYMBOLP(host)) {
    lisp real_host=fd_getenv(SYMBOL_NAME(host));
    if (!(FD_EMPTYP(real_host))) host=real_host;}
  else if (STRINGP(host)) return fd_connect(STRING_DATA(host));
  else if (PRIM_TYPEP(host,pool_type))
    if (((fd_pool)(CPTR_DATA(host)))->type == network_pool)
      s=((fd_network_pool)(CPTR_DATA(host)))->conn;
    else fd_ctype_error("get_connection",_("Not a connection"),host);
  else if (PRIM_TYPEP(host,index_type))
    if (((fd_index)(CPTR_DATA(host)))->type == network_index)
      s=(((fd_network_index)(CPTR_DATA(host)))->conn);
    else fd_ctype_error("get_connection",_("Not a connection"),host);
  if (s) {s->ref_count++; return s;}
  else fd_raise_detailed_exception
    ("Invalid host specification",fd_object_to_string(host));
}

/** Extended arguments **/

FRAMERD_EXPORT
lisp fd_get_extended_arg(char *argname,lisp argv,lisp dflt)
{
  lisp keyword=fd_make_symbol(argname);
  while (PAIRP(argv)) argv=CAR(argv);
  if (VECTORP(argv)) {
    DOTIMES(i,VECTOR_LENGTH(argv)) {
      if ((PAIRP(VECTOR_REF(argv,i))) &&
	  (LISP_EQ(CAR(VECTOR_REF(argv,i)),keyword)))
	return incref(CDR(VECTOR_REF(argv,i)));}
    return incref(dflt);}
  else return incref(dflt);
}

/** Tracking goals **/

static lisp goals_symbol;

static int add_goal(lisp call)
{
  lisp goals=fd_thread_symeval(goals_symbol), state;
  fd_hashtable table;
  if (PRIM_TYPEP(goals,hashtable_type))
    table=(fd_hashtable)CPTR_DATA(goals);
  else {
    fd_decref(goals);
    table=fd_make_hashtable(16);
    goals=fd_make_cptr(hashtable_type,table);
    fd_thread_symbind(goals_symbol,goals);}
  state=fd_hashtable_get(table,call,FD_VOID);
  decref(goals); /* Won't go away because of threadenv pointer */
  if (FD_TRUEP(state))
    return 1;
  else {
    fd_decref(state);
    fd_hashtable_set(table,call,FD_TRUE);
    return 0;}
}

static void finish_goal(lisp call)
{
  lisp goals=fd_thread_symeval(goals_symbol);
  fd_hashtable table;
  if (PRIM_TYPEP(goals,hashtable_type))
    table=(fd_hashtable)CPTR_DATA(goals);
  else {
    fd_decref(goals);
    table=fd_make_hashtable(16);
    goals=fd_make_cptr(hashtable_type,table);
    fd_thread_symbind(goals_symbol,goals);}
  decref(goals); /* Won't go away */
  fd_hashtable_set(table,call,FD_FALSE);
}

/** Tracing the evaluator **/

static int trace_fdscript=0, trace_prune=0;

static lisp lisp_trace_fdscript(lisp flag)
{
  int old=trace_fdscript;
  if (FD_FALSEP(flag)) trace_fdscript=0; else trace_fdscript=1;
  if (old) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_trace_prune(lisp flag)
{
  int old=trace_prune;
  if (FD_FALSEP(flag)) trace_prune=0; else trace_prune=1;
  if (old) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_traced_handler(volatile lisp expr,lispenv env)
{
  volatile lisp value=FD_EMPTY_CHOICE;
  int oldf=trace_fdscript, oldp=trace_prune;
  UNWIND_PROTECT {
    trace_fdscript=1; trace_prune=1;
    value=fd_evaluate(fd_get_arg(expr,1,FD_VOID),env);}
  ON_UNWIND {
    trace_fdscript=oldf; trace_prune=oldp;}
  END_UNWIND;
  return value;
}

FRAMERD_EXPORT
lisp fd_trace_fdscript(int flag)
{
  int old=trace_fdscript;
  trace_fdscript=flag;
  if (old) return FD_TRUE; else return FD_FALSE;
}

FRAMERD_EXPORT
lisp fd_trace_prune(int flag)
{
  int old=trace_prune;
  trace_prune=flag;
  if (old) return FD_TRUE; else return FD_FALSE;
}

/** Stack checking **/

/* This seems to work, but I don't really trust it completely. */

static unsigned int stack_limit=250000;
static unsigned int stack_grows_up=0;

FRAMERD_EXPORT
void fd_set_stack_limit(unsigned int limit)
{
  unsigned int stack_max=1000000;
#ifdef RLIMIT_STACK
  struct rlimit slimit;
  getrlimit(RLIMIT_STACK,&slimit);
  stack_max=slimit.rlim_cur;
#endif
  if (stack_max == 0) {
    fd_warn(_("Can't determine stack limit"));
    stack_max=1000000;}
  if (limit == 0) limit=(stack_max*5)/8;
  if (limit > (7*stack_max)/8)
    fd_raise_exception(_("Stack limit is too high"));
  else stack_limit=limit;
}

static lisp lisp_set_stack_limit_cproc(lisp arg)
{
  if (FD_FALSEP(arg)) stack_limit=0;
  else if (FD_TRUEP(arg)) return LISPFIX(stack_limit);
  else if (FIXNUMP(arg))
    if (FIXLISP(arg) <= 0)
      fd_raise_exception(_("Stack limit must be positive"));
    else {
      if (FIXLISP(arg) < 5000) fd_warn(_("That's a pretty small stack"));
      fd_set_stack_limit(FIXLISP(arg));
      return arg;}
  else fd_type_error(_("invalid stack limit"),arg);
  return incref(arg);
}

static void *stack_base=NULL;

#if FD_THREADS_ENABLED
static fd_tld_key stack_base_key;

FASTOP void check_stack(void *ptr)
{
  void *stack_base=fd_tld_get(stack_base_key);
  if (stack_base) {
    if (stack_grows_up) { 
      if (((unsigned long)ptr) > ((unsigned long)stack_base)) {
	if ((((unsigned long)ptr)-((unsigned long)stack_base)) > stack_limit)
	  fd_raise_exception(fd_StackOverflow);
	else return;}
      else {}}
    else if (((unsigned long)ptr) < ((unsigned long)stack_base)) {
      if ((((unsigned long)stack_base)-((unsigned long)ptr)) > stack_limit)
	fd_raise_exception(fd_StackOverflow);
      else return;}
    else {}}
  fd_tld_set(stack_base_key,ptr);
}

#else

FASTOP check_stack(void *ptr)
{
  if (stack_base) {
    if (stack_grows_up) {
      if (((unsigned long)ptr) > ((unsigned long)stack_base)) 
	if ((((unsigned long)ptr)-((unsigned long)stack_base)) > stack_limit)
	  fd_raise_exception(fd_StackOverflow);
	else return;
      else {}}
    else if (((unsigned long)ptr) < ((unsigned long)stack_base)) 
      if ((((unsigned long)stack_base)-((unsigned long)ptr)) > stack_limit)
	fd_raise_exception(fd_StackOverflow);
      else return;
    else {}}
  stack_base=ptr;
}
#endif

static void init_stack_base(void *ptr)
{
  stack_base=ptr;
}

static void check_stack_direction()
{
  int temp=0;
  if (((void *)&temp) > stack_base) stack_grows_up=1;
  else stack_grows_up=0;
  stack_base=NULL;
}

/** Error/trace reporting **/

static void report_eval(lisp expr)
{
  fd_exprintf(_(";; During the evaluation of \n  %Q\n"),expr);
}

static void report_expansion(lisp expansion,lisp expr)
{
  fd_exprintf(_(";; Expanding macro %Q\n"),expr);
  fd_exprintf(_(";; Yielded expr %Q\n"),expansion);
}

static void report_prune(lisp expr)
{
  fd_exprintf(_(";; Pruned %Q\n"),expr);
}

static void report_value(lisp expr,lisp value)
{
  if (PRIM_TYPEP(value,tail_call_type)) {
    lisp_vector v=PTR_DATA(value,vector);
    int i=0, limit=v->length;
    fd_exprintf(_(";;> Evaluating %Q leads to the application of:\n"),expr);
    while (i < limit) {
      fd_exprintf(";;>   %Q\n",v->elements[i]); i++;}}
  else fd_exprintf(_(";;> Evaluating \n    %Q\n;;>  yielded:\n;;> %q\n"),
		  expr,value);
}

static void report_yield(lisp value)
{
  if (PRIM_TYPEP(value,tail_call_type)) {
    lisp_vector v=PTR_DATA(value,vector);
    int i=0, limit=v->length;
    fd_exprintf(_(";;> Reduces to the application of:\n"));
    while (i < limit) {
      fd_exprintf(";;>   \n     %Q\n",v->elements[i]); i++;}}
  else fd_exprintf(_(";;> Yielded:\n;;> %Q\n"),value);
}

static void describe_error(char *verb,fd_string_stream xio)
{
  char *errstring;
  if (errno) errstring=strerror(errno); else errstring="";
  if (xio)
    fd_printf(xio,_(";;! Unexpected problem `%m' %q (%m) [%s]\n;;   while %s\n"),
	      fd_theException(),fd_exception_object(),
	      fd_exception_details(),errstring,verb);
  else fd_fprintf(stderr,_(";;! Exception %m %q (%m) [%s]\n;;   while %s:\n"),
		  fd_theException(),fd_exception_object(),
		  fd_exception_details(),errstring,verb);
  fd_xio_update();
}

/** Reporting applications for tracing and debugging **/

static void describe_prim_args(char *verb,char *name,lisp *args,int n_args)
{
  struct FD_STRING_STREAM *xio=fd_get_xio(); 
  fd_exception ex=fd_theException();
  int i=0;
  if (ex) {
    char buf[256]; sprintf(buf,"%s primitive %s",verb,name);
    describe_error(buf,xio);}
  else if (xio) fd_printf(xio,_(";; %s %s to:\n"),verb,name);
  else fd_fprintf(stderr,_(";; %s %s to:\n"),verb,name);
  while (i < n_args) {
    if (xio) fd_printf(xio,_(";;>   prim arg #%d = %Q\n"),i,args[i]);
    else fd_fprintf(stderr,_(";;>   prim arg #%d = %Q\n"),i,args[i]);
    i++;}
  fd_xio_update();
}

static void describe_env(fd_lispenv env)
{
  while (env) {
    int i=0, limit;
    if (env->mallocd) env=env->mallocd;
    limit=env->n_bindings;    
    if (env->rib) while (i < limit) {
      fd_exprintf(";;>     %s=%Q\n",
		 SYMBOL_NAME(env->rib[i].var),
		 (env->rib[i].val));
      i++;}
    else if (limit)
      fd_raise_exception("Screwed up environment structure");
    if (env == env->parent) break;
    else env=env->parent;}
}

static void describe_application
   (char *verb,lisp lambda,lispenv env,int n_bindings)
{
  struct FD_STRING_STREAM *xio=fd_get_xio();
  fd_exception ex=fd_theException();
  if (ex) describe_error(verb,xio);
  else if (xio) fd_printf(xio,";;> %m\n",verb);
  else fd_fprintf(stderr,";;> %m\n",verb);
  if (FD_XPROCP(lambda)) {
    fd_sproc s=FD_GET_SPROC(lambda); lambda=s->lambda;}
  if (xio) fd_printf(xio,"     ",verb);
  else fd_fprintf(stderr,"     ",verb);
  if (xio) {
    fd_pprint_lisp_to_string(lambda,xio,5,5,get_ppwidth());
    fd_sputc(xio,'\n');}
  else {
    struct FD_STRING_STREAM out; FD_INITIALIZE_STRING_STREAM(&out,1024); 
    fd_pprint_lisp_to_string(lambda,&out,5,5,get_ppwidth());
    fd_fprintf(stderr,"%m\n",out.ptr); fd_xfree(out.ptr);}
  describe_env(env);
  fd_xio_update();
}

/** The Evaluator **/

static lisp apply_cproc
   (lisp func,lisp expr,lispenv env,int trace);
static lisp apply_sproc(lisp func,lisp expr,lispenv env);

/* fd_eval_in_env:
   start_eval:
     Arguments: a lisp expression and a pointer to a lisp
      environment structure
     Returns: a lisp value
   Evaluates an expression in an environment.
   Note that the return value will need to be
    explicitly freed if it is not returned further.
   The argument expr is not used up.
   The difference between start_eval and fd_eval_in_env is that
    start_eval may return a tail call.
 */
static lisp start_eval(lisp expr,lispenv env)
{
  check_stack(&expr);
  if (SYMBOLP(expr)) {
    lisp v=fd_symeval(expr,env);
    if (FD_VOIDP(v))
      fd_raise_lisp_exception(fd_UnboundVariable,_("in EVAL"),expr);
    else return v;}
  else if (ATOMICP(expr)) return expr;
  else if (CHOICEP(expr)) {
    int eval_the_choice=0;
    DO_CHOICES(e,expr) {
      if ((PAIRP(e)) || (SYMBOLP(e))) eval_the_choice=1;}
    END_DO_CHOICES;
    if (eval_the_choice) {
      lisp result=FD_EMPTY_CHOICE;
      DO_CHOICES(e,expr) {
	lisp v=fd_eval_in_env(e,env);
	ADD_TO_CHOICE(result,v);}
      END_DO_CHOICES;
      return result;}
    else return incref(expr);}
  else if (!(PAIRP(expr))) return incref(expr);
  else {
    lisp header=CAR(expr), proc, v;
    /* Get the value of the CAR */
    proc=sv(fd_evaluate(header,env));
    /* Do any autoloading if neccessary */
    if ((PRIM_TYPEP(proc,cproc_type)) || (FD_XPROCP(proc))) {}
    else if (PRIM_TYPEP(proc,dtype_server_type)) {
      v=fd_make_rproc((fd_server)CPTR_DATA(proc),header);
      fd_decref(proc);
      proc=v;}
    else if ((PAIRP(proc)) && (PAIRP(CDR(proc)))) {
      if (LISP_EQ(CAR(proc),autoload_symbol)) {
	fd_load_file(fd_strdata(CAR(CDR(proc))),NULL,NULL); 
	v=fd_symeval(header,env);
	fd_decref(proc);
	proc=v;}
      else if (LISP_EQ(CAR(proc),autolink_symbol)) {
	load_dll(CAR(CDR(proc))); 
	v=fd_symeval(header,env);
	fd_decref(proc);
	proc=v;}
      else if (LISP_EQ(CAR(proc),use_server_symbol)) {
	fd_server s=get_connection(CAR(CDR(proc)));
	if (PAIRP(CDR(CDR(proc))))
	  v=fd_make_rproc(s,CAR(CDR(CDR(proc))));
	else v=fd_make_rproc(s,header);
	fd_decref(proc);
	proc=v;}}
    /* Figure out what to do with proc */
    if (FD_VOIDP(proc))
      fd_raise_lisp_exception(fd_UnboundFunction,"in EVAL",header);
    else if (CHOICEP(proc)) {
      lisp params=fd_eval_elts(CDR(expr),env);
      lisp value=fd_do_application(proc,params);
      decref(params); decref(proc);
      return return_proper_choice(value);}
    else if (PRIM_TYPEP(proc,cproc_type))
      return apply_cproc(proc,expr,env,0);
    else if ((FD_XPROCP(proc)))
      return apply_sproc(proc,expr,env);
    else if (PRIM_TYPEP(proc,rproc_type))
      return fd_dtapply(proc,get_body(expr,1),env,1);
    /* Continuations are implemented by benign exceptions */
    else if (PRIM_TYPEP(proc,continuation_type)) {
      char *name=(char *)CPTR_DATA(proc);
      lisp arg=fd_evaluate(get_arg(expr,1,FD_VOID),env);
      decref(proc);
      fd_throw(name,NULL,arg);}
    else if ((PAIRP(proc)) && (LISP_EQ(CAR(proc),lambda_symbol))) {
      lisp sproc=
	fd_make_sproc
	(FD_MAKE_PAIR(FD_EMPTY_CHOICE,incref(CDR(proc))),NULL);
      decref(proc);
      return apply_sproc(sproc,expr,env);}
    else if ((PAIRP(proc)) && (LISP_EQ(CAR(proc),macro_symbol))) {
      lisp expansion=FD_VOID, value=FD_VOID;
      lisp macro_body=fd_get_body(proc,2);
      lisp macro_var=fd_get_arg(fd_get_arg(proc,1,FD_VOID),0,FD_VOID);
      FD_WITH_LEXICAL_ENV(macro_env,env,1) {
	fd_bind_value(macro_var,expr,macro_env);
	expansion=eval_body(macro_body,macro_env);}
      FD_END_WITH_LEXICAL_ENV(expansion);
      if (trace_fdscript) report_expansion(expr,expansion);
      value=fd_eval_in_env(expansion,env); decref(expansion);
      return value;}
    else if (OIDP(proc)) {
      /* Might not belong in evaluator, this does a frame modification */
      fd_import_frame(proc,CDR(expr),1);
      return proc;}
    else fd_raise_lisp_exception
	   (fd_NotAFunction,fd_object_to_string(proc),expr);}
}
FRAMERD_EXPORT
/* fd_start_eval
     Arguments: a lisp expression and a lisp environment
     Returns: a lisp value, possibly a tail call
 This starts the evaluation process which can be finished
 with fd_finish_value */
lisp fd_start_eval(lisp expr,lispenv env)
{
  return start_eval(expr,env);
}
FRAMERD_EXPORT
/* fd_eval_in_env
     Arguments: a lisp expression and a lisp environment
     Returns: a lisp value, never a tail call
 This starts and finishes the evaluation process.
 It returns a value which may need to be freed but does
  not use up the value it is passed.
*/
lisp fd_eval_in_env(lisp expr,lispenv env)
{
  lisp v=FD_VOID;
  WITH_HANDLING {
    v=start_eval(expr,env);
    finish_value(v);}
  ON_EXCEPTION {
    if (((strcmp(fd_theException(),"CONTINUATION")) != 0) &&
	(verbose_debuggingp())) {
      char *errstring;
      if (errno) errstring=strerror(errno); errstring="";
      fd_exprintf
	(_(";;! Exception %m %q (%m) [%s]\n;;   while evaluating\n   %Q\n"),
	 fd_theException(),fd_exception_object(),
	 fd_exception_details(),errstring,expr);
      describe_env(env);}
    fd_reraise();}
  END_HANDLING;
  return v;
}

FRAMERD_EXPORT
/* fd_eval:
    Arguments: an expression
    Returns: a lisp pointer
  Evaluates an expression in the top level environment.
  It's argument is not gc'd.
*/
lisp fd_eval(lisp expr) { return fd_eval_in_env(expr,NULL); }

FRAMERD_EXPORT
/* fd_evalstring:
    Arguments: a string
    Returns: a string
  Evaluates an expression in the top level environment.
*/
char *fd_evalstring(char *input,lispenv env)
{
  lisp expr=fd_parse_string(input);
  lisp value=fd_eval_in_env(expr,env);
  char *valuestring=fd_object_to_string(value);
  decref(expr); decref(value);
  return valuestring;
}

FRAMERD_EXPORT
/* fd_streval:
    Arguments: a string
    Returns: a string
  Evaluates an expression in the top level environment.
*/
char *fd_streval(char *input)
{
  lisp expr=fd_parse_string(input);
  lisp value=fd_eval_in_env(expr,NULL);
  struct FD_STRING_STREAM output;
  FD_INITIALIZE_STRING_STREAM(&output,1024);
  {DO_CHOICES(v,value) fd_printf(&output,"%q\n",v); END_DO_CHOICES;}
  return output.ptr;
}

/* Called when evaluating arguments */
FASTOP lisp inline_eval(lisp expr,lispenv env)
{
  if (PAIRP(expr))
    return (fd_eval_in_env(expr,env));
  else if (SYMBOLP(expr)) {
    lisp v=fd_symeval(expr,env);
    if (FD_VOIDP(v))
      fd_raise_lisp_exception(fd_UnboundVariable,"EVAL",expr);
    else return v;}
  else return incref(expr);
}

FASTOP lisp eval_elts(lisp lst,lispenv env)
{
  if (FD_EMPTY_LISTP(lst)) return lst;
  else {
    lisp head=FD_MAKE_LIST1(inline_eval(CAR(lst),env)), tail=head;
    lisp scan=CDR(lst); while (PAIRP(scan)) {
      lisp new=FD_MAKE_LIST1(inline_eval(CAR(scan),env));
      RPLACD(tail,new); tail=new; scan=CDR(scan);}
    return head;}
}

FRAMERD_EXPORT
/*  fd_eval_elts:
       Arguments: a list of expressions
       Returns: a list of objects
 Returns the result of evaluating each element of a list in an
 environment.  Actual implementation is above. */
lisp fd_eval_elts(lisp lst,lispenv env)
{
  return eval_elts(lst,env);
}

/** FDScript primitive procedures (CPROCs) **/

FRAMERD_EXPORT
/* fd_add_cproc:
    Arguments: a null terminated string, an argument count,
               and a pointer to a C function
    Returns: nothing (void)
  Defines a new FDScript primitive with a name, a number of
   arguments and a C implementation.  Note that if the number
   of arguments is negative, it is taken to be a lexpr or special form,
   thought fd_add_special_form is the preferred way to declare such
   primitives.  A special form's implementation function is
   called on the expression being evaluated and its environment,
   without any further processing of the expression.
  fd_add_cproc also declares the symbol to be "safe" for
   evaluation.
*/
void fd_add_cproc(fd_lispenv env,char *name,int n_args,lisp (*proc)())
{
  fd_cproc p=fd_malloca(struct FD_CPROC); lisp symbol, record;
  p->n_refs=1; p->name=name; p->n_args=n_args; p->direct_call=1;
  p->func=(fd_lisp (*)(void))proc; 
  if (n_args < 0) fd_warn(_("Obsolete use of fd_add_cproc: %s"),name);
  record.type=cproc_type; record.data.cproc=p;
  symbol=fd_make_symbol(name);
  fd_bind_value(symbol,record,env);
  fd_decref(record);
  if ((env) && (env->module))
    fd_hashset_add(&(env->module->exports),symbol);
}

static void free_cproc(lisp x)
{
  fd_cproc p=PTR_DATA(x,cproc);
  fd_qfree(p,sizeof(struct FD_CPROC));
}

static void print_cproc(lisp x,fd_string_stream ss)
{
  fd_cproc p=PTR_DATA(x,cproc);
  if (p->n_args == FD_SPECIAL_FORM) 
    fd_printf(ss,"[#SPECFORM \"%s\"]",p->name);
  else if (p->n_args == FD_NORMAL_LEXPR)
    fd_printf(ss,"[#CLEXPR \"%s\"]",p->name);
  else if (p->n_args == FD_ND_LEXPR)
    fd_printf(ss,"[#CNDLEXPR \"%s\"]",p->name);
  else fd_printf(ss,"[#CPROC \"%s\" %d]",p->name,p->n_args);
}

static void print_env(lisp x,fd_string_stream ss)
{
  fd_lispenv e=(fd_lispenv)CPTR_DATA(x);
  if (e->rib) fd_printf(ss,"#<ENVIRONMENT %ld>",(unsigned long)e);
  else if (e->module) fd_printf(ss,"#<PACKAGE %ld>",(unsigned long)e);
  else fd_printf(ss,"#<WEIRD ENVIRONMENT %ld>",(unsigned long)e);
}

static void free_env(fd_lisp x)
{
  fd_lispenv e=(fd_lispenv)CPTR_DATA(x);
  if (e->mallocd) fd_free_env(e);
  fd_qfree(FD_PTR_DATA(x, cptr), sizeof(struct FD_CPTR));
}

FRAMERD_EXPORT
/* fd_add_special_form:
    Arguments: a null terminated string and a pointer to a C function
    Returns: nothing (void)
  Defines a new FDScript special form.  Whenever a form whose head is
   the specified symbol is evaluated, the declared function is called
   on the form and the environment of evaluation.
  fd_add_special_form also declares the symbol to be "safe" for
   evaluation. */
void fd_add_special_form
  (fd_lispenv env,char *name,lisp (*proc)(lisp expr,lispenv env))
{
  fd_cproc p=fd_malloca(struct FD_CPROC); lisp symbol, record;
  p->n_refs=1; p->name=name; p->n_args=FD_SPECIAL_FORM; p->direct_call=1;
  p->func=(fd_lisp (*)(void))proc; 
  record.type=cproc_type; record.data.cproc=p;
  symbol=fd_make_symbol(name);
  fd_bind_value(symbol,record,env);
  fd_decref(record);
  if ((env) && (env->module))
    fd_hashset_add(&(env->module->exports),symbol);
}

FRAMERD_EXPORT
/* fd_add_lexpr:
    Arguments: a null terminated string and a pointer to a C function
    Returns: nothing (void)
  Defines a new FDScript primitive which accepts any number of arguments.
  The C function is called on a single lisp object, pointing to a list of
  arguments.  Note that any non-deterministic arguments are represented as
  sets.
  fd_add_lexpr also declares the symbol to be "safe" for
   evaluation. */
void fd_add_lexpr
  (fd_lispenv env,char *name,int argcode,lisp (*proc)(lisp args))
{
  fd_cproc p; lisp symbol, record;
  if (!((argcode == FD_NORMAL_LEXPR) || (argcode == FD_ND_LEXPR)))
    fd_raise_exception("Invalid lexpr argcode");
  p=fd_malloca(struct FD_CPROC);
  p->n_refs=1; p->name=name; p->n_args=argcode; p->direct_call=1;
  p->func=(fd_lisp (*)(void))proc; 
  record.type=cproc_type; record.data.cproc=p;
  symbol=fd_make_symbol(name);
  fd_bind_value(symbol,record,env);
  fd_decref(record);
  if ((env) && (env->module))
    fd_hashset_add(&(env->module->exports),symbol);
}

FRAMERD_EXPORT
/* fd_add_alias:
    Arguments: a null terminated string, an argument count,
               and a pointer to a C function
    Returns: nothing (void)
  Defines a new FDScript primitive which is the same as another
existing primitive.  This saves a little consing in that it simply
keeps a pointer to the primitive struct rather than making a new one.
It inherits the safety information of the procedure it is aliasing.
*/
void fd_add_alias(fd_lispenv env,char *alias,char *name)
{
  lisp original=fd_make_symbol(name), new=fd_make_symbol(alias);
  lisp value=fd_symeval(original,env);
  fd_bind_value(new,value,env);
  fd_decref(value);
  if ((env) && (env->module))
    fd_hashset_add(&(env->module->exports),new);
}

/** Applying C Primitives **/

static lisp nd_prim_apply(fd_cproc p,lisp *args,int n_args);

/* Applies a special form */
static lisp apply_special_form(fd_cproc p,lisp expr,lispenv env)
{
  lisp result=FD_VOID;
  WITH_HANDLING 
    if (trace_fdscript) {
      result=((lisp (*)(lisp, lispenv)) p->func)(expr,env);
      report_value(expr,result);}
    else result=((lisp (*)(lisp, lispenv)) p->func)(expr,env);
  ON_EXCEPTION 
    if ((strcmp(fd_theException(),"CONTINUATION")) != 0) {
      TIDY_ERRNO(p->name);
      if (verbose_debuggingp()) {
	report_eval(expr);
	describe_error("evaluating special form",fd_get_xio());}
      fd_reraise();}
    else fd_reraise();
  END_HANDLING;
  TIDY_ERRNO(p->name);
  return result;
}

FASTOP lisp reverse_list(lisp lst)
{
  lisp answer=FD_EMPTY_LIST;
  DOLIST(elt,lst)
    answer=FD_MAKE_PAIR(incref(elt),answer);
  return answer;
}

static void apply_normal_lexpr_helper
  (lisp (*fcn)(lisp),lisp d_args,lisp nd_args,lisp *result)
{
  if (FD_EMPTY_LISTP(nd_args)) {
    lisp v=fcn(d_args);
    finish_value(v);
    ADD_TO_CHOICE(*result,v);}
  else {
    DO_CHOICES(each,CAR(nd_args)) {
      lisp temp=FD_MAKE_PAIR(firstv(each),incref(d_args));
      UNWIND_PROTECT
	apply_normal_lexpr_helper(fcn,temp,CDR(nd_args),result);
      ON_UNWIND
	decref(temp);
      END_UNWIND;}
    END_DO_CHOICES;}
}

static lisp apply_normal_lexpr(lisp (*fcn)(lisp),lisp args)
{
  lisp reversed=reverse_list(args);
  lisp result=FD_EMPTY_CHOICE;
  UNWIND_PROTECT
    apply_normal_lexpr_helper(fcn,FD_EMPTY_LIST,reversed,&result);
  ON_UNWIND
    decref(reversed);
  END_UNWIND;
  return result;
}

/* Applies a special procedure. */
static lisp apply_cproc(lisp func,lisp expr,lispenv env,int trace)
{
  lisp args[MAX_CPROC_ARGS], params=CDR(expr);
  lisp ptr=params, result=FD_EMPTY_CHOICE;
  int arg_count=0, prune=0, deterministic=1;
  fd_cproc p=PTR_DATA(func,cproc); 

  decref(func);

  check_stack(&ptr);
  TIDY_ERRNO("Dangling errno:");

  if (p->n_args == FD_SPECIAL_FORM)
    /* Handle special forms (which just get called on the form
       and environment) */
    return apply_special_form(p,expr,env);
  else if ((p->n_args == FD_ND_LEXPR) ||
	   (p->n_args == FD_NORMAL_LEXPR)) {
    /* Handle lexprs (which take any number of arguments, 
       but evaluate them all) */
    lisp args=eval_elts(get_body(expr,1),env), result;
    WITH_HANDLING {
      /* List arguments when applying a primitive */
      if (trace_fdscript || trace) {
	report_eval(expr); describe_prim_args("Applying ",p->name,&args,1);}
      if (p->n_args == FD_ND_LEXPR)
	result=(((lisp (*)(lisp))(p->func)))(args);
      else result=apply_normal_lexpr((lisp (*)(lisp))p->func,args);}
    ON_EXCEPTION {
      if ((strcmp(fd_theException(),"CONTINUATION")) != 0) {
	fd_u8char *details=fd_exception_details();
	if (errno) {TIDY_ERRNO(p->name);}
	if (verbose_debuggingp()) {
	  report_eval(expr); describe_prim_args("applying",p->name,&args,1);}
	if (strchr(details,':') == NULL) {
	  fd_exception ex=fd_theException();
	  int nlen=strlen(p->name)+strlen(details)+3;
	  fd_u8char *new_details=fd_malloc(nlen);
	  fd_lisp obj=fd_incref(fd_exception_object());
	  sprintf(new_details,"%s: %s",p->name,details);
	  fd_pop_exception();
	  fd_set_exception(ex,new_details,obj);
	  fd_free(new_details,nlen);}
	fd_exception_context_push(fd_make_symbol(p->name));
	decref(args);
	fd_reraise();}
      else {decref(args); fd_reraise();}}
    END_HANDLING;
    decref(args);
    if (trace_fdscript || trace)
      fd_fprintf(stderr,_(";;> Evaluating %q produces %q\n"),expr,result);
    TIDY_ERRNO(p->name);
    return return_proper_choice(result);}
  while ((PAIRP(ptr)) && (arg_count < p->n_args)) {
    /* Handle regular combinations (eval arguments and call C) */
    lisp subexpr=CAR(ptr); 
    args[arg_count]=fd_evaluate(subexpr,env);
    if (FD_EMPTYP(args[arg_count])) {
      if (trace_fdscript || trace_prune || trace)
	fd_fprintf(stderr,_(";;> Pruning application of %s\n"),p->name);
      prune=1; break;}
    else if (CHOICEP(args[arg_count])) deterministic=0;
    arg_count++; ptr=CDR(ptr);}
  
  /* Check arity */
  if ((prune == 0) && (!(FD_EMPTY_LISTP(ptr))))
    fd_raise_lisp_exception(fd_TooManyArgs,p->name,expr);
  else if ((prune == 0) && (arg_count != p->n_args))
    fd_raise_lisp_exception(fd_TooFewArgs,p->name,expr);
  
  /* List arguments when applying a primitive */
  if ((prune == 0) && (trace_fdscript || trace)) {
    report_eval(expr);
    describe_prim_args("Applying ",p->name,args,p->n_args);}
  
  if (prune == 0) {
    WITH_HANDLING
      result=nd_prim_apply(p,args,p->n_args);
    ON_EXCEPTION
      if ((strcmp(fd_theException(),"CONTINUATION")) != 0) {
	int i=0;
	TIDY_ERRNO(p->name);
	if (verbose_debuggingp()) {
	  report_eval(expr); 
	  describe_prim_args("applying",p->name,args,p->n_args);}
	while (i < arg_count) {fd_decref(args[i]); i++;}
	fd_reraise();}
      else fd_reraise();
    END_HANDLING}
  else if (trace_prune || trace) report_prune(expr);
  if (trace_fdscript || trace) report_value(expr,result);
  /* Clean up your arguments */
  {int i=0; while (i < arg_count) {decref(args[i]); i++;}}
  
  TIDY_ERRNO(p->name);

  /* Return a neat result */
  return return_proper_choice(result);
}
     
#define func0(p) ((fd_lisp (*)(void))(p->func))
#define func1(p) ((fd_lisp (*)(fd_lisp))(p->func))
#define func2(p) ((fd_lisp (*)(fd_lisp,fd_lisp))(p->func))
#define func3(p) ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func4(p) ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func5(p) \
  ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func6(p) \
  ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))

#define call_cproc0(p) (func0(p))()
#define call_cproc1(p,a0) ((func1(p))(a0))
#define call_cproc2(p,a0,a1) ((func2(p))(a0,a1))
#define call_cproc3(p,a0,a1,a2) ((func3(p))(a0,a1,a2))
#define call_cproc4(p,a0,a1,a2,a3) ((func4(p))(a0,a1,a2,a3))
#define call_cproc5(p,a0,a1,a2,a3,a4) ((func5(p))(a0,a1,a2,a3,a4))
#define call_cproc6(p,a0,a1,a2,a3,a4,a5) ((func6(p))(a0,a1,a2,a3,a4,a5))\

/* This does a non-deterministic enumeration of variables and
   repeatedly applies the given procedure. */
static lisp nd_prim_apply(fd_cproc p,lisp *args,int n_args)
{
  int i=0, nd_call=0;
  lisp result;
  while (i <  n_args)
    if (CHOICEP(args[i])) {nd_call=1; break;}
    else i++;
  if (nd_call == 0) {
    i=0; while (i < n_args) {args[i]=sv(args[i]); i++;}}
  /* There might be a better way to code this, but this works. 
     It just consists of a separate case for each possible argument
     arity, doing value enumeration and accumulation. */
  switch (n_args) {
  case 0: result=call_cproc0(p); break;
  case 1:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	lisp partial=call_cproc1(p,sv(a0));
	ADD_TO_CHOICE(v,partial);}
      END_DO_CHOICES;
      result=v; break;}
    else {result=call_cproc1(p,args[0]); break;}
  case 2:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	DO_CHOICES(a1,args[1]) {
	  lisp partial=call_cproc2(p,sv(a0),sv(a1));
	  ADD_TO_CHOICE(v,partial);}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      result=v; break;}
    else result=call_cproc2(p,args[0],args[1]); break;
  case 3:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	DO_CHOICES(a1,args[1]) {
	  DO_CHOICES(a2,args[2]) {
	    lisp partial=call_cproc3(p,sv(a0),sv(a1),sv(a2));
	    ADD_TO_CHOICE(v,partial);}
	  END_DO_CHOICES;}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      result=v;}
    else result=call_cproc3(p,args[0],args[1],args[2]);
    break;
  case 4:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	DO_CHOICES(a1,args[1]) {
	  DO_CHOICES(a2,args[2]) {
	    DO_CHOICES(a3,args[3]) {
	      lisp partial=call_cproc4(p,sv(a0),sv(a1),sv(a2),sv(a3));
	      ADD_TO_CHOICE(v,partial);}
	    END_DO_CHOICES;}
	  END_DO_CHOICES;}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      result=v;}
    else result=call_cproc4(p,args[0],args[1],args[2],args[3]);
    break;
  case 5:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	DO_CHOICES(a1,args[1]) {
	  DO_CHOICES(a2,args[2]) {
	    DO_CHOICES(a3,args[3]) {
	      DO_CHOICES(a4,args[4]) {
		lisp partial=call_cproc5(p,sv(a0),sv(a1),sv(a2),sv(a3),sv(a4));
		ADD_TO_CHOICE(v,partial);}
	      END_DO_CHOICES;}
	    END_DO_CHOICES;}
	  END_DO_CHOICES;}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      result=v;}
    else result=call_cproc5(p,args[0],args[1],args[2],args[3],args[4]);
    break;
  case 6:
    if (nd_call) {
      lisp v=FD_EMPTY_CHOICE;
      DO_CHOICES(a0,args[0]) {
	DO_CHOICES(a1,args[1]) {
	  DO_CHOICES(a2,args[2]) {
	    DO_CHOICES(a3,args[3]) {
	      DO_CHOICES(a4,args[4]) {
		DO_CHOICES(a5,args[5]) {
		  lisp partial=
		    call_cproc6(p,sv(a0),sv(a1),sv(a2),sv(a3),sv(a4),sv(a5));
		  ADD_TO_CHOICE(v,partial);}
		END_DO_CHOICES;}
	      END_DO_CHOICES;}
	    END_DO_CHOICES;}
	  END_DO_CHOICES;}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      result=v;}
    else result=call_cproc6(p,args[0],args[1],args[2],args[3],args[4],args[6]);
    break;}
  return return_proper_choice(result);
}

/** Applying interpreted procedures **/

static lisp eval_rest_arg(lisp list,lispenv env);
static void describe_application
   (char *verb,lisp lambda,lispenv env,int n_bindings);

/* Top level application function.
   This really just makes a tail call object. */
static lisp apply_sproc(lisp func,lisp expr,lispenv param_env)
{
  lisp_vector v; int i=0, size=0, fail=0;
  fd_sproc s=FD_GET_SPROC(func); fd_lispenv env=s->env;
  lisp lambda=s->lambda, formals=CAR(CDR(lambda));
  lisp args=formals, params=CDR(expr);
  check_stack(&v);
  while (PAIRP(args)) {size++; args=CDR(args);}
  if (SYMBOLP(args)) size++;
  else if (FD_EMPTY_LISTP(args)) size++;
  else fd_raise_lisp_exception(fd_BadArgList,"",formals);
  v=fd_malloca(struct FD_VECTOR);
  v->elements=fd_malloc(sizeof(lisp)*(size+1)); v->length=size+1;
  v->elements[0]=func; i=1;
  args=formals; while ((PAIRP(args)) && (fail == 0)) {
    if (PAIRP(params)) 
      v->elements[i]=fd_evaluate(CAR(params),param_env);
    else if (SYMBOLP(CAR(args)))
      fd_raise_lisp_exception(fd_TooFewArgs,"",expr);
    else if (PAIRP(CAR(args))) {
      fd_lisp dflt_expr=get_arg(CAR(args),1,FD_VOID);
      fd_lisp dflt_value=fd_eval_in_env(dflt_expr,env);
      v->elements[i]=dflt_value;}
    else fd_raise_lisp_exception(fd_BadArgList,"",args);
    args=CDR(args);
    if (PAIRP(params)) params=CDR(params); 
    if (FD_EMPTYP(v->elements[i])) fail=1; else i++;}
  if (fail) {
    int j=0; while (j < i) {decref(v->elements[j]); j++;}
    fd_free(v->elements,sizeof(lisp)*(size+1));
    fd_qfree(v,sizeof(struct FD_VECTOR));
    if (trace_prune || trace_fdscript) report_prune(expr);
    return FD_EMPTY_CHOICE;}
  else if (PAIRP(args))
    fd_raise_lisp_exception(fd_TooFewArgs,"",expr);
  else if (SYMBOLP(args)) 
    v->elements[i++]=eval_rest_arg(params,param_env);
  else if (FD_EMPTY_LISTP(params))
    v->elements[i++]=FD_VOID;
  else fd_raise_lisp_exception(fd_TooManyArgs,"",expr);
  {RETURN_LISP(tail_call_type,vector,v);}
}

/* This does the actual application, binding variables and tail call
    creation to handle non-determinism. */
static lisp make_the_call(lisp *rail,int size)
{
  lisp result, lambda=rail[0], args, tail_call; lispenv env;
  int i=0, n_bindings=size-1;
  fd_sproc s=FD_GET_SPROC(lambda);
  env=s->env; args=CAR(CDR(s->lambda));
  /* Locks for synchronized sprocs */
#if FD_THREADS_ENABLED
  if (PRIM_TYPEP(lambda,ssproc_type)) {
    fd_ssproc ssp=PTR_DATA(lambda,ssproc);
    lock_mutex(&(ssp->lock));}
#endif
  /* GPROCs use the tail call itself as a 'goal'  */
  if (PRIM_TYPEP(lambda,gproc_type)) {
    int i=0; 
    tail_call=fd_make_vector(size); tail_call.type=tail_call_type;
    while (i < size) {
      FD_VECTOR_SET(tail_call,i,incref(rail[i])); i++;}
    if (add_goal(tail_call)) {
      decref(tail_call); return FD_EMPTY_CHOICE;}}
  {
    /* Make your lexical environment */
    FD_WITH_LEXICAL_ENV(tail_env,env,4) {
      WITH_HANDLING {
	i=1; while (i < size) {
	  lisp var;
	  if (PAIRP(args)) var=CAR(args); else var=args;
	  if (PAIRP(var)) var=CAR(var);
	  if (SYMBOLP(var))
	    fd_bind_value(var,rail[i],tail_env);
	  i++; if (PAIRP(args)) args=CDR(args);}
	result=eval_body(CDR(CDR(s->lambda)),tail_env);}
      ON_EXCEPTION {
#if FD_THREADS_ENABLED
	if (PRIM_TYPEP(lambda,ssproc_type)) {
	  fd_ssproc ssp=PTR_DATA(lambda,ssproc);
	  unlock_mutex(&(ssp->lock));}
#endif
	if (PRIM_TYPEP(lambda,gproc_type)) {
	  finish_goal(tail_call); decref(tail_call);}
	if ((strcmp(fd_theException(),"CONTINUATION")) != 0) {
	  fd_exception_context_push(CAR(s->lambda));
	  if (verbose_debuggingp()) 
	    describe_application("applying",lambda,tail_env,n_bindings);
	  fd_reraise();}
	fd_reraise();}
      END_HANDLING}
#if FD_THREADS_ENABLED
    if (PRIM_TYPEP(lambda,ssproc_type)) {
      fd_ssproc ssp=PTR_DATA(lambda,ssproc);
      unlock_mutex(&(ssp->lock));}
#endif
    if (PRIM_TYPEP(lambda,gproc_type)) {
      finish_goal(tail_call); decref(tail_call);}
    if (trace_fdscript) {
      describe_application("Applied",lambda,tail_env,n_bindings);
      report_yield(result);}
    FD_END_WITH_LEXICAL_ENV(result);}
  return result;
}

static void finish_nd_tail_call
  (lisp tail_call,lisp *nd_rail,lisp *d_rail,
   int i,int size,fd_hashset results)
{
  if (i == size) {
    lisp result=make_the_call(d_rail,size);
    finish_value(result);
    if (CHOICEP(result)) {
      DO_CHOICES(each,result) fd_hashset_add(results,each); END_DO_CHOICES;}
    else fd_hashset_add(results,result);
    decref(result);}
  else {
    DO_CHOICES(value,nd_rail[i]) {
      d_rail[i]=value;
      finish_nd_tail_call
	(tail_call,nd_rail,d_rail,i+1,size,results);}
    END_DO_CHOICES;}
}

static lisp finish_tail_call(lisp tail_call)
{
  lisp_vector v=PTR_DATA(tail_call,vector);
  int size=v->length, non_deterministic=0;
  lisp *rail=v->elements;
  int i=1; while (i < size)
    if (CHOICEP(rail[i])) {non_deterministic=1; break;}
    else i++;
  if (non_deterministic) {
    struct FD_HASHSET accumulator;
    lisp *d_rail=fd_malloc(sizeof(lisp)*size), results=FD_VOID;
    WITH_HANDLING {
      fd_init_hashset(&accumulator,256);
      finish_nd_tail_call(tail_call,rail,d_rail,0,size,&accumulator);}
    ON_EXCEPTION {
      fd_free_hashset(&accumulator);
      decref(tail_call); fd_free(d_rail,(sizeof(lisp)*size));
      fd_reraise();}
    END_HANDLING;
    decref(tail_call);
    fd_free(d_rail,(sizeof(lisp)*size));
    results=fd_hashset_elts(&accumulator);
    fd_free_hashset(&accumulator);
    return results;}
  else {
    lisp result=FD_VOID;
    UNWIND_PROTECT
      result=make_the_call(rail,size);
    ON_UNWIND
      decref(tail_call);
    END_UNWIND;
    return return_proper_choice(result);}
}

/* This evaluates a list of elements, intended to evaluate the
   &rest argument of a lexpr. */
static lisp eval_rest_arg(lisp list,lispenv env)
{
  lisp results=FD_EMPTY_LIST, *tail=&results;
  DOLIST(elt,list) {
    lisp value=fd_eval_in_env(elt,env);
    *tail=FD_MAKE_LIST1(value); tail=&(CDR(*tail));}
  return results;
}
							
static int lambda_in_envp(lisp x,lispenv e)
{
  if ((FD_XPROCP(x))) {
    fd_sproc s=FD_GET_SPROC(x);
    return (s->env == e);}
  else return 0;
}

FRAMERD_EXPORT
/* _fd_finalize_static_tail_call
     Arguments: a tail call object and an environment
     Returns: the results of evaluating the tail call until it no
longer depends on the given environment
*/
lisp _fd_finalize_static_tail_call(lisp tc,fd_lispenv env)
{
  lisp_vector v;
  if (PRIM_TYPEP(tc,tail_call_type)) v=PTR_DATA(tc,vector);
  else return tc;
  while (lambda_in_envp(v->elements[0],env)) {
    tc=finish_tail_call(tc);
    if (PRIM_TYPEP(tc,tail_call_type)) v=PTR_DATA(tc,vector);
    else return tc;}
  return tc;
}

FRAMERD_EXPORT
/* fd_finish_value:
     Arguments: a lisp pointer
     Returns: a lisp pointer which isn't a tail call
  This will force any tail call to evaluate to a real value. */
lisp fd_finish_value(lisp value)
{
  if (PRIM_TYPEP(value,tail_call_type))
    return finish_tail_call(value);
  else return value;
}

/** Remote evaluation **/

FRAMERD_EXPORT
/* fd_make_rproc:
     Arguments: a server id and a remote op name, both lisp pointers
     Returns: an rproc to invoke the op name on the named server
*/
lisp fd_make_rproc(fd_server s,lisp op)
{
  lisp result; fd_rproc rp=fd_malloca(struct FD_RPROC);
  rp->server=s; rp->remote_op=op;
  result.type=rproc_type; result.data.rproc=rp;
  return result;
}

FRAMERD_EXPORT 
/* fd_dtapply:
     Arguments: an rproc, a list of arg expressions, and an environment
     Returns: the results of applying the remote procedure to the
results of locally evaluating the arg expressions.
*/
lisp fd_dtapply(lisp proc,lisp args,lispenv env,int eval_args)
{
  if (PRIM_TYPEP(proc,rproc_type)) {
    struct FD_RPROC *rp=PTR_DATA(proc,rproc);
    lisp head=FD_MAKE_LIST1(incref(rp->remote_op)), tail=head;
    lisp return_value;
    DOLIST(arg_expr,args) {
      lisp v, new_tail;
      if (eval_args)
	v=fd_eval_in_env(arg_expr,env);
      else v=incref(arg_expr);
      if ((SYMBOLP(v)) || (PAIRP(v)) || (FD_CHOICEP(v)))
	new_tail=FD_MAKE_LIST1(FD_MAKE_LIST(2,quote_symbol,v));
      else new_tail=FD_MAKE_LIST1(v);
      RPLACD(tail,new_tail); tail=new_tail;}
    return_value=fd_careful_dtype_eval(head,rp->server);
    decref(head); decref(proc);
    return return_value;}
  else fd_ctype_error("fd_dtapply",_("not an rproc"),proc);
}

static lisp lisp_use_server_lexpr(lisp args)
{
  fd_lisp server_id, remote_name; fd_server s;
  fd_get_args("USE-SERVER",args,&server_id,FD_VOID,&remote_name,FD_FALSE,NULL);
  s=get_connection(server_id);
  if (FD_FALSEP(remote_name))
    return fd_make_cptr(dtype_server_type,s);
  else return fd_make_rproc(s,remote_name);
}

static void print_rproc(lisp x,fd_string_stream ss)
{
  fd_rproc p=PTR_DATA(x,rproc);
  fd_printf(ss,"[#RPROC %q on %d@%s]",
	    p->remote_op,p->server->port,p->server->servername);
}

static void free_rproc(lisp x)
{
  fd_rproc p=PTR_DATA(x,rproc); 
  fd_close_connection(p->server); decref(p->remote_op);
  fd_qfree(p,sizeof(struct FD_RPROC));
}

/** APPLY **/

/* Top level application function.
   This really just makes a tail call object. */
static lisp start_application(lisp func,lisp params)
{
  lisp_vector v; int i=0, size=0;
  fd_sproc s=FD_GET_SPROC(func); fd_lispenv env=s->env;
  lisp lambda=s->lambda, formals=CAR(CDR(lambda)), args=formals;
  while (PAIRP(args)) {size++; args=CDR(args);}
  if (!(FD_EMPTY_LISTP(args))) size++;
  v=fd_malloca(struct FD_VECTOR);
  v->elements=fd_malloc(sizeof(lisp)*(size+1)); v->length=size+1;
  v->elements[0]=incref(func); i=1;
  args=formals; while ((PAIRP(params)) && (PAIRP(args))) {
    v->elements[i]=incref(CAR(params));
    params=CDR(params); args=CDR(args); i++;}
  while (PAIRP(args)) /* Process optional arguments */
    if (PAIRP(CAR(args))) {
      lisp dflt_expr=get_arg(CAR(args),1,FD_FALSE);
      lisp dflt_value=fd_eval_in_env(dflt_expr,env);
      v->elements[i]=dflt_value; args=CDR(args); i++;}
    else fd_raise_lisp_exception(fd_TooFewArgs,"",formals);
  if (SYMBOLP(args)) v->elements[i++]=incref(params);
  else if (FD_EMPTY_LISTP(args)) {}
  else fd_raise_lisp_exception(fd_TooManyArgs,"",func);
  {RETURN_LISP(tail_call_type,vector,v);}
}
 
FRAMERD_EXPORT
/* fd_do_application:
     Arguments: two lisp values, a procedure and a list of arguments
     Returns: a value or a tail call resulting from applying the
     procedure to the arguments.
*/
lisp fd_do_application(lisp fcn,lisp argvalues)
{
  if (PRIM_TYPEP(fcn,cproc_type)) {
    fd_cproc p=PTR_DATA(fcn,cproc);
    if ((p->n_args == FD_ND_LEXPR) ||
	(p->n_args == FD_NORMAL_LEXPR)) {
      if (trace_fdscript)
	describe_prim_args("Applying",p->name,&argvalues,1);
      if (p->n_args == FD_NORMAL_LEXPR) 
	return apply_normal_lexpr(((lisp (*)(lisp))(p->func)),argvalues);
      else return ((lisp (*)(lisp))(p->func))(argvalues);}
    else if (p->n_args == FD_SPECIAL_FORM)
      fd_raise_exception(fd_NoSpecialFormApply);
    else {
      lisp args[8]; int n_args=0;
      DOLIST(r,argvalues) 
	if (n_args > 7) break; else args[n_args++]=r;
      if (n_args > p->n_args)
	fd_raise_lisp_exception(fd_TooManyArgs,"APPLY",fcn);
      else if (n_args < p->n_args)
	fd_raise_lisp_exception (fd_TooFewArgs,"",fcn);
      if (trace_fdscript)
	describe_prim_args("Applying",p->name,args,n_args);
      return nd_prim_apply(p,args,n_args);}}
  else if ((PRIM_TYPEP(fcn,sproc_type)) || (PRIM_TYPEP(fcn,gproc_type))) 
    return start_application(fcn,argvalues);
  else if (PRIM_TYPEP(fcn,rproc_type)) {
    fd_rproc p=PTR_DATA(fcn,rproc);
    lisp new_expr=FD_MAKE_PAIR(incref(p->remote_op),incref(argvalues));
    lisp result=fd_careful_dtype_eval(new_expr,p->server);
    decref(new_expr);
    return result;}
  else if (PRIM_TYPEP(fcn,ssproc_type)) {
    fd_ssproc s=PTR_DATA(fcn,ssproc);
    lisp start=start_application(fcn,argvalues);
    finish_value(start);
    return start;}
  else if (PRIM_TYPEP(fcn,continuation_type)) {
    char *name=(char *)CPTR_DATA(fcn);
    lisp arg=incref(get_arg(argvalues,0,FD_VOID));
    decref(argvalues); decref(fcn);
    fd_throw(name,NULL,arg);}
  else if (CHOICEP(fcn)) {
    lisp answer=FD_EMPTY_CHOICE;
    DO_CHOICES(f,fcn) {
      lisp v=fd_do_application(firstv(f),argvalues);
      finish_value(v);
      ADD_TO_CHOICE(answer,v);}
    END_DO_CHOICES;
    return answer;}
  else fd_raise_detailed_exception
	 ("Function value is inapplicable",fd_object_to_string(fcn));
}

FRAMERD_EXPORT
lisp fd_apply(lisp func,lisp args)
{
  lisp v=fd_do_application(func,args);
  finish_value(v);
  return v;
}

FRAMERD_EXPORT
lisp fd_lisp_call(lisp func,lisp arg)
{
  lisp arglist=FD_MAKE_LIST1(incref(arg));
  lisp v=fd_do_application(func,arglist);
  finish_value(v); decref(arglist);
  return v;
}

/** Watching the evaluation of particular forms **/

static lisp lisp_watch_handler(lisp expr,lispenv env)
{
  lisp form=get_arg(expr,1,FD_VOID), value=FD_EMPTY_CHOICE;
  report_eval(form);
  {UNWIND_PROTECT {
    lisp head=CAR(form), fcn;
    if (SYMBOLP(head)) fcn=fd_symeval(head,env);
    else fcn=fd_eval_in_env(head,env);
    if (PRIM_TYPEP(fcn,cproc_type))
      value=apply_cproc(fcn,form,env,1);
    else if (FD_XPROCP(fcn)) {
      value=apply_sproc(fcn,form,env);
      finish_value(value);}
    else value=fd_eval_in_env(form,env);}
   ON_UNWIND {
     if (fd_theException()) {
       fd_string_stream xio=fd_get_xio();
       if (xio)
	 fd_printf(xio,_(";;> Error `%m' (%m) evaluating %q\n"),
		   fd_theException(),fd_exception_details(),form);
       else fd_fprintf(stderr,_(";;> Error `%m' (%m) evaluating %q\n"),
		       fd_theException(),fd_exception_details(),form);}}
   END_UNWIND}
  report_value(form,value);
  return value;
}

/** Autoload, Autolink, and Use-Server references **/

FRAMERD_EXPORT
void fd_autoload(char *name,char *file)
{
  lisp symbol=fd_make_symbol(name);
  lisp autoload_object=FD_MAKE_LIST(2,autoload_symbol,fd_make_string(file));
  fd_set_symbol_value(symbol,autoload_object);
  fd_decref(autoload_object);
}

FRAMERD_EXPORT
void fd_autolink(char *name,char *file)
{
  lisp symbol=fd_make_symbol(name);
  lisp autolink_object=FD_MAKE_LIST(2,autolink_symbol,fd_make_string(file));
  fd_set_symbol_value(symbol,autolink_object);
  fd_decref(autolink_object);
}

/** Using dynamic libraries **/

#if (FD_USING_DLLS || WIN32)
static char *get_init_name(char *filename)
{
  char *base=fd_basename(filename,0);
  char *copy=fd_xmalloc(strlen(base)+64);
  strcpy(copy,"fd_initialize_"); strcat(copy,base);
  fd_xfree(base);
  return copy;
}
#endif

#if (WIN32)
FRAMERD_EXPORT int fd_load_dll(char *mname)
{
  HMODULE lib=LoadLibrary(mname);
  if (lib == NULL) {
    char *found=fd_find_file(mname,fd_getenv("FDPATH"));
    if (found) {lib=LoadLibrary(mname); fd_xfree(found);}}
  if (lib == NULL)
    fd_raise_exception(fd_DLLError);
  else {
    char *init_name=get_init_name(mname);
    FARPROC init_fcn=GetProcAddress(lib,init_name);
    if (init_fcn) init_fcn();
    return 1;}
}
#elif (FD_USING_DLLS) /* But not WIN32 */
static void *find_dll(char *mname)
{
  void *lib;
  fd_u8char *fullname, *found, *suffix=FD_SHARED_LIB_SUFFIX;
  int namelen=strlen(mname), suffixlen=strlen(suffix), needs_suffix=1;
  if ((namelen > suffixlen+1) &&
      (strcmp(mname+namelen-suffixlen,suffix) != 0)) {
    fullname=fd_malloc(namelen+suffixlen+1);
    strcpy(fullname,mname); strcat(fullname,suffix);}
  else fullname=fd_strdup(mname);
  if (fd_file_existsp(fullname)) found=fullname;
  else {
    fd_u8char *libpath=fd_malloc(strlen(LIB_INSTALL_DIR)+namelen+suffixlen+2);
    strcpy(libpath,LIB_INSTALL_DIR); strcat(libpath,DIRSEP);
    strcat(libpath,fullname);
    if (fd_file_existsp(libpath)) found=libpath;
    else {
      fd_lisp search_path=fd_getenv("FDPATH");
      found=fd_find_file(fullname,search_path);
      fd_xfree(libpath);}
    fd_xfree(fullname);}
  if (found) {
    lib=dlopen(found,(RTLD_NOW|RTLD_GLOBAL));
    if (lib) {FD_CLEAR_ERR();}
    fd_xfree(found);
    return lib;}
  else return NULL;
}
FRAMERD_EXPORT int fd_load_dll(char *mname)
{
  void *lib;
  if (fd_source_file_registeredp(mname)) {
    fd_notify(_("Module %s already present"),mname);
    return 1;}
  lib=find_dll(mname);
  if (lib) {
    char *init_name=get_init_name(mname);
    int (*init_fcn)()=dlsym(lib,init_name);
    if (init_fcn) {
      WITH_HANDLING {
	init_fcn();}
      ON_EXCEPTION {
	fd_warn(_("Error (%s:%s) calling init function %s"),
		fd_theException(),fd_exception_details(),init_name);
	fd_clear_exception();}
      END_HANDLING;
      free(init_name);
      return 1;}
    else return 0;}
  else return -1;
}
#else
FRAMERD_EXPORT int fd_load_dll(char *mname)
{
  fd_raise_detailed_exception(_("No dynamic loading"),fd_strdup(FD_OSID));
}
#endif

#if FD_USING_DLLS
static void load_dll(lisp module_name)
{
  char *mname; int code;
  if (STRINGP(module_name))
    mname=fd_strdup(STRING_DATA(module_name));
  else if (SYMBOLP(module_name))
    mname=fd_string_getenv(SYMBOL_NAME(module_name));
  else fd_ctype_error
	 ("load_dll",_("dynamic module name must be symbol or string"),
	  module_name);  
  if (mname == NULL) mname=fd_strdup(SYMBOL_NAME(module_name));
  code=fd_load_dll(mname);
  fd_xfree(mname);
#if (WIN32)
  if (code < 0) fd_raise_exception(fd_DLLError);
#else
  if (code < 0)
    fd_raise_detailed_exception(fd_DLLError,(char *)dlerror());
#endif
}
#else
static void load_dll(lisp module_name)
{
  fd_raise_exception("No dynamic loading");
}
#endif

/** Record access for foreign function libraries **/

static fd_hashtable type_hierarchy;

STATIC_INLINE int record_typep(lisp x,lisp tag)
{
  if (!(RECORDP(x))) return 0;
  else {
    lisp rtag=RECORD_TAG(x);
    if (LISP_EQ(rtag,tag)) return 1;
    else {
      lisp above=fd_hashtable_get(type_hierarchy,rtag,FD_EMPTY_CHOICE);
      int result=fd_choice_containsp(tag,above);
      fd_decref(above);
      return result;}}
}

FRAMERD_EXPORT
/* fd_record_typep:
    Arguments: a lisp record and a tag
    Returns: 1 or 0
Returns 1 if the record's tag is the specified
pointer or one of its `supertypes'.  Returns 0 otherwise
or if the record is not really a record */
int fd_record_typep(lisp x,lisp tag) { return record_typep(x,tag); }

FRAMERD_EXPORT
/* fd_set_supertype:
    Arguments: two lisp tags
    Returns: nothing (void)
Records a super type relationship between two tags */
void fd_set_supertype(lisp tag,lisp super_tag)
{
  lisp super=fd_hashtable_get(type_hierarchy,super_tag,FD_EMPTY_CHOICE);
  fd_hashtable_add(type_hierarchy,tag,super_tag);
  fd_hashtable_add(type_hierarchy,tag,super);
  fd_decref(super);
}

/* Symeval */

static lisp lisp_symeval_handler(lisp expr,lispenv env)
{
  lisp sym=fd_eval_in_env(get_arg(expr,1,FD_VOID),env), value=FD_EMPTY_CHOICE;
  if (FD_SYMBOLP(sym)) 
    return fd_eval_in_env(sym,env);
  else return FD_EMPTY_CHOICE;
}

/** Initializing the evaluator **/

static int evaluator_initialized=0;

FRAMERD_EXPORT
void fd_debug_fdscript(int flag)
{
  default_debug_fdscript=flag;
}

static void print_server(lisp ls,fd_string_stream ss)
{
  fd_server s=(fd_server)CPTR_DATA(ls);
  fd_printf(ss,"[#SERVER %d@%s]",s->port,s->servername);
}

static void free_server(fd_lisp s)
{
  fd_close_connection((fd_server)CPTR_DATA(s));
  fd_qfree(FD_PTR_DATA(s, cptr), sizeof(struct FD_CPTR));
}

static void free_continuation(lisp cont)
{
  char *name=(char *)CPTR_DATA(cont);
  if (name) fd_xfree(name);
  fd_qfree(PTR_DATA(cont,cptr),sizeof(struct FD_CPTR));
}

static void print_continuation(lisp cont,fd_string_stream s)
{
  fd_printf(s,"#[CONTINUATION %x]",CPTR_DATA(cont));
}

static void initialize_default_debug_fdscript()
{
  if (FD_VOIDP(SYMBOL_VALUE(debug_fdscript_symbol))) {
    if (getenv("DEBUG_FDSCRIPT"))
      if ((strcmp(getenv("DEBUG_FDSCRIPT"),"no") == 0) ||
	  (strcmp(getenv("DEBUG_FDSCRIPT"),"0") == 0))
	default_debug_fdscript=0;
      else default_debug_fdscript=1;
    else default_debug_fdscript=0;
    if (default_debug_fdscript) {
      fd_set_symbol_value(debug_fdscript_symbol,FD_TRUE);}
    else {fd_set_symbol_value(debug_fdscript_symbol,FD_FALSE);}}
  else if (FD_FALSEP(SYMBOL_VALUE(debug_fdscript_symbol)))
    default_debug_fdscript=0;
  else default_debug_fdscript=1;
}

static void initialize_evaluator_types()
{
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(cproc_type);
   r->print_fcn=print_cproc; r->gc_fcn=free_cproc;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(env_type);
   r->print_fcn=print_env; r->gc_fcn=free_env;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(dtype_server_type);
   r->print_fcn=print_server; r->gc_fcn=free_server;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(rproc_type);
   r->print_fcn=print_rproc; r->gc_fcn=free_rproc;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(continuation_type);
   r->gc_fcn=free_continuation;
   r->print_fcn=print_continuation;}
}

static void initialize_stack_checking()
{
  int c; init_stack_base(&c);
  fd_set_stack_limit(0); /* Does default */
  check_stack_direction();
#if FD_THREADS_ENABLED
  fd_new_tld_key(&stack_base_key,NULL);
#endif
}

void fd_initialize_eval_c()
{
  if (evaluator_initialized) return;
  
#if FD_THREADS_ENABLED
  fd_init_mutex(&symbol_change_mutex);
#endif
  
  initialize_evaluator_types();
  initialize_stack_checking();
  
  type_hierarchy=fd_make_hashtable(16);

  define_symbol=fd_make_symbol("DEFINE");
  lambda_symbol=fd_make_symbol("LAMBDA");
  quote_symbol=fd_make_symbol("QUOTE");
  toplevel_symbol=fd_make_symbol("%TOPLEVEL%");
  goals_symbol=fd_make_symbol("%GOALS%");
  ppwidth_symbol=fd_make_symbol("PPWIDTH");
  
  macro_symbol=fd_make_symbol("MACRO");

  autoload_symbol=fd_make_symbol("AUTOLOAD");
  autolink_symbol=fd_make_symbol("AUTOLINK");
  use_server_symbol=fd_make_symbol("USE-SERVER");

  debug_fdscript_symbol=fd_make_symbol("%DEBUG");
  initialize_default_debug_fdscript();

  fd_add_special_form(NULL,"SYMEVAL",lisp_symeval_handler);

  fd_add_restricted_special_form("WATCH",lisp_watch_handler);
  fd_add_restricted_cproc
    ("SET-STACK-LIMIT!",1,lisp_set_stack_limit_cproc);
  fd_add_restricted_special_form("TRACED",lisp_traced_handler);
  fd_add_restricted_cproc("TRACE-FDSCRIPT",1,lisp_trace_fdscript);
  fd_add_restricted_cproc("TRACE-PRUNE",1,lisp_trace_prune);
  fd_add_restricted_lexpr("USE-SERVER",FD_NORMAL_LEXPR,lisp_use_server_lexpr);

  fd_register_source_file("eval",__DATE__,vcid);
  evaluator_initialized=1;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: eval.c,v $
   Revision 1.30  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.29  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.28  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.27  2004/04/17 17:51:11  haase
   Made modules be loaded globally

   Revision 1.26  2003/12/22 03:20:33  haase
   Update modules for fdtext/fdwww division

   Revision 1.25  2003/12/18 03:39:42  haase
   Cleaned up module loading and made it use the shared library suffix

   Revision 1.24  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.23  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.22  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.21  2003/09/13 21:57:56  haase
   Fixed automatic closing of unused network connections

   Revision 1.20  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.19.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.19.2.2  2003/08/02 13:51:47  haase
   Renamed fd_xprintf to fd_exprintf and made fd_xprintf do a printf to an XFILE

   Revision 1.19.2.1  2003/01/26 20:37:17  haase
   Misc. fixes, especially GC

   Revision 1.19  2002/06/24 18:08:33  haase
   Fixed some source file registrations

   Revision 1.18  2002/06/08 20:26:59  haase
   Add evaluation of optional arguments in procedure environment

   Revision 1.17  2002/05/29 18:43:57  haase
   Made trace output added a newline after procedure bodies when going to xio

   Revision 1.16  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.15  2002/05/11 13:27:59  haase
   Added library functions for dealing with multiple values

   Revision 1.14  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.13  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.12  2002/04/19 19:30:29  haase
   Fixed bug in specform printing

   Revision 1.11  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.10  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
