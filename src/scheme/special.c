/* C Mode */

/* special.c
   Lots of special forms
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

static char vcid[] = "$Id: special.c,v 1.36 2005/03/22 03:55:39 haase Exp $";

#include "fdscript.h"

static lisp t_symbol, else_symbol, cond_apply_symbol;
static lisp bq_symbol, bq_escape, bq_splice;
static lisp autolink_symbol, autoload_symbol, debug_fdscript_symbol;

#if FD_DYNAMIC_LINKING_ENABLED
#if HAVE_DLFCN_H
#define USE_DLOPEN 1
#include <dlfcn.h>
#else
#define USE_DLOPEN 0
#endif
#endif

#define finish_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x);
#define discard_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x); \
  decref(x);

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

lisp eval_exprs(lisp exprs,lispenv env)
{
  lisp final=FD_VOID;
  DOLIST(expr,exprs) {
    discard_value(final); final=fd_partial_eval(expr,env);}
  return final;
}

void eval_exprs_noreturn(lisp exprs,lispenv env)
{
  lisp final=FD_VOID;
  DOLIST(expr,exprs) {
    discard_value(final); final=fd_evaluate(expr,env);}
  discard_value(final);
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

static lisp load_dll(lisp module_name)
{
#if (FD_USING_DLLS)
  char *mname; int code;
  if (STRINGP(module_name))
    mname=fd_strdup(STRING_DATA(module_name));
  else if (SYMBOLP(module_name))
    mname=fd_string_getenv(SYMBOL_NAME(module_name));
  else fd_type_error(_("module name must be symbol or string"),module_name);  
  if (mname == NULL) mname=fd_strdup(SYMBOL_NAME(module_name));
  code=fd_load_dll(mname);
  fd_xfree(mname);
  if (code < 0)
#if (WIN32)
    fd_raise_exception(fd_DLLError);
#elif (FD_USING_DLLS)
    if (errno)
      fd_raise_exception(fd_DLLError);
    else fd_raise_detailed_exception(fd_DLLError,(char *)dlerror());
#endif
  else if (code) return FD_TRUE;
  else return FD_FALSE;
#else
  fd_raise_exception(_("No DLLS!"));
#endif
}

/** Scheme Special forms **/

/* Just returns its first argument, unevaluated */
static lisp quote_handler(lisp expr,lispenv env)
{
  return incref(get_arg(expr,1,FD_VOID));
}

/* Sets a symbol's value to the result of evaluating an expression. */
static lisp set_value_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value;
  var=get_arg(expr,1,FD_VOID); value_expression=get_arg(expr,2,FD_VOID);
  value=fd_eval_in_env(value_expression,env);
  fd_set_value(var,value,env);
  fd_decref(value);
  return FD_VOID;
}

/* Sets a symbol's value to the result of evaluating an expression. */
static lisp safe_set_value_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value;
  var=get_arg(expr,1,FD_VOID); value_expression=get_arg(expr,2,FD_VOID);
  value=fd_eval_in_env(value_expression,env);
  fd_safe_set_value(var,value,env);
  fd_decref(value);
  return FD_VOID;
}

/* Sets a symbol's value to the result of evaluating an expression. */
static lisp set_in_env_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value, lenv; fd_lispenv inner_env;
  var=get_arg(expr,1,FD_VOID);
  value_expression=get_arg(expr,2,FD_VOID);
  lenv=fd_eval_in_env(get_arg(expr,3,FD_VOID),env);
  if (PRIM_TYPEP(lenv,env_type)) inner_env=CPTR_DATA(lenv);
  else fd_type_error(_("not an environment"),lenv);
  value=fd_eval_in_env(value_expression,env);
  fd_set_value(var,value,inner_env);
  fd_decref(lenv);
  fd_decref(value);
  return FD_VOID;
}

/* Adds to a symbol's value the result of evaluating an expression. */
static lisp set_plus_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, current, addition; int changed=0;
  var=get_arg(expr,1,FD_VOID); value_expression=get_arg(expr,2,FD_VOID);
  if (SYMBOLP(var)) {
    current=fd_symeval(var,env);
    if (FD_VOIDP(current)) {changed=1; current=FD_EMPTY_CHOICE;}
    addition=fd_eval_in_env(value_expression,env);
    {DO_CHOICES(v,addition)
       if (!(fd_choice_containsp(v,current))) {
	 ADD_TO_CHOICE(current,incref(v)); changed=1;}
     END_DO_CHOICES;}
    if (changed) fd_set_value(var,current,env);
    decref(current);
    decref(addition);}
  else fd_raise_lisp_exception(fd_SetRequiresSymbol,"not a symbol",var);
  return FD_VOID;
}

/* Sets a symbol's value to the result of evaluating an expression. */
static lisp unset_value_handler(lisp expr,lispenv env)
{
  lisp var, value;
  var=get_arg(expr,1,FD_VOID); value=FD_VOID;
  fd_set_value(var,value,env);
  return FD_VOID;
}

/* This sets a value in the global environment.  Definitely not safe. */
static lisp global_set_value_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value;
  var=get_arg(expr,1,FD_VOID); value_expression=get_arg(expr,2,FD_VOID);
  value=fd_eval_in_env(value_expression,env);
  if (SYMBOLP(var)) {
    fd_set_symbol_value(var,value);
    fd_decref(value);
    return FD_VOID;}
  else fd_raise_lisp_exception(fd_SetRequiresSymbol,"not a symbol",var);
}

static lisp gvalue_proc(lisp sym)
{
  if (SYMBOLP(sym)) return incref(SYMBOL_VALUE(sym));
  else fd_type_error(_("not a symbol"),sym);
}

static lisp lisp_boundp_handler(lisp expr,lispenv env)
{
  lisp symbol=fd_get_arg(expr,1,FD_VOID);
  if (!(SYMBOLP(symbol))) fd_type_error(_("not a symbol"),symbol);
  else if (!(FD_VOIDP((SYMBOL_VALUE(symbol))))) return FD_TRUE;
  else {
    lisp v=fd_symeval(symbol,env);
    if (FD_VOIDP(v)) return FD_FALSE;
    else {decref(v); return FD_TRUE;}}
}

static lisp lisp_symbol_boundp_handler(lisp expr,fd_lispenv env)
{
  lisp symbol_arg=fd_get_arg(expr,1,FD_VOID);
  lisp symbol=fd_eval_in_env(symbol_arg,env);
  lisp env_arg=fd_get_arg(expr,2,FD_FALSE);
  lisp env_val=fd_eval_in_env(env_arg,env);
  fd_lispenv check_env=
    ((FD_FALSEP(env_val)) ? (env) : (fd_environment_ptr(env_val)));
  int bound=0;
  if (FD_SYMBOLP(symbol)) {
    lisp value=fd_symeval(symbol,check_env);
    if (FD_VOIDP(value)) bound=0; else bound=1;
    decref(symbol); decref(env_val); decref(value);}
  else {
    decref(symbol); decref(env_val);
    fd_type_error(_("not a symbol"),symbol);}
  if (bound) return FD_TRUE; else return FD_FALSE;
}

/* Sets a symbol's value in the current thread to the result of evaluating
   an expression. */
static lisp tset_value_handler(lisp expr,lispenv env)
{
  lisp var, value_expression, value;
  var=get_arg(expr,1,FD_VOID); value_expression=get_arg(expr,2,FD_VOID);
  value=fd_eval_in_env(value_expression,env);
  if (SYMBOLP(var)) {
    fd_thread_symbind(var,value);
    fd_decref(value);
    return FD_VOID;}
  else fd_raise_lisp_exception(fd_SetRequiresSymbol,"not a symbol",var);
}

/** Conditional expressions **/

/* Evaluates each test until one returns true and then either
   evaluates that clauses body or applies its 'func' to the
   result of the test. */
static lisp cond_handler(lisp cond_form,lispenv env)
{
  lisp expr=get_body(cond_form,1);
  DOLIST(clause,expr) {
    lisp test_expr=CAR(clause), test;
    if ((LISP_EQ(test_expr,t_symbol)) ||
	(LISP_EQ(test_expr,else_symbol)))
      test=FD_TRUE;
    else test=fd_eval_in_env(test_expr,env);
    if (FD_EMPTYP(test)) {}
    else if (!(LISP_TEST_FALSEP(test))) {
      if ((PAIRP(CDR(clause))) &&
	  (LISP_EQ(CAR(CDR(clause)),cond_apply_symbol))) {
	lisp proc=fd_eval_in_env(get_arg(clause,2,FD_VOID),env);
	lisp args=FD_MAKE_LIST1(test);
	lisp values=fd_do_application(proc,args);
	decref(proc); decref(args); 
	return values;}
      else {
	decref(test);
	return eval_exprs(CDR(clause),env);}}}
  return FD_EMPTY_CHOICE;
}

static lisp case_handler(lisp case_form,lispenv env)
{
  lisp key=fd_eval_in_env(fd_get_arg(case_form,1,FD_VOID),env);
  lisp clauses=get_body(case_form,2), else_forms=FD_VOID;
  DOLIST(clause,clauses) {
    lisp elts=fd_get_arg(clause,0,FD_VOID);
    if (LISP_EQ(elts,else_symbol)) else_forms=fd_get_body(clause,1);
    else if (PAIRP(elts)) {
      DOLIST(elt,elts)
	if ((LISP_EQ(elt,key)) ||
	    ((NUMBERP(key)) && (LISP_EQUAL(elt,key)))) {
	  decref(key); return eval_exprs(fd_get_body(clause,1),env);}}
    else if ((LISP_EQ(elts,key)) ||
	     ((NUMBERP(key)) && (LISP_EQUAL(elts,key)))) {
      decref(key); return eval_exprs(fd_get_body(clause,1),env);}}
  decref(key);
  if (PAIRP(else_forms)) return eval_exprs(else_forms,env);
  else return FD_VOID;
}

static lisp qase_handler(lisp case_form,lispenv env)
{
  lisp key=fd_eval_in_env(fd_get_arg(case_form,1,FD_VOID),env);
  lisp clauses=get_body(case_form,2), else_forms=FD_VOID;
  if (ATOMICP(key)) {
    DOLIST(clause,clauses) {
      lisp elts=fd_get_arg(clause,0,FD_VOID);
      if (LISP_EQ(elts,else_symbol)) else_forms=fd_get_body(clause,1);
      else if (PAIRP(elts)) {
	DOLIST(elt,elts)
	  if ((LISP_EQ(CAR(clause),key)) ||
	      ((NUMBERP(key)) && (LISP_EQUAL(elt,key)))) {
	    decref(key); return eval_exprs(fd_get_body(clause,1),env);}}
      else if ((LISP_EQ(elts,key)) ||
	       ((NUMBERP(key)) && (LISP_EQUAL(elts,key)))) {
	decref(key); return eval_exprs(fd_get_body(clause,1),env);}}}
  else {
    DOLIST(clause,clauses) {
      lisp elts=fd_get_arg(clause,0,FD_VOID);
      if (LISP_EQ(elts,else_symbol)) else_forms=fd_get_body(clause,1);
      else if (PAIRP(elts)) {
	DOLIST(elt,elts)
	  if (LISP_EQUAL(CAR(clause),key)) {
	    decref(key); return eval_exprs(fd_get_body(clause,1),env);}}
      else if (LISP_EQUAL(elts,key)) {
	decref(key); return eval_exprs(fd_get_body(clause,1),env);}}}
  decref(key);
  if (PAIRP(else_forms)) return eval_exprs(else_forms,env);
  else return FD_VOID;
}

static lisp when_handler(lisp when_form,lispenv env)
{
  lisp test_expr=get_arg(when_form,1,FD_VOID), body=get_body(when_form,2);
  lisp test=fd_eval_in_env(test_expr,env);
  if (FD_EMPTYP(test)) return test;
  else if (!(LISP_TEST_FALSEP(test))) {
    decref(test); return eval_exprs(body,env);}
  return FD_VOID;
}

static lisp unless_handler(lisp when_form,lispenv env)
{
  lisp test_expr=get_arg(when_form,1,FD_VOID), body=get_body(when_form,2);
  lisp test=fd_eval_in_env(test_expr,env);
  if (FD_EMPTYP(test)) return test;
  else if (LISP_TEST_FALSEP(test)) 
    return eval_exprs(body,env);
  else {decref(test); return FD_VOID;}
}

/** Sequencing and binding **/

static lisp begin_handler(lisp expr,lispenv env)
{
  lisp body=get_body(expr,1);
  return eval_exprs(body,env);
}

static lisp named_let_handler(lisp expr,lispenv env)
{
  lisp lambda, proc, result;
  lisp params=FD_EMPTY_LIST, *params_tail=&params;
  lisp args=FD_EMPTY_LIST, *args_tail=&args;
  lisp proc_name=fd_get_arg(expr,1,FD_VOID);
  DOLIST(binding,fd_get_arg(expr,2,FD_VOID)) {
    lisp var=fd_get_arg(binding,0,FD_VOID);
    lisp init_expr=fd_get_arg(binding,1,FD_VOID);
    lisp newp=FD_MAKE_LIST1(var);
    lisp newa=FD_MAKE_LIST1(fd_eval_in_env(init_expr,env));
    *params_tail=newp; params_tail=&(CDR(newp));
    *args_tail=newa; args_tail=&(CDR(newa));}
  {
    FD_WITH_LEXICAL_ENV(named_let_env,env,1) {
      lambda=
	FD_MAKE_PAIR(params,incref(fd_get_body(expr,3)));
      proc=fd_make_sproc(FD_MAKE_PAIR(proc_name,lambda),named_let_env);
      fd_bind_value(proc_name,proc,named_let_env);
      result=fd_do_application(proc,args);
      fd_decref(proc); finish_value(result);
      fd_bind_value(proc_name,FD_VOID,named_let_env);}
    FD_END_WITH_LEXICAL_ENV(result);}
  decref(args); 
  return result;
}

static lisp let_handler(lisp expr,lispenv env)
{
  lisp bindings=fd_get_arg(expr,1,FD_VOID);
  lisp body=fd_get_body(expr,2), value=FD_EMPTY_CHOICE;
  if (SYMBOLP(bindings)) return named_let_handler(expr,env);
  else {
    FD_WITH_LEXICAL_ENV(let_env,env,4) {
      DOLIST(binding,bindings) {
	lisp var=fd_get_arg(binding,0,FD_VOID);
	lisp value_expr=fd_get_arg(binding,1,FD_VOID);
	lisp value=fd_eval_in_env(value_expr,env);
	lisp first_value=firstv(value);
	fd_bind_value(var,first_value,let_env);
	fd_decref(first_value); fd_decref(value);}
      value=eval_exprs(body,let_env);}
    FD_END_WITH_LEXICAL_ENV(value);}
  return value;
}

static lisp letrec_handler(lisp expr,lispenv env)
{
  lisp bindings=fd_get_arg(expr,1,FD_VOID);
  lisp body=fd_get_body(expr,2), value=FD_EMPTY_CHOICE;
  FD_WITH_LEXICAL_ENV(let_env,env,4) {
    DOLIST(binding,bindings) {
      lisp var=fd_get_arg(binding,0,FD_VOID);
      lisp value_expr=fd_get_arg(binding,1,FD_VOID);
      lisp value=fd_eval_in_env(value_expr,let_env);
      lisp first_value=firstv(value);
      fd_bind_value(var,first_value,let_env);
      fd_decref(first_value); fd_decref(value);}
    value=eval_exprs(body,let_env);}
  FD_END_WITH_LEXICAL_ENV(value);
  return value;
}

static lisp let_star_handler(lisp expr,lispenv env)
{
  lisp bindings=fd_get_arg(expr,1,FD_VOID);
  lisp body=fd_get_body(expr,2), value=FD_EMPTY_CHOICE;
  if (SYMBOLP(bindings)) return named_let_handler(expr,env);
  else {
    FD_WITH_LEXICAL_ENV(let_env,env,4) {
      DOLIST(binding,bindings) {
	lisp var=fd_get_arg(binding,0,FD_VOID);
	lisp value_expr=fd_get_arg(binding,1,FD_VOID);
	lisp value=fd_eval_in_env(value_expr,let_env);
	lisp first_value=firstv(value);
	fd_bind_value(var,first_value,let_env);
	fd_decref(first_value); fd_decref(value);}
      value=eval_exprs(body,let_env);}
    FD_END_WITH_LEXICAL_ENV(value);}
  return value;
}
   
/** Continuations **/

static lisp callcc(lisp proc)
{
  char *throw_point=fd_strdup("CONTINUATION");
  lisp continuation=
    fd_make_cptr(continuation_type,throw_point);
  lisp args=FD_MAKE_LIST1(continuation), value=FD_VOID;
  WITH_HANDLING {
    value=fd_do_application(proc,args);
    finish_value(value);}
  ON_EXCEPTION
    if (fd_theException() == throw_point) {
      value=incref(fd_exception_object());
      fd_clear_exception();}
    else {
      if (PTR_REFCOUNT(continuation) != 1)
	fd_notify(_("Dangling continuation somewhere"));
      else decref(args);
      fd_reraise();}
  END_HANDLING;
  if (PTR_REFCOUNT(continuation) != 1)
    fd_notify(_("Dangling continuation somewhere"));
  else decref(args);
  return value;
}

/** Unwind protect **/

static lisp unwind_protect_handler(lisp expr,lispenv env)
{
  lisp core=get_arg(expr,1,FD_VOID), result=FD_EMPTY_CHOICE;
  lisp unwind=get_body(expr,2);
  UNWIND_PROTECT
    result=fd_eval_in_env(core,env);
  ON_UNWIND {
    DOLIST(ex,unwind) {
      lisp result=fd_eval_in_env(ex,env); decref(result);}}
  END_UNWIND;
  return result;
}

static lisp dynamic_wind_cproc(lisp wind,lisp thunk,lisp unwind)
{
  lisp result, wind_result=fd_apply(wind,FD_EMPTY_LIST);
  UNWIND_PROTECT {
    decref(wind_result); result=FD_VOID;
    result=fd_apply(thunk,FD_EMPTY_LIST);}
  ON_UNWIND {
    lisp unwind_result=fd_apply(unwind,FD_EMPTY_LIST);
    decref(unwind_result);}
  END_UNWIND;
  return result;
}

/** Error handling special forms **/

static lisp eval_error_handlers
  (lispenv env,lisp unwind,
   lisp exception,lisp details,lisp irritant,
   lisp backtrace)
{
  lisp result=FD_VOID;
  FD_WITH_LEXICAL_ENV(err_env,env,4) {
    fd_bind_value(fd_make_symbol("EXCEPTION"),exception,err_env);
    fd_bind_value
      (fd_make_symbol("EXCEPTION-DETAILS"),details,err_env);
    fd_bind_value(fd_make_symbol("IRRITANT"),irritant,err_env);
    fd_bind_value(fd_make_symbol("BACKTRACE"),backtrace,err_env);
    result=eval_exprs(unwind,err_env);}
  FD_END_WITH_LEXICAL_ENV(result);
  return result;
}

static lisp on_error_handler(lisp expr,lispenv env)
{
  lisp core=get_arg(expr,1,FD_VOID), result=FD_EMPTY_CHOICE;
  lisp exception, details, irritant, backtrace, old_debug_val;
  lisp unwind=get_body(expr,2); int success=0;
  struct FD_STRING_STREAM s; 
  WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&s,512);
    fd_direct_xio(&s,NULL,NULL);
    old_debug_val=fd_symbol_value(debug_fdscript_symbol);
    fd_set_symbol_value(debug_fdscript_symbol,FD_TRUE);
    result=fd_eval_in_env(core,env);
    success=1;}
  ON_EXCEPTION {
    exception=fd_copy_string(fd_theException());
    details=fd_copy_string(fd_exception_details());
    irritant=incref(fd_exception_object());
    backtrace=fd_copy_string(s.ptr);
    fd_clear_exception();}
  END_HANDLING;
  fd_set_symbol_value(debug_fdscript_symbol,old_debug_val);
  fd_decref(old_debug_val);
  fd_direct_xio(NULL,NULL,NULL); fd_xfree(s.ptr);
  if (success) return result;
  else return eval_error_handlers
	 (env,unwind,exception,details,irritant,backtrace);
}

static lisp signals_errorp_handler(lisp expr,lispenv env)
{
  lisp arg=get_arg(expr,1,FD_VOID), result=FD_FALSE;
  struct FD_STRING_STREAM s;
  WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&s,512);
    fd_direct_xio(&s,NULL,NULL);
    result=fd_eval_in_env(arg,env);
    fd_decref(result);
    result=FD_FALSE;}
  ON_EXCEPTION {
    lisp exception=fd_copy_string(fd_theException());
    lisp details=fd_copy_string(fd_exception_details());
    lisp irritant=fd_exception_object();
    fd_direct_xio(NULL,NULL,NULL);
    if (FD_VOIDP(irritant))
      result=FD_MAKE_LIST(2,exception,details);
    else result=FD_MAKE_LIST
	   (3,exception,details,incref(irritant));    
    result=fd_make_error(result);
    fd_clear_exception();}
  END_HANDLING;
  fd_direct_xio(NULL,NULL,NULL); fd_xfree(s.ptr); 
  return result;
}

static lisp signals_errorp_plus_handler(lisp expr,lispenv env)
{
  lisp arg=get_arg(expr,1,FD_VOID), result=FD_FALSE;
  struct FD_STRING_STREAM s; int error=0;
  WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&s,512);
    fd_direct_xio(&s,NULL,NULL);
    result=fd_eval_in_env(arg,env);}
  ON_EXCEPTION {
    lisp exception=fd_copy_string(fd_theException());
    lisp details=fd_copy_string(fd_exception_details());
    lisp irritant=fd_exception_object();
    fd_direct_xio(NULL,NULL,NULL);
    if (FD_VOIDP(irritant))
      result=FD_MAKE_LIST(2,exception,details);
    else result=FD_MAKE_LIST
	   (3,exception,details,incref(irritant));    
    result=fd_make_error(result); error=1;
    fd_clear_exception();}
  END_HANDLING;
  fd_direct_xio(NULL,NULL,NULL); fd_xfree(s.ptr); 
  if (error) return result;
  else if (FD_VOIDP(result)) return FD_FALSE;
  else if ((!(FD_CHOICEP(result))) &&
	   (!(FD_PRIM_TYPEP(result,multiple_value_type)))) {
    fd_lisp values[2], mv; values[0]=FD_FALSE; values[1]=result;
    mv=fd_mv_return(values,2); fd_decref(result);
    return mv;}
  else {
    fd_lisp extended_results=FD_EMPTY_CHOICE;
    FD_DO_CHOICES(each,result) {
      fd_lisp mv;
      if (FD_PRIM_TYPEP(each,multiple_value_type)) {
	int ret_len=FD_VECTOR_LENGTH(each);
	fd_lisp *new=fd_malloc(sizeof(fd_lisp)*(ret_len+1));
	memcpy(new+sizeof(fd_lisp),FD_VECTOR_ELEMENTS(each),
	       sizeof(fd_lisp)*ret_len);
	new[0]=FD_FALSE;
	mv=fd_mv_return(new,ret_len+1);
	fd_free(new,sizeof(fd_lisp)*(ret_len+1));}
      else {
	fd_lisp values[2]; values[0]=FD_FALSE; values[1]=each;
	mv=fd_mv_return(values,2);}
      FD_ADD_TO_CHOICE(extended_results,mv);}
    END_FD_DO_CHOICES;
    fd_decref(result);
    return extended_results;}
}

static lisp catch_errors_handler(lisp expr,lispenv env)
{
  lisp body=get_body(expr,1), result=FD_FALSE;
  struct FD_STRING_STREAM s; 
  WITH_HANDLING {
    FD_INITIALIZE_STRING_STREAM(&s,512);
    fd_direct_xio(&s,NULL,NULL);
    result=eval_exprs(body,env);
    finish_value(result);}
  ON_EXCEPTION {
    lisp exception=fd_copy_string(fd_theException());
    lisp details=fd_copy_string(fd_exception_details());
    lisp irritant=fd_exception_object();
    fd_direct_xio(NULL,NULL,NULL);
    if (FD_VOIDP(irritant))
      result=FD_MAKE_LIST(2,exception,details);
    else result=FD_MAKE_LIST
	   (3,exception,details,incref(irritant));    
    result=fd_make_error(result);
    fd_xfree(s.ptr); fd_clear_exception();}
  END_HANDLING;
  fd_direct_xio(NULL,NULL,NULL);
  return result;
}

static lisp lisp_errorp(lisp arg) {
  if (FD_LRECORD_TYPEP(arg,fd_error_tag)) return FD_TRUE;
  else return FD_FALSE;}
static lisp lisp_error_exception(lisp arg) {
  if (FD_LRECORD_TYPEP(arg,fd_error_tag)) return fd_car(FD_LRECORD_DATA(arg));
  else return FD_FALSE;}
static lisp lisp_error_details(lisp arg) {
  if (FD_LRECORD_TYPEP(arg,fd_error_tag)) {
    fd_lisp cdr=FD_CDR(FD_LRECORD_DATA(arg));
    return fd_car(cdr);}
  else return FD_FALSE;}
static lisp lisp_error_irritant(lisp arg) {
  if (FD_LRECORD_TYPEP(arg,fd_error_tag)) {
    fd_lisp cdr=FD_CDR(FD_LRECORD_DATA(arg));
    if (FD_PAIRP(FD_CDR(cdr))) {
      fd_lisp cddr=FD_CDR(cdr);
      return fd_car(cddr);}
    else return FD_EMPTY_CHOICE;}
  else return FD_FALSE;}

/** Returning errors and exceptions **/

static lisp return_error_lexpr(lisp args)
{
  if (!(PAIRP(args)))
    return fd_make_error(fd_make_string("Unspecified error"));
  if (FD_EMPTY_LISTP(CDR(args)))
    return fd_make_error(incref(CAR(args)));
  else return fd_make_error(incref(args));
}

static lisp return_exception_lexpr(lisp args)
{
  if (!(PAIRP(args)))
    return fd_make_error(fd_make_string("Unspecified error"));
  if (FD_EMPTY_LISTP(CDR(args)))
    return fd_make_exception(incref(CAR(args)));
  else return fd_make_exception(incref(args));
}

static lisp signal_exception_lexpr(lisp args)
{
  lisp exception_name=fd_get_arg(args,0,FD_VOID);
  lisp exception_details=fd_get_arg(args,1,FD_FALSE);
  lisp exception_data=fd_get_body(args,2);
  char *exname, *exdetails; lisp exobject;
  if (STRINGP(exception_name))
    exname=STRING_DATA(exception_name);
  else if (SYMBOLP(exception_name))
    exname=SYMBOL_NAME(exception_name);
  else exname=fd_object_to_string(exception_name);
  if (STRINGP(exception_details)) {
    exdetails=STRING_DATA(exception_details);
    exobject=exception_data;}
  else if (SYMBOLP(exception_details)) {
    exdetails=SYMBOL_NAME(exception_details);
    exobject=exception_data;}
  else {
    exdetails="Weird details";
    exobject=incref(fd_get_body(args,1));}
  fd_raise_lisp_exception(exname,exdetails,exobject);
  return FD_VOID;
}

static lisp apply_handler(lisp cdr)
{
  lisp fcn=get_arg(cdr,0,FD_VOID);
  lisp args=get_body(cdr,1), topass=FD_EMPTY_LIST, *tail=&topass;
  lisp value=FD_EMPTY_CHOICE;
  while (PAIRP(args)) {
    if (FD_EMPTY_LISTP(CDR(args))) {
      *tail=incref((CAR(args))); args=CDR(args);}
    else {
      lisp arg=CAR(args), new;
      /* Any choices we've get here are really quoted choices. */
      if (FD_CHOICEP(arg)) FD_SET_PRIM_TYPE(arg,quoted_choice_type);
      else if (FD_EMPTYP(arg)) arg=FD_QUOTED_EMPTY_CHOICE;
      new=FD_MAKE_LIST1(incref(arg));
      *tail=new; tail=&(CDR(new)); args=CDR(args);}}
  if (PRIM_TYPEP(fcn,rproc_type))
    value=fd_dtapply(incref(fcn),topass,NULL,0);
  else
    value=fd_do_application(fcn,topass);
  finish_value(value);
  decref(topass);
  return value;
}

static lisp lisp_safe_eval_cproc(lisp expr)
{
  return fd_eval_in_env(expr,NULL);
}

static lisp lisp_eval_handler(lisp eval_expr,fd_lispenv env)
{
  lisp expr=fd_eval_in_env(fd_get_arg(eval_expr,1,FD_VOID),env);
  lisp env_arg=fd_eval_in_env(fd_get_arg(eval_expr,2,FD_FALSE),env);
  if (FD_FALSEP(env_arg)) {
    lisp result=fd_eval_in_env(expr,env);
    decref(expr); decref(env_arg);
    return result;}
  else if (PRIM_TYPEP(env_arg,env_type)) {
    lisp result=fd_eval_in_env(expr,CPTR_DATA(env_arg));
    decref(expr); decref(env_arg);
    return result;}
  else fd_type_error(_("not an environment"),env_arg);
}

static lisp lisp_remote_eval_cproc(lisp expr,lisp server)
{
  fd_server s=get_connection(server);
  fd_lisp result=fd_dtype_eval(expr,s);
  fd_close_connection(s);
}

/* DTCALL from fdscript */
static lisp dtcall_handler(lisp expr,lispenv env)
{
  lisp host_spec=get_arg(expr,1,FD_VOID);
  lisp host=fd_eval_in_env(host_spec,env);
  lisp command=get_arg(expr,2,FD_VOID);
  lisp args=get_body(expr,3);
  fd_server s=get_connection(host);
  lisp rproc=fd_make_rproc(s,command);
  fd_decref(host); 
  return fd_dtapply(rproc,args,env,1);
}

/** LIST and backquote **/

static lisp list_handler(lisp args)
{
  return incref(args);
}

static lisp vector2list(lisp vec);
static lisp list2vector(lisp vec);

static lisp bq_copy(lisp expr,lispenv env,int depth)
{
  if (PAIRP(expr)) {
    lisp head=FD_EMPTY_LIST, *tail=&head;
    lisp scan=expr, elt;
    while (PAIRP(scan)) {
      lisp new; elt=CAR(scan); scan=CDR(scan);
      if (LISP_EQ(elt,bq_escape)) {
	if ((PAIRP(scan)) && (FD_EMPTY_LISTP(CDR(scan)))) {
	  new=fd_eval_in_env(CAR(scan),env);
	  scan=CDR(scan);}
	else fd_raise_lisp_exception("Bad backquote","dotted escape",expr);}
      else if (!(PAIRP(elt))) new=FD_MAKE_LIST1(bq_copy(elt,env,depth));
      else if (LISP_EQ(CAR(elt),bq_escape))
	if (depth == 0)
	  new=FD_MAKE_LIST1(fd_eval_in_env(get_arg(elt,1,FD_VOID),env));
	else if (PAIRP(elt))
	  new=FD_MAKE_LIST1
	    (FD_MAKE_PAIR(incref(CAR(elt)),
			  bq_copy(CDR(elt),env,depth-1)));
	else new=FD_MAKE_LIST1(bq_copy(elt,env,depth-1));
      else if (LISP_EQ(CAR(elt),bq_splice))
	new=fd_eval_in_env(get_arg(elt,1,FD_VOID),env);
      else if (LISP_EQ(CAR(elt),bq_symbol))
	new=FD_MAKE_LIST1
	  (FD_MAKE_PAIR(bq_symbol,bq_copy(CDR(elt),env,depth+1)));
      else new=FD_MAKE_LIST1(bq_copy(elt,env,depth));
      if (PAIRP(new))
	{lisp scan=new, next=CDR(new);
	 while (PAIRP(next)) {scan=next; next=CDR(scan);}
	 *tail=new; tail=&(CDR(scan));}
      else if (FD_EMPTY_LISTP(new)) {}
      else if (FD_EMPTY_LISTP(scan)) *tail=new;
      else fd_raise_exception("QUASIQUOTE can only splice lists");}
    return head;}
  else if (VECTORP(expr)) {
    lisp aslist=vector2list(expr);
    lisp transformed=bq_copy(aslist,env,depth);
    lisp asvector=list2vector(transformed);
    decref(aslist); decref(transformed);
    return asvector;}
  else return incref(expr);
}

static lisp bq_handler(lisp expr,lispenv env)
{
  return bq_copy(get_arg(expr,1,FD_VOID),env,0);
}

static lisp null_listp(lisp x)
{ if (FD_EMPTY_LISTP(x)) return FD_TRUE; else return FD_FALSE;}

/** vector->list and list->vector (used  by backquote) **/

static lisp list2vector(lisp lst)
{
  int len=fd_list_length(lst), i=0;
  lisp vec=fd_make_vector(len);
  DOLIST(elt,lst)
    {FD_VECTOR_SET(vec,i,incref(elt)); i++;}
  return vec;
}

static lisp vector2list(lisp vec)
{
  int i=fd_vector_length(vec);
  lisp answer=FD_EMPTY_LIST;
  if (i == 0) return FD_EMPTY_LIST;
  else while (i > 0) {
    i--; answer=FD_MAKE_PAIR(incref(VECTOR_REF(vec,i)),answer);}
  return answer;
}

/** Iteration: MAP and FOR-EACH **/

static void do_map(lisp fcn,lisp lists,lisp *answer)
{
  lisp args=FD_EMPTY_LIST, *next_args=&args;
  lisp next=FD_EMPTY_LIST, *next_tail=&next;
  int at_end=FD_EMPTY_LISTP(CAR(lists));
  DOLIST(lst,lists)
    if (at_end) {
      if (!(FD_EMPTY_LISTP(lst)))      
	fd_raise_exception("Map arguments have different lengths");}
    else if (FD_EMPTY_LISTP(lst))
      fd_raise_exception("Map arguments have different lengths");
    else {
      lisp new_state=FD_MAKE_LIST1(incref(CDR(lst)));
      lisp new_args=FD_MAKE_LIST1(incref(CAR(lst)));
      *next_args=new_args; next_args=&((CDR(new_args)));
      *next_tail=new_state; next_tail=&(CDR(new_state));}
  {
    lisp tmp=FD_VOID;
    UNWIND_PROTECT 
      if (at_end) {}
      else {
	tmp=fd_do_application(fcn,args);
	finish_value(tmp);
	if (answer) *answer=FD_MAKE_LIST1(tmp);
	else {decref(tmp);}}
    ON_UNWIND {
      decref(args); decref(lists);
      if (fd_theException()) decref(next);}
    END_UNWIND;}
  if (at_end) {}
  else if (answer) do_map(fcn,next,&(CDR(*answer)));
  else do_map(fcn,next,answer);
}

static lisp map_lexpr(lisp args)
{
  lisp fcn=get_arg(args,0,FD_VOID), lists=incref(get_body(args,1));
  lisp answer=FD_EMPTY_LIST; int all_null=FD_EMPTY_LISTP(fd_car_noref(lists));
  DOLIST(elt,lists) 
    if (PAIRP(elt)) {} 
    else if ((FD_EMPTY_LISTP(elt)) && (all_null)) {} 
    else fd_type_error("MAP: args have different lengths",args);
  do_map(fcn,lists,&answer); 
  return answer;
}

static lisp for_each_lexpr(lisp args)
{
  lisp fcn=get_arg(args,0,FD_VOID), lists=incref(get_body(args,1));
  int all_null=FD_EMPTY_LISTP(fd_car_noref(lists));
  DOLIST(elt,lists) 
    if (PAIRP(elt)) {} 
    else if ((FD_EMPTY_LISTP(elt)) && (all_null)) {} 
    else fd_type_error("FOR-EACH: args have different lengths",args);
  do_map(fcn,lists,NULL);
  return FD_VOID;
}

/** Iteration expressions: DO **/

struct UPDATER {lisp var, forms, newv;};

static lisp do_handler(lisp expr,lispenv env)
{
  lisp vars=get_arg(expr,1,FD_VOID);
  lisp control=get_arg(expr,2,FD_VOID);
  lisp test=get_arg(control,0,FD_VOID);
  lisp finishers=get_body(control,1);
  lisp body=get_body(expr,3), result;
  int i=0, n_vars=fd_list_length(vars);
  struct UPDATER *updaters=fd_malloc(sizeof(struct UPDATER)*n_vars);
  FD_WITH_LEXICAL_ENV(do_env,env,5) {
    DOLIST(var,vars) {
      fd_lisp mv, v;
      updaters[i].var=get_arg(var,0,FD_VOID);
      updaters[i].forms=get_body(var,2);      
      updaters[i].newv=FD_VOID;
      mv=fd_eval_in_env(get_arg(var,1,FD_FALSE),env);
      v=firstv(mv); fd_decref(mv);
      fd_bind_value(updaters[i].var,v,do_env);
      fd_decref(v);
      i++;}
    while (1) {
      lisp stopit=fd_eval_in_env(test,do_env);
      if (LISP_TEST_FALSEP(stopit)) {
	eval_exprs_noreturn(body,do_env);
	i=0; while (i < n_vars) {
	  lisp forms=updaters[i].forms;
	  if (!(FD_EMPTY_LISTP(forms))) {
	    lisp pv=eval_exprs(forms,do_env);
	    finish_value(pv);
	    updaters[i++].newv=pv;}
	  else i++;}
	i=0; while (i < n_vars) {
	  if (!(FD_EMPTY_LISTP(updaters[i].forms))) {
	    fd_set_value(updaters[i].var,updaters[i].newv,
			 do_env);
	    fd_decref(updaters[i].newv);}
	  i++;}}
      else {
	result=eval_exprs(finishers,do_env);
	decref(stopit); break;}}}
  FD_END_WITH_LEXICAL_ENV(result);
  fd_free(updaters,sizeof(struct UPDATER)*n_vars);
  return result;
}

/** Iteration expressions: DOTIMES, DOLIST, etc **/

static lisp dotimes_handler(lisp expr,lispenv env)
{
  lisp spec=get_arg(expr,1,FD_VOID), body=get_body(expr,2);
  lisp var=get_arg(spec,0,FD_VOID);
  lisp llimit=fd_eval_in_env(get_arg(spec,1,FD_VOID),env);
  int i=0, limit=fd_lisp2int(llimit);
  if (!(PRIM_TYPEP(var,symbol_type)))
    fd_raise_lisp_exception(fd_SyntaxError,"DOTIMES var not symbol",expr);
  else {
    FD_WITH_LEXICAL_ENV(dotimes_env,env,1) {
      lisp value=FD_VOID;
      fd_bind_value(var,FD_VOID,dotimes_env);
      while (i < limit) {
	fd_set_value(var,LISPFIX(i),dotimes_env); i++;
	eval_exprs_noreturn(body,dotimes_env);
	decref(value);}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();}
  return FD_VOID;
}

static lisp dolist_handler(lisp expr,lispenv env)
{
  lisp spec=get_arg(expr,1,FD_VOID), body=get_body(expr,2);
  lisp var=get_arg(spec,0,FD_VOID);
  lisp values=fd_eval_in_env(get_arg(spec,1,FD_VOID),env);
  if (!(PRIM_TYPEP(var,symbol_type)))
    fd_raise_lisp_exception(fd_SyntaxError,"DOLIST var not symbol",expr);
  else {
    FD_WITH_LEXICAL_ENV(dolist_env,env,1) {
      fd_bind_value(var,FD_VOID,dolist_env);
      {DO_CHOICES(lvalue,values)
	 if (FD_EMPTY_LISTP(lvalue)) {}
	 else if (!(PRIM_TYPEP(lvalue,pair_type)))
	   fd_type_error("not a list",lvalue);
	 else {DOLIST(elt,lvalue) {
	   fd_set_value(var,elt,dolist_env);
	   eval_exprs_noreturn(body,dolist_env);}}
      END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();}
  decref(values);
  return FD_VOID;
}

/** Iteration expressions: WHILE and UNTIL **/

static lisp while_handler(lisp expr,lispenv env)
{
  lisp test_expr=fd_get_arg(expr,1,FD_VOID);
  lisp test_value=fd_eval_in_env(test_expr,env);
  lisp body=fd_get_body(expr,2);
  while (!(LISP_TEST_FALSEP(test_value))) {
    decref(test_value);
    {DOLIST(subexpr,body) {
      lisp v=fd_eval_in_env(subexpr,env); decref(v);}
    test_value=fd_eval_in_env(test_expr,env);}}
  return FD_VOID;
}

static lisp until_handler(lisp expr,lispenv env)
{
  lisp test_expr=fd_get_arg(expr,1,FD_VOID),
    test_value=fd_eval_in_env(test_expr,env);
  lisp body=fd_get_body(expr,2);
  while (LISP_TEST_FALSEP(test_value)) {
    DOLIST(subexpr,body) {
      lisp v=fd_eval_in_env(subexpr,env); decref(v);}
    test_value=fd_eval_in_env(test_expr,env);}
  decref(test_value);
  return FD_VOID;
}

/** Using a symbol value as a cache **/

static lisp symbol_cache_handler(lisp expr,lispenv env)
{
  lisp symbol=get_arg(expr,1,FD_VOID), generator=get_arg(expr,2,FD_VOID);
  if (!(PRIM_TYPEP(symbol,symbol_type)))
    fd_raise_exception(_("Can only cache into symbols"));
  else if (!(FD_EMPTYP(SYMBOL_VALUE(symbol))))
    return incref(SYMBOL_VALUE(symbol));
  else {
    lisp value=fd_eval_in_env(generator,env);
    fd_set_symbol_value(symbol,value);
    return value;}
}

static lisp make_autoload(lisp arg) 
{
  return FD_MAKE_LIST(2,autoload_symbol,incref(arg));
}

static lisp make_autolink(lisp arg) 
{
  return FD_MAKE_LIST(2,autolink_symbol,incref(arg));
}

/** Non-deterministic special forms **/

static lisp choices_to_list_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) fd_raise_exception(fd_TooFewArgs);
  else if (FD_PAIRP(FD_CDR(args)))
    fd_raise_lisp_exception(fd_TooManyArgs,"CHOICES->LIST",args);
  else {
    lisp value=fd_get_arg(args,0,FD_VOID), answer=FD_EMPTY_LIST;
    DO_CHOICES(r,value) answer=FD_MAKE_PAIR(incref(r),answer); END_DO_CHOICES;
    return answer;}
}

static lisp choices_to_vector_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) fd_raise_exception(fd_TooFewArgs);
  else if (FD_PAIRP(FD_CDR(args)))
    fd_raise_lisp_exception(fd_TooManyArgs,"CHOICES->VECTOR",args);
  else {
    lisp given=fd_incref(fd_get_arg(args,0,FD_VOID));
    lisp value=fd_return_proper_choice(given);
    lisp answer=fd_make_vector(FD_CHOICE_SIZE(value));
    int i=0, len=FD_CHOICE_SIZE(value);
    DO_CHOICES(r,value)
      if (i >= len) fd_raise_exception("Choice->vector weirdness");
      else {FD_VECTOR_SET(answer,i++,incref(r));}
    END_DO_CHOICES;
    return answer;}
}

extern lisp eval_exprs(lisp exprs,lispenv env);
extern void eval_exprs_noreturn(lisp exprs,lispenv env);

static lisp dochoices_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var, values, ivar;
  if (PAIRP(spec)) {
    var=fd_get_arg(spec,0,FD_VOID);
    if (!(PRIM_TYPEP(var,symbol_type)))
      fd_raise_lisp_exception(fd_SyntaxError,"DOCHOICES var not symbol",expr);
    values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
    ivar=fd_get_arg(spec,2,FD_FALSE);}
  else if (SYMBOLP(spec)) {
    var=spec; ivar=FD_FALSE; values=fd_symeval(spec,env);}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"DOCHOICES choice expression",expr);
  {
    int i=0;
    UNWIND_PROTECT {
      FD_WITH_LEXICAL_ENV(dochoices_env,env,3) {
	DO_CHOICES(lvalue,values) {
	  fd_bind_value(var,lvalue,dochoices_env);
	  if (!(FD_FALSEP(ivar))) {
	    fd_bind_value(ivar,FD_LISPFIX(i),dochoices_env); i++;}
	  eval_exprs_noreturn(body,dochoices_env);}
	END_DO_CHOICES;}
      FD_END_WITH_LEXICAL_ENV_NOVALUE();}
    ON_UNWIND
      {decref(values);}
    END_UNWIND;}
  return FD_VOID;
}

static lisp for_choices_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var, values, answer=FD_EMPTY_CHOICE;
  if (PAIRP(spec)) {
    var=fd_get_arg(spec,0,FD_VOID);
    if (!(PRIM_TYPEP(var,symbol_type)))
      fd_raise_lisp_exception
	(fd_SyntaxError,"FOR-CHOICES var not symbol",expr);
    values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);}
  else if (SYMBOLP(spec)) {
    var=spec; values=fd_symeval(spec,env);}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"FOR-CHOICES choice expression",expr);
  {
    UNWIND_PROTECT {
      FD_WITH_LEXICAL_ENV(dochoices_env,env,3) {
	DO_CHOICES(lvalue,values) {
	  lisp choice_result;
	  fd_bind_value(var,lvalue,dochoices_env);
	  choice_result=eval_exprs(body,dochoices_env);
	  finish_value(choice_result);
	  ADD_TO_CHOICE(answer,choice_result);}
	END_DO_CHOICES;}
      FD_END_WITH_LEXICAL_ENV(answer);}
    ON_UNWIND {
      decref(values);}
    END_UNWIND;}
  return fd_return_proper_choice(answer);
}

static lisp doresults_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var, values, ivar, wvar, wvar_expr; int window;
  if (PAIRP(spec)) {
    var=fd_get_arg(spec,0,FD_VOID);
    if (!(PRIM_TYPEP(var,symbol_type)))
      fd_raise_lisp_exception(fd_SyntaxError,"DOCHOICES var not symbol",expr);
    values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
    wvar_expr=fd_get_arg(spec,2,FD_LISPFIX(5000));
    ivar=fd_get_arg(spec,3,FD_FALSE);
    if ((FD_NUMBERP(ivar)) &&  (FD_SYMBOLP(wvar_expr))) {
      fd_lisp tmp=ivar; ivar=wvar_expr; wvar_expr=tmp;
      wvar=fd_eval_in_env(wvar_expr,env);
      window=fd_fixlisp(wvar);}
    else {
      wvar=fd_eval_in_env(wvar_expr,env);
      window=fd_fixlisp(wvar);}}
  else if (SYMBOLP(spec)) {
    var=spec; ivar=FD_FALSE; values=fd_symeval(spec,env);}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"DOCHOICES choice expression",expr);
  {
    int i=0;
    UNWIND_PROTECT {
      FD_WITH_LEXICAL_ENV(dochoices_env,env,3) {
	DO_CHOICES(lvalue,values) {
	  if ((window) && ((i%window) == 0)) {
	    fd_lisp prefetch=FD_EMPTY_CHOICE; int j=0;
	    if ((_elt_type) && (_elt_type == object_type)) {
	      fd_lisp elt; elt.type=_elt_type;
	      while (j < window) {
		elt.data=_dscan[j];
		FD_ADD_TO_CHOICE(prefetch,elt); j++;}}
	    else {
	      int j=0; while (j < window) {
		fd_lisp elt=_scan[j++];
		if (FD_OIDP(elt)) {
		  FD_ADD_TO_CHOICE(prefetch,_scan[j]);}}}
	    fd_prefetch_oids(prefetch); fd_decref(prefetch);}
	  fd_bind_value(var,lvalue,dochoices_env);
	  if (!(FD_FALSEP(ivar))) {
	    fd_bind_value(ivar,FD_LISPFIX(i),dochoices_env);}
	  eval_exprs_noreturn(body,dochoices_env); i++;
	  if (FD_OIDP(lvalue)) fd_swap_out(lvalue);}
	END_DO_CHOICES;}
      FD_END_WITH_LEXICAL_ENV_NOVALUE();}
    ON_UNWIND
      {decref(values);}
    END_UNWIND;}
  return FD_VOID;
}

/** Choice processing/iteration forms **/

static lisp filter_choices_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var=fd_get_arg(spec,0,FD_VOID);
  lisp values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
  lisp answer=FD_EMPTY_CHOICE;
  if (PRIM_TYPEP(var,symbol_type)) {
    FD_WITH_LEXICAL_ENV(filter_choices_env,env,4) {
      fd_bind_value(var,FD_VOID,filter_choices_env);
      {DO_CHOICES(lvalue,values) {
	lisp forms=body; int passes=1;
	fd_set_value(var,lvalue,filter_choices_env);
	while ((passes) && (PAIRP(forms))) {
	  lisp test_expr=CAR(forms); 
	  lisp value=fd_eval_in_env(test_expr,filter_choices_env);
	  if (LISP_TEST_FALSEP(value)) passes=0;
	  else {decref(value); forms=CDR(forms);}}
	if (passes) {ADD_TO_CHOICE(answer,incref(lvalue));}}
       END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV(answer);
    decref(values);
    return answer;}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"FILTER-CHOICES var not symbol",expr);
}

static lisp count_choices_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var=fd_get_arg(spec,0,FD_VOID);
  lisp values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
  int count=0;
  if (PRIM_TYPEP(var,symbol_type)) {
    FD_WITH_LEXICAL_ENV(filter_choices_env,env,4) {
      fd_bind_value(var,FD_VOID,filter_choices_env);
      {DO_CHOICES(lvalue,values) {
	lisp forms=body; int passes=1;
	fd_set_value(var,lvalue,filter_choices_env);
	while ((passes) && (PAIRP(forms))) {
	  lisp test_expr=CAR(forms); 
	  lisp value=fd_eval_in_env(test_expr,filter_choices_env);
	  if (LISP_TEST_FALSEP(value)) passes=0;
	  else {decref(value); forms=CDR(forms);}}
	if (passes) count++;}
       END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    decref(values);
    return FD_LISPFIX(count);}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"COUNT-CHOICES var not symbol",expr);
}

static lisp lisp_proper_choice_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) fd_raise_exception(fd_TooFewArgs);
  else if (FD_PAIRP(FD_CDR(args)))
    fd_raise_lisp_exception(fd_TooManyArgs,"PROPER-CHOICE?",args);
  else if (fd_proper_choicep(fd_get_arg(args,0,FD_VOID)))
    return FD_TRUE;
  else return FD_FALSE;
}

/** VALUES and friends **/

static lisp values_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) return FD_VOID;
  else if (FD_EMPTY_LISTP(CDR(args))) return incref(CAR(args));
  else {
    int i=0, size=0; lisp_vector v=fd_malloca(struct FD_VECTOR); lisp *elts;
    {DOLIST(elt,args) size++;}
    if (size) elts=v->elements=fd_malloc(size*sizeof(lisp));
    else v->elements=NULL;
    v->length=size;
    while (i < size) {
      elts[i]=incref(CAR(args)); args=CDR(args); i++;}
    {RETURN_LISP(multiple_value_type,vector,v);}}
}

static lisp call_with_values_cproc(lisp producer,lisp consumer)
{
  lisp values=fd_apply(producer,FD_EMPTY_LIST);
  lisp results=FD_EMPTY_CHOICE;
  DO_CHOICES(vals,values) {
    lisp arglist=FD_EMPTY_LIST, result;
    if (FD_VOIDP(vals)) arglist=FD_EMPTY_LIST;
    else if (PRIM_TYPEP(vals,multiple_value_type)) {
      lisp *elts=FD_VECTOR_ELEMENTS(vals); int i=VECTOR_LENGTH(vals);
      while (i > 0) {
	i--; arglist=FD_MAKE_PAIR(incref(elts[i]),arglist);}}
    else arglist=FD_MAKE_LIST1(incref(vals));
    result=fd_apply(consumer,arglist); decref(arglist);
    ADD_TO_CHOICE(results,result);}
  END_DO_CHOICES;
  decref(values);
  return results;
}

static lisp apply_to_values_handler(lisp expr,fd_lispenv env)
{
  lisp proc=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp vals_choice=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp results=FD_EMPTY_CHOICE;
  DO_CHOICES(vals,vals_choice) {
    lisp arglist=FD_EMPTY_LIST, result;
    if (PRIM_TYPEP(vals,multiple_value_type)) {
      lisp *elts=FD_VECTOR_ELEMENTS(vals); int i=VECTOR_LENGTH(vals);
      if (i == 0) arglist=FD_MAKE_LIST1(FD_VOID);
      else while (i > 0) {
	i--; arglist=FD_MAKE_PAIR(incref(elts[i]),arglist);}}
    else arglist=FD_MAKE_LIST1(incref(vals));
    result=fd_apply(proc,arglist); decref(arglist);
    ADD_TO_CHOICE(results,result);}
  END_DO_CHOICES;
  decref(proc); decref(vals_choice);
  return results;
}

static lisp multiple_value_bind_handler(lisp expr,fd_lispenv env)
{
  lisp vars=fd_get_arg(expr,1,FD_VOID);
  lisp generator=fd_get_arg(expr,2,FD_VOID);
  lisp values=fd_eval_in_env(generator,env);
  lisp body=fd_get_body(expr,3);
  lisp results=FD_EMPTY_CHOICE;
  DO_CHOICES(val,values) {
    fd_lisp value=FD_EMPTY_CHOICE;
    int m_valuesp=(FD_PRIM_TYPEP(val,multiple_value_type));
    int n_values=((m_valuesp) ? (FD_VECTOR_LENGTH(val)) : (1));
    FD_WITH_LEXICAL_ENV(mvb_env,env,4) {
      int i=0; DOLIST(var,vars) {
	if (m_valuesp)
	  if (i >= n_values) fd_bind_value(var,FD_VOID,mvb_env);
	  else fd_bind_value(var,FD_VECTOR_REF(val,i),mvb_env);
	else if (i == 0) fd_bind_value(var,val,mvb_env);
	else fd_bind_value(var,FD_VOID,mvb_env);
	i++;}
      value=eval_exprs(body,mvb_env);
      finish_value(value);}
    FD_END_WITH_LEXICAL_ENV(value);
    FD_ADD_TO_CHOICE(results,value);}
  END_DO_CHOICES;
  fd_decref(values);
  return results;
}  

/** PROG1 **/

static lisp prog1_handler(lisp expr,fd_lispenv env)
{
  fd_lisp first_expr=fd_get_arg(expr,1,FD_VOID);
  fd_lisp remaining=fd_get_body(expr,2);
  fd_lisp retval=fd_eval_in_env(first_expr,env);
  DOLIST(elt,remaining) {
    fd_lisp v=fd_eval_in_env(elt,env); fd_decref(v);}
  return retval;
}

/** CACHECALL **/

static fd_hashtable callcaches;
static fd_hashtable get_callcache(fd_lisp fcn,fd_lisp *lptr);

static fd_lisp do_cachecall(fd_lisp fcn,fd_lisp args,fd_lisp key)
{
  fd_lisp ptr; fd_hashtable callcache=get_callcache(fcn,&ptr); 
  fd_lisp value=fd_hashtable_get(callcache,key,FD_VOID);
  if (FD_VOIDP(value)) {
    value=fd_apply(fcn,args);
    fd_hashtable_set(callcache,key,value);
    fd_decref(ptr);
    return value;}
  else {
    fd_decref(ptr);
    return value;}
}

static fd_lisp cachecall_lexpr(fd_lisp fcn_and_args)
{
  fd_lisp fcn=fd_get_arg(fcn_and_args,0,FD_VOID);
  fd_lisp args=fd_get_body(fcn_and_args,1);
  if (FD_CHOICEP(fcn)) {
    fd_lisp results=FD_EMPTY_CHOICE;
    FD_DO_CHOICES(fn,fcn) {
      fd_lisp some_results=do_cachecall(fn,args,args);
      FD_ADD_TO_CHOICE(results,some_results);}
    FD_END_DO_CHOICES;
    return results;}
  else return do_cachecall(fcn,args,args);
}

static fd_lisp cachecall_key_lexpr(fd_lisp fcn_key_and_args)
{
  fd_lisp fcn=fd_get_arg(fcn_key_and_args,0,FD_VOID);
  fd_lisp key=fd_get_arg(fcn_key_and_args,1,FD_VOID);
  fd_lisp args=fd_get_body(fcn_key_and_args,2);
  if (FD_CHOICEP(fcn)) {
    fd_lisp results=FD_EMPTY_CHOICE;
    FD_DO_CHOICES(fn,fcn) {
      fd_lisp some_results=do_cachecall(fn,args,key);
      FD_ADD_TO_CHOICE(results,some_results);}
    FD_END_DO_CHOICES;}
  else return do_cachecall(fcn,args,key);
}

static fd_lisp cachecall1_lexpr(fd_lisp fcn_and_args)
{
  fd_lisp fcn=fd_get_arg(fcn_and_args,0,FD_VOID);
  fd_lisp args=fd_get_body(fcn_and_args,1);
  if (FD_CHOICEP(fcn)) {
    fd_lisp results=FD_EMPTY_CHOICE;
    FD_DO_CHOICES(fn,fcn) {
      fd_lisp some_results=do_cachecall(fn,args,FD_CAR(args));
      FD_ADD_TO_CHOICE(results,some_results);}
    FD_END_DO_CHOICES;}
  else return do_cachecall(fcn,args,FD_CAR(args));
}

static fd_hashtable get_callcache(fd_lisp fcn,fd_lisp *ptr)
{
  fd_lisp table=fd_hashtable_get(callcaches,fcn,FD_VOID);
  if (FD_VOIDP(table)) {
    fd_hashtable htable=fd_make_hashtable(32);
    *ptr=fd_make_cptr(hashtable_type,(void *)htable);
    fd_hashtable_dedangle(callcaches);
    fd_hashtable_set(callcaches,fcn,*ptr); 
    return htable;}
  else {
    *ptr=table; return FD_CPTR_DATA(table);}
}

static fd_lisp cachecall_clear(fd_lisp fcn)
{
  fd_lisp table=fd_hashtable_get(callcaches,fcn,FD_VOID);
  if (FD_VOIDP(table)) return FD_FALSE;
  else {
    fd_hashtable h=FD_CPTR_DATA(table);
    fd_reinit_hashtable(h,h->n_keys*2,0);
    return FD_TRUE;}
}

static void clear_cachecall_table(fd_lisp fcn,fd_lisp table,void *ptr)
{
  if (FD_PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=FD_CPTR_DATA(table);
    fd_reinit_hashtable(h,h->n_keys*2,0);}
}

static fd_lisp cachecall_clear_all()
{
  fd_hashtable_map(callcaches,clear_cachecall_table,NULL);
  return FD_TRUE;
}

/** CLEAR-SYMBOLS-CHANGED! **/

static lisp clear_env_changes_handler(lisp expr,fd_lispenv env)
{
  if (env->module) {
    decref(env->module->changes);
    env->module->changes=FD_EMPTY_CHOICE;}
  return FD_VOID;
}

/** Initialization **/

void fd_initialize_special_c()
{
  t_symbol=fd_make_symbol("T");
  else_symbol=fd_make_symbol("ELSE");
  cond_apply_symbol=fd_make_symbol("=>");
  
  debug_fdscript_symbol=fd_make_symbol("%DEBUG");

  bq_symbol=fd_make_symbol("QUASIQUOTE");
  bq_escape=fd_make_symbol("UNQUOTE");
  bq_splice=fd_make_symbol("UNQUOTE-SPLICING");

  autoload_symbol=fd_make_symbol("AUTOLOAD");
  autolink_symbol=fd_make_symbol("AUTOLINK");

  callcaches=fd_make_hashtable(64);

  fd_add_restricted_special_form("EVAL",lisp_eval_handler);
  fd_add_restricted_cproc("SAFE-EVAL",1,lisp_safe_eval_cproc);
  fd_add_restricted_cproc("REMOTE-EVAL",2,lisp_remote_eval_cproc);
  fd_add_lexpr(NULL,"APPLY",FD_NORMAL_LEXPR,apply_handler);
  fd_add_special_form(NULL,"COND",cond_handler);
  fd_add_special_form(NULL,"CASE",case_handler);
  fd_add_special_form(NULL,"QASE",qase_handler);
  fd_add_special_form(NULL,"WHEN",when_handler);
  fd_add_special_form(NULL,"UNLESS",unless_handler);
  fd_add_special_form(NULL,"QUOTE",quote_handler);
  fd_add_special_form(NULL,"BACKQUOTE",bq_handler);
  fd_add_alias(NULL,"QUASIQUOTE","BACKQUOTE");
  fd_add_special_form(NULL,"BEGIN",begin_handler);
  fd_add_special_form(NULL,"PROG1",prog1_handler);
  fd_add_special_form(NULL,"SCACHE",symbol_cache_handler);

  fd_add_special_form(NULL,"LETREC",letrec_handler);
  fd_add_special_form(NULL,"LET",let_handler);
  fd_add_special_form(NULL,"LET*",let_star_handler);
  fd_add_restricted_special_form("SET!",set_value_handler);
  fd_add_special_form(fd_global_env,"SET!",safe_set_value_handler);
  fd_add_restricted_special_form("UNSET!",unset_value_handler);
  fd_add_restricted_special_form("SET-IN-ENV!",set_in_env_handler);
  fd_add_restricted_special_form("SET+!",set_plus_handler);
  fd_add_special_form(fd_global_env,"SET+!",set_plus_handler);
  fd_add_restricted_special_form("TSET!",tset_value_handler); 
  
  fd_add_restricted_special_form("GSET!",global_set_value_handler);
  fd_add_restricted_cproc("GVALUE",1,gvalue_proc);

  fd_add_special_form(NULL,"BOUND?",lisp_boundp_handler);
  fd_add_special_form(NULL,"SYMBOL-BOUND?",lisp_symbol_boundp_handler);

  fd_add_special_form(NULL,"DTCALL",dtcall_handler);
  fd_add_restricted_cproc("LOAD-DLL",1,load_dll);
  fd_add_restricted_cproc("AUTOLOAD",1,make_autoload);
  fd_add_restricted_cproc("AUTOLINK",1,make_autolink);

  fd_add_lexpr(NULL,"RETURN-ERROR",FD_ND_LEXPR,return_error_lexpr);
  fd_add_lexpr(NULL,"RETURN-EXCEPTION",FD_ND_LEXPR,return_exception_lexpr);
  fd_add_lexpr(NULL,"SIGNAL-EXCEPTION",FD_ND_LEXPR,signal_exception_lexpr);

  fd_add_special_form(NULL,"DO",do_handler);
  fd_add_special_form(NULL,"DOTIMES",dotimes_handler);
  fd_add_special_form(NULL,"DOLIST",dolist_handler);
  fd_add_special_form(NULL,"WHILE",while_handler);
  fd_add_special_form(NULL,"UNTIL",until_handler);

  fd_add_lexpr(NULL,"LIST",FD_NORMAL_LEXPR,list_handler);
  fd_add_cproc(NULL,"LIST->VECTOR",1,list2vector);
  fd_add_cproc(NULL,"VECTOR->LIST",1,vector2list);
  fd_add_lexpr(NULL,"MAP",FD_NORMAL_LEXPR,map_lexpr);
  fd_add_lexpr(NULL,"FOR-EACH",FD_NORMAL_LEXPR,for_each_lexpr);
  fd_add_cproc(NULL,"NULL?",1,null_listp);

  fd_add_cproc(NULL,"DYNAMIC-WIND",3,dynamic_wind_cproc);
  fd_add_special_form(NULL,"UNWIND-PROTECT",unwind_protect_handler);
  fd_add_special_form(NULL,"ON-ERROR",on_error_handler);
  fd_add_special_form(NULL,"CATCH-ERRORS",catch_errors_handler);
  fd_add_special_form(NULL,"SIGNALS-ERROR?",signals_errorp_handler);
  fd_add_special_form(NULL,"SIGNALS-ERROR+?",signals_errorp_plus_handler);

  fd_add_cproc(NULL,"ERROR?",1,lisp_errorp);
  fd_add_cproc(NULL,"ERROR-EXCEPTION",1,lisp_error_exception);
  fd_add_cproc(NULL,"ERROR-DETAILS",1,lisp_error_details);
  fd_add_cproc(NULL,"ERROR-IRRITANT",1,lisp_error_irritant);

  fd_add_lexpr(NULL,"CACHECALL",FD_ND_LEXPR,cachecall_lexpr);
  fd_add_lexpr(NULL,"CACHECALL1",FD_ND_LEXPR,cachecall1_lexpr);
  fd_add_lexpr(NULL,"CACHECALL-KEY",FD_ND_LEXPR,cachecall_key_lexpr);
  fd_add_cproc(NULL,"CACHECALL-CLEAR!",1,cachecall_clear);
  fd_add_cproc(NULL,"CACHECALL-CLEAR-ALL!",0,cachecall_clear_all);

  fd_add_cproc(NULL,"CALL-WITH-CURRENT-CONTINUATION",1,callcc);
  fd_add_alias(NULL,"CALL/CC","CALL-WITH-CURRENT-CONTINUATION");

  fd_add_lexpr(NULL,"VALUES",FD_NORMAL_LEXPR,values_lexpr);
  fd_add_cproc(NULL,"CALL-WITH-VALUES",2,call_with_values_cproc);
  fd_add_special_form(NULL,"APPLY-TO-VALUES",apply_to_values_handler);
  fd_add_special_form(NULL,"MULTIPLE-VALUE-BIND",multiple_value_bind_handler);

  fd_add_lexpr(NULL,"CHOICES->VECTOR",FD_ND_LEXPR,choices_to_vector_lexpr);
  fd_add_lexpr(NULL,"CHOICES->LIST",FD_ND_LEXPR,choices_to_list_lexpr);
  fd_add_alias(NULL,"CHOICE->LIST","CHOICES->LIST");
  fd_add_special_form(NULL,"DO-CHOICES",dochoices_handler);
  fd_add_special_form(NULL,"DO-RESULTS",doresults_handler);
  fd_add_special_form(NULL,"FILTER-CHOICES",filter_choices_handler);
  fd_add_special_form(NULL,"COUNT-CHOICES",count_choices_handler);
  fd_add_special_form(NULL,"FOR-CHOICES",for_choices_handler);
  fd_add_lexpr
    (NULL,"PROPER-CHOICE?",FD_ND_LEXPR,lisp_proper_choice_lexpr);

  fd_add_special_form(NULL,"CLEAR-ENV-CHANGES!",clear_env_changes_handler);

  fd_register_source_file("special",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: special.c,v $
   Revision 1.36  2005/03/22 03:55:39  haase
   Fix failure to return value from cachecall with non-deterministic function arg

   Revision 1.35  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.34  2004/08/24 15:21:04  haase
   Added DO-RESULTS implementation which prefetches a window of choices as it proceeds

   Revision 1.33  2004/08/24 14:47:05  haase
   Added do-results which does OID prefetching

   Revision 1.32  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.31  2004/07/19 14:54:27  haase
   Added count-choices

   Revision 1.30  2004/07/14 13:43:15  haase
   Fixed cachecall variants

   Revision 1.29  2004/05/16 14:03:12  haase
   Added CACHECALL-CLEAR-ALL\!

   Revision 1.28  2004/05/16 09:08:30  haase
   Fixed firstv problem with letrec

   Revision 1.27  2004/05/15 18:55:35  haase
   Fix memory leak in DO

   Revision 1.26  2004/04/03 19:57:38  haase
   Fixed leak introduced when stripping multiple values when binding in let and let*

   Revision 1.25  2004/04/02 12:44:14  haase
   Made binding constructs just bind single values (not multiple value returns)

   Revision 1.24  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.23  2003/11/30 17:03:58  haase
   Remodularized cachecall

   Revision 1.22  2003/11/29 18:01:10  haase
   Fixed bug in cachecall_clear

   Revision 1.21  2003/11/29 14:07:45  haase
   Added cachecall-clear\!

   Revision 1.20  2003/11/26 12:36:15  haase
   Added cachecall automatic caching function

   Revision 1.19  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.18  2003/09/13 21:57:56  haase
   Fixed automatic closing of unused network connections

   Revision 1.17  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.16.2.4  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.16.2.3  2003/01/26 20:56:06  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.16.2.2  2002/08/13 01:26:23  haase
   Fixed some FD_ prefix problems

   Revision 1.16.2.1  2002/08/01 15:22:37  haase
   Fixed bug in interaction of quoted choices with APPLY

   Revision 1.16  2002/07/01 02:44:53  haase
   Added probably redundant bounds check to choice->vector

   Revision 1.15  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.14  2002/06/24 14:52:20  haase
   Added CHOICES->VECTOR

   Revision 1.13  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.12  2002/05/11 13:29:12  haase
   Fixed SIGNALS-ERROR? to take single argument and added SIGNALS-ERROR+? to return actual return values

   Revision 1.11  2002/04/18 20:28:41  haase
   Added multiple-value-bind

   Revision 1.10  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.9  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
