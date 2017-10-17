/* C Mode */

/* reflect.c
   FDScript reflection functions (for interrogating the language itself)
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

static char vcid[] = "$Id: reflect.c,v 1.21 2005/01/14 16:48:49 haase Exp $";

/** Meta evaluator functions **/
/** APROPOS **/
/** Getting properties of procedures **/
/** Initialization **/

#include "fdscript.h"

static lisp lambda_symbol;

/** Meta evaluator functions **/

static lisp lisp_primitivep(lisp x)
{
  if (PRIM_TYPEP(x,cproc_type)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_environmentp(lisp x)
{
  if (PRIM_TYPEP(x,env_type)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_procedurep(lisp x)
{
  if ((PRIM_TYPEP(x,cproc_type)) || (PRIM_TYPEP(x,continuation_type)) ||
      (PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_compound_procedurep(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_applicablep(lisp x)
{
  if (((PRIM_TYPEP(x,cproc_type)) &&
       ((PTR_DATA(x,cproc))->n_args != FD_SPECIAL_FORM)) ||
      (PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type)) || (PRIM_TYPEP(x,rproc_type)) ||
      (PRIM_TYPEP(x,continuation_type)) ||
      ((PAIRP(x)) && (LISP_EQ(CAR(x),lambda_symbol))))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_continuationp(lisp x)
{
  if (PRIM_TYPEP(x,continuation_type)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_remote_procedurep(lisp x)
{
  if (PRIM_TYPEP(x,rproc_type)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_goal_procedurep(lisp x)
{
  if (PRIM_TYPEP(x,gproc_type)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_special_formp(lisp x)
{
  if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc p=PTR_DATA(x,cproc);
    if (p->n_args == FD_SPECIAL_FORM) return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp lisp_lexprp(lisp x)
{
  if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc p=PTR_DATA(x,cproc);
    if ((p->n_args == FD_NORMAL_LEXPR) ||
	(p->n_args == FD_ND_LEXPR))
      return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp lisp_ndlexprp(lisp x)
{
  if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc p=PTR_DATA(x,cproc);
    if (p->n_args == FD_ND_LEXPR)
      return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp lisp_dtype_serverp(lisp x)
{
  if (PRIM_TYPEP(x,dtype_server_type)) return FD_TRUE;
  else return FD_FALSE;
}

/** APROPOS **/

static lisp apropos_result; static char *apropos_query;

static void apropos_loop(lisp symbol)
{
  if (strstr(SYMBOL_NAME(symbol),apropos_query))
    apropos_result=FD_MAKE_PAIR(symbol,apropos_result);
}

static lisp apropos(lisp arg)
{
  lisp temp; apropos_result=FD_EMPTY_LIST;
  if (STRINGP(arg)) apropos_query=STRING_DATA(arg);
  else if (SYMBOLP(arg)) apropos_query=SYMBOL_NAME(arg);
  else return FD_EMPTY_CHOICE;
  fd_for_all_symbols(apropos_loop);
  temp=apropos_result; apropos_result=FD_EMPTY_LIST;
  return temp;
}

/** Getting properties of procedures **/

static lisp arg_length(lisp args)
{
  int i=0;
  while (PAIRP(args)) {i++; args=CDR(args);}
  if (FD_EMPTY_LISTP(args)) return LISPFIX(i);
  else return FD_FALSE;
}

static lisp lisp_get_arg_lexpr(lisp args)
{
  fd_lisp the_args, pos_arg; int len=fd_list_length(args);
  if (len < 2)
    fd_raise_lisp_exception(fd_TooFewArgs,"GET-ARG",args);
  if (len > 3)
    fd_raise_lisp_exception(fd_TooManyArgs,"GET-ARG",args);
  the_args=fd_get_arg(args,0,FD_VOID); pos_arg=fd_get_arg(args,1,FD_VOID);
  if (!(FD_FIXNUMP(pos_arg))) fd_type_error("not a fixnum",pos_arg);
  if (len == 3) {
    fd_lisp dflt=fd_get_arg(args,2,FD_VOID);
    fd_lisp actual=fd_get_arg(the_args,FD_FIXLISP(pos_arg),dflt);
    return fd_incref(actual);}
  else return fd_incref(fd_get_arg(the_args,FD_FIXLISP(pos_arg),FD_VOID));
}

static lisp lisp_procedure_arity(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) 
    return arg_length(CAR(CDR(FD_GET_SPROC(x)->lambda)));
  else if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc c=PTR_DATA(x,cproc);
    if (c->n_args >= 0) return LISPFIX(c->n_args);
    else if (c->n_args == FD_ND_LEXPR) return FD_FALSE;
    else if (c->n_args == FD_NORMAL_LEXPR) return FD_FALSE;
    else return FD_EMPTY_CHOICE;}
  else fd_type_error(_("not a procedure"),x);
}

static lisp lisp_procedure_arguments(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) 
    return incref(CAR(CDR(FD_GET_SPROC(x)->lambda)));
  else if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc c=PTR_DATA(x,cproc); 
    if (c->n_args == 0) return FD_EMPTY_LIST;
    if (c->n_args >= 0) {
      lisp args=FD_EMPTY_LIST; int n=c->n_args-1;
      while (n >= 0) {
	char argname[16]; sprintf(argname,"ARG%d",n);
	n--; args=FD_MAKE_PAIR(fd_make_symbol(argname),args);}
      return args;}
    else if (c->n_args == FD_ND_LEXPR)
      return fd_make_symbol("NDARGLIST");
    else if (c->n_args == FD_NORMAL_LEXPR)
      return fd_make_symbol("ARGLIST");
    else return FD_EMPTY_CHOICE;}
  else fd_type_error(_("not a procedure"),x);
}

static lisp lisp_procedure_name(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) 
    return incref(CAR(FD_GET_SPROC(x)->lambda));
  else if (PRIM_TYPEP(x,cproc_type)) {
    fd_cproc c=PTR_DATA(x,cproc);
    return fd_make_symbol(c->name);}
  else fd_type_error(_("not a procedure"),x);
}

static lisp lisp_procedure_body(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) 
    return incref(CDR(CDR(FD_GET_SPROC(x)->lambda)));
  else if (PRIM_TYPEP(x,cproc_type))
    fd_raise_lisp_exception(_("I ain't got no body..."),"PROCEDURE-BODY",x);
  else fd_type_error(_("not a procedure"),x);
}

static lisp lisp_procedure_environment(lisp x)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) {
    fd_lispenv env=fd_mallocd_env(FD_GET_SPROC(x)->env);
    return fd_make_cptr(env_type,env);}
  else if (PRIM_TYPEP(x,cproc_type)) return FD_EMPTY_CHOICE;
  else fd_type_error(_("not a procedure"),x);
}

static lisp lisp_set_procedure_body(lisp x,lisp expr)
{
  if ((PRIM_TYPEP(x,sproc_type)) || (PRIM_TYPEP(x,ssproc_type)) ||
      (PRIM_TYPEP(x,gproc_type))) {
    fd_lisp lambda=FD_CDR(FD_GET_SPROC(x)->lambda);
    fd_decref(FD_CDR(lambda));
    RPLACD(lambda,fd_incref(expr));
    return FD_VOID;}
  else if (PRIM_TYPEP(x,cproc_type))
    fd_raise_lisp_exception(_("I ain't got no body..."),"SET-PROCEDURE-BODY!",x);
  else fd_type_error(_("not a procedure"),x);
}

/** Modules **/

static lisp lisp_modulep(lisp x)
{
  if (PRIM_TYPEP(x,env_type))
    if (((fd_lispenv)(CPTR_DATA(x)))->module) return FD_TRUE;
    else return FD_FALSE;
  else return FD_FALSE;
}

static lisp lisp_restricted_module_handler(lisp module_expr,fd_lispenv env)
{
  fd_lispenv safe_env=fd_make_module();
  DOLIST(expr,CDR(module_expr)) {
    lisp v=fd_eval_in_env(expr,safe_env); decref(v);}
  return fd_make_cptr(env_type,safe_env);
}

static lisp lisp_enabled_module_handler(lisp module_expr,fd_lispenv env)
{
  fd_lispenv new_env=fd_make_module();
  fd_module_uses(new_env,fd_enabled_env);
  {DOLIST(expr,CDR(module_expr)) {
    lisp v=fd_eval_in_env(expr,new_env); decref(v);}}
  return fd_make_cptr(env_type,new_env);
}

static lisp module_export_handler(lisp expr,fd_lispenv env)
{
  lisp syms=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  DO_CHOICES(sym,syms)
    if (env->module) fd_module_export(env,sym);
    else fd_raise_exception(_("Can't export from non-module"));
  END_DO_CHOICES;
  decref(syms);
  return FD_VOID;
}

static lisp module_containsp(lisp symbol,lisp module)
{
  if (PRIM_TYPEP(module,env_type))
    if (((fd_lispenv)(CPTR_DATA(module)))->module) {
      fd_lispenv env=(CPTR_DATA(module));
      if (fd_hashtable_probe(&((env->module)->bindings),symbol))
	return FD_TRUE;
      else return FD_FALSE;}
    else fd_type_error(_("not a module"),module);
  else fd_type_error(_("not a module"),module);
}

static fd_lisp hashtable_keys(fd_hashtable h)
{
  lisp answer=FD_EMPTY_CHOICE;
  FD_WITH_MUTEX_LOCKED(&(h->lock)) {
    fd_pair *scan=h->table, *limit=scan+h->n_slots;
    while (scan < limit)
      if (*scan) {
	ADD_TO_CHOICE(answer,incref((*scan)->car));
	scan++;}
      else scan++;}
  END_WITH_MUTEX_LOCKED(&(h->lock));
  return answer;
}

static lisp get_module_exports(lisp module)
{
  if (PRIM_TYPEP(module,env_type))
    if (((fd_lispenv)(CPTR_DATA(module)))->module) {
      fd_hashset h=&((((fd_lispenv)(CPTR_DATA(module)))->module)->exports);
      return fd_hashset_elts(h);}
    else fd_type_error(_("not a module"),module);
  else fd_type_error(_("not a module"),module);
}

static lisp get_module_bindings(lisp module)
{
  if (PRIM_TYPEP(module,env_type))
    if (((fd_lispenv)(CPTR_DATA(module)))->module) {
      fd_hashtable h=&((((fd_lispenv)(CPTR_DATA(module)))->module)->bindings);
      return hashtable_keys(h);}
    else fd_type_error(_("not a module"),module);
  else fd_type_error(_("not a module"),module);
}

static lisp get_global_modules()
{
  return fd_hashset_elts(&(fd_module_table->module->exports));
}

static lisp get_all_modules()
{
  return fd_hashset_elts(&(fd_module_table->module->exports));
}

static lisp get_module_cproc(lisp module)
{
  fd_lispenv env=NULL;
  WITH_HANDLING {
    if (SYMBOLP(module))
      env=fd_get_module(SYMBOL_NAME(module));
    else if (STRINGP(module))
      env=fd_get_module(FD_STRING_DATA(module));
    else env=NULL;}
  ON_EXCEPTION
    if (fd_theException() == fd_ModuleNotFound) {
      fd_clear_exception();}
    else fd_reraise();
  END_HANDLING;
  if (env) return fd_make_cptr(env_type,fd_mallocd_env(env));
  else return FD_EMPTY_CHOICE;
}

static lisp get_enabled_module_cproc(lisp module)
{
  fd_lispenv env=NULL;
  WITH_HANDLING {
    if (SYMBOLP(module))
      env=fd_get_module(SYMBOL_NAME(module));
    else if (STRINGP(module))
      env=fd_get_module(FD_STRING_DATA(module));
    else env=NULL;}
  ON_EXCEPTION
    if (fd_theException() == fd_ModuleNotFound) {
      fd_clear_exception();}
    else fd_reraise();
  END_HANDLING;
  if (env) return fd_make_cptr(env_type,fd_mallocd_env(env));
  else return FD_EMPTY_CHOICE;
}

/** Initialization **/
void fd_initialize_reflect_c()
{
  lambda_symbol=fd_make_symbol("LAMBDA");

  fd_add_cproc(NULL,"SPECIAL-FORM?",1,lisp_special_formp);
  fd_add_cproc(NULL,"PRIMITIVE?",1,lisp_primitivep);
  fd_add_cproc(NULL,"CONTINUATION?",1,lisp_continuationp);
  fd_add_cproc(NULL,"PROCEDURE?",1,lisp_procedurep);
  fd_add_cproc(NULL,"COMPOUND-PROCEDURE?",1,lisp_compound_procedurep);
  fd_add_cproc(NULL,"APPLICABLE?",1,lisp_applicablep);
  fd_add_cproc(NULL,"ENVIRONMENT?",1,lisp_environmentp);
  fd_add_cproc(NULL,"REMOTE-PROCEDURE?",1,lisp_remote_procedurep);
  fd_add_cproc(NULL,"GOAL-PROCEDURE?",1,lisp_goal_procedurep);
  fd_add_cproc(NULL,"SPECIAL-FORM?",1,lisp_special_formp);
  fd_add_cproc(NULL,"LEXPR?",1,lisp_lexprp);
  fd_add_cproc(NULL,"ND-LEXPR?",1,lisp_ndlexprp);
  fd_add_cproc(NULL,"DTYPE-SERVER?",1,lisp_dtype_serverp);

  fd_add_cproc(NULL,"PROCEDURE-NAME",1,lisp_procedure_name);
  fd_add_cproc(NULL,"PROCEDURE-ARITY",1,lisp_procedure_arity);
  fd_add_alias(NULL,"N-ARGS","PROCEDURE-ARITY");
  fd_add_cproc(NULL,"PROCEDURE-ARGUMENTS",1,lisp_procedure_arguments);
  fd_add_alias(NULL,"ARGLIST","PROCEDURE-ARGUMENTS");
  fd_add_cproc(NULL,"PROCEDURE-BODY",1,lisp_procedure_body);

  fd_add_cproc(NULL,"PROCEDURE-ENVIRONMENT",1,lisp_procedure_environment);
  fd_add_cproc(NULL,"SET-PROCEDURE-BODY!",2,lisp_set_procedure_body);

  fd_add_special_form(NULL,"MODULE-EXPORT!",module_export_handler);
  fd_add_cproc(NULL,"MODULE?",1,lisp_modulep);
  fd_add_special_form(NULL,"SAFE-MODULE",lisp_restricted_module_handler);
  fd_add_alias(NULL,"RESTRICTED-MODULE","SAFE-MODULE");
  fd_add_cproc(NULL,"MODULE-CONTAINS?",2,module_containsp);
  fd_add_restricted_special_form
    ("STANDARD-MODULE",lisp_enabled_module_handler);
  fd_add_restricted_special_form
    ("ENABLED-MODULE",lisp_enabled_module_handler);

  fd_add_lexpr(NULL,"GET-ARG",FD_NORMAL_LEXPR,lisp_get_arg_lexpr);

  fd_add_cproc(NULL,"GET-MODULE-EXPORTS",1,get_module_exports);
  fd_add_cproc(NULL,"GET-MODULE-BINDINGS",1,get_module_bindings);
  fd_add_cproc(fd_global_env,"ALL-MODULES",0,get_global_modules);
  fd_add_restricted_cproc("ALL-MODULES",0,get_all_modules);

  fd_add_cproc(fd_global_env,"GET-MODULE",1,get_module_cproc);
  fd_add_restricted_cproc("GET-MODULE",1,get_enabled_module_cproc);

  fd_add_cproc(NULL,"APROPOS",1,apropos);

  fd_register_source_file("reflect",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: reflect.c,v $
   Revision 1.21  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.20  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.19  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.18  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.17  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.16.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.16.2.1  2003/01/26 20:56:06  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.16  2002/06/15 20:04:27  haase
   Made RESTRICTED-MODULE be an alias

   Revision 1.15  2002/04/24 20:06:20  haase
   Fixed problems with GET-ARG

   Revision 1.14  2002/04/19 19:28:56  haase
   Added compound-procedure?
   Made GPROCS be procedure? true
   Fixed error in SET-PROCEDURE-BODY!

   Revision 1.13  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
