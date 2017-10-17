/* C Mode */

/* lambda.c
   Compound procedures in FDScript
   Together with procedures for handling environments and modules
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

static char vcid[] = "$Id: lambda.c,v 1.23 2005/01/14 16:48:44 haase Exp $";

/** Modules **/
/** Dealing with environments **/
/** Looking up symbol values **/
/** Changing symbol values **/
/** Interpreted procedures (SPROCs) **/
/** Synchronized Interpreted procedures (SSPROCs) **/
/** Goal-like Interpreted procedures (GPROCs) **/
/** Local and global DEFINE **/
/** Delays **/
/** Making Modules **/
/** Initialization **/

#include "fdeval.h"

static lisp goals_symbol, delay_symbol;

static fd_exception CantModifyGlobalEnvironment=_("Can't modify global environment");

FRAMERD_EXPORT fd_lispenv fd_module_table, fd_global_env;
FRAMERD_EXPORT fd_lisp fd_safe_symbol;
fd_lispenv fd_module_table, fd_global_env;
fd_lisp fd_safe_symbol;

static fd_lisp fake_sproc(lisp sp);

STATIC_INLINE fd_lisp unquote_choice(lisp x)
{
  if (PRIM_TYPEP(x,quoted_choice_type)) {
    RETURN_LISP(choice_type,choice,PTR_DATA(x,choice));}
  else if (FD_QUOTED_EMPTY_CHOICEP(x)) return FD_EMPTY_CHOICE;
  else return x;
}

/* From inside fdscript but not exported */
extern lisp eval_exprs(lisp exprs,lispenv env);

/* Threadsafe version of SYMBOL_VALUE */
FASTOP lisp FAST_SYMBOL_VALUE(lisp x)
{
  lisp v;
  FD_LOCK_CELL(x);
  v=(PTR_DATA(x,symbol)->value);
  incref(v);
  FD_UNLOCK_CELL(x);
  return v;
}

/** Modules **/

static struct FD_MODULE *top_level_module;

STATIC_INLINE lisp module_ref(struct FD_MODULE *m,lisp sym)
{
  lisp v=fd_hashtable_get(&(m->bindings),sym,FD_VOID);
  if (FD_VOIDP(v)) {
    int i=0, limit=m->n_uses;
    while (i < limit) {
      struct FD_MODULE *im=m->uses[i]->module;
      if (fd_hashset_get(&(im->exports),sym)) {
	lisp v=fd_hashtable_get(&(im->bindings),sym,FD_VOID);
	if (FD_VOIDP(v)) return module_ref(im,sym);
	else return v;}
      else i++;}
    return FD_VOID;}
  else return v;
}

static lisp module_lookup(lisp symbol,fd_lispenv env)
{
  struct FD_MODULE *m=env->module;
  if (m == NULL) return FD_VOID;
  else if (fd_hashset_get(&(m->exports),symbol))
    return fd_hashtable_get(&(m->bindings),symbol,FD_VOID);
  else return FD_VOID;
}

FRAMERD_EXPORT
/* fd_module_uses:
    Arguments: two pointers to modules
    Returns: void
    Adds the second argument to the uses for the first argument,
if it is not already included. */
void fd_module_uses(fd_lispenv env,fd_lispenv use)
{
  struct FD_MODULE *module=env->module;
  if (module == NULL) module=top_level_module;
  if (module->n_uses >= 40)
    fd_raise_exception(_("Module has too many inclusions"));
  else {
    int i=0, lim=module->n_uses;
    while (i < lim)
      if (module->uses[i] == use) return;
      else i++;
    module->uses[i]=use;
    module->n_uses++;}
}

FRAMERD_EXPORT
/* fd_module_export:
    Arguments: a pointer to a module and a lisp symbol
    Returns: void
    Arranges for the module to export the specified symbol. */
void fd_module_export(fd_lispenv modenv,lisp symbol)
{
  struct FD_MODULE *module=modenv->module;
  if (module != NULL)
    fd_hashset_add(&(module->exports),symbol);
}

/** Dealing with environments **/

/* The most complicated thing here is the use of environment copies
    to make dynamic versions of stack allocated environments.  The
    short version of what happens is that stack allocated environments
    don't have a ->copy field, but one is created on demand, while
    dynamic environments are their own ->copy field.  When a copy exists,
    reference counting is done on it, where parent, copy, and "outside"
    pointers all count as references.
*/


static free_bindings(fd_lispenv env)
{
  struct FD_BINDING *scan=env->rib, *limit=scan+env->n_bindings;
  while (scan < limit) {decref(scan->val); scan++;}
  if (env->mallocd_rib)
    fd_free(env->rib,sizeof(struct FD_BINDING)*env->max_bindings);
}

#if FD_THREADS_ENABLED
static fd_mutex env_malloc_lock;
#endif

/* Environment structure:
     There is a top level environment consisting of symbol values.
     This is used mostly for general, safe, primitives.
     There are "modules" consisting of a hash table of values and a hashset
      of exported symbols; such modules are usually the top of a chain
      of environments, each of which associates some set of variables to
      values in a "rib" pairing symbols and values.
     Each also has the following:
        n_refs (how many environments refer to this one)
        copy   (when non-NULL, a heap allocated version of this
                environment, which is always used when it exists)
        parent (the environment "above" this one)
        sprocs (a set of procedures which are closed in this environment)
     Environments usually start out stack-consed and may then be dynamically
      copied.  We keep the pointers to sprocs so that we can make a copy
      if neccessary.  _fd_finish_stack_env takes an environment and creates
      the dynamic copy if neccessary.
*/

FRAMERD_EXPORT 
/* _fd_finish_stack_env:
     Arguments: an env pointer
     Returns: nothing
  Cleans up a stack allocated environment, taking care of dangling
  sprocs, bound variables, etc. */
void _fd_finish_stack_env(fd_lispenv env)
{
  if (env->mallocd) {
    if (env->mallocd_rib)
      fd_free(env->rib,sizeof(struct FD_BINDING)*env->max_bindings);
    fd_free_env(env->mallocd);}
  else {
    free_bindings(env);}
}
FRAMERD_EXPORT
/* fd_make_env
     Arguments: a parent environment
     Returns: a new environment
  This dynamically allocates a new environment
with a specified parent.
*/
fd_lispenv fd_make_env(fd_lispenv env)
{
  fd_lispenv new_env=fd_malloc(sizeof(struct FD_LISPENV));
  new_env->mallocd=new_env; new_env->n_refs=1; env->module=NULL;
  new_env->rib=fd_malloc(sizeof(struct FD_BINDING)*16);
  new_env->n_bindings=0; new_env->max_bindings=16;
  new_env->mallocd_rib=1;
  if (env) new_env->parent=fd_mallocd_env(env);
  else new_env->parent=NULL;
  return new_env;
}

static fd_lispenv create_mallocd_env(fd_lispenv env)
{
  if (env == NULL) return env;
  else if (env->mallocd) {
    env->mallocd->n_refs++;
    return env->mallocd;}
  else {
    int i=0, limit=env->n_bindings;
    fd_lispenv copy=fd_malloc(sizeof(struct FD_LISPENV));
    copy->mallocd=env->mallocd=copy; copy->module=env->module;
    if (env->parent) copy->parent=create_mallocd_env(env->parent);
    else copy->parent=NULL;
    copy->n_refs=2; copy->mallocd_rib=1;
    copy->n_bindings=env->n_bindings;
    copy->max_bindings=env->max_bindings;
    if (env->mallocd_rib) {
      copy->rib=env->rib; env->rib=NULL; env->mallocd_rib=0;}
    else {
      copy->rib=fd_malloc(sizeof(struct FD_BINDING)*env->max_bindings);
      while (i < limit) {
	copy->rib[i].var=env->rib[i].var;
	copy->rib[i].val=env->rib[i].val; i++;}
      env->rib=NULL;}
    return copy;}
}

FRAMERD_EXPORT
/* fd_mallocd_env
     Arguments: an environment
     Returns: a dynamically allocated environment
  Returns the existing dynamic copy of an environment or
makes one if one doesn't exist.
*/
fd_lispenv fd_mallocd_env(fd_lispenv env)
{
  if (env == NULL) return env;
  else {
    fd_lock_mutex(&env_malloc_lock);
    if (env->mallocd) {
      env->mallocd->n_refs++;
      fd_unlock_mutex(&env_malloc_lock);
      return env->mallocd;}
    else {
      fd_lispenv mallocd=create_mallocd_env(env);
      fd_unlock_mutex(&env_malloc_lock);
      return mallocd;}}
}

static void free_module(struct FD_MODULE *m)
{
  int i=0;
  fd_free_hashtable(&(m->bindings));
  fd_free_hashset(&(m->exports));
  while (i < 0) {
    fd_lispenv uses=m->uses[i];
    fd_free_env(uses); i++;}
  fd_free(m,sizeof(struct FD_MODULE));
}

FRAMERD_EXPORT
/* fd_free_env
     Arguments: an environment
     Returns: void
Frees a dynamically allocated environment
*/
void fd_free_env(fd_lispenv env)
{
  if (env == NULL) return;
  else if (env->mallocd == NULL) return;
  else if (env->mallocd != env) return;
  else {
    fd_lock_mutex(&env_malloc_lock);
    if ((--env->n_refs) == 0) {
      fd_unlock_mutex(&env_malloc_lock);
      free_bindings(env);
      if (env->parent) fd_free_env(env->parent);
      if (env->module) free_module(env->module);
      fd_free(env,sizeof(struct FD_LISPENV));}
    else fd_unlock_mutex(&env_malloc_lock);}
}


/** Looking up symbol values **/

FRAMERD_EXPORT
/* fd_symeval:
     Arguments: a lisp symbol and an environment (possibly NULL)
     Returns: a lisp object
  This looks up a symbol, handling both lexical references, module references,
and global references.
*/
fd_lisp fd_symeval(fd_lisp sym,lispenv env)
{
  if (!(SYMBOLP(sym)))
    fd_ctype_error("fd_symeval",_("var not a symbol"),sym);
  while (env) {
    if (env->mallocd) env=env->mallocd;
    if (env->rib) {
      int i=0, limit=env->n_bindings;    
      while (i < limit)
	if (LISP_EQ((env->rib[i].var),sym)) {
	  lisp v=(env->rib[i].val);
	  /* fake_sproc is a trick from Freidman & Weise to
	     handle recursive environment structures with a
	     reference counting GC.  Note that there is special
	     code for EQ? and EQUAL? to ignore differences
	     between faked and real sprocs */
	  if ((PRIM_TYPEP(v,sproc_type)) || (PRIM_TYPEP(v,gproc_type)))
	    return fake_sproc(v);
	  else return incref(v);}
	else i++;}
    if (env->module) {
      lisp fv=FAST_SYMBOL_VALUE(sym);
      if (FD_VOIDP(fv))
	return module_ref(env->module,sym);
      else return fv;}
    env=env->parent;}
  {
    lisp v=FAST_SYMBOL_VALUE(sym);
    if (FD_VOIDP(v)) v=module_ref(top_level_module,sym);
    return v;}
}

FRAMERD_EXPORT
/* fd_lexical_symeval:
     Arguments: a lisp symbol and an environment
     Returns: a lisp object
  This looks up a symbol only in the local (lexical) environment.
*/
fd_lisp fd_lexical_symeval(fd_lisp sym,lispenv env)
{
  if (!(SYMBOLP(sym)))
    fd_ctype_error("fd_local_symeval",_("var not a symbol"),sym);
  if ((env) && (env->mallocd)) env=env->mallocd;
  if (env) {
    if (env->mallocd) env=env->mallocd;
    if (env->rib) {
      int i=0, limit=env->n_bindings;    
      while (i < limit)
	if (LISP_EQ((env->rib[i].var),sym)) {
	  lisp v=(env->rib[i].val);
	  /* fake_sproc is a trick from Freidman & Weise to
	     handle recursive environment structures with a
	     reference counting GC.  Note that there is special
	     code for EQ? and EQUAL? to ignore differences
	     between faked and real sprocs */
	  if ((PRIM_TYPEP(v,sproc_type)) || (PRIM_TYPEP(v,gproc_type)))
	    return fake_sproc(v);
	  else return incref(v);}
	else i++;}}
  return FD_VOID;
}

FRAMERD_EXPORT
/* fd_required_symeval:
     Arguments: a lisp symbol and an environment (possibly NULL)
     Returns: a lisp object
  Does an environment lookup (using fd_symeval) but signals
an error if it fails 
*/
fd_lisp fd_required_symeval(fd_lisp sym,lispenv env)
{
  lisp v=fd_symeval(sym,env);
  if (FD_VOIDP(v))
    fd_raise_lisp_exception(fd_UnboundVariable,"SYMEVAL",sym);
  else return v;
}


/** Changing symbol values **/

FRAMERD_EXPORT
/* fd_bind_value:
     Arguments: a symbol, a value, and an environment
     Returns: 1 on success, 0 on failure
  Sets the binding for symbol in the first frame of environment
 or creates a new one if one does not exist.  Returns 1 if a new
 binding had to be created.
*/     
void fd_bind_value(lisp sym,lisp val,lispenv env)
{
  val=unquote_choice(val);
  if (!(SYMBOLP(sym)))
    fd_ctype_error("fd_bind_value",_("var not a symbol"),sym);
  if (env == NULL) {
    fd_set_symbol_value(sym,val);
    return;}
  if (env->module) {
    lisp fv=FAST_SYMBOL_VALUE(sym);
    if (FD_VOIDP(fv)) {
      fd_hashtable_set(&(env->module->bindings),sym,val);
      if (!(fd_choice_containsp(sym,(env->module->changes)))) {
	ADD_TO_CHOICE((env->module->changes),sym);}
      return;}
    else if (env->rib) {}
    else {
      env->mallocd_rib=1;
      env->rib=fd_malloc(8*sizeof(struct FD_BINDING));
      env->n_bindings=0; env->max_bindings=8;}
    fd_decref(fv);}
  if (env->mallocd) env=env->mallocd;
  if (env->rib) {
    int i=0, limit=env->n_bindings;
    struct FD_BINDING *rib=env->rib;
    fd_incref(val);
    while (i < limit)
      if (LISP_EQ((rib[i].var),sym)) {
	lisp current=rib[i].val;
	if ((FD_XPROCP(current)) &&
	    (env == (PTR_DATA(current,sproc)->env)))
	  env->n_refs++;
	decref(current); rib[i].val=val; return;}
      else i++;
    if (env->n_bindings == env->max_bindings) {
      if (env->mallocd_rib) {
	/* If the rib was mallocd in the first place, we just call
	   realloc to grow it */
	env->rib=fd_realloc
	  (env->rib,sizeof(struct FD_BINDING)*env->max_bindings*2,
	   sizeof(struct FD_BINDING)*env->max_bindings);
	env->max_bindings=env->max_bindings*2;}
      else {
	/* If the rib wasn't mallocd and we need to grow it,
	   we have to convert it into a mallocd rib */
	struct FD_BINDING *nrib=fd_malloc
	  (((env->max_bindings)+4)*sizeof(struct FD_BINDING));
	int i=0; while (i < env->max_bindings) {
	  nrib[i].var=env->rib[i].var; nrib[i].val=env->rib[i].val; i++;}
	env->rib=nrib; env->mallocd_rib=1;
	env->max_bindings=((env->max_bindings)+4);}}
    i=env->n_bindings++;
    /* The recursive binding kludge */
    if ((FD_XPROCP(val)) &&
	(env == (PTR_DATA(val,sproc)->env)))
      env->n_refs--;
    env->rib[i].var=sym;
    env->rib[i].val=val;}
}

FRAMERD_EXPORT
/* fd_set_value:
     Arguments: a symbol, a value, and an environment
     Returns: void
  Sets the binding for symbol in an environment chain, setting
it in the innermost module or the global environment if the variable
is not directly bound.
*/     
void fd_set_value(lisp sym,lisp val,lispenv env)
{
  lisp v;
  fd_lispenv scan=env;
  val=unquote_choice(val);
  while (scan) {
    if (scan->mallocd) scan=scan->mallocd;
    if (scan->rib) {
      int i=0, l=scan->n_bindings;
      while (i < l)
	if (LISP_EQ(scan->rib[i].var,sym)) {
	  incref(val); decref(scan->rib[i].val); scan->rib[i].val=val;
	  return;}
	else i++;}
    if (scan->module) {
      lisp gv=FAST_SYMBOL_VALUE(sym);
      if (FD_VOIDP(gv)) {
	fd_hashtable_set(&(scan->module->bindings),sym,val);
	if (!(fd_choice_containsp(sym,(scan->module->changes)))) {
	  ADD_TO_CHOICE((scan->module->changes),sym);}}
      else {fd_set_symbol_value(sym,val);}
      decref(gv);
      return;}
    else scan=scan->parent;}
  fd_set_symbol_value(sym,val);
}

FRAMERD_EXPORT
/* fd_safe_set_value:
     Arguments: a symbol, a value, and an environment
     Returns: void
  Sets the value for a variable, but never effecting the
global environment.
*/     
void fd_safe_set_value(lisp sym,lisp val,lispenv env)
{
  fd_lispenv scan=env;
  val=unquote_choice(val);
  while (scan) {
    if (scan->mallocd) scan=scan->mallocd;
    if (scan->rib) {
      int i=0, l=scan->n_bindings;
      while (i < l)
	if (LISP_EQ(scan->rib[i].var,sym)) {
	  incref(val); decref(scan->rib[i].val); scan->rib[i].val=val;
	  return;}
	else i++;}
    if (scan->module) {
      lisp gv=FAST_SYMBOL_VALUE(sym);
      if (FD_VOIDP(gv)) {
	fd_hashtable_set(&(scan->module->bindings),sym,val);}
      else {
	fd_decref(gv);
	fd_raise_exception(CantModifyGlobalEnvironment);}
      return;}
    else scan=scan->parent;}
  fd_raise_exception(CantModifyGlobalEnvironment);
}

/** Interpreted procedures (SPROCs) **/

FRAMERD_EXPORT
/* fd_make_sproc: 
     Arguments: a lisp lambda expression and an environment
     Returns: a procedure object closing the expression in the environment
*/
lisp fd_make_sproc(lisp lambda,lispenv env)
{
  lisp result; fd_sproc new=fd_malloca(struct FD_SPROC);
  new->lambda=lambda; new->env=fd_mallocd_env(env);
  result.type=sproc_type; result.data.sproc=new;
  return result;
}

static lisp copy_sproc(lisp x)
{
  fd_sproc s=PTR_DATA(x,sproc), new=fd_malloca(struct FD_SPROC);
  new->lambda=incref(s->lambda); new->env=fd_mallocd_env(s->env);
  {RETURN_LISP(sproc_type,sproc,new);}
}

static unsigned int hash_sproc(lisp x,fd_hashfn hfn)
{
  fd_sproc s=FD_GET_SPROC(x);
  fd_lisp lambda=s->lambda;
  return hfn(CAR(lambda))+hfn(CAR(CDR(lambda)));
}

static void print_sproc(lisp x,fd_string_stream ss)
{
  fd_sproc s=PTR_DATA(x,sproc); fd_lisp lambda=s->lambda;
  if (FD_PAIRP(CAR(CDR(lambda))))
    fd_printf(ss,"[#SPROC %q%q]",CAR(lambda),CAR(CDR(lambda)));
  else fd_printf(ss,"[#SPROC %q(.%q)]",CAR(lambda),CAR(CDR(lambda)));
}

static void free_sproc(lisp x)
{
  fd_sproc s=PTR_DATA(x,sproc);
  decref(s->lambda); fd_free_env(s->env);
  fd_qfree(s,sizeof(struct FD_SPROC));
}

static unsigned int compare_sprocs(lisp a1,lisp a2) 
{
  fd_sproc s1=FD_GET_SPROC(a1), s2=FD_GET_SPROC(a2);
  if (!(LISP_EQ(s1->lambda,s2->lambda)) ) return 0;
  else if (s1->env != s2->env) return 0;
  else return 1;
}

static lisp fake_sproc(lisp p)
{
  fd_sproc original=PTR_DATA(p,sproc), newp=fd_malloca(struct FD_SPROC);
  fd_lispenv env=original->env;
  lisp result;
  newp->lambda=incref(original->lambda); newp->env=fd_mallocd_env(env);
  result.type=PTR_TYPE(p); PTR_DATA(result,sproc)=newp;
  return result;
}

/** Synchronized Interpreted procedures (SSPROCs) **/

FRAMERD_EXPORT
/* fd_make_ssproc: 
     Arguments: a lisp lambda expression and an environment
     Returns: a procedure object closing the expression in the environment
with a lock for synchronizing application. */
lisp fd_make_ssproc(lisp lambda,lispenv env)
{
  lisp result;
  fd_ssproc new=fd_malloc(sizeof(struct FD_SSPROC));
  new->n_refs=1; new->lambda=lambda; new->env=fd_mallocd_env(env); 
#if (FD_THREADS_ENABLED)
  fd_init_mutex(&(new->lock));
#endif
  result.type=ssproc_type; result.data.ssproc=new;
  return result;
}

static lisp copy_ssproc(lisp x)
{
  fd_lisp result;
  fd_ssproc s=PTR_DATA(x,ssproc);
  fd_ssproc new=fd_malloc(sizeof(struct FD_SSPROC));
  new->n_refs=1; new->lambda=incref(s->lambda); new->env=fd_mallocd_env(s->env);
#if (FD_THREADS_ENABLED)
  fd_init_mutex(&(new->lock));
#endif
  result.type=ssproc_type; result.data.ssproc=new;
  return result;
}

static void print_ssproc(lisp x,fd_string_stream ss)
{
  fd_ssproc s=PTR_DATA(x,ssproc);
  fd_lisp lambda=s->lambda, name=CAR(lambda);
  if (FD_PAIRP(CAR(CDR(lambda))))
    fd_printf(ss,"[#SSPROC %q%q]",CAR(lambda),CAR(CDR(lambda)));
  else fd_printf(ss,"[#SSPROC %q(.%q)]",CAR(lambda),CAR(CDR(lambda)));
}

static void free_ssproc(lisp x)
{
  fd_ssproc s=PTR_DATA(x,ssproc);
  decref(s->lambda); fd_free_env(s->env);
  fd_qfree(s,sizeof(struct FD_SSPROC));
}

/** Goal-like Interpreted procedures (GPROCs) **/

FRAMERD_EXPORT
/* fd_make_gproc: 
     Arguments: a lisp lambda expression and an environment
     Returns: a procedure object closing the expression in the environment
*/
lisp fd_make_gproc(lisp lambda,lispenv env)
{
  lisp result; fd_sproc new=fd_malloca(struct FD_SPROC);
  new->lambda=lambda; new->env=fd_mallocd_env(env);
  result.type=gproc_type; result.data.sproc=new;
  return result;
}

static lisp copy_gproc(lisp x)
{
  fd_sproc s=PTR_DATA(x,sproc), new=fd_malloca(struct FD_SPROC);
  new->lambda=incref(s->lambda); new->env=fd_mallocd_env(s->env);
  {RETURN_LISP(gproc_type,sproc,new);}
}

static void print_gproc(lisp x,fd_string_stream ss)
{
  fd_sproc s=PTR_DATA(x,sproc);
  fd_lisp lambda=s->lambda, name=CAR(lambda);
  if (FD_PAIRP(CAR(CDR(lambda))))
    fd_printf(ss,"[#GPROC %q%q]",CAR(lambda),CAR(CDR(lambda)));
  else fd_printf(ss,"[#GPROC %q(.%q)]",CAR(lambda),CAR(CDR(lambda)));
}

static void free_gproc(lisp x)
{
  fd_sproc s=PTR_DATA(x,sproc);
  decref(s->lambda); fd_free_env(s->env);
  fd_qfree(s,sizeof(struct FD_SPROC));
}

static lisp lisp_clear_goals_cproc()
{
  lisp goals=fd_thread_symeval(goals_symbol);
  fd_hashtable table;
  if (PRIM_TYPEP(goals,hashtable_type)) {
    table=(fd_hashtable)CPTR_DATA(goals);
    fd_free_hashtable(table);
    fd_init_hashtable(table,16);}
  decref(goals);
  return FD_VOID;
}

/** Local and global DEFINE **/

/* This defines a new intepreted function or sets a variable value
    (which are the same thing).  We support MIT-style DEFINEs, e.g.
     (define (fact n) ....) === (define fact (lambda (n) ...))
*/
static lisp define_handler(lisp define_args,lispenv env)
{
  lisp parameters, body;
  lisp var, val;

  body=fd_get_body(define_args,2);
  parameters=fd_get_arg(define_args,1,FD_VOID);

  if (SYMBOLP(parameters)) {
    var=parameters;
    val=fd_eval_in_env(fd_get_arg(define_args,2,FD_VOID),env);}
  else if (PAIRP(parameters)) {
    var=CAR(parameters);
    if (!(SYMBOLP(var)))
      fd_raise_lisp_exception(fd_SyntaxError,"DEFINE",define_args);
    val=fd_make_sproc
      (FD_MAKE_PAIR(var,FD_MAKE_PAIR(incref(CDR(parameters)),incref(body))),
       env);}
  else fd_raise_lisp_exception(fd_SyntaxError,"DEFINE",define_args);
  fd_bind_value(var,val,env);
  fd_decref(val);
  return FD_VOID;
}

static lisp sdefine_handler(lisp define_args,lispenv env)
{
  lisp parameters, body;
  lisp var, val;

  body=fd_get_body(define_args,2);
  parameters=fd_get_arg(define_args,1,FD_VOID);

  if (SYMBOLP(parameters)) {
    var=parameters;
    val=fd_eval_in_env(fd_get_arg(define_args,2,FD_VOID),env);}
  else if (PAIRP(parameters)) {
    var=CAR(parameters);
    if (!(SYMBOLP(var)))
      fd_raise_lisp_exception(fd_SyntaxError,"SDEFINE",define_args);
    val=fd_make_ssproc
      (FD_MAKE_PAIR(var,FD_MAKE_PAIR(incref(CDR(parameters)),incref(body))),
       env);}
  else fd_raise_lisp_exception(fd_SyntaxError,"SDEFINE",define_args);
  fd_bind_value(var,val,env);
  fd_decref(val);
  return FD_VOID;
}

static lisp gdefine_handler(lisp define_args,lispenv env)
{
  lisp parameters, body;
  lisp var, val;

  body=fd_get_body(define_args,2);
  parameters=fd_get_arg(define_args,1,FD_VOID);

  if (SYMBOLP(parameters)) {
    var=parameters;
    val=fd_eval_in_env(fd_get_arg(define_args,2,FD_VOID),env);}
  else if (PAIRP(parameters)) {
    var=CAR(parameters);
    if (!(SYMBOLP(var)))
      fd_raise_lisp_exception(fd_SyntaxError,"GDEFINE",define_args);
    val=fd_make_gproc
      (FD_MAKE_PAIR(var,FD_MAKE_PAIR(incref(CDR(parameters)),incref(body))),
       env);}
  else fd_raise_lisp_exception(fd_SyntaxError,"GDEFINE",define_args);
  fd_bind_value(var,val,env);
  fd_decref(val);
  return FD_VOID;
}

/* This defines a new intepreted function or sets a variable value
    if the variable does not currently have a value.
*/
static lisp define_if_needed_handler(lisp define_args,lispenv env)
{
  lisp parameters, body;
  lisp var, val, cur;

  body=fd_get_body(define_args,2);
  parameters=CAR(CDR(define_args));

  if (SYMBOLP(parameters)) var=parameters;
  else if (PAIRP(parameters)) var=CAR(parameters);
  else fd_raise_lisp_exception(fd_SyntaxError,"DEFINE-IF-NEEDED",define_args);

  if (SYMBOLP(var)) {
    lisp v=FAST_SYMBOL_VALUE(var);
    if (!(FD_VOIDP(v))) {fd_decref(v); return FD_VOID;}
    else { /* Try harder */
      v=fd_symeval(var,env);
      if (!(FD_VOIDP(v))) {fd_decref(v); return FD_VOID;}}}
  else fd_raise_lisp_exception(fd_SyntaxError,"DEFINE-IF-NEEDED",define_args);
  
  if (SYMBOLP(parameters)) val=fd_eval_in_env(CAR(body),env);
  else if (PAIRP(parameters))
    val=fd_make_sproc(FD_MAKE_PAIR(var,FD_MAKE_PAIR(incref(CDR(parameters)),
						incref(body))),
		      env);
  else fd_raise_lisp_exception(fd_SyntaxError,"DEFINE-IF-NEEDED",define_args);
  fd_set_symbol_value(var,val);
  fd_decref(val);
  return FD_VOID;
}

/* Constructs an sproc from a lambda expression in the current environment. */
static lisp lambda_handler(lisp expr,lispenv env)
{
  return fd_make_sproc(incref(expr),env);
}

/* Constructs an sproc from a lambda expression in the current environment. */
static lisp slambda_handler(lisp expr,lispenv env)
{
  return fd_make_ssproc(incref(expr),env);
}

/* Constructs an sproc from a lambda expression in the current environment. */
static lisp glambda_handler(lisp expr,lispenv env)
{
  return fd_make_gproc(incref(expr),env);
}

/** Delays **/

static lisp lisp_make_delay_cproc(lisp thunk)
{
  fd_pair c=fd_malloca(struct FD_PAIR);
  c->car=incref(thunk); c->cdr=FD_VOID;
  {RETURN_LISP(delay_type,pair,c);}
}

static lisp lisp_delay_handler(lisp delay_expr,fd_lispenv env)
{
  lisp expr=fd_get_arg(delay_expr,1,FD_VOID);
  lisp lambda=FD_MAKE_LIST(3,delay_symbol,FD_EMPTY_LIST,incref(expr));
  lisp sproc=fd_make_sproc(lambda,env);
  lisp delay=lisp_make_delay_cproc(sproc);
  decref(sproc);
  return delay;
}

static lisp lisp_force_cproc(lisp delay)
{
  if (PRIM_TYPEP(delay,delay_type)) {
    if (FD_VOIDP(CAR(delay))) return incref(CDR(delay));
    else {
      lisp thunk=fake_sproc(CAR(delay));
      lisp v=fd_apply(thunk,FD_EMPTY_LIST);
      decref(thunk);
      if (FD_VOIDP(CAR(delay))) {
	decref(v);
	return incref(CDR(delay));}
      else {
	decref(CAR(delay)); RPLACA(delay,FD_VOID);
	RPLACD(delay,v);
	return incref(v);}}}
  else fd_type_error(_("Not a delay"),delay);
}

static void print_delay(lisp x,fd_string_stream ss)
{
  if (FD_VOIDP(CAR(x)))
    fd_printf(ss,"[#FORCED %q]",CDR(x));
  else fd_printf(ss,"[#DELAY %q]",CAR(x));
}

static void free_delay(lisp x)
{
  fd_pair c=PTR_DATA(x,pair);
  decref(c->car); decref(c->cdr);
  fd_qfree(c,sizeof(struct FD_PAIR));
}

/** Making modules **/

FRAMERD_EXPORT
/* fd_make_module
    Arguments: none
    Returns: a lisp environment
Returns a module environment.
*/
fd_lispenv fd_make_module()
{
  struct FD_MODULE *m=fd_malloc(sizeof(struct FD_MODULE));
  fd_lispenv env=fd_malloc(sizeof(struct FD_LISPENV));
  fd_init_hashtable(&(m->bindings),256); 
  fd_init_hashset(&(m->exports),16);
  m->n_uses=0; m->wrapper=env; m->changes=FD_EMPTY_CHOICE; 
  /* The load status indicates whether or not the module has been initialized.
     The access bits indicate special contexts which may have access to this module. */
  m->load_status=unloaded; m->access_bits=0;
  env->module=m; env->parent=NULL; env->mallocd=env;
  env->n_bindings=0; env->max_bindings=0; env->rib=NULL;
  env->mallocd_rib=0; env->n_refs=1; 
  return env;
}

/** Initialization **/

static void initialize_evaluator_types()
{
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(sproc_type);
   r->copy_fcn=copy_sproc; r->gc_fcn=free_sproc; r->print_fcn=print_sproc;
   r->hash_fcn=hash_sproc; r->compare_fcn=compare_sprocs;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(ssproc_type);
   r->copy_fcn=copy_ssproc; r->gc_fcn=free_ssproc; r->print_fcn=print_ssproc;
   r->hash_fcn=hash_sproc; r->compare_fcn=compare_sprocs;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(gproc_type);
   r->copy_fcn=copy_gproc; r->gc_fcn=free_gproc; r->print_fcn=print_gproc;
   r->hash_fcn=hash_sproc; r->compare_fcn=compare_sprocs;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(delay_type);
   r->gc_fcn=free_delay; r->print_fcn=print_delay;}

}

static void initialize_top_level_module()
{
  fd_global_env=fd_make_module();
  top_level_module=fd_global_env->module;
}

void fd_initialize_lambda_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&env_malloc_lock);
#endif
  
  initialize_top_level_module();

  initialize_evaluator_types();

  fd_module_table=fd_make_module();

  fd_safe_symbol=fd_make_symbol("SAFE");

  goals_symbol=fd_make_symbol("%GOALS%");
  delay_symbol=fd_make_symbol("LAMBDA");

  fd_add_special_form(NULL,"DEFINE",define_handler);
  fd_add_special_form(NULL,"SDEFINE",sdefine_handler);
  fd_add_special_form(NULL,"GDEFINE",gdefine_handler);
  fd_add_special_form(NULL,"DEFINE-IF-NEEDED",define_if_needed_handler);
  fd_add_special_form(NULL,"SLAMBDA",slambda_handler);
  fd_add_special_form(NULL,"LAMBDA",lambda_handler);
  fd_add_special_form(NULL,"GLAMBDA",glambda_handler);

  fd_add_cproc(NULL,"MAKE-DELAY",1,lisp_make_delay_cproc);
  fd_add_cproc(NULL,"FORCE",1,lisp_force_cproc);
  fd_add_special_form(NULL,"DELAY",lisp_delay_handler);

  fd_add_restricted_cproc("CLEAR-GOALS!",0,lisp_clear_goals_cproc);

  fd_register_source_file("lambda",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: lambda.c,v $
   Revision 1.23  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.22  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.21  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.20  2004/06/25 16:02:18  haase
   Moved more module functions into modules.c and added locks for module registration race conditions

   Revision 1.19  2004/05/15 16:39:26  haase
   Fix copy_ssproc's failure to init refcount

   Revision 1.18  2004/03/31 22:46:33  haase
   Fixed double unlocking problem

   Revision 1.17  2003/12/05 14:58:44  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.16  2003/12/02 15:02:36  haase
   Added mutex protection for malloc'd environment reference counting

   Revision 1.15  2003/12/02 14:28:34  haase
   Added GC of environments which have modules

   Revision 1.14  2003/11/26 16:55:30  haase
   Fixed define-if-needed to check local environment as well as global

   Revision 1.13  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.12  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.4  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11.2.3  2003/02/16 21:41:22  haase
   Minor patches to ralphc patches

   Revision 1.11.2.2  2003/01/26 20:37:17  haase
   Misc. fixes, especially GC

   Revision 1.11.2.1  2002/09/26 02:03:38  haase
   Fixed collision between textlet bindings and global bindings by making textlet bindings always check and bind locally

   Revision 1.11  2002/07/18 19:15:22  haase
   Upped max use count for modules

   Revision 1.10  2002/06/15 20:42:31  haase
   Made IN-MODULE default to the same level of security as the current context

   Revision 1.9  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.8  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.7  2002/04/19 20:15:20  haase
   Made SPROCs only hash on name and args

   Revision 1.6  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
