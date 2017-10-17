/* C Mode */

/* ndeval.c
   Primitives for dealing with non-deterministic values
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

static char vcid[] = "$Id: ndeval.c,v 1.14 2005/01/14 16:48:45 haase Exp $";

/** Primitive forms for choices **/
/** Testing for membership of an element in a set **/
/** Patterns of non-deterministic combination **/
/** Iteration expressions: non-deterministic stuff **/
/** Choice processing/iteration forms **/
/** Initialization **/

#include "fdeval.h"

#define funcn(p) ((fd_lisp (*)(lisp))(p->func))
#define func0(p) ((fd_lisp (*)(void))(p->func))
#define func1(p) ((fd_lisp (*)(fd_lisp))(p->func))
#define func2(p) ((fd_lisp (*)(fd_lisp,fd_lisp))(p->func))
#define func3(p) ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func4(p) ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func5(p) \
  ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))
#define func6(p) \
  ((fd_lisp (*)(fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp,fd_lisp))(p->func))

void free_env(lispenv env);

/** Primitive forms for choices **/

/* The big guy */
static lisp choice_lexpr(lisp args)
{
  lisp answer=FD_EMPTY_CHOICE;
  struct FD_HASHSET s; int size=0;
  {DOLIST(elt,args) size=size+CHOICE_SIZE(elt);}
  fd_init_hashset(&s,size);
  {DOLIST(elt,args) {
    DO_CHOICES(each,elt) 
      if (!(fd_hashset_get(&s,each))) {
	lisp refd=incref(each);
	_fd_hashset_add_nc(&s,refd);
	ADD_TO_CHOICE(answer,refd);}
    END_DO_CHOICES;}}
  fd_free_hashset(&s);
  return answer;
}

static lisp quoted_choice_lexpr(lisp args)
{
  fd_lisp ch=choice_lexpr(args);
  if (FD_CHOICEP(ch)) {
    RETURN_LISP(quoted_choice_type,choice,PTR_DATA(ch,choice));}
  else if (FD_EMPTYP(ch))
    return FD_QUOTED_EMPTY_CHOICE;
  else return ch;
}

static lisp lisp_fail_cproc()
{
  return FD_EMPTY_CHOICE;
}

#define ONLY_VALUE(args,function) \
  ((!(FD_PAIRP(args))) ? (fd_raise_exception(fd_TooFewArgs),FD_VOID) : \
   (FD_PAIRP(FD_CDR(args))) ?                                          \
    (fd_raise_lisp_exception(fd_TooManyArgs,function,args),FD_VOID) :  \
    (FD_CAR(args)))

static lisp lisp_emptyp_lexpr(fd_lisp args)
{
  lisp value=ONLY_VALUE(args,"EMPTY?");
  if (FD_EMPTYP(value)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_existsp_lexpr(fd_lisp args)
{
  lisp value=ONLY_VALUE(args,"EXISTS?");
  if (FD_EMPTYP(value)) return FD_FALSE;
  else return FD_TRUE;
}

static lisp choice_size_lexpr(lisp args)
{
  lisp value=ONLY_VALUE(args,"CHOICE-SIZE"); int size=1;
  if (FD_EMPTYP(value)) return LISPFIX(0);
  else if (CHOICEP(value)) {
    fd_sort_choice(value);
    size=CHOICE_SIZE(value);}
  return LISPFIX(size);
}

static lisp singletonp_lexpr(lisp args)
{
  lisp value=ONLY_VALUE(args,"SINGLETON?");
  if (FD_EMPTYP(value)) return FD_FALSE;
  else if (CHOICEP(value)) return FD_FALSE;
  else return FD_TRUE;
}

static lisp singleton_lexpr(lisp args)
{
  lisp value=ONLY_VALUE(args,"SINGLETON");
  if (CHOICEP(value)) return FD_EMPTY_CHOICE;
  else return fd_incref(value);
}

static lisp proper_choice_lexpr(lisp args)
{
  lisp value=ONLY_VALUE(args,"PROPER-CHOICE");
  if (FD_EMPTYP(value)) return value;
  else if (CHOICEP(value)) {
    value.type=choice_type;
    return fd_return_proper_choice(incref(value));}
  else return incref(value);
}

static lisp sorted_choice_lexpr(lisp args)
{
  lisp value=ONLY_VALUE(args,"SORTED-CHOICE");
  if (FD_EMPTYP(value)) return value;
  else if (CHOICEP(value)) 
    return incref(value); /* Fix this */
  else return incref(value);
}

static lisp lisp_satisfiedp(lisp args)
{
  lisp v=ONLY_VALUE(args,"SATISFIED?");
  if (FD_EMPTYP(v)) return FD_FALSE;
  else if (FD_FALSEP(v)) return FD_FALSE;
  else return FD_TRUE;
}

/** Testing for membership of an element in a set **/

static lisp lisp_contains_lexpr(fd_lisp args)
{
  fd_lisp ch1, ch2;
  fd_get_args("CONTAINS?",args,&ch1,FD_VOID,&ch2,FD_VOID,NULL);
  if (fd_choice_containsp(ch1,ch2)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_overlaps_lexpr(fd_lisp args)
{
  fd_lisp ch1, ch2;
  fd_get_args("OVERLAPS?",args,&ch1,FD_VOID,&ch2,FD_VOID,NULL);
  if (fd_choice_overlapsp(ch1,ch2)) return FD_TRUE;
  else return FD_FALSE;
}

/** Patterns of non-deterministic combination **/

/* Keeps evaluating subexpressions until one succeeds (returns non empty) */
static lisp trying(lisp expr,lispenv env)
{
  lisp body=fd_get_body(expr,1);
  {DOLIST(form,body) {
    lisp v=fd_eval_in_env(form,env);
    if (!(FD_EMPTYP(v))) return v;}
  return FD_EMPTY_CHOICE;}
}

/* Returns common results to subexpression evaluations */
static lisp intersection_handler(lisp expr,lispenv env)
{
  int i, size=0; lisp scan=CDR(expr), *choice_vec, result;
  while (PAIRP(scan)) {size++; scan=CDR(scan);}
  if (size == 0) return (FD_EMPTY_CHOICE);
  choice_vec=fd_malloc(sizeof(lisp)*size);
  i=0; scan=CDR(expr); while (i < size) {
    lisp value=fd_eval_in_env(CAR(scan),env);
    if (FD_EMPTYP(value)) {
      int j=0; while (j < i) {decref(choice_vec[j]); j++;}
      fd_free(choice_vec,sizeof(lisp)*size);
      return FD_EMPTY_CHOICE;}
    else choice_vec[i++]=value;
    scan=CDR(scan);}
  result=fd_intersect_choices(choice_vec,size);
  i=0; while (i < size) {decref(choice_vec[i]); i++;}
  fd_free(choice_vec,sizeof(lisp)*size);
  return result;
}

/* Returns common results to subexpression evaluations */
static lisp identical_lexpr(lisp args)
{
  fd_lisp arg1, arg2;
  fd_get_args("IDENTICAL?",args,&arg1,FD_VOID,&arg2,FD_VOID,NULL);
  if (fd_lisp_equal(arg1,arg2)) return FD_TRUE;
  else return FD_FALSE;
}

/* Returns common results to subexpression evaluations */
static lisp intersection_lexpr(lisp args)
{
  int i, size=0; lisp scan=args, *choice_vec, result;
  while (PAIRP(scan)) {size++; scan=CDR(scan);}
  if (size == 0) return FD_EMPTY_CHOICE;
  choice_vec=fd_malloc(sizeof(lisp)*size);
  i=0; scan=args; while (i < size) {
    lisp value=incref(CAR(scan));
    if (FD_EMPTYP(value)) {
      int j=0; while (j < i) {decref(choice_vec[j]); j++;}
      fd_free(choice_vec,sizeof(lisp)*size);
      return FD_EMPTY_CHOICE;}
    else choice_vec[i++]=value;
    scan=CDR(scan);}
  result=fd_intersect_choices(choice_vec,size);
  i=0; while (i < size) {decref(choice_vec[i]); i++;}
  fd_free(choice_vec,sizeof(lisp)*size);
  return result;
}

/* Returns elements of the first result not in subsequent results */
static lisp difference_handler(lisp expr,lispenv env)
{
  struct FD_HASHSET s; int hashset_needs_init=1;
  lisp v1_expr=fd_get_arg(expr,1,FD_VOID);
  lisp v1=fd_eval_in_env(v1_expr,env);
  lisp diff_exprs=fd_get_body(expr,2);
  lisp answer=FD_EMPTY_CHOICE;
  DOLIST(subexpr,diff_exprs) {
    lisp diff=fd_eval_in_env(subexpr,env);
    if (hashset_needs_init) {
      int size=CHOICE_SIZE(diff)*2;
      if (size < 16) size=256;
      fd_init_hashset(&s,size);}
    {DO_CHOICES(v,diff) fd_hashset_add(&s,v); END_DO_CHOICES;}
    decref(diff);}
  {DO_CHOICES(v,v1)
     if (!(fd_hashset_get(&s,v))) {
       lisp cv=incref(v); ADD_TO_CHOICE(answer,cv);}
   END_DO_CHOICES;}
  fd_free_hashset(&s); decref(v1);
  return answer;
}

static lisp exists_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var=fd_get_arg(spec,0,FD_VOID);
  lisp values=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
  int passes=0;
  if (PRIM_TYPEP(var,symbol_type)) {
    FD_WITH_LEXICAL_ENV(exists_env,env,3) {
      fd_bind_value(var,FD_VOID,exists_env);
      {DO_CHOICES(lvalue,values) {
	lisp forms=body; passes=1;
	fd_set_value(var,lvalue,exists_env);
	while ((passes) && (PAIRP(forms))) {
	  lisp test_expr=CAR(forms); 
	  lisp value=fd_eval_in_env(test_expr,exists_env);
	  if (FD_FALSEP(value)) passes=0;
	  else {decref(value); forms=CDR(forms);}}
	if (passes) break;}
      END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    decref(values);
    if (passes) return FD_TRUE; else return FD_FALSE;}
  else fd_raise_lisp_exception
	 (fd_SyntaxError,"EXISTS var not symbol",expr);
}

static lisp pick_one_lexpr(fd_lisp args)
{
  lisp choices=ONLY_VALUE(args,"PICK-ONE");
  if (FD_EMPTYP(choices))
    return choices;
  else if (CHOICEP(choices)) {
    int r=fd_random()%(CHOICE_SIZE(choices));
    fd_choice ch=PTR_DATA(choices,choice); lisp v;
    if (ch->elt_type) {
      union FD_DATA *elts=ch->elements.data; 
      v.type=ch->elt_type; v.data=elts[r];}
    else {
      lisp *elts=ch->elements.lisp; v=elts[r];}
    return fd_incref(v);}
  else return fd_incref(choices);
}

static lisp pick_n_lexpr(fd_lisp args)
{
  lisp choices, count; int n;
  fd_get_args("PICK-N",args,&choices,FD_EMPTY_CHOICE,&count,FD_VOID,NULL);
  n=fd_fixlisp(count);
  if (FD_EMPTYP(choices))
    return choices;
  else if (n == 0) return FD_EMPTY_CHOICE;
  else if (!(FD_CHOICEP(choices))) return fd_incref(choices);
  else if (FD_CHOICE_SIZE(choices) < n) return fd_incref(choices);
  else {
    fd_lisp result=fd_init_choice(n);
    fd_choice ch=PTR_DATA(choices,choice);
    union FD_DATA *delts; fd_lisp *lelts;
    int pos, ch_size;
    char *taken;
    pos=0; ch_size=ch->size;
    taken=fd_malloc(ch_size);
    delts=ch->elements.data; lelts=ch->elements.lisp;
    fd_lock_mutex(&(ch->lock));
    {int j=0; while (j < ch_size) taken[j++]=0;}
    while (FD_CHOICE_SIZE(result) < n) {
      fd_lisp v;
      pos=(pos+fd_random())%ch_size;
      while (taken[pos]) pos=(pos+1)%ch_size;
      taken[pos]=1; if (ch->elt_type) {
	v.type=ch->elt_type; v.data=delts[pos];}
      else v=lelts[pos];
      FD_ADD_TO_CHOICE(result,incref(v));}
    fd_unlock_mutex(&(ch->lock));
    fd_free(taken,ch_size);
    return result;}
}

/** NDCALL **/

static lisp ndcall_helper(lisp fcn,lisp args)
{
  if (PRIM_TYPEP(fcn,cproc_type)) {
    struct FD_CPROC *c=PTR_DATA(fcn,cproc);
    if ((c->n_args == FD_NORMAL_LEXPR) ||
	(c->n_args == FD_ND_LEXPR))
      return funcn(c)(args);
    else if (c->n_args == FD_SPECIAL_FORM)
      fd_raise_lisp_exception
	("Can't NDCALL a special form",c->name,args);
    else {
      int len=fd_list_length(args);
      if (len > c->n_args)
	fd_raise_lisp_exception(fd_TooManyArgs,c->name,args);
      else if (len < c->n_args)
	fd_raise_lisp_exception(fd_TooFewArgs,c->name,args);
      else switch (c->n_args) {
      case 0: return func0(c)();
      case 1: return func1(c)(fd_get_arg(args,0,FD_VOID));
      case 2: return func2(c)(fd_get_arg(args,0,FD_VOID),
			      fd_get_arg(args,1,FD_VOID));
      case 3: return func3(c)(fd_get_arg(args,0,FD_VOID),
			      fd_get_arg(args,1,FD_VOID),
			      fd_get_arg(args,2,FD_VOID));
      case 4: return func4(c)(fd_get_arg(args,0,FD_VOID),
			      fd_get_arg(args,1,FD_VOID),
			      fd_get_arg(args,2,FD_VOID),
			      fd_get_arg(args,3,FD_VOID));
      case 5: return func5(c)(fd_get_arg(args,0,FD_VOID),
			      fd_get_arg(args,1,FD_VOID),
			      fd_get_arg(args,2,FD_VOID),
			      fd_get_arg(args,3,FD_VOID),
			      fd_get_arg(args,4,FD_VOID));
      case 6: return func6(c)(fd_get_arg(args,0,FD_VOID),
			      fd_get_arg(args,1,FD_VOID),
			      fd_get_arg(args,2,FD_VOID),
			      fd_get_arg(args,3,FD_VOID),
			      fd_get_arg(args,4,FD_VOID),
			      fd_get_arg(args,5,FD_VOID));
      default: fd_raise_exception(fd_TooManyArgs);}}}
  else if (PRIM_TYPEP(fcn,sproc_type)) {
    fd_sproc s=PTR_DATA(fcn,sproc);
    lisp lambda=s->lambda, value=FD_VOID;
    lisp sproc_args=CAR(CDR(lambda)), body=CDR(CDR(lambda));
    FD_WITH_LEXICAL_ENV(sproc_env,s->env,8) {
      lisp scan_args=sproc_args, scan_vals=args, scan_body=body;
      while ((PAIRP(scan_args)) && (PAIRP(scan_vals))) {
	lisp arg=CAR(scan_args), argval=CAR(scan_vals);
	if (PAIRP(arg)) arg=CAR(arg);
	if (!(SYMBOLP(arg)))
	  fd_raise_lisp_exception(fd_BadLambda,"",fcn);
	fd_bind_value(arg,argval,sproc_env);
	scan_args=CDR(scan_args); scan_vals=CDR(scan_vals);}
      if (SYMBOLP(scan_args)) 
	fd_bind_value(scan_args,scan_vals,sproc_env);
      else if (FD_EMPTY_LISTP(scan_vals))
	if (PAIRP(scan_args)) fd_raise_exception(fd_TooFewArgs);
	else {}
      else fd_raise_exception(fd_TooManyArgs);
      while (PAIRP(scan_body)) {
	decref(value); value=fd_eval_in_env(CAR(scan_body),sproc_env);
	scan_body=CDR(scan_body);}}
    FD_END_WITH_LEXICAL_ENV(value);
    return value;}
  else fd_type_error(_("not ndcallable"),fcn);
}

static lisp ndcall_lexpr(lisp args)
{
  lisp fcns=fd_get_arg(args,0,FD_VOID);
  lisp fcn_args=fd_get_body(args,1);
  lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(fcn,fcns) {
    lisp result=ndcall_helper(fcn,fcn_args);
    ADD_TO_CHOICE(answer,result);}
  END_DO_CHOICES;
  return answer;
}

/** Initialization **/

void fd_initialize_ndeval_c()
{
  fd_add_lexpr(NULL,"CHOICE",FD_ND_LEXPR,choice_lexpr);
  fd_add_lexpr(NULL,"QUOTED-CHOICE",FD_ND_LEXPR,quoted_choice_lexpr);
  fd_add_lexpr(NULL,"NDCALL",FD_ND_LEXPR,ndcall_lexpr);

  fd_add_alias(NULL,"AMB","CHOICE");
  fd_add_alias(NULL,"QC","QUOTED-CHOICE");
  fd_add_special_form(NULL,"TRY",trying);
  fd_add_special_form(NULL,"INTERSECTION",intersection_handler);
  fd_add_special_form(NULL,"DIFFERENCE",difference_handler);
  fd_add_lexpr(NULL,"INTERSECTOR",FD_ND_LEXPR,intersection_lexpr);
  fd_add_lexpr(NULL,"IDENTICAL?",FD_ND_LEXPR,identical_lexpr);
  fd_add_alias(NULL,"EITHER","CHOICE");
  fd_add_alias(NULL,"UNION","CHOICE");
  fd_add_cproc(NULL,"FAIL",0,lisp_fail_cproc);
  fd_add_lexpr(NULL,"SATISFIED?",FD_ND_LEXPR,lisp_satisfiedp);
  fd_add_lexpr(NULL,"CONTAINS?",FD_ND_LEXPR,lisp_contains_lexpr);
  fd_add_lexpr(NULL,"OVERLAPS?",FD_ND_LEXPR,lisp_overlaps_lexpr);
  fd_add_lexpr(NULL,"EMPTY?",FD_ND_LEXPR,lisp_emptyp_lexpr);
  fd_add_lexpr(NULL,"EXISTS?",FD_ND_LEXPR,lisp_existsp_lexpr);
  fd_add_alias(NULL,"FAIL?","EMPTY?");
  fd_add_lexpr(NULL,"PICK-ONE",FD_ND_LEXPR,pick_one_lexpr);
  fd_add_lexpr(NULL,"PICK-N",FD_ND_LEXPR,pick_n_lexpr);
  fd_add_lexpr(NULL,"CHOICE-SIZE",FD_ND_LEXPR,choice_size_lexpr);
  fd_add_lexpr(NULL,"SINGLETON?",FD_ND_LEXPR,singletonp_lexpr);
  fd_add_lexpr(NULL,"SINGLETON",FD_ND_LEXPR,singleton_lexpr);
  fd_add_lexpr(NULL,"SORTED-CHOICE",FD_ND_LEXPR,sorted_choice_lexpr);
  fd_add_lexpr(NULL,"PROPER-CHOICE",FD_ND_LEXPR,proper_choice_lexpr);
  /* Deprecated */
  fd_add_alias(NULL,"SET-SIZE","CHOICE-SIZE");

  fd_add_special_form(NULL,"EXISTS",exists_handler);

  fd_register_source_file("ndeval",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: ndeval.c,v $
   Revision 1.14  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.13  2004/08/01 01:36:35  haase
   Added SINGLETON primitive

   Revision 1.12  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.11  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.10.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.10.2.1  2003/01/26 20:56:05  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.10  2002/06/29 01:21:47  haase
   Added PICK-N

   Revision 1.9  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.8  2002/04/05 03:28:38  haase
   Defined SINGLETON?

   Revision 1.7  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
