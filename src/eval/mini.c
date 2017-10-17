/* C Mode */

/* mini.c
   Minimal set of primitives for use in FramerD demons
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

static char vcid[] = "$Id: mini.c,v 1.25 2005/08/04 23:37:20 haase Exp $";

#include "fdeval.h"

static lisp background_symbol, foreground_symbol, pool_symbol;
static lisp obj_name_symbol, quote_symbol;

fd_exception
  fd_UnknownFrameName=_("Unknown frame name"),
  fd_AmbiguousFrameName=_("Ambiguous frame name");

static lisp reverse_list(lisp seq)
{
  lisp result=FD_EMPTY_LIST;
  DOLIST(elt,seq)
    result=FD_MAKE_PAIR(incref(elt),result);
  return result;
}

static fd_index convert_to_index(fd_lisp frame,fd_u8char *cxt)
{
  fd_index ix=fd_interpret_index(frame);
  if (ix == NULL)
    fd_raise_lisp_exception(_("not an index"),cxt,frame);
  else return ix;
}

/** Comparisons **/

static lisp lisp_eq(lisp a1,lisp a2)
{
  if (LISP_EQ(a1,a2)) return FD_TRUE;
  else if ((FD_XPROCP(a1)) && (FD_XPROCP(a2))) {
    /* Kludge for circular GC fix */
    fd_sproc s1=FD_GET_SPROC(a1), s2=FD_GET_SPROC(a2);
    if (!(LISP_EQ(s1->lambda,s2->lambda)) ) return FD_FALSE;
    else if (s1->env != s2->env) return FD_FALSE;
    else return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_equal(lisp a1,lisp a2)
{
  if (LISP_EQ(a1,a2)) return FD_TRUE;
  else if (ATOMICP(a1)) return FD_FALSE;
  else if (fd_lisp_equal(a1,a2)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp is_literal_handler(lisp expr,lispenv env)
{
  lisp arg_expr=fd_get_arg(expr,1,FD_VOID);
  lisp arg=fd_eval_in_env(arg_expr,env);
  lisp options=fd_get_body(expr,2);
  DOLIST(elt,options)
    if (LISP_EQUAL(arg,elt)) {decref(arg); return FD_TRUE;}
  decref(arg); return FD_FALSE;
}

/** Cheap arithmetic **/

static lisp lisp_plus(lisp a1,lisp a2)
{
  if (FIXNUMP(a1))
    if (FIXNUMP(a2))
      return LISPFIX(FIXLISP(a1)+FIXLISP(a2));
    else if (FLONUMP(a2))
      return LISPFLOAT(FIXLISP(a1)+FLOATLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else if (FLONUMP(a1))
    if (FLONUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)+FLOATLISP(a2));
    else if (FIXNUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)+FIXLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else fd_type_error(_("not a simple number"),a1);
}

static lisp lisp_times(lisp a1,lisp a2)
{
  if (FIXNUMP(a1))
    if (FIXNUMP(a2))
      return LISPFIX(FIXLISP(a1)*FIXLISP(a2));
    else if (FLONUMP(a2))
      return LISPFLOAT(FIXLISP(a1)*FLOATLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else if (FLONUMP(a1))
    if (FLONUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)*FLOATLISP(a2));
    else if (FIXNUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)*FIXLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else fd_type_error(_("not a simple number"),a1);
}

static lisp lisp_minus(lisp a1,lisp a2)
{
  if (FIXNUMP(a1))
    if (FIXNUMP(a2))
      return LISPFIX(FIXLISP(a1)-FIXLISP(a2));
    else if (FLONUMP(a2))
      return LISPFLOAT(FIXLISP(a1)-FLOATLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else if (FLONUMP(a1))
    if (FLONUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)-FLOATLISP(a2));
    else if (FIXNUMP(a2))
      return LISPFLOAT(FLOATLISP(a1)-FIXLISP(a2));
    else fd_type_error(_("not a simple number"),a2);
  else fd_type_error(_("not a simple number"),a1);
}

static lisp lisp_gt(lisp a1,lisp a2)
{
  if (FIXNUMP(a1))
    if (FIXNUMP(a2))
      if (FIXLISP(a1)>FIXLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else if (FLONUMP(a2))
      if (FIXLISP(a1)>FLOATLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else fd_type_error(_("not a simple number"),a2);
  else if (FLONUMP(a1))
    if (FLONUMP(a2))
      if (FLOATLISP(a1)>FLOATLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else if (FIXNUMP(a2))
      if (FLOATLISP(a1)>FIXLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else fd_type_error(_("not a simple number"),a2);
  else fd_type_error(_("not a simple number"),a1);
}

static lisp lisp_lte(lisp a1,lisp a2)
{
  lisp v=lisp_gt(a1,a2);
  if (FD_TRUEP(v)) return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_lt(lisp a1,lisp a2)
{
  if (FIXNUMP(a1))
    if (FIXNUMP(a2))
      if (FIXLISP(a1)<FIXLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else if (FLONUMP(a2))
      if (FIXLISP(a1)<FLOATLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else fd_type_error(_("not a simple number"),a2);
  else if (FLONUMP(a1))
    if (FLONUMP(a2))
      if (FLOATLISP(a1)<FLOATLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else if (FIXNUMP(a2))
      if (FLOATLISP(a1)<FIXLISP(a2)) return FD_TRUE;
      else return FD_FALSE;
    else fd_type_error(_("not a simple number"),a2);
  else fd_type_error(_("not a simple number"),a1);
}

static lisp lisp_gte(lisp a1,lisp a2)
{
  lisp v=lisp_lt(a1,a2);
  if (FD_TRUEP(v)) return FD_FALSE;
  else return FD_TRUE;
}


/** Conditional forms **/

/* Just returns its first argument, unevaluated */
static lisp quote_handler(lisp expr,lispenv env)
{
  return incref(get_arg(expr,1,FD_VOID));
}

/* Just returns VOID */
static lisp comment_handler(lisp expr,lispenv env)
{
  return FD_VOID;
}

static lisp if_handler(lisp expr,lispenv env)
{
  lisp test_expression=get_arg(expr,1,FD_VOID), body=get_body(expr,3);
  lisp test=fd_eval_in_env(test_expression,env);
  if (FD_EMPTYP(test))
    if (!(FD_EMPTY_LISTP(body)))
      return fd_partial_eval(CAR(body),env);
    else return FD_VOID;
  else if (!(LISP_TEST_FALSEP(test))) {
    lisp consequent=get_arg(expr,2,FD_VOID);
    decref(test); return fd_partial_eval(consequent,env);}
  else if (!(FD_EMPTY_LISTP(body))) return fd_partial_eval(CAR(body),env);
  else return FD_VOID;
}

static lisp lisp_not(lisp value)
{
  if (FD_FALSEP(value)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_or(lisp expr,lispenv env)
{
  lisp body=get_body(expr,1);
  DOLIST(form,body) {
    lisp v=fd_eval_in_env(form,env);
    if (LISP_TEST_FALSEP(v)) {}
    else if (FD_EMPTYP(v)) {}
    else return v;}
  return FD_FALSE;
}

static lisp lisp_and(lisp expr,lispenv env)
{
  lisp value=FD_TRUE, body=get_body(expr,1);
  DOLIST(form,body) {
    decref(value); value=fd_eval_in_env(form,env);
    if (LISP_TEST_FALSEP(value)) return value;}
  return value;
}

/** Framerd Functions **/

static lisp lisp_add_lexpr(lisp assertion_args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("ADD!",assertion_args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  if ((FD_EMPTYP(values)) || (FD_EMPTYP(slotids)) || (FD_EMPTYP(frames)))
    return FD_VOID;
  else {
    DO_CHOICES(slotid,slotids) {
      if ((PRIM_TYPEP(slotid,index_type)) ||
	  (PRIM_TYPEP(slotid,hashtable_type)) ||
	  (FD_STRINGP(slotid))) {
	DO_CHOICES(frame,frames) {
	  if (PRIM_TYPEP(slotid,hashtable_type))
	    fd_hashtable_add((fd_hashtable)FD_CPTR_DATA(slotid),frame,values);
	  else if ((PRIM_TYPEP(slotid,index_type)) || (FD_STRINGP(slotid)))
	    fd_index_add(convert_to_index(slotid,"ADD!"),frame,values);
	  else {}}
	END_DO_CHOICES;}
      else {
	DO_CHOICES(frame,frames) {
	  if ((PRIM_TYPEP(frame,index_type)) ||
	      (STRINGP(frame)) || (SYMBOLP(frame))) {
	    fd_index ix=convert_to_index(frame,"ADD!");
	    fd_index_add(ix,slotid,values);}
	  else {
	    fd_prim_add(frame,slotid,values);}}
	END_DO_CHOICES;}}
    END_DO_CHOICES;
    return FD_VOID;}
}

static lisp lisp_drop_lexpr(lisp assertion_args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("DROP!",assertion_args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  if ((FD_EMPTYP(values)) || (FD_EMPTYP(slotids)) || (FD_EMPTYP(frames)))
    return FD_VOID;
  else {
    DO_CHOICES(slotid,slotids) {
      if ((PRIM_TYPEP(slotid,index_type)) ||
	  (PRIM_TYPEP(slotid,hashtable_type)) ||
	  (FD_STRINGP(slotid))) {
	DO_CHOICES(frame,frames) {
	  if (PRIM_TYPEP(slotid,hashtable_type))
	    fd_hashtable_drop((fd_hashtable)FD_CPTR_DATA(slotid),frame,values);
	  else if ((PRIM_TYPEP(slotid,index_type)) || (FD_STRINGP(slotid)))
	    fd_index_drop(convert_to_index(slotid,"DROP!"),frame,values);
	  else {}}
	END_DO_CHOICES;}
      else {
	DO_CHOICES(frame,frames) {
	  if ((PRIM_TYPEP(frame,index_type)) ||
	      (STRINGP(frame)) || (SYMBOLP(frame))) {
	    fd_index ix=convert_to_index(frame,"DROP!");
	    fd_index_drop(ix,slotid,values);}
	  else {
	    fd_prim_drop(frame,slotid,values);}}
	END_DO_CHOICES;}}
    END_DO_CHOICES;
    return FD_VOID;}
}

/* Inference */

static lisp lisp_get(lisp frame,lisp slot)
{
  if (PRIM_TYPEP(slot,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(slot);
    return fd_hashtable_get(h,frame,FD_EMPTY_CHOICE);}
  else if ((PRIM_TYPEP(frame,slotmap_type)) || PRIM_TYPEP(frame,object_type))
    return fd_frame_get(frame,slot);
  else if ((PRIM_TYPEP(frame,index_type)) ||
      (STRINGP(frame)) || (SYMBOLP(frame))) {
    fd_index ix=convert_to_index(frame,"GET");
    return fd_index_get(ix,slot,FD_EMPTY_CHOICE);}
  else if (PRIM_TYPEP(frame,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(frame);
    return fd_hashtable_get(h,slot,FD_EMPTY_CHOICE);}
  else if (FD_APPLICABLEP(frame)) {
    fd_lisp arglist=FD_MAKE_LIST1(fd_incref(slot)), result; 
    result=fd_apply(frame,arglist);
    fd_decref(arglist);
    return result;}
  else if ((FD_PAIRP(frame)) && (FD_SLOTP(FD_CAR(frame))))
    if (FD_LISP_EQ(slot,FD_CAR(frame))) return fd_cdr(frame);
    else return FD_EMPTY_CHOICE;
  else if ((FD_VECTORP(frame)) &&
	   (FD_VECTOR_LENGTH(frame) > 1) &&
	   (FD_SLOTP(FD_VECTOR_REF(frame,0))))
    if (FD_LISP_EQ(slot,FD_VECTOR_REF(frame,0)))
      return fd_incref(FD_VECTOR_REF(frame,1));
    else return FD_EMPTY_CHOICE;
  else return fd_frame_get(frame,slot);
}

static lisp lisp_assert_lexpr(lisp assertion_args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("ASSERT!",assertion_args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  if ((FD_EMPTYP(values)) || (FD_EMPTYP(slotids)) || (FD_EMPTYP(frames)))
    return FD_VOID;
  else {
    DO_CHOICES(frame,frames)
      if ((PRIM_TYPEP(frame,index_type)) ||
	  (STRINGP(frame)) || (SYMBOLP(frame))) {
	fd_index ix=convert_to_index(frame,"ASSERT");
	FD_DO_CHOICES(slot,slotids) 
	  fd_index_add(ix,slot,values);
	FD_END_DO_CHOICES;}
      else {
	FD_DO_CHOICES(slot,slotids) 
	  fd_frame_add(frame,slot,values);
	FD_END_DO_CHOICES;}
    END_DO_CHOICES;
    return FD_VOID;}
}

static lisp lisp_retract_lexpr(lisp assertion_args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("RETRACT!",assertion_args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  if ((FD_EMPTYP(values)) || (FD_EMPTYP(slotids)) || (FD_EMPTYP(frames)))
    return FD_VOID;
  else {
    DO_CHOICES(frame,frames) 
      if ((PRIM_TYPEP(frame,index_type)) ||
	  (STRINGP(frame)) || (SYMBOLP(frame))) {
	fd_index ix=convert_to_index(frame,"RETRACT");
	DO_CHOICES(slot,slotids) {
	  fd_lisp current=fd_index_get(ix,slot,FD_EMPTY_CHOICE);
	  fd_lisp remaining=FD_EMPTY_CHOICE;
	  DO_CHOICES(each,current) {
	    if (!(fd_choice_containsp(each,values))) {
	      FD_ADD_TO_CHOICE(remaining,incref(each));}}
	  END_DO_CHOICES;
	  fd_decref(current);
	  fd_index_set(ix,slot,remaining);
	  fd_decref(remaining);}
	END_DO_CHOICES;}
      else {
	DO_CHOICES(slot,slotids) 
	  fd_frame_remove(frame,slot,values);
	END_DO_CHOICES;}
    END_DO_CHOICES;
    return FD_VOID;}
}

static lisp lisp_test_lexpr(lisp assertion_args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("TEST",assertion_args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  if ((FD_EMPTYP(values)) || (FD_EMPTYP(slotids)) || (FD_EMPTYP(frames)))
    return FD_FALSE;
  else {
    int proven=0;
    DO_CHOICES(frame,frames) {
      DO_CHOICES(slot,slotids)
	if (PRIM_TYPEP(slot,hashtable_type)) {
	  fd_hashtable h=(fd_hashtable)CPTR_DATA(slot);
	  if (fd_hashtable_test(h,frame,values)) {
	    proven=1; break;}}
	else if ((PRIM_TYPEP(frame,index_type)) ||
		 (STRINGP(frame)) || (SYMBOLP(frame))) {
	  fd_index ix=convert_to_index(frame,"TEST");
	  lisp current=fd_index_get(ix,slot,FD_EMPTY_CHOICE);
	  DO_CHOICES(v,values) {
	    if (fd_choice_containsp(v,current)) {proven=1; break;}}
	  END_DO_CHOICES;
	  fd_decref(current);
	  if (proven) break;}
	else if (fd_frame_test(frame,slot,values)) {proven=1; break;}
	else {}
      END_DO_CHOICES; /* slots */
      if (proven) break;}
    END_DO_CHOICES; /* frames */
    if (proven) return FD_TRUE;
    else return FD_FALSE;}
}

/** GET* implementation */

STATIC_INLINE fd_lisp getter(fd_lisp f,fd_lisp s)
{
if (FD_PRIM_TYPEP(s,hashtable_type))
  return fd_hashtable_get((fd_hashtable)FD_CPTR_DATA(s),f,FD_EMPTY_CHOICE);
 else  if (FD_OIDP(f)) return fd_frame_get(f,s);
 else if (FD_PRIM_TYPEP(f,hashtable_type))
   return fd_hashtable_get((fd_hashtable)FD_CPTR_DATA(f),s,FD_EMPTY_CHOICE);
 else return FD_EMPTY_CHOICE;
}

static void get_star_helper(fd_hashset h,fd_lisp frames,fd_lisp slotids)
{
  DO_CHOICES(frame,frames)
    if (fd_hashset_add(h,frame)) {
      {DO_CHOICES(slotid,slotids) {
	fd_lisp values=getter(frame,slotid);
	get_star_helper(h,values,slotids);
	fd_decref(values);}
      END_DO_CHOICES;}}
  END_DO_CHOICES;
}

static fd_lisp lisp_get_star_lexpr(fd_lisp args)
{
  fd_lisp values, frames, slotids;
  fd_get_args("GET*",args,&frames,FD_VOID,&slotids,FD_VOID,NULL);
  if (FD_EMPTYP(slotids)) return FD_EMPTY_CHOICE;
  else {
    struct FD_HASHSET hs; fd_init_hashset(&hs,512);
    get_star_helper(&hs,frames,slotids);
    values=fd_hashset_elts(&hs); fd_free_hashset(&hs);
    return values;}
}

/** GET> implementation */

static void get_max_helper
  (fd_hashset h,fd_lisp frames,fd_lisp slotids,fd_lisp *results)
{
  DO_CHOICES(frame,frames)
    if (fd_hashset_add(h,frame)) {
      int any_values=0;
      {DO_CHOICES(slotid,slotids) {
	fd_lisp values=getter(frame,slotid);
	if (!(FD_EMPTYP(values))) any_values=1;
	get_max_helper(h,values,slotids,results);
	fd_decref(values);}
      END_DO_CHOICES;}
      if (any_values) {
	FD_ADD_TO_CHOICE(*results,fd_incref(frame));}}
  END_DO_CHOICES;
}

static fd_lisp lisp_get_max_lexpr(fd_lisp args)
{
  fd_lisp values=FD_EMPTY_CHOICE, frames, slotids;
  struct FD_HASHSET hs;
  fd_get_args("GET-MAX",args,&frames,FD_VOID,&slotids,FD_VOID,NULL);
  fd_init_hashset(&hs,512);
  get_max_helper(&hs,frames,slotids,&values);
  fd_free_hashset(&hs);
  return values;
}

/* Other functions */

static lisp lisp_pick_lexpr(lisp pick_args)
{
  lisp frames=fd_get_arg(pick_args,0,FD_VOID);
  lisp current=incref(frames), next=FD_EMPTY_CHOICE;
  lisp scan=CDR(pick_args);
  if (FD_EMPTYP(frames)) return frames;
  else fd_prefetch_oids(frames);
  while (PAIRP(scan)) {
    lisp slots=fd_get_arg(scan,0,FD_VOID);
    lisp values=fd_get_arg(scan,1,FD_VOID);
    DO_CHOICES(f,current) 
      if (fd_frame_test(f,slots,values)) {ADD_TO_CHOICE(next,fd_incref(f));}
    END_DO_CHOICES;
    scan=CDR(CDR(scan));
    decref(current); current=next; next=FD_EMPTY_CHOICE;}
  return current;
}

static lisp lisp_just_oids(lisp args)
{
  lisp values; fd_get_args("JUST-OIDS",args,&values,FD_VOID,NULL);
  if (FD_OIDP(values)) return incref(values);
  else if (FD_EMPTYP(values)) return values;
  else if (FD_CHOICEP(values)) {
    lisp just_oids=FD_EMPTY_CHOICE;
    FD_DO_CHOICES(v,values) {
      if (FD_OIDP(v)) {FD_ADD_TO_CHOICE(just_oids,incref(v));}}
    END_FD_DO_CHOICES;
    return just_oids;}
  else return FD_EMPTY_CHOICE;
}

static lisp lisp_ptest_cproc(lisp frame,lisp slotid,lisp value)
{
  if (fd_prim_test(frame,slotid,value)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_pget_cproc(lisp obj,lisp slotid)
{
  return fd_prim_get(obj,slotid);
}

static lisp lisp_padd_cproc(lisp obj,lisp slotid,lisp value)
{
  fd_prim_add(obj,slotid,value);
  return FD_VOID;
}

static lisp lisp_pset_lexpr(lisp args)
{
  fd_lisp frames, slotids, values;
  fd_get_args("%SET!",args,&frames,FD_VOID,&slotids,FD_VOID,&values,FD_VOID,NULL);
  {DO_CHOICES(frame,frames) {
    DO_CHOICES(slotid,slotids) {
      fd_prim_set(frame,slotid,values);}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  return FD_VOID;
}

static lisp lisp_premove_cproc(lisp obj,lisp slotid,lisp value)
{
  fd_prim_drop(obj,slotid,value);
  return FD_VOID;
}

/** Strict searching **/

/** FIND-FRAMES **/

static lisp get_background()
{
  return fd_symbol_value(background_symbol);
}
FRAMERD_EXPORT fd_lisp fd_get_background() { return get_background(); }

static lisp get_foreground()
{
  return fd_symbol_value(foreground_symbol);
}
FRAMERD_EXPORT fd_lisp fd_get_foreground() { return get_foreground(); }

static lisp get_pool()
{
  return fd_symbol_value(pool_symbol);
}
FRAMERD_EXPORT fd_lisp fd_get_conspool() { return get_pool(); }

static lisp do_search(fd_lisp foreground,fd_lisp background,fd_lisp specs)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  DO_CHOICES(fg,foreground)
    if (PAIRP(fg)) {
      fd_lisp inner_result=fd_strict_search(CAR(fg),specs);
      FD_ADD_TO_CHOICE(results,inner_result);}
    else {
      fd_lisp inner_result=fd_strict_search(fg,specs);
      FD_ADD_TO_CHOICE(results,inner_result);}
  END_DO_CHOICES;
  ADD_TO_CHOICE(results,fd_strict_search(background,specs));
  return results;
}

static lisp background_search_handler(lisp expr,fd_lispenv env)
{
  lisp fg_indices=get_foreground();
  lisp bg_indices=get_background();
  lisp specs=fd_get_body(expr,1), search_spec=FD_EMPTY_LIST;
  lisp response=FD_EMPTY_CHOICE;
  if (((FD_VOIDP(fg_indices)) || (FD_EMPTYP(fg_indices))) &&
      ((FD_VOIDP(bg_indices)) || (FD_EMPTYP(bg_indices)))) {
    fd_warn(_("No foreground or background indices"));
    return FD_EMPTY_CHOICE;}
  while (PAIRP(specs)) {
    lisp slot=fd_get_arg(specs,0,FD_VOID);
    lisp value_expr=fd_get_arg(specs,1,FD_VOID);
    lisp slot_value;
    if (!((OIDP(slot)) || (SYMBOLP(slot))))
      fd_type_error(_("not a slotid"),slot);
    slot_value=FD_MAKE_PAIR(incref(slot),fd_eval_in_env(value_expr,env));
    search_spec=FD_MAKE_PAIR(slot_value,search_spec);
    specs=CDR(CDR(specs));}
  response=do_search(fg_indices,bg_indices,search_spec);
  decref(fg_indices); decref(bg_indices); decref(search_spec);
  return response;
}

static lisp unquote(fd_lisp x)
{
  if ((FD_PAIRP(x)) &&
      (FD_LISP_EQ(CAR(x),quote_symbol)) &&
      (FD_PAIRP(FD_CDR(x))))
    return FD_CAR(FD_CDR(x));
  else return x;
    
}

static lisp background_search_lexpr(lisp specs)
{
  lisp fg_indices=get_foreground();
  lisp bg_indices=get_background();
  lisp search_spec=FD_EMPTY_LIST;
  lisp response=FD_EMPTY_CHOICE;
  if (((FD_VOIDP(fg_indices)) || (FD_EMPTYP(fg_indices))) &&
      ((FD_VOIDP(bg_indices)) || (FD_EMPTYP(bg_indices)))) {
    fd_warn(_("No foreground or background indices"));
    return FD_EMPTY_CHOICE;}
  while (PAIRP(specs)) {
    lisp slots=unquote(fd_get_arg(specs,0,FD_VOID));
    lisp values=fd_get_arg(specs,1,FD_VOID);
    lisp slot_value;
    {FD_DO_CHOICES(slot,slots)
       if (!((OIDP(slot)) || (SYMBOLP(slot))))
	 fd_type_error(_("not a slotid"),slot);
    FD_END_DO_CHOICES;}
    slot_value=FD_MAKE_PAIR(incref(slots),incref(values));
    search_spec=FD_MAKE_PAIR(slot_value,search_spec);
    specs=CDR(CDR(specs));}
  response=do_search(fg_indices,bg_indices,search_spec);
  decref(fg_indices); decref(bg_indices); decref(search_spec);
  return response;
}

FASTOP fd_pool interpret_pool(fd_lisp pool_spec)
{
  if (FD_PRIM_TYPEP(pool_spec,pool_type))
    return (fd_pool)(FD_CPTR_DATA(pool_spec));
  else return fd_interpret_pool(pool_spec);
}

FASTOP fd_index interpret_index(fd_lisp index_spec)
{
  if (FD_PRIM_TYPEP(index_spec,index_type))
    return (fd_index)(FD_CPTR_DATA(index_spec));
  else return fd_interpret_index(index_spec);
}

static lisp make_features(fd_lisp slotid,fd_lisp values)
{
  fd_lisp features=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(value,values) {
    fd_lisp feature=FD_MAKE_PAIR(slotid,fd_incref(value));
    FD_ADD_TO_CHOICE(features,feature);}
  END_FD_DO_CHOICES;
  return features;
}

static int test_pool(fd_pool p,fd_lisp pools)
{
  int result=0;
  if (FD_CHOICEP(pools)) {
    FD_DO_CHOICES(pool,pools) {
      fd_pool op=interpret_pool(pool);
      if (p == op) {result=1; break;}}
    END_FD_DO_CHOICES;
    return result;}
  else {
    fd_pool op=interpret_pool(pools);
    if (p == op) return 1; else return 0;}
}

static int foreground_index_add
  (fd_lisp foreground,fd_lisp frame,fd_lisp slotid,fd_lisp values)
{
  int done=0;
  fd_lisp features=make_features(slotid,values);
  /* First, we check for all indices which have both
     pools and slotids defined */
  {
    FD_DO_CHOICES(fg,foreground)
      if (!((FD_VECTORP(fg)) && ((FD_VECTOR_LENGTH(fg)) == 3))) {}
      else if ((FD_EMPTYP(FD_VECTOR_REF(fg,1))) ||
	       (FD_EMPTYP(FD_VECTOR_REF(fg,2))))
	{}
      else {
	fd_lisp slotids=FD_VECTOR_REF(fg,1);
	fd_lisp pools=FD_VECTOR_REF(fg,2);
	fd_pool p=fd_get_pool(frame);
	if ((fd_choice_containsp(slotid,slotids)) && (test_pool(p,pools))) {
	  fd_index ix=fd_interpret_index(VECTOR_REF(fg,0));
	  fd_index_add(ix,features,frame);
	  done=3;}}
    END_FD_DO_CHOICES;}
  if (done) return done;
  /* Next, we look for indices which have either pool or slotid constraints defined */
  {
    FD_DO_CHOICES(fg,foreground) {
      if (!((FD_VECTORP(fg)) && ((FD_VECTOR_LENGTH(fg)) == 3))) {}
      else if (!((FD_EMPTYP(FD_VECTOR_REF(fg,1))) ||
		 (FD_EMPTYP(FD_VECTOR_REF(fg,2))))) {} /* handled above */
      else if ((FD_EMPTYP(FD_VECTOR_REF(fg,1))) &&
	       (FD_EMPTYP(FD_VECTOR_REF(fg,2)))) {} /* skip unconstrained */
      else if (!FD_EMPTYP(FD_VECTOR_REF(fg,2))) {
	/* No slotid constraints, just pools */
	fd_pool p=fd_get_pool(frame);
	fd_lisp pools=FD_VECTOR_REF(fg,2);
	if (test_pool(p,pools)) {
	  fd_index ix=fd_interpret_index(VECTOR_REF(fg,0));
	  fd_index_add(ix,features,frame);
	  done=1;}}
      else if (!FD_EMPTYP(FD_VECTOR_REF(fg,1))) {
	/* No pool constraints, just slotids */
	fd_lisp slotids=FD_VECTOR_REF(fg,1);
	if (fd_choice_containsp(slotid,slotids)) {
	  fd_index ix=fd_interpret_index(VECTOR_REF(fg,0));
	  fd_index_add(ix,features,frame);
	  done=2;}}}
    END_FD_DO_CHOICES;}
  if (done) return done;
  /* Finally, if we still haven't found any indices, we look for general indices. */
  {
    FD_DO_CHOICES(fg,foreground)
      if (!((FD_VECTORP(fg)) && ((FD_VECTOR_LENGTH(fg)) == 3))) {}
      else {
	fd_index ix=fd_interpret_index(fg);
	fd_index_add(ix,features,frame);
	done=1;}
    END_FD_DO_CHOICES;}
  return done;
}

static lisp foreground_index_lexpr(fd_lisp args)
{
  fd_lisp foreground=get_foreground();
  int len=fd_list_length(args);
  if (len == 3) {
    FD_DO_CHOICES(frame,CAR(args)) {
      FD_DO_CHOICES(slotid,CAR(CDR(args))) {
	foreground_index_add(foreground,frame,slotid,CAR(CDR(CDR(args))));}
      END_FD_DO_CHOICES;}
    END_FD_DO_CHOICES;}
  else if (len == 2) {
    FD_DO_CHOICES(frame,CAR(args)) {
      FD_DO_CHOICES(slotid,CAR(CDR(args))) {
	fd_lisp values=fd_frame_get(frame,slotid);
	foreground_index_add(foreground,frame,slotid,values);
	fd_decref(values);}
      END_FD_DO_CHOICES;}
    END_FD_DO_CHOICES;}
  else if (len == 1) {
    FD_DO_CHOICES(frame,CAR(args)) {
      FD_DO_SLOTS(slotid,dvalues,frame) {
	fd_lisp values=
	  ((OIDP(slotid)) ?
	   (fd_frame_get(frame,slotid)) : (fd_incref(dvalues)));
	foreground_index_add(foreground,frame,slotid,values);
	fd_decref(values);}}
    END_FD_DO_CHOICES;}
  else {}
  fd_decref(foreground);
  if (len > 3) fd_raise_exception(fd_TooManyArgs);
  else if (len == 0) fd_raise_exception(fd_TooFewArgs);
  return FD_VOID;
}

static lisp lookup_frame_name(lisp name)
{
  lisp key=FD_MAKE_PAIR(obj_name_symbol,incref(name));
  lisp foreground=get_foreground();
  lisp frames=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(fg,foreground) {
    fd_index ix=NULL; fd_lisp result;
    if (FD_PAIRP(fg)) ix=fd_interpret_index(FD_CAR(fg));
    else ix=fd_interpret_index(fg);
    result=fd_index_get(ix,key,FD_EMPTY_CHOICE);
    if (FD_EMPTYP(result)) {}
    else if (FD_OIDP(result))
      if (FD_EMPTYP(frames)) frames=result;
      else {
	fd_decref(foreground); fd_decref(key); fd_decref(result);
	fd_raise_lisp_exception(fd_AmbiguousFrameName,"",name);}
    else {
      fd_decref(foreground); fd_decref(key); fd_decref(result);
      fd_raise_lisp_exception(fd_AmbiguousFrameName,"",name);}}
  END_FD_DO_CHOICES; fd_decref(foreground);
  if (OIDP(frames)) {
    fd_decref(key); return frames;}
  else {
    lisp background=get_background();
    if (FD_EMPTYP(background)) return FD_EMPTY_CHOICE;
    else {
      FD_DO_CHOICES(bg,background) {
	fd_index ix=fd_interpret_index(bg);
	fd_lisp result=fd_index_get(ix,key,FD_EMPTY_CHOICE);
	if (FD_EMPTYP(result)) {}
	else if (FD_OIDP(result))
	  if (FD_EMPTYP(frames)) frames=result;
	  else if (FD_LISP_EQ(frames,result)) {}
	  else {
	    fd_decref(background); fd_decref(key); fd_decref(result);
	    fd_raise_lisp_exception(fd_AmbiguousFrameName,"",name);}
	else {
	  fd_decref(background); fd_decref(key); fd_decref(result);
	  fd_raise_lisp_exception(fd_AmbiguousFrameName,"",name);}}
      END_FD_DO_CHOICES;
      fd_decref(background);}
    fd_decref(key);
    return frames;}
}

FRAMERD_EXPORT
fd_lisp fd_lookup_frame(fd_u8char *string)
{
  fd_lisp probe=fd_make_symbol(string);
  fd_lisp results=lookup_frame_name(probe);
  if (!(FD_EMPTYP(results))) return results;
  else {
    fd_lisp probe=fd_copy_string(string);
    results=lookup_frame_name(probe); fd_decref(probe);
    return results;}
}

static lisp lookup_frame_handler(lisp expr,fd_lispenv env)
{
  lisp name=fd_get_arg(expr,1,FD_VOID);
  if (FD_EMPTY_LISTP(CDR(CDR(expr)))) {
    lisp frame=lookup_frame_name(name);
    if (FD_EMPTYP(frame))
      fd_raise_lisp_exception(fd_UnknownFrameName,"",name);
    else if (OIDP(frame)) return frame;
    else fd_raise_lisp_exception(fd_AmbiguousFrameName,"",name);}
  else fd_raise_lisp_exception("Syntax error: extra params","$$",expr);
}  

static fd_lisp parse_named_oid(fd_u8char *string_arg)
{
  fd_u8char *string;
  if (*string_arg == '@') string=string_arg+1; else string=string_arg;
  if (isxdigit(*string)) {
    int hi, lo;
    if (sscanf(string,"%x/%x",&hi,&lo) != 2)
      fd_raise_detailed_exception(fd_ParseError,string);
    else {
      FD_OID id; FD_SET_OID_HIGH(id,hi); FD_SET_OID_LOW(id,lo);
      return fd_make_oid(id);}}
  else if (*string == '/') {
    fd_u8char *slash_pos=strchr(string+1,'/'); int prefix_length;
    fd_pool p; char pool_name[32];
    if (slash_pos==NULL)
      fd_raise_detailed_exception(fd_ParseError,string);
    else prefix_length=slash_pos-(string+1);
    if (slash_pos-(string+1) < 32) {
      strncpy(pool_name,string+1,slash_pos-(string+1));
      pool_name[slash_pos-(string+1)]=0;}
    else strcpy(pool_name,"");
    if ((p=fd_find_pool_named(pool_name)) != NULL) {
      FD_OID base=p->base;
      int offset=strtoul(slash_pos+1,NULL,16);
      FD_SET_OID_LOW(base,FD_OID_LOW(base)+offset);
      return fd_make_oid(base);}
    else fd_raise_detailed_exception(fd_UnknownPool,string);}
  else if (*string == '?') {
    fd_u8char *scan=string+1;
    fd_lisp name=fd_parse_lisp_from_string(&scan);
    fd_lisp frame=lookup_frame_name(name);
    fd_decref(name);
    if (FD_EMPTYP(frame))
      fd_raise_lisp_exception(fd_UnknownFrameName,"",name);
    else return frame;}
  else fd_raise_detailed_exception(_("Invalid OID format"),string_arg);
}

static fd_lisp lookup_named_oid(fd_lisp name)
{
  fd_lisp frame=lookup_frame_name(name);
  fd_decref(name);
  if (FD_EMPTYP(frame))
    fd_raise_lisp_exception(fd_UnknownFrameName,"",name);
  else return frame;
}

/** Frame creation **/

static lisp make_frame_handler(lisp expr,fd_lispenv env)
{
  lisp pool_id=get_pool(), args=fd_get_body(expr,1);
  fd_pool p=fd_interpret_pool(pool_id);
  lisp frame=fd_frame_create(p);
  while ((PAIRP(args)) && (PAIRP(CDR(args)))) {
    lisp value=fd_eval_in_env(CAR(CDR(args)),env), slot_spec=CAR(args), slotid;
    slotid=fd_eval_in_env(slot_spec,env);
    fd_frame_add(frame,slotid,value);
    decref(value); decref(slotid);
    args=CDR(CDR(args));}
  return frame;
}

static lisp use_frame_handler(lisp expr,fd_lispenv env)
{
  lisp name=fd_get_arg(expr,1,FD_VOID);
  lisp index=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp frame;
  if (!(SYMBOLP(name)))
    fd_type_error(_("arg not a symbol"),name);
  if (FD_FALSEP(index)) frame=lookup_frame_name(name);
  else {
    fd_index ix=convert_to_index(index,"USE-FRAME");
    lisp key=FD_MAKE_PAIR(obj_name_symbol,name);
    frame=fd_index_get(ix,key,FD_VOID);
    decref(index); decref(key);}
  if (OIDP(frame)) {
    fd_bind_value(name,frame,env);
    return frame;}
  else fd_raise_detailed_exception
	 (fd_UnknownFrameName,SYMBOL_NAME(name));
}

/** **/

static fd_lisp raise_exception_lexpr(fd_lisp args)
{
  int len=fd_list_length(args);
  lisp arg1=fd_get_arg(args,0,FD_VOID);
  lisp arg2=fd_get_arg(args,1,FD_FALSE);
  lisp arg3=fd_get_arg(args,2,FD_FALSE);
  fd_u8char *exname, *exdetails;
  if (FD_SYMBOLP(arg1)) exname=FD_SYMBOL_NAME(arg1);
  else if (FD_STRINGP(arg1)) {
    fd_warn("First arg to RAISE-EXCEPTION is not a symbol");
    exname=fd_strdup(STRING_DATA(arg1));}
  else {
    fd_warn("First arg to RAISE-EXCEPTION is not a symbol");
    exname=fd_object_to_string(arg1);}
  if (FD_FALSEP(arg2)) {}
  else if (FD_STRINGP(arg2)) exdetails=fd_strdup(STRING_DATA(arg2));
  else if (FD_SYMBOLP(arg2)) exdetails=FD_SYMBOL_NAME(arg2);
  else {
    fd_warn("Second arg to RAISE-EXCEPTION is not a string");
    exdetails=fd_object_to_string(arg2);}
  if (len > 3)
    fd_warn("Too many arguments to RAISE-EXCEPTION: %q",args);
  if (len == 1) fd_raise_exception(exname);
  else if (len == 2) fd_raise_detailed_exception(exname,exdetails);
  else if (len >= 3)
    fd_raise_lisp_exception(exname,exdetails,incref(arg3));
  return FD_VOID; /* Never reached */
}

/** Initialization **/

FRAMERD_EXPORT
void fd_initialize_mini_c()
{
  fd_add_cproc(NULL,"EQ?",2,lisp_eq);
  fd_add_cproc(NULL,"EQUAL?",2,lisp_equal);
  fd_add_cproc(NULL,"EQUAL",2,lisp_equal);
  fd_add_special_form(NULL,"IS-LITERAL?",is_literal_handler);

  fd_add_cproc(NULL,"+",2,lisp_plus);
  fd_add_cproc(NULL,"-",2,lisp_minus);
  fd_add_cproc(NULL,"*",2,lisp_times);
  fd_add_cproc(NULL,">",2,lisp_gt);
  fd_add_cproc(NULL,">=",2,lisp_gte);
  fd_add_cproc(NULL,"<",2,lisp_lt);
  fd_add_cproc(NULL,"<=",2,lisp_lte);
  
  fd_add_special_form(NULL,"COMMENT",comment_handler);

  fd_add_special_form(NULL,"IF",if_handler);
  fd_add_special_form(NULL,"AND",lisp_and);
  fd_add_special_form(NULL,"OR",lisp_or);
  fd_add_cproc(NULL,"NOT",1,lisp_not);
  fd_add_special_form(NULL,"QUOTE",quote_handler);

  background_symbol=fd_make_symbol("%BACKGROUND");
  if (FD_VOIDP(SYMBOL_VALUE(background_symbol))) {
    FD_SET_SYMBOL_VALUE(background_symbol,FD_EMPTY_CHOICE);}
  foreground_symbol=fd_make_symbol("%FOREGROUND");
  if (FD_VOIDP(SYMBOL_VALUE(foreground_symbol))) {
    FD_SET_SYMBOL_VALUE(foreground_symbol,FD_EMPTY_CHOICE);}
  pool_symbol=fd_make_symbol("%POOL");
  if (FD_VOIDP(SYMBOL_VALUE(pool_symbol))) {
    FD_SET_SYMBOL_VALUE(pool_symbol,FD_EMPTY_CHOICE);}
  obj_name_symbol=fd_make_symbol("%ID");
  quote_symbol=fd_make_symbol("QUOTE");

  fd_add_cproc(NULL,"GET",2,lisp_get);
  fd_add_lexpr(NULL,"ADD!",FD_ND_LEXPR,lisp_add_lexpr);
  fd_add_lexpr(NULL,"DROP!",FD_NORMAL_LEXPR,lisp_drop_lexpr);
  fd_add_lexpr(NULL,"ASSERT!",FD_ND_LEXPR,lisp_assert_lexpr);
  fd_add_lexpr(NULL,"RETRACT!",FD_ND_LEXPR,lisp_retract_lexpr);
  fd_add_lexpr(NULL,"PICK",FD_ND_LEXPR,lisp_pick_lexpr);
  fd_add_lexpr(NULL,"TEST",FD_ND_LEXPR,lisp_test_lexpr);
  fd_add_lexpr(NULL,"GET*",FD_ND_LEXPR,lisp_get_star_lexpr);
  fd_add_cproc(NULL,"%GET",2,lisp_pget_cproc);
  fd_add_cproc(NULL,"%ADD",3,lisp_padd_cproc);
  fd_add_cproc(NULL,"%ADD!",3,lisp_padd_cproc);
  fd_add_lexpr(NULL,"%SET!",FD_ND_LEXPR,lisp_pset_lexpr);
  fd_add_cproc(NULL,"%TEST",3,lisp_ptest_cproc);
  fd_add_cproc(NULL,"%REMOVE",3,lisp_premove_cproc);

  fd_add_lexpr(NULL,"JUST-OIDS",FD_ND_LEXPR,lisp_just_oids);

  fd_add_cproc(NULL,"LOOKUP-FRAME",1,lookup_frame_name);
  fd_add_special_form(NULL,"$$",lookup_frame_handler);
  fd_add_special_form(NULL,"MAKE-FRAME",make_frame_handler);
  fd_add_special_form(NULL,"USE-FRAME",use_frame_handler);

  fd_add_special_form(NULL,"$?",background_search_handler);
  fd_add_lexpr(NULL,"?INDEX",FD_ND_LEXPR,background_search_lexpr);
  fd_add_lexpr(NULL,"INDEX!",FD_ND_LEXPR,foreground_index_lexpr);
  fd_add_alias(NULL,"??","?INDEX");

  fd_add_lexpr(NULL,"RAISE-EXCEPTION",FD_ND_LEXPR,raise_exception_lexpr);

  fd_configure_oid_io(fd_print_oid,parse_named_oid,lookup_named_oid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: mini.c,v $
   Revision 1.25  2005/08/04 23:37:20  haase
   Changed obj-name to %id

   Revision 1.24  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.23  2004/10/19 22:14:34  haase
   Remove vestiges of background caching pass 1

   Revision 1.22  2004/10/18 15:25:08  haase
   Added compound indices

   Revision 1.21  2004/09/28 23:38:45  haase
   Added non-symbol oid lookup

   Revision 1.20  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.19  2004/06/10 15:35:14  haase
   Fixed GET* of hashtable 'slots'

   Revision 1.18  2004/03/16 13:36:07  haase
   Optimization for pick

   Revision 1.17  2003/11/04 02:57:00  haase
   Fixed leak in named OID lookup

   Revision 1.16  2003/10/28 18:24:37  haase
   Separately retrieved names aren't ambiguous when they agree

   Revision 1.15  2003/10/05 15:35:13  haase
   Added @? parsing

   Revision 1.14  2003/08/31 16:58:03  haase
   Fixed GET to handle simple pairs and vectors by treating CAR/ELT0 as the key

   Revision 1.13  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.12.2.4  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.12.2.3  2003/08/02 21:32:19  haase
   Made GET with a procedural first argument just do an apply

   Revision 1.12.2.2  2003/07/17 07:21:44  haase
   Fixed GC error in handling of slotmap args by PICK

   Revision 1.12.2.1  2003/01/26 20:54:41  haase
   GC fixes

   Revision 1.12  2002/07/02 15:07:21  haase
   Made if/or/and interpret {} as #F

   Revision 1.11  2002/06/24 18:07:54  haase
   Added C interface to foreground, background, and oid name lookup

   Revision 1.10  2002/06/15 16:00:06  haase
   Handle quoted slotids in 1

   Revision 1.9  2002/06/15 14:57:24  haase
   Made 0 handle quoted slotids

   Revision 1.8  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
