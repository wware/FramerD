/* C Mode */

/* libfdeval.c
   Implements init function for FDScript, including primitives
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

static char vcid[] = "$Id: libfdeval.c,v 1.20 2005/03/29 00:28:24 haase Exp $";

#include "fdeval.h"

#define finish_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x);

#ifndef OTHER_INITS
#define OTHER_INITS
#endif

static int fdeval_initialized=0;
static fd_lispenv method_env;

fd_exception fd_UnknownMethod=_("Unknown frame method");

FRAMERD_EXPORT void fd_initialize_framerd(void);
extern void fd_initialize_sandbox_c(void);
extern void fd_initialize_mini_c(void);
extern void fd_initialize_eval_c(void);
extern void fd_initialize_load_c(void);
extern void fd_initialize_ndeval_c(void);
extern void fd_initialize_lambda_c(void);
extern void fd_initialize_threads_c(void);

static lisp frame_symbol, slot_symbol, value_symbol;
static lisp through_slot, derive_slot, inverse_slot;
static lisp multi_slot, multi_primary_slot, key_slot;
static lisp closure_of_slot, index_slot;
static lisp background_symbol;
static lisp inv_slotid;

static fd_lisp method_apply(lisp method,lisp frame,lisp slot,lisp value)
{
  lisp args, retval;
  if (FD_VOIDP(value)) args=FD_MAKE_LIST(2,incref(frame),slot);
  else args=FD_MAKE_LIST(3,incref(frame),slot,incref(value));
  retval=fd_apply(method,args); 
  finish_value(retval); decref(args);
  return retval;
}

static fd_lisp try_to_load_method(lisp symbol)
{
    u8char *name=SYMBOL_NAME(symbol), *colon;
    lisp v=FD_VOID; int len=strlen(name);
    if ((colon=strchr(name,':')) != NULL) {
      u8char *module_name=fd_malloc((colon-name+1));
      lisp symbol; fd_lispenv module;
      strncpy(module_name,name,colon-name); module_name[colon-name]=0;
      module=fd_get_module(module_name);
      symbol=fd_make_symbol(colon+1);
      if (module) v=fd_symeval(symbol,module);
      else {
	fd_free(module_name,colon-name+1);
	fd_raise_detailed_exception("Unknown module",name);}
      fd_free(module_name,colon-name+1);}
    else fd_raise_detailed_exception(fd_UnknownMethod,name);
    fd_set_symbol_value(symbol,v);
    return v;
}

static fd_lisp lisp_lookup_method_cproc(lisp symbol)
{
  lisp v=SYMBOL_VALUE(symbol);
  if (FD_VOIDP(v)) {
    u8char *name=SYMBOL_NAME(symbol), *colon;
    lisp v=FD_VOID; int len=strlen(name);
    if ((colon=strchr(name,':')) != NULL) {
      u8char *module_name=fd_malloc((colon-name+1));
      lisp symbol; fd_lispenv module;
      strncpy(module_name,name,colon-name); module_name[colon-name]=0;
      module=fd_get_module(module_name);
      symbol=fd_make_symbol(colon+1);
      if (module) v=fd_symeval(symbol,module);
      else {
	fd_free(module_name,colon-name+1);
	fd_raise_detailed_exception("Unknown module",name);}
      fd_free(module_name,colon-name+1);}
    else v=FD_EMPTY_CHOICE;
    fd_set_symbol_value(symbol,v);
    /* return incref(v); */
    return v;}
  return incref(v);
}

STATIC_INLINE fd_lisp lookup_method(fd_lisp symbol)
{
  lisp v=SYMBOL_VALUE(symbol);
  if (FD_VOIDP(v))
    return try_to_load_method(symbol);
  else return v;
}

FRAMERD_EXPORT
/* fd_eval_slot_method:
     Arguments: lisp pointers to some methods, a frame, a slot, and a value
     Returns: a lisp pointer
     Applies the methods to the frame slot and value.
 If the method is a symbol, it is looked up and applied to the frame slot and
  value; if it is a pair, it is evaluated in an environment where the
  variables FRAME, SLOTID, and VALUE are bound to the arguments passed in 
  here.
*/
lisp fd_eval_slot_method(lisp methods,lisp frame,lisp slot,lisp value)
{
  if (SYMBOLP(methods)) {
    lisp method=lookup_method(methods), result;
    if (FD_VOIDP(method))
      fd_raise_lisp_exception
	(_("Unknown slot method"),SYMBOL_NAME(methods),slot);
    else result=method_apply(method,frame,slot,value);
    /* fd_decref(method); */
    return result;}
  else {
    lisp result=FD_EMPTY_CHOICE;
    FD_WITH_LEXICAL_ENV(rll_env,method_env,4) {
      fd_bind_value(frame_symbol,frame,rll_env);
      fd_bind_value(slot_symbol,slot,rll_env);
      fd_bind_value(value_symbol,value,rll_env);
      {DO_CHOICES(method,methods) {
	fd_lisp meth_result=(FD_EMPTY_CHOICE);
	if (FD_VOIDP(method))
	  fd_warn(_("Void method for %q"),slot);
	else if (SYMBOLP(method)) {
	  fd_lisp handler=lookup_method(method);
	  if (FD_VOIDP(handler)) 
	    fd_warn(_("Unknown slot method %s for %q(%q)"),
		    SYMBOL_NAME(method),frame);
	  else meth_result=method_apply(handler,frame,slot,value);}
	else meth_result=fd_eval_in_env(method,rll_env);
	ADD_TO_CHOICE(result,meth_result);}
      END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV(result);
    return result;}
}

static lisp cached_get_method(fd_lisp f,fd_lisp sl)
{
  fd_lisp cached=fd_prim_get(f,sl);
  if (FD_EMPTYP(cached)) {
    fd_lisp methods=fd_prim_get(sl,derive_slot);
    fd_lisp computed=fd_eval_slot_method(methods,f,sl,FD_VOID);
    /* Add the values to the slot and then decref the methods */
    fd_prim_add(f,sl,computed);
    fd_decref(methods);
    return computed;}   
  else return cached;
}

static lisp cached_test_method(lisp f,lisp sl,lisp v)
{
  fd_lisp cached=fd_prim_get(f,sl);
  int exists;
  if (FD_EMPTYP(cached)) {
    fd_lisp methods=fd_prim_get(sl,derive_slot);
    fd_lisp computed=fd_eval_slot_method(methods,f,sl,FD_VOID);
    /* Add the values to the slot, check for membership, and then do the decrefs */
    fd_prim_add(f,sl,computed);
    exists=fd_choice_containsp(v,computed);
    fd_decref(methods); fd_decref(computed);
    if (exists)
      return FD_TRUE;
    else return FD_FALSE;}
  exists=fd_choice_containsp(v,cached);
  fd_decref(cached);
  if (exists)
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp cachable_get_method(fd_lisp f,fd_lisp sl)
{
  fd_lisp cached=fd_prim_get(f,sl);
  if (FD_EMPTYP(cached)) {
    fd_lisp methods=fd_prim_get(sl,derive_slot);
    fd_lisp computed=fd_eval_slot_method(methods,f,sl,FD_VOID);
    fd_decref(methods);
    return computed;}   
  else return cached;
}

static lisp cachable_test_method(lisp f,lisp sl,lisp v)
{
  fd_lisp cached=fd_prim_get(f,sl);
  int exists;
  if (FD_EMPTYP(cached)) {
    fd_lisp methods=fd_prim_get(sl,derive_slot);
    fd_lisp computed=fd_eval_slot_method(methods,f,sl,FD_VOID);
    exists=fd_choice_containsp(v,computed);
    fd_decref(methods); fd_decref(computed);
    if (exists)
      return FD_TRUE;
    else return FD_FALSE;}
  exists=fd_choice_containsp(v,cached);
  fd_decref(cached);
  if (exists)
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp inherited_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp through=fd_prim_get(slotid,through_slot);
  fd_lisp result=fd_inherit_values(root,slotid,through);
  fd_decref(through);
  return result;
}

static lisp multi_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp answer=FD_EMPTY_CHOICE;
  fd_lisp slotids=fd_prim_get(slotid,multi_slot);
  DO_CHOICES(slid,slotids) {
    fd_lisp v=fd_frame_get(root,slid);
    FD_ADD_TO_CHOICE(answer,v);}
  END_DO_CHOICES;
  fd_decref(slotids);
  return answer;
}

static fd_lisp multi_test_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  int answer=0;
  fd_lisp slotids=fd_prim_get(slotid,multi_slot);
  DO_CHOICES(slid,slotids) {
    if (fd_frame_test(root,slid,value)) {answer=1; break;}}
  END_DO_CHOICES;
  fd_decref(slotids);
  if (answer) return FD_TRUE; else return FD_FALSE;
}

static fd_lisp multi_add_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  fd_lisp primary_slot=fd_prim_get(slotid,multi_primary_slot);
  if (FD_EMPTYP(primary_slot)) {
    fd_lisp slotids=fd_prim_get(slotid,multi_slot);
    DO_CHOICES(subslotid,slotids) {
      fd_lisp v=fd_frame_get(root,subslotid);
      if (!(FD_EMPTYP(v))) fd_frame_add(root,subslotid,value);
      fd_decref(v);}
    END_DO_CHOICES;
    fd_decref(slotids);}
  else fd_frame_add(root,primary_slot,value);
  fd_decref(primary_slot);
  return FD_VOID;
}

static fd_lisp multi_drop_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  fd_lisp slotids=fd_prim_get(slotid,multi_slot);
  DO_CHOICES(subslotid,slotids) {
    if (fd_frame_test(root,subslotid,value))
      fd_frame_remove(root,subslotid,value);}
  END_DO_CHOICES;
  fd_decref(slotids);
  return FD_VOID;
}

static lisp ix_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp index_spec=fd_prim_get(slotid,index_slot);
  fd_index ix=fd_interpret_index(index_spec);
  fd_lisp answer=fd_index_get(ix,root,FD_EMPTY_CHOICE);
  fd_decref(index_spec);
  return answer;
}

static fd_lisp ix_test_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  fd_lisp index_spec=fd_prim_get(slotid,index_slot);
  fd_index ix=fd_interpret_index(index_spec);
  fd_lisp answer=fd_index_get(ix,root,FD_EMPTY_CHOICE);
  fd_decref(index_spec);
  if (fd_choice_containsp(value,answer)) {
    fd_decref(answer); return FD_TRUE;}
  else {
    fd_decref(answer); return FD_FALSE;}
}

static fd_lisp ix_add_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  fd_lisp index_spec=fd_prim_get(slotid,index_slot);
  fd_index ix=fd_interpret_index(index_spec);
  fd_index_add(ix,root,value);
  fd_decref(index_spec);
  return FD_VOID;
}

static fd_lisp ix_drop_method(fd_lisp root,fd_lisp slotid,lisp value)
{
  fd_lisp index_spec=fd_prim_get(slotid,index_slot);
  fd_index ix=fd_interpret_index(index_spec);
  fd_index_drop(ix,root,value);
  fd_decref(index_spec);
  return FD_VOID;
}

STATIC_INLINE fd_lisp getter(fd_lisp f,fd_lisp s)
{
  if (FD_OIDP(f)) return fd_frame_get(f,s);
  else if (FD_PRIM_TYPEP(f,hashtable_type))
    return fd_hashtable_get((fd_hashtable)FD_CPTR_DATA(f),s,FD_EMPTY_CHOICE);
  else if (FD_PRIM_TYPEP(s,hashtable_type))
    return fd_hashtable_get((fd_hashtable)FD_CPTR_DATA(s),f,FD_EMPTY_CHOICE);
  else return FD_EMPTY_CHOICE;
}

static void kleene_get_helper(fd_hashset h,fd_lisp frames,fd_lisp slotids)
{
  DO_CHOICES(frame,frames)
    if (fd_hashset_get(h,frame)) {}
    else {
      fd_hashset_add(h,frame);
      {DO_CHOICES(slotid,slotids) {
	fd_lisp values=getter(frame,slotid);
	kleene_get_helper(h,values,slotids);
	fd_decref(values);}
      END_DO_CHOICES;}}
  END_DO_CHOICES;
}

static lisp kleene_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp slotids=fd_frame_get(slotid,closure_of_slot), results;
  struct FD_HASHSET hs; fd_init_hashset(&hs,1024);
  kleene_get_helper(&hs,root,slotids);
  results=fd_hashset_elts(&hs);
  fd_decref(slotids); fd_free_hashset(&hs);
  return results;
}

static lisp inherited_test_method(fd_lisp root,fd_lisp slotid,fd_lisp value)
{
  fd_lisp through=fd_prim_get(slotid,through_slot);
  if (fd_inherits_valuep(root,slotid,through,value)) {
    fd_decref(through); return (FD_TRUE);}
  else {fd_decref(through); return (FD_FALSE);}
}

static lisp inverse_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp answer=fd_prim_get(root,slotid);
  fd_lisp background=FD_SYMBOL_VALUE(background_symbol);
  fd_lisp inv_slots=fd_prim_get(slotid,inverse_slot);
  fd_lisp others=fd_find_frames(background,inv_slots,root,FD_VOID);
  FD_ADD_TO_CHOICE(answer,others);
  fd_decref(inv_slots);
  return answer;
}

static lisp inverse_test_method(fd_lisp root,fd_lisp slotid,fd_lisp value)
{
  if (fd_prim_test(root,slotid,value)) return (FD_TRUE);
  else {
    fd_lisp inv_slots=fd_prim_get(slotid,inverse_slot);
    int found=0;
    FD_DO_CHOICES(inv_slot,inv_slots) 
      if (fd_frame_test(value,inv_slot,root)) {found=1; break;}
    END_FD_DO_CHOICES;
    fd_decref(inv_slots);
    if (found) return (FD_TRUE); else return (FD_FALSE);}
}

static lisp inv_get_method(fd_lisp root,fd_lisp slotid)
{
  fd_lisp answer=fd_prim_get(root,slotid);
  fd_lisp background=FD_SYMBOL_VALUE(background_symbol);
  fd_lisp inv_slots=fd_prim_get(slotid,inv_slotid);
  fd_lisp others=fd_find_frames(background,inv_slots,root,FD_VOID);
  FD_ADD_TO_CHOICE(answer,others);
  fd_decref(inv_slots);
  return answer;
}

static lisp inv_test_method(fd_lisp root,fd_lisp slotid,fd_lisp value)
{
  if (fd_prim_test(root,slotid,value)) return (FD_TRUE);
  else {
    fd_lisp inv_slots=fd_prim_get(slotid,inv_slotid);
    int found=0;
    FD_DO_CHOICES(inv_slot,inv_slots) 
      if (fd_frame_test(value,inv_slot,root)) {found=1; break;}
    END_FD_DO_CHOICES;
    fd_decref(inv_slots);
    if (found) return (FD_TRUE); else return (FD_FALSE);}
}

static lisp assoc_get_method(fd_lisp f,fd_lisp slotid)
{
  fd_lisp answers=FD_EMPTY_CHOICE;
  fd_lisp through=fd_frame_get(slotid,through_slot);
  fd_lisp key=fd_frame_get(slotid,key_slot);
  DO_CHOICES(th_slot,through) {
    fd_lisp entries=fd_frame_get(f,th_slot);
    DO_CHOICES(e,entries)
      if ((PAIRP(e)) && (PAIRP(CDR(e))) &&
	  ((FD_CHOICEP(key)) ? (fd_choice_containsp(CAR(e),key)) :
	   (LISP_EQ(CAR(e),key)))) {
	ADD_TO_CHOICE(answers,incref(CAR(CDR(e))));}
    END_DO_CHOICES;
    fd_decref(entries);}
  END_DO_CHOICES;
  fd_decref(through); fd_decref(key); 
  return answers;
}

static fd_lisp assoc_add_method(fd_lisp f,fd_lisp slotid,fd_lisp v)
{
  fd_lisp through=fd_frame_get(slotid,through_slot);
  fd_lisp key=fd_frame_get(slotid,key_slot);
  fd_lisp entry=FD_MAKE_PAIR(key,fd_incref(v));
  fd_prim_add(f,through,entry);
  fd_decref(entry);
  return FD_VOID;
}

static fd_lisp assoc_drop_method(fd_lisp f,fd_lisp slotid,fd_lisp v)
{
  fd_lisp through=fd_frame_get(slotid,through_slot);
  fd_lisp key=fd_frame_get(slotid,key_slot);
  fd_lisp entry=FD_MAKE_PAIR(key,fd_incref(v));
  fd_prim_drop(f,through,entry);
  fd_decref(entry);
  return FD_VOID;
}

static fd_lisp assoc_test_method(fd_lisp f,fd_lisp slotid,fd_lisp v)
{
  int result;
  fd_lisp through=fd_frame_get(slotid,through_slot);
  fd_lisp key=fd_frame_get(slotid,key_slot);
  fd_lisp entry=FD_MAKE_PAIR(key,fd_incref(v));
  result=fd_prim_test(f,through,entry);
  fd_decref(entry);
  if (result) return FD_TRUE; else return FD_FALSE;
}

static lisp car_get_method(fd_lisp f,fd_lisp slotid)
{
  fd_lisp result=(FD_EMPTY_CHOICE);
  fd_lisp values=fd_prim_get(f,slotid);
  FD_DO_CHOICES(value,values)
    if (FD_PAIRP(value)) {ADD_TO_CHOICE(result,fd_incref(FD_CAR(value)));}
    else {
      fd_warn("%q(%q) not a pair: %q",slotid,f,value);}
  END_FD_DO_CHOICES;
  fd_decref(values);
  return result;
}

static lisp paired_get_method(fd_lisp f,fd_lisp slotid)
{
  fd_lisp result=(FD_EMPTY_CHOICE);
  fd_lisp values=fd_prim_get(f,slotid);
  FD_DO_CHOICES(value,values)
    if (FD_PAIRP(value)) {ADD_TO_CHOICE(result,fd_incref(FD_CAR(value)));}
    else {ADD_TO_CHOICE(result,fd_incref(value));}
  END_FD_DO_CHOICES;
  fd_decref(values);
  return result;
}

static lisp paired_test_method(fd_lisp f,fd_lisp slotid,fd_lisp v)
{
  int found=0;
  fd_lisp values=fd_prim_get(f,slotid);
  FD_DO_CHOICES(value,values) {
    if (FD_PAIRP(value)) {
      if (FD_LISP_EQUAL(FD_CAR(value),v)) {found=1; break;}}
    else if (FD_LISP_EQUAL(value,v)) {found=1; break;}}
  END_FD_DO_CHOICES;
  fd_decref(values);
  if (found) return FD_TRUE; else return FD_FALSE;
}

static lisp paired_drop_method(fd_lisp f,fd_lisp slotid,fd_lisp v)
{
  if (FD_PAIRP(v)) {
    fd_prim_drop(f,slotid,v); return FD_VOID;}
  else {
    int found=0;
    fd_lisp values=fd_prim_get(f,slotid);
    FD_DO_CHOICES(value,values) {
      if (FD_PAIRP(value)) {
	if (FD_LISP_EQUAL(FD_CAR(value),v)) {found=1; break;}}
      else if (FD_LISP_EQUAL(value,v)) {found=1; break;}}
    END_FD_DO_CHOICES;
    if (found) {
      fd_lisp new_values=FD_EMPTY_CHOICE;
      FD_DO_CHOICES(value,values) {
	if (FD_PAIRP(value)) 
	  if (FD_LISP_EQUAL(FD_CAR(value),v)) {}
	  else {FD_ADD_TO_CHOICE(new_values,fd_incref(value));}
	else if (FD_LISP_EQUAL(value,v)) {}
	else {FD_ADD_TO_CHOICE(new_values,fd_incref(value));}}
      END_FD_DO_CHOICES;
      fd_prim_set(f,slotid,new_values);
      fd_decref(values); fd_decref(new_values);
      return FD_VOID;}
    else {
      fd_decref(values); return FD_VOID;}}
}

/* Initialization */

static struct FD_HASHSET preloaded;

FRAMERD_EXPORT
/* fd_do_preloads:
     Arguments: none
     Returns: nothing
  Preloads the files or modules in the environment/global variable PRELOADS.
*/
void fd_do_preloads()
{
  fd_lisp preloads=fd_getenv("PRELOAD");
  FD_DO_CHOICES(preload,preloads)
    if (fd_hashset_get(&preloaded,preload)) {}
    else if (FD_PAIRP(preload)) {
      FD_DOLIST(v,preload)
	if (fd_hashset_get(&preloaded,v)) {}
	else if (FD_SYMBOLP(v)) {
	  fd_load_module(v);
	  fd_hashset_add(&preloaded,v);}
	else if (FD_STRINGP(v)) {
	  fd_load_file(FD_STRING_DATA(v),NULL,NULL);
	  fd_hashset_add(&preloaded,v);}
	else fd_warn("Invalid preload %q",v);}
    else if (FD_SYMBOLP(preload)) {
      fd_load_module(preload);
      fd_hashset_add(&preloaded,preload);}
    else if (FD_STRINGP(preload)) {
      fd_load_file(FD_STRING_DATA(preload),NULL,NULL);
      fd_hashset_add(&preloaded,preload);}
    else fd_warn("Invalid preload %q",preload);
  FD_END_DO_CHOICES;
}

FRAMERD_EXPORT void fd_initialize_fdeval()
{
  if (fdeval_initialized) return; else fdeval_initialized=1;
  fd_initialize_framerd();
  fd_initialize_sandbox_c();
  fd_initialize_eval_c();
  fd_initialize_ndeval_c();
  fd_initialize_lambda_c();
  fd_initialize_threads_c();
  fd_initialize_mini_c();
  fd_initialize_load_c();
  fd_initialize_modules_c();

  fd_init_hashset(&preloaded,256);

  method_env=fd_make_module();

  /* For some legacy stuff */
  fd_add_cproc(method_env,"FGET",2,fd_frame_get);

  fd_register_module("METHOD-MODULE",method_env,FD_SAFE_ENV,0);

  background_symbol=fd_make_symbol("%BACKGROUND");
  frame_symbol=fd_make_symbol("FRAME");
  slot_symbol=fd_make_symbol("SLOTID");
  value_symbol=fd_make_symbol("VALUE");
  through_slot=fd_make_symbol("THROUGH");
  key_slot=fd_make_symbol("KEY");
  derive_slot=fd_make_symbol("DERIVATION");
  inverse_slot=fd_make_symbol("INVERSE");
  closure_of_slot=fd_make_symbol("CLOSURE-OF");
  multi_slot=fd_make_symbol("SLOTS");
  multi_primary_slot=fd_make_symbol("PRIMARY-SLOT");
  index_slot=fd_make_symbol("INDEX");
  {
    FD_OID addr;
    FD_SET_OID_HIGH(addr,1); FD_SET_OID_LOW(addr,0x2c27a);
    inv_slotid=fd_make_oid(addr);}

  fd_add_cproc(NULL,"FD:CACHED-GET",2,cached_get_method);
  fd_add_cproc(NULL,"FD:CACHED-TEST",3,cached_test_method);
  fd_add_cproc(NULL,"FD:CACHABLE-GET",2,cachable_get_method);
  fd_add_cproc(NULL,"FD:CACHABLE-TEST",3,cachable_test_method);
  fd_add_cproc(NULL,"FD:INHERITED-GET",2,inherited_get_method);
  fd_add_cproc(NULL,"FD:INHERITED-TEST",3,inherited_test_method);
  fd_add_cproc(NULL,"FD:MULTI-GET",2,multi_get_method);
  fd_add_cproc(NULL,"FD:MULTI-TEST",3,multi_test_method);
  fd_add_cproc(NULL,"FD:MULTI-ADD",3,multi_add_method);
  fd_add_cproc(NULL,"FD:MULTI-DROP",3,multi_drop_method);
  fd_add_cproc(NULL,"FD:INVERSE-GET",2,inverse_get_method);
  fd_add_cproc(NULL,"FD:INVERSE-TEST",3,inverse_test_method);
  fd_add_cproc(NULL,"FD:INV-GET",2,inv_get_method);
  fd_add_cproc(NULL,"FD:INV-TEST",3,inv_test_method);
  fd_add_cproc(NULL,"FD:ASSOC-GET",2,assoc_get_method);
  fd_add_cproc(NULL,"FD:ASSOC-ADD",3,assoc_add_method);
  fd_add_cproc(NULL,"FD:ASSOC-DROP",3,assoc_drop_method);
  fd_add_cproc(NULL,"FD:ASSOC-TEST",3,assoc_test_method);
  fd_add_cproc(NULL,"FD:CAR-GET",2,car_get_method);
  fd_add_cproc(NULL,"FD:KLEENE-GET",2,kleene_get_method);
  fd_add_cproc(NULL,"FD:IX-GET",2,ix_get_method);
  fd_add_cproc(NULL,"FD:IX-TEST",3,ix_test_method);
  fd_add_cproc(NULL,"FD:IX-ADD",3,ix_add_method);
  fd_add_cproc(NULL,"FD:IX-DROP",3,ix_drop_method);
  fd_add_cproc(NULL,"FD:PAIRED-GET",2,paired_get_method);
  fd_add_cproc(NULL,"FD:PAIRED-TEST",3,paired_test_method);
  fd_add_cproc(NULL,"FD:PAIRED-DROP",3,paired_drop_method);

  fd_add_cproc(NULL,"LOOKUP-METHOD",1,lisp_lookup_method_cproc);

  OTHER_INITS;

}


/* File specific stuff */

/* The CVS log for this file
   $Log: libfdeval.c,v $
   Revision 1.20  2005/03/29 00:28:24  haase
   Fixed some double GC bugs

   Revision 1.19  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.18  2004/09/26 00:56:02  haase
   Added some new libfdeval stuff

   Revision 1.17  2004/09/07 14:42:37  haase
   Added more extensive assoc slot methods

   Revision 1.16  2004/07/31 14:13:36  haase
   Initialize preloaded hashset

   Revision 1.15  2004/07/31 14:06:12  haase
   Added fd_do_preload

   Revision 1.14  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.13  2003/12/05 14:58:44  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.12  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.5  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11.2.4  2003/05/20 22:44:21  haase
   Fixes to paired slots

   Revision 1.11.2.3  2003/05/19 20:57:53  haase
   Added paired-get procedures

   Revision 1.11.2.2  2003/02/16 21:41:22  haase
   Minor patches to ralphc patches

   Revision 1.11.2.1  2003/01/26 20:54:41  haase
   GC fixes

   Revision 1.11  2002/04/09 21:13:45  haase
   Fix definition of multi-drop

   Revision 1.10  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
