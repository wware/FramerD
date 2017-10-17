/* C Mode */

/* framerd.c
   FramerD access primitives for FDScript
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

static char vcid[] = "$Id: framerd.c,v 1.50 2005/08/04 23:37:20 haase Exp $";

/** Setup **/
/** Pool and index printing functions **/
/** Operations on OIDs **/
/** Operations on Frames and Slots **/
/** Slot caches, autoindexing, etc **/
/** Operations on whole frames **/
/** FIND-FRAMES **/
/** Fuzzy searching **/
/** Indexing frames **/
/** DOSLOTS **/
/** WITH-SLOTS **/
/** Export and import **/
/** Accessing indices **/
/** Accessing pools **/
/** OID creation, reversion, etc **/
/** Making new frames **/
/** Annotating frames **/
/** OID arithmetic **/
/** Inheritance support functions **/
/** Initialization **/

#include "fdscript.h"

fd_exception fd_DefframeExpectationFailure=
   _("one of the DEFFRAME expectation failed");

fd_lispenv fd_fdinternals_env;

static fd_lisp bground_symbol;

extern lisp eval_exprs(lisp exprs,lispenv env);
extern void eval_exprs_noreturn(lisp exprs,lispenv env);

/** Setup **/

#include <time.h>
#include <limits.h>

static lisp get_load_symbol, quote_symbol, obj_name_symbol;

static fd_index interpret_index_arg(fd_lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix) return ix;
  else fd_type_error(fd_BadIndexSpec,spec);
}

static fd_pool interpret_pool_arg(fd_lisp spec)
{
  fd_pool p=fd_interpret_pool(spec);
  if (p) return p;
  else fd_type_error(fd_BadPoolSpec,spec);
}

/** Pool and index printing functions **/

static void print_pool(lisp lp,fd_string_stream s)
{
  fd_pool p=(fd_pool)CPTR_DATA(lp);
  if (p->type == file_pool) {
    fd_file_pool fp=(fd_file_pool)p;
    fd_printf(s,"[#POOL %s @%x/%x+%d/%d %q]",
	      p->id,FD_OID_HIGH(p->base),FD_OID_LOW(p->base),
	      fp->load,p->capacity,p->label);}
  else fd_printf(s,"[#POOL %s @%x/%x+%d %q]",
		 p->id,FD_OID_HIGH(p->base),FD_OID_LOW(p->base),
		 p->capacity,p->label);
}

static void print_index(lisp li,fd_string_stream s)
{
  fd_index ix=(fd_index)CPTR_DATA(li);
  fd_printf(s,"[#INDEX %s]",ix->id);
}

/** Operations on OIDs **/

static lisp lisp_oid_loadedp_cproc(lisp oid) 
{
  if (OIDP(oid))
    if (fd_oid_loadedp(oid)) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not an oid"),oid);
}

static lisp lisp_oid_modifiedp_cproc(lisp oid) 
{
  if (OIDP(oid))
    if (fd_oid_modifiedp(oid)) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not an oid"),oid);
}

static lisp lisp_oid_value_cproc(lisp oid) 
{
  if (OIDP(oid)) return fd_oid_value(oid);
  else fd_type_error(_("not an oid"),oid);
}

/* Done as a lexpr to allow non-deterministic values to be stored. */
static lisp lisp_set_oid_value_cproc(lisp args)
{
  lisp oids=fd_get_arg(args,0,FD_VOID), v=fd_get_arg(args,1,FD_VOID);
  DO_CHOICES(oid,oids) fd_set_oid_value(oid,v); END_DO_CHOICES;
  return FD_VOID;
}

static lisp lisp_load_oid_cproc(lisp oid)
{
  if (OIDP(oid)) {
    lisp v=fd_oid_value(oid); /* Ignored */
    if (FD_VOIDP(v)) return FD_FALSE;
    else {
      fd_decref(v);
      return oid;}}
  else fd_type_error(_("not an oid"),oid);
}

static lisp lisp_prefetch_oids_lexpr(lisp args)
{
  fd_lisp oids=fd_get_arg(args,0,FD_VOID);
  fd_prefetch_oids(oids);
  return fd_incref(oids);
}

static lisp lisp_prefetch_oids_noret_lexpr(lisp args)
{
  fd_lisp oids=fd_get_arg(args,0,FD_VOID);
  fd_prefetch_oids(oids);
  return FD_VOID;
}

static lisp lisp_swap_out_cproc(lisp oid)
{
  if (OIDP(oid)) {
    fd_swap_out(oid);
    return FD_VOID;}
  else fd_type_error(_("not an oid"),oid);
}

/** Operations on Frames and Slots **/

static lisp lisp_frame_test_cproc(lisp frame,lisp slotid,lisp value)
{
  if (fd_frame_test(frame,slotid,value)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_frame_confirm_cproc(lisp frame,lisp slotid,lisp value)
{
  if (fd_frame_test(frame,slotid,value)) return FD_TRUE;
  else fd_raise_exception("I have a reservation (Confirmation failed)");
}

static lisp lisp_frame_add_cproc(lisp frame,lisp slotid,lisp value)
{
  fd_frame_add(frame,slotid,value);
  return FD_VOID;
}

static lisp lisp_frame_set_lexpr(lisp args)
{
  lisp frame=fd_get_arg(args,0,FD_VOID);
  lisp slotid=fd_get_arg(args,1,FD_VOID);
  lisp value=fd_get_arg(args,2,FD_VOID);
  fd_frame_set(frame,slotid,value);
  return FD_VOID;
}

static lisp lisp_frame_remove_cproc(lisp frame,lisp slotid,lisp value)
{
  fd_frame_remove(frame,slotid,value);
  return FD_VOID;
}

static lisp lisp_prim_get_cproc(lisp obj,lisp slotid)
{
  return fd_prim_get(obj,slotid);
}

static lisp lisp_prim_set_lexpr(lisp args)
{
  lisp obj=fd_get_arg(args,0,FD_VOID),
    slotid=fd_get_arg(args,1,FD_VOID),
    value=fd_get_arg(args,2,FD_VOID);
  fd_prim_set(obj,slotid,value);
  return FD_VOID;
}

static lisp lisp_prim_add_cproc(lisp obj,lisp slotid,lisp value)
{
  fd_prim_add(obj,slotid,value);
  return FD_VOID;
}

static lisp lisp_count_slots_cproc(lisp frame)
{
  lisp smap;
  if (OIDP(frame)) smap=fd_oid_value(frame);
  else smap=fd_incref(frame);
  if (SLOTMAPP(smap)) {
    fd_slotmap sm=PTR_DATA(smap,slotmap); int n_slots=sm->size;
    fd_decref(smap);
    return LISPFIX(n_slots);}
  else {
    fd_decref(smap);
    fd_type_error(_("not a frame"),frame);}
}

/** Slot caches, autoindexing, etc **/

static lisp lisp_enable_slot_cache_lexpr(fd_lisp args)
{
  fd_lisp slotid=fd_get_arg(args,0,FD_VOID);
  fd_lisp table=fd_get_arg(args,1,FD_FALSE);
  fd_enable_slot_cache(slotid,table);
  return FD_VOID;
}

static lisp lisp_disable_slot_cache_cproc(lisp slotid)
{
  fd_disable_slot_cache(slotid);
  return FD_VOID;
}

static lisp lisp_clear_slot_cache_handler(lisp expr,lispenv env)
{
  lisp slotid=fd_eval_in_env(fd_get_arg(expr,1,FD_FALSE),env);
  lisp frame=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  if (FD_FALSEP(slotid)) fd_reset_slot_cache();
  else if (FD_FALSEP(frame)) fd_clear_slot_cache(slotid,FD_VOID);
  else fd_clear_slot_cache(slotid,frame);
  fd_decref(slotid);
  fd_decref(frame);
  return FD_VOID;
}

static lisp lisp_use_autoindex_cproc(lisp index)
{
  if (FD_FALSEP(index)) {
    fd_use_autoindex(NULL);
    return FD_VOID;}
  else {
    fd_index ix=interpret_index_arg(index);
    fd_use_autoindex(ix);
    return FD_VOID;}
}

static fd_lisp lisp_use_adjunct_lexpr(fd_lisp args)
{
  fd_lisp slotid=fd_get_arg(args,0,FD_VOID);
  fd_lisp index_arg=fd_get_arg(args,1,FD_VOID);
  fd_lisp base=fd_get_arg(args,2,FD_VOID);
  fd_lisp cap=fd_get_arg(args,3,FD_FALSE);
  fd_index ix=fd_interpret_index(index_arg);
  if (FD_OIDP(base)) 
    fd_register_adjunct_store(slotid,ix,FD_OID_ADDR(base),FD_FIXLISP(cap));
  else {
    fd_pool p=fd_interpret_pool(base);
    if (p)
      fd_register_adjunct_store(slotid,ix,p->base,p->capacity);}
  return FD_VOID;
}

/** Operations on whole frames **/

static lisp lisp_fdd_cproc(lisp frame)
{
  fd_describe_frame(frame,stdout);
  fflush(stdout);
  return frame;
}

static lisp lisp_control_frame_printing_cproc(lisp arg)
{
  fd_control_frame_printing(fd_lisp2int(arg));
  return FD_VOID;
}

static lisp lisp_show_poolids_cproc(lisp args)
{
  fd_lisp flag=fd_get_arg(args,0,FD_TRUE);
  if (FD_FALSEP(flag))
    fd_show_poolids(0);
  else fd_show_poolids(1);
  return FD_VOID;
}

/* Making pools and indices read-only */

static lisp lisp_index_writablep_cproc(fd_lisp arg)
{
  fd_index ix=interpret_index_arg(arg);
  if (ix->read_only) return FD_FALSE;
 else return FD_TRUE;
}

static lisp lisp_set_index_read_only_cproc(fd_lisp arg)
{
  fd_index ix=interpret_index_arg(arg);
  if (ix->read_only) return FD_FALSE;
  else if ((ix->adds.n_keys) || (ix->drops.n_keys)) 
    fd_raise_detailed_exception("Index already modified",ix->id);
  else {
    ix->read_only=1; return FD_TRUE;}
}

static lisp lisp_pool_writablep_cproc(fd_lisp arg)
{
  fd_pool p=interpret_pool_arg(arg);
  if (p->read_only == FD_POOL_READ_ONLY)
    return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_set_pool_read_only_cproc(fd_lisp arg)
{
  fd_pool p=interpret_pool_arg(arg);
  if (p->read_only == FD_POOL_READ_ONLY) return FD_FALSE;
  else if (p->modifiedp)
    fd_raise_detailed_exception("Pool already modified",p->id);
  else {
    p->read_only=FD_POOL_READ_ONLY; return FD_TRUE;}
}

/** FIND-FRAMES **/

static lisp lisp_find_frames_lexpr(lisp args)
{
  lisp indices=fd_get_arg(args,0,FD_VOID);
  lisp spec=fd_get_body(args,1);
  return fd_strict_search(indices,spec);
}

static lisp lisp_find_similar_lexpr(lisp args)
{
  lisp indices=fd_get_arg(args,0,FD_VOID);
  lisp samples=fd_get_arg(args,1,FD_VOID);
  lisp slotids=fd_get_arg(args,2,FD_FALSE);
  return fd_find_similar(indices,samples,slotids);
}

/** Scored searching **/

static lisp lisp_get_scores_lexpr(lisp args)
{
  lisp candidates=fd_get_arg(args,0,FD_VOID);
  lisp indices=fd_get_arg(args,1,FD_VOID);
  lisp specs=fd_get_body(args,2); 
  fd_hashtable h=fd_score_from_spec(indices,candidates,specs);
  return fd_make_cptr(hashtable_type,h);
}

static lisp lisp_get_scores_from_samples_lexpr(lisp args)
{
  lisp candidates=fd_get_arg(args,0,FD_VOID);
  lisp indices=fd_get_arg(args,1,FD_VOID);
  lisp samples=fd_get_arg(args,2,FD_VOID); 
  lisp slotids=fd_get_arg(args,3,FD_FALSE); 
  fd_hashtable h=fd_score_from_samples(indices,candidates,samples,slotids);
  return fd_make_cptr(hashtable_type,h);
}

static lisp lisp_get_best_lexpr(lisp args)
{
  lisp candidates=fd_get_arg(args,0,FD_VOID);
  lisp indices=fd_get_arg(args,1,FD_VOID);
  lisp specs=fd_get_body(args,2); 
  fd_hashtable h=fd_score_from_spec(indices,candidates,specs);
  lisp best=fd_hashtable_max(h);
  fd_free_hashtable(h); fd_free(h,sizeof(struct FD_HASHTABLE));
  return best;
}

static lisp lisp_get_best_from_samples_lexpr(lisp args)
{
  lisp candidates=fd_get_arg(args,0,FD_VOID);
  lisp indices=fd_get_arg(args,1,FD_VOID);
  lisp samples=fd_get_arg(args,2,FD_VOID); 
  lisp slotids=fd_get_arg(args,3,FD_FALSE); 
  lisp best=FD_EMPTY_CHOICE;
  fd_hashtable h=fd_score_from_samples(indices,candidates,samples,slotids);
  DO_CHOICES(c,samples) fd_hashtable_set(h,c,LISPFIX(0)); END_DO_CHOICES;
  best=fd_hashtable_max(h);
  fd_free_hashtable(h); fd_free(h,sizeof(struct FD_HASHTABLE));
  return best;
}

static lisp lisp_get_similar_lexpr(lisp args)
{
  lisp indices=fd_get_arg(args,0,FD_VOID);
  lisp samples=fd_get_arg(args,1,FD_VOID); 
  lisp require=fd_get_arg(args,2,FD_FALSE); 
  lisp candidates=fd_find_similar(indices,samples,require);
  lisp best=FD_EMPTY_CHOICE;
  fd_hashtable h=
    fd_score_from_samples(indices,candidates,samples,FD_FALSE);
  DO_CHOICES(c,samples) fd_hashtable_set(h,c,LISPFIX(0)); END_DO_CHOICES;
  best=fd_hashtable_max(h);
  fd_free_hashtable(h); fd_free(h,sizeof(struct FD_HASHTABLE));
  return best;
}

static lisp lisp_get_similar_with_score_lexpr(lisp args)
{
  lisp indices=fd_get_arg(args,0,FD_VOID);
  lisp samples=fd_get_arg(args,1,FD_VOID); 
  lisp require=fd_get_arg(args,2,FD_FALSE); 
  lisp candidates=fd_find_similar(indices,samples,require);
  lisp best=FD_EMPTY_CHOICE; lisp best_score=LISPFIX(0);
  fd_hashtable h=
    fd_score_from_samples(indices,candidates,samples,FD_FALSE);
  DO_CHOICES(c,samples) fd_hashtable_set(h,c,LISPFIX(0)); END_DO_CHOICES;
  best=fd_hashtable_max(h);
  best_score=fd_hashtable_get(h,fd_first_choice(best),LISPFIX(0));
  fd_free_hashtable(h); fd_free(h,sizeof(struct FD_HASHTABLE));
  return FD_MAKE_PAIR(best_score,best);
}

/** Access to searching internals **/

static lisp lisp_get_frame_features_cproc(lisp args)
{
  lisp frames=fd_get_arg(args,0,FD_VOID);
  lisp slots=fd_get_arg(args,1,FD_FALSE);
  lisp values=fd_get_arg(args,2,FD_FALSE);
  if (FD_FALSEP(slots)) return fd_get_frame_features(frames);
  else if (FD_FALSEP(values) && (fd_list_length(args) == 3))
    return fd_get_slot_features(frames,slots);
  else return fd_get_slot_value_features(slots,values);
}

/** Indexing frames **/

static lisp lisp_index_frame_lexpr(lisp args)
{
  lisp ixs=fd_get_arg(args,0,FD_VOID);
  lisp frames=fd_get_arg(args,1,FD_VOID);
  lisp slotids=fd_get_arg(args,2,FD_FALSE);
  lisp slotvals=fd_get_arg(args,3,FD_FALSE);
  int n_args=fd_list_length(args);
  DO_CHOICES(ix,ixs) {
    fd_index idx=interpret_index_arg(ix);
    if (FD_FALSEP(slotids)) {
      DO_CHOICES(frame,frames) fd_index_frame(idx,frame); END_DO_CHOICES;}
    else if (FD_FALSEP(slotvals)) {
      DO_CHOICES(frame,frames) 
	fd_index_slots(idx,frame,slotids);
      END_DO_CHOICES;}
    else fd_index_slot_values(idx,frames,slotids,slotvals);}
  END_DO_CHOICES;
  return FD_VOID;
}

/** DOSLOTS **/

static lisp lisp_doslots_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp unit_arg=fd_get_arg(spec,0,FD_VOID);
  lisp slot_arg=fd_get_arg(spec,1,FD_VOID);
  lisp value_arg=fd_get_arg(spec,2,FD_VOID);
  lisp value_expr=fd_get_arg(spec,3,FD_VOID);
  lisp object=fd_eval_in_env(value_expr,env);
  FD_WITH_LEXICAL_ENV(doslots_env,env,4) {
    fd_bind_value(value_arg,FD_VOID,doslots_env);
    fd_bind_value(slot_arg,FD_VOID,doslots_env);
    fd_bind_value(unit_arg,FD_VOID,doslots_env);
    {DO_CHOICES(o,object) {
      if (PRIM_TYPEP(o,object_type)) {
	fd_set_value(unit_arg,o,doslots_env);
	{DO_SLOTS(sl,vl,o) {
	  fd_set_value(slot_arg,sl,doslots_env);	  
	  fd_set_value(value_arg,vl,doslots_env);	  
	  {DOLIST(expr,body) {
	    lisp value=fd_eval_in_env(expr,doslots_env);
	    decref(value);}}}}}
      else if (PRIM_TYPEP(o,slotmap_type)) {
	int i=0, l=SLOTMAP_SIZE(o);
	fd_set_value(unit_arg,o,doslots_env);
	while (i < l) {
	  lisp sl=SLOTMAP_KEY(o,i);
	  lisp vl=SLOTMAP_VALUE(o,i);	  
	  fd_set_value(slot_arg,sl,doslots_env);
	  fd_set_value(value_arg,vl,doslots_env);
	  {DOLIST(expr,body) {
	    lisp value=fd_eval_in_env(expr,doslots_env);
	    decref(value);}}
	  i++;}}
      else fd_type_error("DOSLOTS not used with a frame",o);}
    END_DO_CHOICES;}}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
  decref(object);
  return FD_VOID;
}

/** WITH-SLOTS **/

static lisp lisp_with_slots_handler(lisp expr,lispenv env)
{
  lisp frames_expr=fd_get_arg(expr,2,FD_VOID);
  lisp frames=fd_eval_in_env(frames_expr,env);
  lisp slots=fd_get_arg(expr,1,FD_VOID);
  int n_slots=fd_list_length(slots);
  lisp *old_values=fd_malloc(sizeof(lisp)*n_slots);
  lisp *slot_names=fd_malloc(sizeof(lisp)*n_slots);
  lisp body=fd_get_body(expr,3);
  lisp answer=FD_EMPTY_CHOICE;
  {DO_CHOICES(frame,frames) {
    lisp value=FD_EMPTY_CHOICE; int i=0;
    {FD_WITH_LEXICAL_ENV(with_slots_env,env,8) {
      UNWIND_PROTECT {
	DOLIST(slotid,slots) {
	  if (!(SYMBOLP(slotid)))
	    fd_type_error(_("not a symbol"),slotid);
	  else {
	    lisp current=fd_prim_get(frame,slotid);
	    old_values[i]=current; slot_names[i]=slotid; i++;
	    fd_bind_value(slotid,current,with_slots_env);}}
	value=eval_exprs(body,with_slots_env);
	ADD_TO_CHOICE(answer,value);}
      ON_UNWIND {
	int j=0; while (j < i) {
	  lisp newval=fd_symeval(slot_names[j],with_slots_env);
	  if (!(LISP_EQUAL(newval,old_values[j])))
	    fd_prim_set(frame,slot_names[j],newval);
	  decref(old_values[j]); decref(newval); j++;}}
      END_UNWIND;}
    FD_END_WITH_LEXICAL_ENV(answer);}}
   END_DO_CHOICES;}
  fd_free(slot_names,sizeof(lisp)*n_slots);
  fd_free(old_values,sizeof(lisp)*n_slots);
  return answer;
}

/** Export and import **/

static lisp lisp_export_frame_lexpr(lisp args)
{
  lisp frames=fd_get_arg(args,0,FD_VOID);
  lisp default_focus_slots=fd_getenv("%FOCUS-SLOTS");
  lisp focus_slots=fd_get_arg(args,1,default_focus_slots);
  lisp answers=FD_EMPTY_CHOICE; 
  int just_focus=(!(FD_EMPTYP(focus_slots)));
  DO_CHOICES(f,frames) {
    lisp export=fd_export_frame(f,focus_slots,just_focus);
    ADD_TO_CHOICE(answers,export);}
  END_DO_CHOICES;
  return answers;
}

static lisp lisp_import_frame_cproc(lisp frame,lisp slots)
{
  fd_import_frame(frame,slots,0);
  return frame;
}

/** Accessing indices **/

static lisp lisp_open_index_cproc(lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix) return fd_make_cptr(index_type,ix);
  else if (FD_STRINGP(spec))
    fd_raise_lisp_exception(fd_BadIndexSpec,FD_STRING_DATA(spec),spec);
  else if (FD_SYMBOLP(spec))
    fd_raise_lisp_exception(fd_BadIndexSpec,FD_SYMBOL_NAME(spec),spec);
  else fd_raise_lisp_exception(fd_BadIndexSpec,"",spec);
}

static fd_lisp lisp_open_compound_index_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) return FD_EMPTY_CHOICE;
  else if (FD_EMPTY_LISTP(FD_CDR(args))) {
    fd_index ix=fd_interpret_index(FD_CAR(args));
    return fd_make_cptr(index_type,ix);}
  else {
    struct FD_COMPOUND_INDEX *cix=fd_make_compound_index(0,NULL);
    FD_DOLIST(arg,args) {
      fd_index ix=fd_interpret_index(arg);
      fd_add_to_compound_index(cix,ix);}
    return fd_make_cptr(index_type,cix);}
}

static lisp lisp_trim_compound_index(lisp arg)
{
  if (FD_PRIM_TYPEP(arg,index_type)) {
    fd_index ix=(fd_index)FD_CPTR_DATA(arg);
    if (ix->type == compound_index)
      fd_trim_compound_index((struct FD_COMPOUND_INDEX *)ix);}
  return FD_VOID;
}

static fd_lisp add_to_background(fd_index ix)
{
  if (ix == NULL) return FD_EMPTY_CHOICE;
  else {
    fd_lisp bground=fd_symbol_value(bground_symbol);
    fd_lisp lix=fd_make_cptr(index_type,ix);
    if ((FD_EMPTYP(bground)) || (FD_VOIDP(bground))) {
      fd_index *indices=fd_xmalloc(sizeof(fd_index)*1);
      struct FD_COMPOUND_INDEX *cix; fd_lisp ixptr;
      indices[0]=ix;
      cix=fd_make_compound_index(1,indices);
      ixptr=fd_make_cptr(index_type,cix);
      fd_set_symbol_value(bground_symbol,ixptr);
      fd_decref(ixptr);
      return lix;}
    else if (FD_PRIM_TYPEP(bground,index_type)) {
      fd_index bix=FD_CPTR_DATA(bground);
      if (bix->type == compound_index) {
	struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)bix;
	fd_add_to_compound_index(cix,ix);
	fd_decref(bground);
	return lix;}}
    if (!(fd_choice_containsp(lix,bground))) {
      ADD_TO_CHOICE(bground,incref(lix));
      fd_set_symbol_value(bground_symbol,bground);}
    fd_decref(bground);
    return lix;}
}

static lisp lisp_use_index_cproc(lisp spec)
{
  if (FD_PRIM_TYPEP(spec,index_type)) 
    return add_to_background(FD_CPTR_DATA(spec));
  else if ((FD_SYMBOLP(spec)) &&
	   (FD_PRIM_TYPEP((FD_SYMBOL_VALUE(spec)),index_type)))
    return add_to_background(FD_CPTR_DATA(FD_SYMBOL_VALUE(spec)));
  else {
    fd_u8char *stringspec, *amp=NULL;
    if (FD_STRINGP(spec)) stringspec=FD_STRING_DATA(spec);
    else if ((FD_SYMBOLP(spec)) && (FD_STRINGP(FD_SYMBOL_VALUE(spec))))
      stringspec=FD_STRING_DATA(FD_SYMBOL_VALUE(spec));
    else fd_type_error(_("not an index specifier"),spec);
    if (amp=strchr(stringspec,'&')) {
      fd_lisp results=FD_EMPTY_CHOICE; int slen=strlen(stringspec);
      fd_u8char *buf=fd_malloc(slen), *scan=stringspec;
      while (amp) {
	fd_lisp ix;
	strncpy(buf,scan,amp-scan); buf[amp-scan]=NUL;
	ix=add_to_background(fd_open_index(buf));
	FD_ADD_TO_CHOICE(results,ix);
	scan=amp+1; amp=strchr(scan,'&');}
      if (*scan) {FD_ADD_TO_CHOICE(results,add_to_background(fd_open_index(scan)));}
      fd_free(buf,slen);
      return results;}
    else 
      return add_to_background(fd_open_index(stringspec));}
}
      
static lisp lisp_indexp_cproc(lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix == NULL) return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_file_indexp_cproc(lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix == NULL) return FD_FALSE;
  else if (ix->type == file_index) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_network_indexp_cproc(lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix == NULL) return FD_FALSE;
  else if (ix->type == network_index) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_compound_indexp_cproc(lisp spec)
{
  fd_index ix=fd_interpret_index(spec);
  if (ix == NULL) return FD_FALSE;
  else if (ix->type == compound_index) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_index_get_lexpr(lisp args)
{
  lisp key=fd_get_arg(args,1,FD_VOID);
  lisp indices=fd_get_arg(args,0,FD_VOID);
  if ((CHOICEP(key)) || (CHOICEP(indices))) {
    lisp answers=FD_EMPTY_CHOICE;
    DO_CHOICES(ix,indices) {
      fd_index idx=interpret_index_arg(ix);
      DO_CHOICES(ekey,key) {
	lisp v=fd_index_get(idx,ekey,FD_EMPTY_CHOICE);
	ADD_TO_CHOICE(answers,v);} /* 	ADD_TO_CHOICE(answers,incref(v)); */
      END_DO_CHOICES;}
    END_DO_CHOICES;
    return answers;}
  else {
    fd_index idx=interpret_index_arg(indices);
    return fd_index_get(idx,key,FD_EMPTY_CHOICE);}
}

static lisp lisp_index_prefetch_lexpr(lisp args)
{
  lisp indices=fd_get_arg(args,0,FD_VOID);
  lisp keys=fd_get_arg(args,1,FD_VOID);
  DO_CHOICES(index,indices) {
    fd_index idx=interpret_index_arg(index);
    fd_index_prefetch(idx,keys);}
  END_DO_CHOICES;
  return FD_VOID;
}

static lisp lisp_index_get_size_lexpr(lisp args)
{
  lisp indices, keys; int sum=0;
  fd_get_args("INDEX-GET-SIZE",args,
	      &indices,FD_EMPTY_CHOICE,
	      &keys,FD_EMPTY_CHOICE,NULL);	     
  {FD_DO_CHOICES(index,indices) {
    fd_index idx=interpret_index_arg(index);
    FD_DO_CHOICES(key,keys) 
      sum=sum+fd_index_get_size(idx,key);
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;}
  return LISPFIX(sum);
}

static lisp lisp_index_set_handler(lisp expr,lispenv env)
{
  lisp index_arg=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp key_arg=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp value_arg=fd_eval_in_env(fd_get_arg(expr,3,FD_VOID),env);
  DO_CHOICES(each_index,index_arg) {
    fd_index idx=interpret_index_arg(each_index);
    DO_CHOICES(key,key_arg)
      fd_index_set(idx,key,value_arg);
    END_DO_CHOICES;}
  END_DO_CHOICES;
  decref(index_arg); decref(key_arg); decref(value_arg);
  return FD_VOID;
}

static lisp lisp_index_add_lexpr(lisp args)
{
  lisp key=fd_get_arg(args,1,FD_VOID);
  lisp val=fd_get_arg(args,2,FD_VOID);
  DO_CHOICES(ix,fd_get_arg(args,0,FD_VOID)) {
    fd_index idx=interpret_index_arg(ix);
    fd_index_add(idx,key,val);}
  END_DO_CHOICES;
  return FD_VOID;
}

static lisp lisp_index_drop_lexpr(lisp args)
{
  lisp key=fd_get_arg(args,1,FD_VOID);
  lisp val=fd_get_arg(args,2,FD_VOID);
  DO_CHOICES(ix,fd_get_arg(args,0,FD_VOID)) {
    fd_index idx=interpret_index_arg(ix);
    fd_index_drop(idx,key,val);}
  END_DO_CHOICES;
  return FD_VOID;
}

static lisp lisp_index_zap_cproc(lisp index,lisp key)
{
  fd_index idx=interpret_index_arg(index);
  fd_lisp v=fd_index_get(idx,key,FD_EMPTY_CHOICE);
  fd_index_drop(idx,key,v);
  fd_decref(v);
  return FD_VOID;
}

static lisp lisp_index_all_keys_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  return fd_index_keys(idx);
}

static lisp lisp_commit_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_commit_index(idx);
  return FD_VOID;
}

static lisp lisp_revert_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_revert_index(idx);
  return FD_VOID;
}

static lisp lisp_swap_out_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_swap_out_index(idx);
  return FD_VOID;
}

static lisp lisp_sync_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  int changes=fd_sync_index(idx);
  if (changes>=0) return FD_LISPFIX(changes);
  else return FD_FALSE;
}

static lisp lisp_autosync_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  if (idx) {
    fd_autosync_index(idx);
    return FD_TRUE;}
  else return FD_FALSE;
}

#define size_convert(x) ((FIXNUMP(x)) ? (FIXLISP(x)) : (-1))

static lisp lisp_set_index_sizes_lexpr(lisp args)
{
  fd_index idx=interpret_index_arg(fd_get_arg(args,0,FD_VOID));
  fd_lisp cache_size=fd_get_arg(args,1,FD_FALSE);
  fd_lisp adds_size=fd_get_arg(args,2,FD_FALSE);
  fd_lisp drops_size=fd_get_arg(args,3,FD_FALSE);
  fd_lisp sizes_size=fd_get_arg(args,4,FD_FALSE);
  fd_index_set_sizes
    (idx,
     size_convert(cache_size),size_convert(adds_size),
     size_convert(drops_size),size_convert(sizes_size));
  return FD_VOID;
}

static lisp lisp_cache_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  if (idx->type == file_index)
    fd_cache_file_index((fd_file_index)idx);
  else {
    fprintf(stderr,
	    _("[Warning: no use caching offsets for network index %s]\n"),
	    idx->id);
    return FD_FALSE;}
  return FD_VOID;
}

static lisp lisp_close_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_close_index(idx);
  return FD_TRUE;
}

static lisp lisp_intern_index_values_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_intern_index_values(idx);
  return FD_VOID;
}

static lisp lisp_set_index_threshold_cproc(lisp index,lisp thresh)
{
  fd_index idx=interpret_index_arg(index);
  fd_set_index_zipf_threshold(idx,fd_lisp2int(thresh));
  return FD_VOID;
}

static lisp lisp_preload_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_preload_index(idx);
  return FD_VOID;
}

static lisp lisp_unpreload_index_cproc(lisp index)
{
  fd_index idx=interpret_index_arg(index);
  fd_unpreload_index(idx);
  return FD_VOID;
}

static lisp lisp_auto_cache_file_indices_cproc()
{
  fd_auto_cache_file_indices();
  return FD_VOID;
}

static lisp lisp_index_cache_handler(lisp expr,lispenv env)
{
  lisp index_expr=fd_get_arg(expr,1,FD_VOID),
       key_expr=fd_get_arg(expr,2,FD_VOID),
       generator=fd_get_arg(expr,3,FD_VOID);
  lisp index=fd_eval_in_env(index_expr,env);
  fd_index idx=interpret_index_arg(index);
  lisp key=fd_eval_in_env(key_expr,env);
  lisp known=fd_index_get(idx,key,FD_VOID);
  if (FD_VOIDP(known)) {
    lisp new=fd_eval_in_env(generator,env);
    fd_index_add(idx,key,new);
    fd_decref(key); fd_decref(index); 
    return new;}
  else {
    fd_decref(key); fd_decref(index); 
    return known;}
}

static void note_index(fd_index i,void *vval)
{
  lisp *val=(lisp *) vval;
  lisp li=fd_make_cptr(index_type,(void *) i);
  ADD_TO_CHOICE((*val),li);
}

static lisp lisp_all_indices_cproc()
{
  lisp answer=FD_EMPTY_CHOICE; fd_for_indices(note_index,&answer);
  return answer;
}

/** Accessing pools **/

static lisp lisp_use_pool_cproc(lisp spec)
{
  fd_pool p=fd_interpret_pool(spec);
  if (p) return fd_make_cptr(pool_type,p);
  else if (FD_STRINGP(spec))
    fd_raise_lisp_exception(fd_BadPoolSpec,FD_STRING_DATA(spec),spec);
  else if (FD_SYMBOLP(spec))
    fd_raise_lisp_exception(fd_BadPoolSpec,FD_SYMBOL_NAME(spec),spec);
  else fd_raise_lisp_exception(fd_BadPoolSpec,"",spec);
}

static lisp lisp_name2pool_cproc(lisp spec)
{
  fd_pool p=fd_interpret_pool(spec);
  if (p) return fd_make_cptr(pool_type,p);
  else return FD_EMPTY_CHOICE;
}

static lisp lisp_cache_file_pool_cproc(lisp lp)
{
  fd_pool p=fd_interpret_pool(lp);
  if (p->type == file_pool) fd_cache_file_pool((fd_file_pool)p);
  return FD_VOID;
}

static lisp lisp_revert_pool_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p != NULL) {
    fd_revert_pool(p); return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_commit_pool_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p != NULL) {
    fd_commit_pool(p); return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_swap_out_pool_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p != NULL) {
    fd_swap_out_pool(p); return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_sync_pool_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p != NULL) {
    int changes=fd_sync_pool(p);
    if (changes>=0) return FD_LISPFIX(changes);
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp lisp_autosync_pool_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p != NULL) {
    fd_autosync_pool(p);
    return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_close_pool_cproc(lisp index)
{
  fd_pool p=interpret_pool_arg(index);
  fd_close_pool(p);
  return FD_TRUE;
}

static lisp lisp_pool_id_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (p->type == file_pool)
    return fd_make_string(((fd_file_pool)p)->filename);
  else return fd_copy_string(p->id);
}

static lisp lisp_poolp_cproc(lisp pl)
{
  fd_pool p=fd_interpret_pool(pl);
  if (p == NULL) return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_file_poolp_cproc(lisp pl)
{
  fd_pool p=fd_interpret_pool(pl);
  if (p == NULL) return FD_FALSE;
  else if (p->type == file_pool) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_network_poolp_cproc(lisp pl)
{
  fd_pool p=fd_interpret_pool(pl);
  if (p == NULL) return FD_FALSE;
  else if (p->type == network_pool) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_get_pool_cproc(lisp oid)
{
  fd_pool p=fd_get_pool(oid);
  if (p) return fd_make_cptr(pool_type,p);
  else return FD_EMPTY_CHOICE;
}

static lisp lisp_get_pool_named_cproc(lisp name)
{
  fd_pool p=NULL;
  if (FD_PRIM_TYPEP(name,pool_type)) p=(fd_pool)(FD_CPTR_DATA(name));
  else if (FD_STRINGP(name)) p=fd_find_pool_named(FD_STRING_DATA(name));
  else if (FD_SYMBOLP(name)) p=fd_find_pool_named(FD_SYMBOL_NAME(name));
  else fd_type_error(_("Not a string or symbol"),name);
  if (p) return fd_make_cptr(pool_type,p);
  else return FD_EMPTY_CHOICE;
}

static lisp lisp_in_same_poolp_cproc(lisp oid1,lisp oid2)
{
  fd_pool p1=fd_get_pool(oid1), p2=fd_get_pool(oid2);
  if (p1 == p2) return FD_TRUE; 
  else return FD_FALSE;
}

static lisp lisp_in_poolp_cproc(lisp oid,lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  if (OID_ADDR_HIGH(oid) != FD_OID_HIGH(p->base))
    return FD_FALSE;
  else if (OID_ADDR_LOW(oid) < FD_OID_LOW(p->base))
    return FD_FALSE;
  else if ((OID_ADDR_LOW(oid)-FD_OID_LOW(p->base)) > p->capacity)
    return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_pool_base_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  return fd_make_oid(p->base);
}

static lisp lisp_pool_capacity_cproc(lisp pl)
{
  if ((FD_STRINGP(pl)) && (strchr(FD_STRING_DATA(pl),'@') == NULL)) {
    unsigned int cap=fd_file_pool_capacity(FD_STRING_DATA(pl));
    return FD_LISPFIX(cap);}
  else {
    fd_pool p=interpret_pool_arg(pl);
    return LISPFIX(p->capacity);}
}

static lisp lisp_pool_load_cproc(lisp pl)
{
  if ((FD_STRINGP(pl)) && (strchr(FD_STRING_DATA(pl),'@') == NULL)) {
    unsigned int load=fd_file_pool_load(FD_STRING_DATA(pl));
    return FD_LISPFIX(load);}
  else {
    fd_pool p=interpret_pool_arg(pl);
    int ld=fd_pool_load(p);
    return LISPFIX(ld);}
}

static lisp lisp_pool_freespace_cproc(lisp pl)
{
  if ((FD_STRINGP(pl)) && (strchr(FD_STRING_DATA(pl),'@') == NULL)) {
    unsigned int freespace=fd_file_pool_freespace(FD_STRING_DATA(pl));
    return FD_LISPFIX(freespace);}
  else {
    fd_pool p=interpret_pool_arg(pl);
    int ld=fd_pool_load(p);
    return LISPFIX(p->capacity-ld);}
}

static lisp lisp_pool_freespacep_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  unsigned int ld=fd_pool_load(p);
  if (ld < p->capacity) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_pool_label_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  return incref(p->label);
}

static void record_pool_id(fd_pool p,void *data)
{
  lisp *x=(lisp *) data;
  if (p->type == network_pool) {
    ADD_TO_CHOICE(*x,fd_copy_string(p->id));}
  else {
    fd_file_pool fp=(fd_file_pool) p;
    ADD_TO_CHOICE(*x,fd_make_string(fp->filename));}
}

static lisp lisp_get_poolids_cproc()
{
  lisp answer=FD_EMPTY_CHOICE;
  fd_for_pools(record_pool_id,&answer);
  return answer;
}

static lisp lisp_random_oid_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  return fd_random_oid(p);
}

static lisp lisp_oid_writablep_cproc(lisp oid)
{
  fd_pool p=fd_get_pool(oid);
  if (p == NULL) return FD_FALSE;
  else if ((p->read_only == FD_POOL_WRITABLE) ||
      (p->read_only == FD_POOL_LOCKABLE))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_pool_contents_cproc(lisp pl)
{
  lisp answer=FD_EMPTY_CHOICE;
  fd_pool p=interpret_pool_arg(pl);
  FD_OID x=p->base;
  unsigned int i=0, load;
  if (p->type == file_pool) load=((fd_file_pool)p)->load;
  else if (p->type == network_pool) {
    lisp expr=FD_MAKE_LIST1(get_load_symbol);
    lisp ld=fd_dtype_eval(expr,((fd_network_pool)p)->conn);
     fd_decref(expr);
     if (FIXNUMP(ld)) load=FIXLISP(ld);
     else fd_raise_exception("Networked pool won't provide data");}
  else fd_raise_exception("POOL-CONTENTS: Funny pool");
  while (i < load)
    {lisp oid=fd_make_oid(x); ADD_TO_CHOICE(answer,oid);
     FD_SET_OID_LOW(x,(FD_OID_LOW(x)+1)); i++;}
  return answer;
}

static lisp lisp_pregrow_oid_table_cproc(lisp atleast)
{
#if (!(FD_LIGHTWEIGHT_OIDS))
  fd_grow_oid_table(FIXLISP(atleast));
#endif
  return FD_TRUE;
}

/* Arranges for file pools to be automatically cached */
static lisp lisp_auto_cache_file_pools_cproc()
{
  fd_auto_cache_file_pools();
  return FD_VOID;
}

static lisp lisp_dopool_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID), body=fd_get_body(expr,2);
  lisp var=fd_get_arg(spec,0,FD_VOID);
  lisp start=fd_eval_in_env(fd_get_arg(spec,2,FD_FALSE),env);
  fd_pool p;
  if (SYMBOLP(var)) {
    lisp pool_spec=fd_eval_in_env(fd_get_arg(spec,1,FD_VOID),env);
    FD_OID base, scan; unsigned int load, i=0;
    FD_WITH_LEXICAL_ENV(dopool_env,env,1) {
      fd_bind_value(var,FD_VOID,dopool_env);
      p=interpret_pool_arg(pool_spec);
      base=p->base; load=fd_pool_load(p);
      FD_SET_OID_HIGH(scan,FD_OID_HIGH(base)); FD_SET_OID_LOW(scan,FD_OID_LOW(base));
      if (OIDP(start)) i=OID_ADDR_LOW(start)-FD_OID_LOW(base);
      while (i < load) {
	FD_SET_OID_LOW(scan,FD_OID_LOW(base)+i);
	fd_set_value(var,fd_make_oid(scan),dopool_env);
	eval_exprs_noreturn(body,dopool_env);
	i++;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    fd_decref(start);
    fd_decref(pool_spec);
    return FD_VOID;}
  else fd_raise_detailed_exception
    (fd_SyntaxError,fd_object_to_string(CAR(expr)));
}

static lisp lisp_pool_elts_cproc(lisp pool_spec)
{
  lisp elements=FD_EMPTY_CHOICE;
  fd_pool p=interpret_pool_arg(pool_spec);
  FD_OID base=p->base, scan=base;
  unsigned int load=fd_pool_load(p), i=0;
  while (i < load) {
    FD_SET_OID_LOW(scan,FD_OID_LOW(base)+i);
    ADD_TO_CHOICE(elements,fd_make_oid(scan));
    i++;}
  return elements;
}

static void note_pool(fd_pool p,void *vval)
{
  lisp *val=(lisp *) vval;
  lisp lp=fd_make_cptr(pool_type,(void *) p);
  ADD_TO_CHOICE((*val),lp);
}

static lisp lisp_all_pools_cproc()
{
  lisp answer=FD_EMPTY_CHOICE; fd_for_pools(note_pool,&answer);
  return answer;
}

static lisp lisp_commit_pools_cproc() { fd_commit_pools(); return FD_TRUE;}
static lisp lisp_commit_indices_cproc() { fd_commit_indices(); return FD_TRUE;}
static lisp lisp_revert_pools_cproc() { fd_revert_pools(); return FD_TRUE;}
static lisp lisp_revert_indices_cproc() { fd_revert_indices(); return FD_TRUE;}
static lisp lisp_swap_out_pools_cproc() { fd_swap_out_oids(); return FD_TRUE;}
static lisp lisp_swap_out_indices_cproc() { fd_swap_out_indices(); return FD_TRUE;}
static lisp lisp_never_save_cproc() { fd_never_save(); return FD_TRUE;}

static lisp lisp_sync_pools_cproc() { fd_sync_pools(); return FD_TRUE;}
static lisp lisp_sync_indices_cproc() { fd_sync_indices(); return FD_TRUE;}
static lisp lisp_autosync_cproc() { fd_autosync(); return FD_TRUE;}
static lisp lisp_autosync_all_cproc(fd_lisp flag)
{
  int result=
    ((FD_TRUEP(flag)) ? (fd_autosync_all(1)) : (fd_autosync_all(0))); 
  if (result) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_commit_all_cproc() 
{ fd_commit_indices(); fd_commit_pools(); return FD_TRUE;}
static lisp lisp_revert_all_cproc() 
{ fd_revert_indices(); fd_revert_pools(); return FD_TRUE;}
static lisp lisp_swap_out_all_cproc() 
{ fd_swap_out_indices(); fd_swap_out_oids(); return FD_TRUE;}
static lisp lisp_sync_all_cproc() 
{ fd_sync_indices(); fd_sync_pools(); return FD_TRUE;}


/** OID creation, reversion, etc **/

static lisp lisp_new_oid_cproc(lisp pl)
{
  fd_pool p=interpret_pool_arg(pl);
  return fd_new_oid(p);
}

static lisp lisp_revert_oid_cproc(lisp oid)
{
  if (OIDP(oid))
    if (fd_revert_oid(oid)) return FD_TRUE; else return FD_FALSE;
  else fd_type_error(_("not an OID"),oid);
}

static lisp lisp_commit_oid_cproc(lisp oid)
{
  if (OIDP(oid))
    if (fd_commit_oid(oid)) return FD_TRUE; else return FD_FALSE;
  else fd_type_error(_("not an OID"),oid);
}

static void copy_slots(fd_lisp to,fd_lisp from)
{
  FD_DO_SLOTS(slotid,slotvals,from)
    fd_prim_add(to,slotid,slotvals);
}

/** Making new frames **/

/* Creates a new frame in a particular pool with initial slot values. */
static lisp lisp_new_frame_handler(lisp expr,lispenv env)
{
  lisp frame, args=fd_get_body(expr,2);
  lisp pool_spec=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  if (FD_FALSEP(pool_spec)) frame=fd_make_slotmap(8);
  else {
    fd_pool p=interpret_pool_arg(pool_spec);
    frame=fd_frame_create(p);}
  decref(pool_spec);
  if ((PAIRP(args)) && (!(PAIRP(CDR(args))))) {
    fd_lisp single_arg=fd_eval_in_env(CAR(args),env);
    if (FD_OIDP(frame)) fd_set_oid_value(frame,single_arg);
    else {
      FD_DO_CHOICES(value,single_arg)
	if (FD_FRAMEP(value)) copy_slots(frame,value);
	else if (FD_SLOTMAPP(value)) copy_slots(frame,value);
	else fd_type_error(_("Not a frame or slotmap"),value);
      END_FD_DO_CHOICES;}
    fd_decref(single_arg);
    return frame;}
  else while ((PAIRP(args)) && (PAIRP(CDR(args)))) {
    lisp value=fd_eval_in_env(CAR(CDR(args)),env), slot_spec=CAR(args), slotid;
    slotid=fd_eval_in_env(slot_spec,env);
    fd_frame_add(frame,slotid,value);
    decref(value); decref(slotid);
    args=CDR(CDR(args));}
  return frame;
}

/* Creates a copy of a frame in a particular pool. */
static lisp lisp_copy_frame_lexpr(lisp args)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  fd_lisp pools=fd_get_arg(args,0,FD_VOID);
  fd_lisp frames=fd_get_arg(args,1,FD_VOID);
  fd_lisp slots=fd_get_arg(args,2,FD_FALSE);
  FD_DO_CHOICES(pool,pools) {
    fd_pool p=interpret_pool_arg(pool);
    FD_DO_CHOICES(frame,frames) {
      if (FD_FALSEP(slots)) {
	fd_lisp copy=fd_copy_frame(frame,p);
	FD_ADD_TO_CHOICE(results,copy);}
      else {
	fd_lisp copy=fd_frame_create(p);
	FD_DO_CHOICES(slotid,slots) {
	  fd_lisp v=fd_frame_get(frame,slotid);
	  fd_frame_add(copy,slotid,v);}
	END_FD_DO_CHOICES;
	FD_ADD_TO_CHOICE(results,copy);}}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;
  return results;
}

static lisp lisp_modify_frame_lexpr(lisp args)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  fd_lisp frames=fd_get_arg(args,0,FD_VOID);
  fd_lisp slotspecs=fd_get_body(args,1);
  FD_DO_CHOICES(frame,frames) {
    fd_lisp scan=slotspecs;
    while (!(FD_EMPTY_LISTP(scan))) {
      fd_lisp slotid=fd_get_arg(scan,0,FD_VOID);
      fd_lisp value=fd_get_arg(scan,1,FD_VOID);
      fd_frame_add(frame,slotid,value);
      scan=CDR(CDR(scan));}}
  END_FD_DO_CHOICES;
  return fd_incref(frames);
}

/** OID arithmetic **/

static lisp lisp_oid_addr_low_cproc(lisp oid)
{
  if (OIDP(oid))
    return LISPFIX(OID_ADDR_LOW(oid));
  else fd_type_error(_("not an OID"),oid);
}

static lisp lisp_oid_addr_high_cproc(lisp oid)
{
  if (OIDP(oid))
    return LISPFIX(OID_ADDR_HIGH(oid));
  else fd_type_error(_("not an OID"),oid);
}

static lisp lisp_oid_difference_cproc(lisp oid1,lisp oid2)
{
  if (!(OIDP(oid1))) fd_type_error(_("not an OID"),oid1);
  if (!(OIDP(oid2))) fd_type_error(_("not an OID"),oid2);
  if (OID_ADDR_HIGH(oid1) != OID_ADDR_HIGH(oid2))
    return FD_FALSE;
  else return LISPFIX(OID_ADDR_LOW(oid1)-OID_ADDR_LOW(oid2));
}

static lisp lisp_oid_plus_cproc(lisp oid1,lisp offset)
{
  if (!(OIDP(oid1))) fd_type_error(_("not an OID"),oid1);
  else if (!(FIXNUMP(offset)))
    fd_type_error(_("not a fixnum"),offset);
  else {
    FD_OID id=OID_ADDR(oid1);
    FD_SET_OID_LOW(id,FD_OID_LOW(id)+FIXLISP(offset));
    return fd_make_oid(id);}
}

static lisp lisp_oid_compare_cproc(lisp oid1,lisp oid2)
{
  if (OID_ADDR_HIGH(oid1) == OID_ADDR_HIGH(oid2))
    if (OID_ADDR_LOW(oid1) > OID_ADDR_LOW(oid2)) return FD_TRUE;
    else return FD_FALSE;
  else if (OID_ADDR_HIGH(oid1) > OID_ADDR_HIGH(oid2))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_make_oid_cproc(lisp hi,lisp lo)
{
  FD_OID id;
  if (!((FIXNUMP(hi)) && (FIXNUMP(lo))))
    fd_raise_exception(_("OID components must be fixnums"));
  FD_SET_OID_HIGH(id,FIXLISP(hi)); FD_SET_OID_LOW(id,FIXLISP(lo));
  return fd_make_oid(id);
}

/** Inheritance support functions **/

static lisp lisp_inherits_valuep_cproc
   (lisp root,lisp slotid,lisp through,lisp value)
{
  if (fd_inherits_valuep(root,slotid,through,value))
    return (FD_TRUE);
  else return (FD_FALSE);
}

static lisp lisp_pathp_cproc(lisp from,lisp slotid,lisp to)
{
  if (fd_pathp(from,slotid,to))
    return (FD_TRUE);
  else return (FD_FALSE);
}

/** DEFFRAME **/

static int check_frame(lisp frame,lisp spec,fd_lispenv env)
{
  while ((PAIRP(spec)) && (PAIRP(CDR(spec)))) {
    lisp slotid=fd_eval_in_env(CAR(spec),env);
    lisp value=fd_eval_in_env(CAR(CDR(spec)),env);
    int check=fd_frame_test(frame,slotid,value);
    decref(value); decref(slotid);
    if (check == 0) return 0;
    spec=CDR(CDR(spec));}
  return 1;
}

static lisp defframe_handler(lisp expr,fd_lispenv env)
{
  lisp name=fd_get_arg(expr,1,FD_VOID);
  lisp pool=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  lisp index=fd_eval_in_env(fd_get_arg(expr,3,FD_VOID),env);
  lisp bound, frame, key, specs=fd_get_body(expr,4);
  fd_pool p=interpret_pool_arg(pool);
  fd_index ix=interpret_index_arg(index); 
  /* Clean up intermediate values */
  decref(pool); decref(index);
  /* Start checking syntax */
  if (!(SYMBOLP(name)))
    fd_type_error(_("DEFFRAME arg must be symbol"),name);
  else bound=fd_symeval(name,env);
  /* If it's bound, check it and return if it's okay */
  if (OIDP(bound)) {
    if (check_frame(bound,specs,env)) return incref(bound);
    else fd_raise_lisp_exception
	   ("DEFFRAME expectation failure (current binding)",
	    SYMBOL_NAME(name),specs);}
  else if (FD_VOIDP(bound)) {}
  else {
    fd_decref(bound);
    fd_raise_lisp_exception
	   ("DEFFRAME expectation failure (current binding)",
	    SYMBOL_NAME(name),bound);}
  /* Now look in the index */
  key=FD_MAKE_PAIR(obj_name_symbol,name);
  frame=fd_index_get(ix,key,FD_VOID);
  /* If it's already there, bind it and return it. */
  if (OIDP(frame)) {
     decref(key);
     if (check_frame(frame,specs,env)) {
      fd_bind_value(name,frame,env);
      return frame;}
    else fd_raise_lisp_exception
	   (fd_DefframeExpectationFailure,SYMBOL_NAME(name),specs);}
  else {   /* If it's not there, make it, index it, bind it, and initialize it. */
    fd_decref(frame);
    frame=fd_frame_create(p);
    fd_frame_add(frame,obj_name_symbol,name);
    fd_index_add(ix,key,frame);
    fd_decref(key);
    fd_bind_value(name,frame,env);
    /* Initialize it */
    while ((PAIRP(specs)) && (PAIRP(CDR(specs)))) {
      lisp slotid=fd_eval_in_env(CAR(specs),env);
      lisp value=fd_eval_in_env(CAR(CDR(specs)),env);
      fd_frame_add(frame,slotid,value);
      decref(value); decref(slotid);
      specs=CDR(CDR(specs));}
    return frame;}
}

/** Initialization **/

static void initialize_framerd_types()
{
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(pool_type);
   r->compare_fcn=fd_compare_cptrs;
   r->copy_fcn=fd_copy_cptr; r->gc_fcn=fd_free_cptr;
   r->print_fcn=print_pool;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(index_type);
   r->compare_fcn=fd_compare_cptrs; r->copy_fcn=fd_copy_cptr;
   r->gc_fcn=fd_free_cptr; r->print_fcn=print_index;}
}

FDSCRIPT_EXPORT
void fd_initialize_framerd_c()
{
  fd_lispenv fim;
  fim=fd_fdinternals_env=fd_make_module();
  fd_register_module("FDINTERNALS",fim,FD_UNSAFE_ENV,0);

  initialize_framerd_types();

  get_load_symbol=fd_make_symbol("GET-LOAD");
  quote_symbol=fd_make_symbol("QUOTE");
  obj_name_symbol=fd_make_symbol("%ID");
  bground_symbol=fd_make_symbol("%BACKGROUND");

  fd_add_special_form(NULL,"FRAME-CREATE",lisp_new_frame_handler);
  fd_add_cproc(NULL,"FDD",1,lisp_fdd_cproc);
  fd_add_alias(NULL,"D","FDD");
  fd_add_special_form(NULL,"DOSLOTS",lisp_doslots_handler);
  fd_add_lexpr
    (NULL,"INDEX-FRAME",FD_ND_LEXPR,lisp_index_frame_lexpr);
  fd_add_lexpr(NULL,"FIND-FRAMES",FD_ND_LEXPR,lisp_find_frames_lexpr);
  fd_add_lexpr(NULL,"FIND-SIMILAR",FD_ND_LEXPR,lisp_find_similar_lexpr);

  fd_add_cproc(fim,"OID-VALUE",1,lisp_oid_value_cproc);
  fd_add_restricted_cproc("OID-VALUE",1,lisp_oid_value_cproc);
  fd_add_cproc(fim,"OID-LOADED?",1,lisp_oid_loadedp_cproc);
  fd_add_cproc(fim,"OID-MODIFIED?",1,lisp_oid_modifiedp_cproc);
  fd_add_lexpr
    (fim,"SET-OID-VALUE!",FD_ND_LEXPR,lisp_set_oid_value_cproc);

  fd_add_cproc(fim,"GROW-OID-TABLE",1,lisp_pregrow_oid_table_cproc);

  fd_add_cproc(fim,"ALLOCATE-OID",1,lisp_new_oid_cproc);
  fd_add_cproc(fim,"REVERT-OID",1,lisp_revert_oid_cproc);
  fd_add_cproc(fim,"COMMIT-OID",1,lisp_commit_oid_cproc);
  fd_add_cproc(fim,"LOAD-OID",1,lisp_load_oid_cproc);
  fd_add_cproc(fim,"SWAP-OUT",1,lisp_swap_out_cproc);
  fd_add_lexpr(fim,"PREFETCH",FD_ND_LEXPR,lisp_prefetch_oids_lexpr);
  fd_add_lexpr(fim,"PREFETCH!",FD_ND_LEXPR,lisp_prefetch_oids_noret_lexpr);

  /* OID Arithmetic (A Bad Idea) */
  fd_add_cproc(fim,"OID-ADDR-LOW",1,lisp_oid_addr_low_cproc);
  fd_add_cproc(fim,"OID-ADDR-HIGH",1,lisp_oid_addr_high_cproc);
  fd_add_cproc(fim,"OID-DIFFERENCE",2,lisp_oid_difference_cproc);
  fd_add_cproc(fim,"OID-PLUS",2,lisp_oid_plus_cproc);
  fd_add_cproc(fim,"COMPARE-OIDS",2,lisp_oid_compare_cproc);
  fd_add_cproc(fim,"MAKE-OID",2,lisp_make_oid_cproc);

  /* Frame functions */
  fd_add_lexpr(fim,"COPY-FRAME",FD_ND_LEXPR,lisp_copy_frame_lexpr);
  fd_add_lexpr(fim,"MODIFY-FRAME",FD_ND_LEXPR,lisp_modify_frame_lexpr);
  fd_add_cproc
    (fim,"CONTROL-FRAME-PRINTING",1,lisp_control_frame_printing_cproc);
  fd_add_cproc(fim,"SHOW-POOLIDS",1,lisp_show_poolids_cproc);

  /* Getting slots with inference */
  fd_add_cproc(fim,"FGET",2,fd_frame_get);
  fd_add_cproc(fim,"FRAME-GET",2,fd_frame_get);
  fd_add_cproc(fim,"FRAME-TEST",3,lisp_frame_test_cproc);
  fd_add_cproc(fim,"FTEST",3,lisp_frame_test_cproc);
  fd_add_cproc(fim,"FCHECK",3,lisp_frame_confirm_cproc);
  fd_add_cproc(fim,"FRAME-ADD!",3,lisp_frame_add_cproc);
  fd_add_cproc(fim,"FADD!",3,lisp_frame_add_cproc);
  fd_add_lexpr(fim,"FRAME-SET!",FD_ND_LEXPR,lisp_frame_set_lexpr);
  fd_add_lexpr(fim,"FSET!",FD_ND_LEXPR,lisp_frame_set_lexpr);
  fd_add_cproc(fim,"FRAME-ZAP!",3,lisp_frame_remove_cproc);
  fd_add_cproc(fim,"FZAP!",3,lisp_frame_remove_cproc);
  fd_add_cproc(fim,"FRAME-SLOTS",1,fd_frame_slots);
  fd_add_cproc(fim,"FSLOTS",1,fd_frame_slots);

  /* Getting slots without inference */
  fd_add_cproc(fim,"PGET",2,lisp_prim_get_cproc);
  fd_add_lexpr(fim,"PSET!",FD_ND_LEXPR,lisp_prim_set_lexpr);
  fd_add_cproc(fim,"PADD!",3,lisp_prim_add_cproc);
  fd_add_cproc(fim,"COUNT-SLOTS",1,lisp_count_slots_cproc);

  /* A binding form which is like 'within frame' */
  fd_add_special_form(fim,"WITH-SLOTS",lisp_with_slots_handler);

  /* Slot caching functions */
  fd_add_lexpr(fim,"ENABLE-SLOT-CACHE!",FD_NORMAL_LEXPR,lisp_enable_slot_cache_lexpr);
  fd_add_cproc(fim,"DISABLE-SLOT-CACHE!",1,lisp_disable_slot_cache_cproc);
  fd_add_restricted_special_form("CLEAR-SLOT-CACHE!",lisp_clear_slot_cache_handler);

  /* Indexing frames */
  fd_add_cproc(fim,"USE-AUTOINDEX!",1,lisp_use_autoindex_cproc);
  fd_add_lexpr
    (fim,"INDEX-FRAME",FD_ND_LEXPR,lisp_index_frame_lexpr);
  fd_add_lexpr(fim,"INDEX-VALUES",FD_ND_LEXPR,lisp_index_frame_lexpr);

  fd_add_lexpr(fim,"REGISTER-ADJUNCT!",FD_NORMAL_LEXPR,lisp_use_adjunct_lexpr);

  /* Searching for frames */
  fd_add_lexpr(fim,"FIND-FRAMES",FD_ND_LEXPR,lisp_find_frames_lexpr);
  fd_add_lexpr(fim,"FIND-SIMILAR",FD_ND_LEXPR,lisp_find_similar_lexpr);

  fd_add_lexpr(fim,"GET-SCORES",FD_ND_LEXPR,lisp_get_scores_lexpr);
  fd_add_lexpr(fim,"GET-SCORES-FROM-SAMPLES",FD_ND_LEXPR,
	       lisp_get_scores_from_samples_lexpr);

  fd_add_lexpr(fim,"GET-SIMILAR",FD_ND_LEXPR,lisp_get_similar_lexpr);
  fd_add_lexpr(fim,"GET-SIMILAR-SCORED",FD_ND_LEXPR,
	       lisp_get_similar_with_score_lexpr);
  fd_add_lexpr(fim,"GET-BEST",FD_ND_LEXPR,lisp_get_best_lexpr);
  fd_add_lexpr(fim,"GET-BEST-FROM-SAMPLES",FD_ND_LEXPR,
	       lisp_get_best_from_samples_lexpr);

  /* Advanced searching */
  fd_add_lexpr(fim,"FRAME-FEATURES",FD_ND_LEXPR,
	       lisp_get_frame_features_cproc);

  /* Pool functions */
  fd_add_cproc(fim,"USE-POOL",1,lisp_use_pool_cproc);
  fd_add_restricted_cproc("USE-POOL",1,lisp_use_pool_cproc);
  fd_add_cproc(fim,"NAME->POOL",1,lisp_name2pool_cproc);
  fd_add_restricted_cproc("NAME->POOL",1,lisp_name2pool_cproc);

  fd_add_special_form(fim,"DO-POOL",lisp_dopool_handler);
  fd_add_cproc(fim,"POOL-CONTENTS",1,lisp_pool_elts_cproc);
  fd_add_cproc(fim,"POOL-ELTS",1,lisp_pool_elts_cproc);
  fd_add_cproc(fim,"ALL-POOLS",0,lisp_all_pools_cproc);

  fd_add_cproc(fim,"GET-POOLS",0,lisp_get_poolids_cproc);

  fd_add_cproc(fim,"CLOSE-POOL",1,lisp_close_pool_cproc);
  fd_add_cproc(fim,"COMMIT-POOL",1,lisp_commit_pool_cproc);  
  fd_add_cproc(fim,"REVERT-POOL",1,lisp_revert_pool_cproc);
  fd_add_cproc(fim,"SWAP-OUT-POOL",1,lisp_swap_out_pool_cproc);
  fd_add_cproc(fim,"SYNC-POOL",1,lisp_sync_pool_cproc);
  fd_add_cproc(fim,"AUTOSYNC-POOL",1,lisp_autosync_pool_cproc);

  fd_add_cproc(fim,"REVERT-POOLS",0,lisp_revert_pools_cproc);
  fd_add_cproc(fim,"SWAP-OUT-POOLS",0,lisp_swap_out_pools_cproc);  

  fd_add_cproc(NULL,"GET-POOL",1,lisp_get_pool_cproc);
  fd_add_cproc(NULL,"NAME->POOL",1,lisp_get_pool_named_cproc);
  fd_add_cproc(NULL,"POOL-LABEL",1,lisp_pool_label_cproc);
  fd_add_cproc(NULL,"POOL-ID",1,lisp_pool_id_cproc);
  fd_add_cproc(NULL,"IN-POOL?",2,lisp_in_poolp_cproc);
  fd_add_cproc(NULL,"POOL?",1,lisp_poolp_cproc);
  fd_add_cproc(NULL,"RANDOM-OID",1,lisp_random_oid_cproc);
  fd_add_cproc(NULL,"WRITABLE?",1,lisp_oid_writablep_cproc);

  fd_add_cproc(fim,"IN-SAME-POOL?",2,lisp_in_same_poolp_cproc);
  fd_add_cproc(fim,"FILE-POOL?",1,lisp_file_poolp_cproc);
  fd_add_cproc(fim,"NETWORK-POOL?",1,lisp_network_poolp_cproc);
  fd_add_cproc(fim,"POOL-BASE",1,lisp_pool_base_cproc);
  fd_add_cproc(fim,"POOL-CAPACITY",1,lisp_pool_capacity_cproc);
  fd_add_cproc(fim,"POOL-LOAD",1,lisp_pool_load_cproc);
  fd_add_cproc(fim,"POOL-FREESPACE",1,lisp_pool_freespace_cproc);
  fd_add_cproc(fim,"POOL-FREESPACE?",1,lisp_pool_freespacep_cproc);
  fd_add_cproc(fim,"POOL-CONTENTS",1,lisp_pool_contents_cproc);
  fd_add_cproc(fim,"CACHE-POOL",1,lisp_cache_file_pool_cproc);
  fd_add_cproc(fim,"AUTO-CACHE-FILE-POOLS",0,lisp_auto_cache_file_pools_cproc);
  fd_add_restricted_cproc
    ("AUTO-CACHE-FILE-POOLS",0,lisp_auto_cache_file_pools_cproc);

  fd_add_cproc(fim,"POOL-WRITABLE?",1,lisp_pool_writablep_cproc);
  fd_add_cproc(fim,"SET-POOL-READ-ONLY!",1,lisp_set_pool_read_only_cproc);


  /* Index primitives */
  fd_add_cproc(fim,"USE-INDEX",1,lisp_use_index_cproc);
  fd_add_restricted_cproc("USE-INDEX",1,lisp_use_index_cproc);

  fd_add_cproc(NULL,"INDEX?",1,lisp_indexp_cproc);
  fd_add_cproc(fim,"INDEX?",1,lisp_indexp_cproc);
  fd_add_cproc(fim,"FILE-INDEX?",1,lisp_file_indexp_cproc);
  fd_add_cproc(fim,"NETWORK-INDEX?",1,lisp_network_indexp_cproc);
  fd_add_cproc(fim,"COMPOUND-INDEX?",1,lisp_compound_indexp_cproc);

  fd_add_lexpr
    (fim,"OPEN-COMPOUND-INDEX",FD_NORMAL_LEXPR,lisp_open_compound_index_lexpr);
  fd_add_cproc(fim,"TRIM-COMPOUND-INDEX",1,lisp_trim_compound_index);

  fd_add_lexpr(NULL,"INDEX-GET",FD_ND_LEXPR,lisp_index_get_lexpr);
  fd_add_lexpr(NULL,"INDEX-GET-SIZE",FD_ND_LEXPR,lisp_index_get_size_lexpr);
  fd_add_lexpr(NULL,"INDEX-ADD!",FD_ND_LEXPR,lisp_index_add_lexpr);
  fd_add_lexpr(NULL,"INDEX-DROP!",FD_ND_LEXPR,lisp_index_drop_lexpr);
  fd_add_cproc(NULL,"INDEX-ZAP!",2,lisp_index_zap_cproc);
  fd_add_special_form(NULL,"INDEX-SET!",lisp_index_set_handler);
  fd_add_cproc(NULL,"INDEX-KEYS",1,lisp_index_all_keys_cproc);
  fd_add_alias(NULL,"INDEX-ADD","INDEX-ADD!");
  
  fd_add_lexpr(NULL,"INDEX-PREFETCH",
	       FD_ND_LEXPR,lisp_index_prefetch_lexpr);

  fd_add_cproc(fim,"OPEN-INDEX",1,lisp_open_index_cproc);
  fd_add_cproc(fim,"REVERT-INDEX",1,lisp_revert_index_cproc);
  fd_add_lexpr(fim,"SET-INDEX-SIZES!",FD_NORMAL_LEXPR,
	       lisp_set_index_sizes_lexpr);
  fd_add_alias(fim,"GROW-INDEX","SET_INDEX-SIZES!");
  fd_add_cproc(fim,"SWAP-OUT-INDEX",1,lisp_swap_out_index_cproc);
  fd_add_cproc(fim,"COMMIT-INDEX",1,lisp_commit_index_cproc);
  fd_add_cproc(fim,"COMMIT-INDEX",1,lisp_commit_index_cproc);
  fd_add_cproc(fim,"SYNC-INDEX",1,lisp_sync_index_cproc);
  fd_add_cproc(fim,"AUTOSYNC-INDEX",1,lisp_autosync_index_cproc);
  
  fd_add_cproc(fim,"CACHE-INDEX",1,lisp_cache_index_cproc);
  fd_add_cproc(fim,"CLOSE-INDEX",1,lisp_close_index_cproc);

  fd_add_cproc(fim,"INDEX-WRITABLE?",1,lisp_index_writablep_cproc);
  fd_add_cproc(fim,"SET-INDEX-READ-ONLY!",1,lisp_set_index_read_only_cproc);

  fd_add_cproc(fim,"INTERN-INDEX-VALUES!",1,lisp_intern_index_values_cproc);
  fd_add_cproc(fim,"PRELOAD-INDEX!",1,lisp_preload_index_cproc);
  fd_add_alias(fim,"PRELOAD-INDEX!","PRELOAD-FILE-INDEX!");
  fd_add_cproc(fim,"UNPRELOAD-INDEX!",1,lisp_unpreload_index_cproc);
  fd_add_alias(fim,"UNPRELOAD-INDEX!","UNPRELOAD-FILE-INDEX!");
  fd_add_cproc(fim,"SET-INDEX-THRESHOLD!",2,lisp_set_index_threshold_cproc);
  fd_add_restricted_cproc
    ("AUTO-CACHE-FILE-INDICES",0,lisp_auto_cache_file_indices_cproc);
  fd_add_cproc
    (fim,"AUTO-CACHE-FILE-INDICES",0,lisp_auto_cache_file_indices_cproc);
  fd_add_cproc(fim,"ALL-INDICES",0,lisp_all_indices_cproc);

  fd_add_restricted_special_form("INDEX-CACHE",lisp_index_cache_handler);

  fd_add_cproc(NULL,"AUTOSYNC-ALL",1,lisp_autosync_all_cproc);

  /* More frame procedures */

  fd_add_cproc(NULL,"INHERIT-VALUES",3,fd_inherit_values);
  fd_add_cproc(NULL,"INHERITS-VALUE?",4,lisp_inherits_valuep_cproc);
  fd_add_cproc(NULL,"VALUE-PATH?",3,lisp_pathp_cproc);

  fd_add_lexpr(NULL,"XF",FD_ND_LEXPR,lisp_export_frame_lexpr);
  fd_add_lexpr(fim,"EXPORT-FRAME",FD_ND_LEXPR,lisp_export_frame_lexpr);
  fd_add_cproc(fim,"IMPORT-FRAME",2,lisp_import_frame_cproc);

  /* Committing, reverting, and swapping */

  fd_add_cproc(fim,"COMMIT-POOLS",0,lisp_commit_pools_cproc);
  fd_add_cproc(fim,"COMMIT-INDICES",0,lisp_commit_indices_cproc);
  fd_add_cproc(fim,"COMMIT-ALL",0,lisp_commit_all_cproc);
  fd_add_cproc(NULL,"COMMIT-ALL",0,lisp_commit_all_cproc);
  fd_add_cproc(NULL,"AUTOSYNC",0,lisp_autosync_cproc);

  fd_add_cproc(fim,"SYNC-POOLS",0,lisp_sync_pools_cproc);
  fd_add_cproc(fim,"SYNC-INDICES",0,lisp_sync_indices_cproc);
  fd_add_cproc(fim,"SYNC-ALL",0,lisp_sync_all_cproc);
  fd_add_cproc(NULL,"SYNC-ALL",0,lisp_sync_all_cproc);

  fd_add_cproc(fim,"REVERT-POOLS",0,lisp_revert_pools_cproc);
  fd_add_cproc(fim,"REVERT-INDICES",0,lisp_revert_indices_cproc);
  fd_add_cproc(fim,"REVERT-ALL",0,lisp_revert_all_cproc);

  fd_add_cproc(fim,"SWAP-OUT-POOLS",0,lisp_swap_out_pools_cproc);
  fd_add_cproc(fim,"SWAP-OUT-INDICES",0,lisp_swap_out_indices_cproc);
  fd_add_cproc(fim,"SWAP-OUT-ALL",0,lisp_swap_out_all_cproc);

  fd_add_cproc(fim,"NEVER-SAVE",0,lisp_never_save_cproc);

  fd_add_special_form(NULL,"DEFFRAME",defframe_handler);

  fd_register_source_file("framerd",__DATE__,vcid);

}


/* File specific stuff */

/* The CVS log for this file
   $Log: framerd.c,v $
   Revision 1.50  2005/08/04 23:37:20  haase
   Changed obj-name to %id

   Revision 1.49  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.48  2004/10/27 17:25:41  haase
   Add FDSCript access to adjuncts

   Revision 1.47  2004/10/19 00:17:11  haase
   Made the background into a compound index

   Revision 1.46  2004/10/18 15:25:08  haase
   Added compound indices

   Revision 1.45  2004/09/17 08:31:34  haase
   Fixed leak in lisp index-get and added access to complete slot cache resets

   Revision 1.44  2004/08/26 19:16:22  haase
   Removed overlays

   Revision 1.43  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.42  2004/05/14 14:41:57  haase
   Made preloading be an option for all kinds of indices

   Revision 1.41  2004/03/31 11:19:56  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.40  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.39  2004/03/30 19:16:27  haase
   Fixes to schema implementation

   Revision 1.38  2004/03/30 08:10:11  haase
   Support for using schemas

   Revision 1.37  2004/02/17 18:36:31  haase
   Indices can now serve as slot caches, allowing persistence

   Revision 1.36  2004/02/14 14:53:22  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.35  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.34  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.33  2003/11/21 17:48:21  haase
   Made indexing with explicit values not do automatic expansion

   Revision 1.32  2003/11/03 00:21:36  haase
   Error checking for pool functions

   Revision 1.31  2003/10/28 18:23:52  haase
   Added name->pool

   Revision 1.30  2003/10/21 16:50:13  haase
   Fixed bug with USE-INDEX when index opening fails

   Revision 1.29  2003/10/20 09:24:36  haase
   Better error checking on index args

   Revision 1.28  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.27.2.4  2003/08/15 13:37:01  haase
   Updated LISP call to cache file pool for new prototype

   Revision 1.27.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.27.2.2  2003/01/26 20:41:46  haase
   Misc. fixes especially some GC

   Revision 1.27.2.1  2002/08/09 19:08:21  haase
   Made prefetch return its argument, made PREFETCH\! not

   Revision 1.27  2002/06/29 01:22:42  haase
   Made USE-INDEX work with & specs

   Revision 1.26  2002/06/18 16:26:25  haase
   Added primitives for making pools/indices read only

   Revision 1.25  2002/06/15 14:55:20  haase
   Removed interface to deleted search-max functionality

   Revision 1.24  2002/05/11 13:43:57  haase
   Restored old slot-copying frame-create semantics

   Revision 1.23  2002/04/24 20:28:43  haase
   Fixed typo in declaration of POOL-LABEL

   Revision 1.22  2002/04/24 20:26:18  haase
   Fixed changelog entries for various files

   Revision 1.21  2002/04/24 20:06:17  haase
   Fixed bug/typo for IN-SAME-POOL?
   Removed redundant entries for some functions from global and 
   FDINTERNALS environment

   Revision 1.20  2002/04/22 17:50:31  haase
   Made COMMIT-OID have the right cproc bound to it

   Revision 1.19  2002/04/17 20:30:45  haase
   Made index-get-size add the sizes together when it is passed indices and keys non deterministically.

   Revision 1.18  2002/04/11 00:26:58  haase
   FDScript access to pools from names

   Revision 1.17  2002/04/10 17:13:04  haase
   When spec is string or symbol, index/pool access functions pass it along as details

   Revision 1.16  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
