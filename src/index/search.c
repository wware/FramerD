/* C Mode */

/* search.c
   Indexing and search support for FramerD
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

static char vcid[] = "$Id: search.c,v 1.19 2005/01/14 16:48:46 haase Exp $";

/** Setup **/
/** Stop features **/
/** Search access to indices **/
/** Getting frame features for indexing **/
/** Indexing frames **/
/** Strict searches **/
/** Fuzzy searches **/
/** Initialization procedures **/

#include "framerd.h"

FRAMERD_EXPORT fd_lisp fd_apply(fd_lisp func,fd_lisp args);

/** Setup **/

#include <assert.h>
#include <time.h>
#include <stdarg.h>

static lisp stop_slots_symbol;
static lisp expanders_symbol, default_expanders_symbol;
FASTOP fd_index lisp2index(fd_lisp x)
{
  if (PRIM_TYPEP(x,index_type))
    return (fd_index) CPTR_DATA(x);
  else return fd_interpret_index(x);
}

static lisp index_prefetch(fd_lisp indices,fd_lisp keys)
{
  DO_CHOICES(index,indices) {
    fd_index ix=lisp2index(index);
    if (ix) fd_index_prefetch(ix,keys);
    else fd_warn("Invalid index spec %q",index);}
  END_DO_CHOICES;
  return keys;
}

FRAMERD_EXPORT
lisp fd_search_get(lisp indices,lisp keys)
{
  if (CHOICEP(indices)) {
    lisp result=FD_EMPTY_CHOICE;
    DO_CHOICES(index,indices) {
      fd_index ix=lisp2index(index);
      DO_CHOICES(key,keys) {
	fd_lisp v=fd_index_get(ix,key,FD_EMPTY_CHOICE);
	ADD_TO_CHOICE(result,v);}
      END_DO_CHOICES;}
    END_DO_CHOICES;
    return result;}
  else if (CHOICEP(keys)) {
    fd_index ix=lisp2index(indices);
    lisp result=FD_EMPTY_CHOICE;
    DO_CHOICES(key,keys) {
      lisp v=fd_index_get(ix,key,FD_EMPTY_CHOICE);
      ADD_TO_CHOICE(result,v);}
    END_DO_CHOICES;
    return result;}
  else {
    fd_index ix=lisp2index(indices);
    return fd_index_get(ix,keys,FD_EMPTY_CHOICE);}
}

/** Stop features **/

/* Determines whether a slot should be ignored */
FASTOP int stop_slotp(lisp slotid)
{
  int stop=0;
  DO_CHOICES(dont_index,SYMBOL_VALUE(stop_slots_symbol))
    if (LISP_EQ(slotid,dont_index)) {stop=1; break;}
  END_DO_CHOICES;
  return stop;
}

/** Getting frame features for indexing **/

static lisp expander_get(lisp map,lisp slotid)
{
  if (PRIM_TYPEP(map,hashtable_type))
    return fd_hashtable_get
      ((fd_hashtable)CPTR_DATA(map),slotid,FD_EMPTY_CHOICE);
  else if (PRIM_TYPEP(map,index_type))
    return fd_index_get
      ((fd_index)CPTR_DATA(map),slotid,FD_EMPTY_CHOICE);
  else if (PAIRP(map))
    if (LISP_EQ(CAR(map),slotid)) return incref(CDR(map));
    else return FD_EMPTY_CHOICE;
  else return FD_EMPTY_CHOICE;
}

static lisp apply_expander(lisp exp,lisp value)
{
  if (FD_TRUEP(exp)) return incref(value);
  else if (SYMBOLP(exp)) return fd_prim_get(value,exp);
  else if (OIDP(exp)) return fd_frame_get(value,exp);
  else if (PRIM_TYPEP(exp,hashtable_type)) 
    return fd_hashtable_get
      ((fd_hashtable)CPTR_DATA(exp),value,FD_EMPTY_CHOICE);
  else if (PRIM_TYPEP(exp,index_type)) 
    return fd_index_get
      ((fd_index)CPTR_DATA(exp),value,FD_EMPTY_CHOICE);
  else if ((PRIM_TYPEP(exp,cproc_type)) ||
	   (PRIM_TYPEP(exp,sproc_type)) ||
	   (PRIM_TYPEP(exp,gproc_type)) ||
	   (PRIM_TYPEP(exp,ssproc_type)) ||
	   (PRIM_TYPEP(exp,rproc_type))) {
    lisp argl=FD_MAKE_LIST1(incref(value));
    lisp v=fd_apply(exp,argl);
    decref(argl);
    return v;}
  return FD_EMPTY_CHOICE;
}

static lisp get_default_slot_expander(lisp slot)
{
  if (OIDP(slot)) {
    lisp explicit=fd_prim_get(slot,expanders_symbol);
    if (!(FD_EMPTYP(explicit))) return explicit;
    else return (fd_symbol_value(default_expanders_symbol));
  }
  else return (fd_symbol_value(default_expanders_symbol));
}

/* Accumulates a set of feature pairs for a slot and set of values,
   expanding along associations */
static lisp make_slot_features(lisp slotid,lisp values)
{
  int expanded=0;
  lisp features=FD_EMPTY_CHOICE;
  lisp exp_maps=FD_SYMBOL_VALUE(expanders_symbol);
  {DO_CHOICES(exp_map,exp_maps) {
    lisp expanders=expander_get(exp_map,slotid);
    DO_CHOICES(expander,expanders) {
      DO_CHOICES(value,values) {
	lisp expansion=apply_expander(expander,value);
	DO_CHOICES(exp,expansion) {
	  expanded=1;
	  ADD_TO_CHOICE(features,FD_MAKE_PAIR(slotid,fd_incref(exp)));}
	END_DO_CHOICES;
      	decref(expansion);}
      END_DO_CHOICES;}
    fd_decref(expanders);
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  if (expanded == 0) {
    lisp expanders=get_default_slot_expander(slotid);
    {DO_CHOICES(expander,expanders) {
      DO_CHOICES(value,values) {
	lisp expansion=apply_expander(expander,value);
	{DO_CHOICES(exp,expansion) {
	  ADD_TO_CHOICE(features,FD_MAKE_PAIR(slotid,incref(exp)));}
	END_DO_CHOICES;}
	decref(expansion);}
      END_DO_CHOICES;}
    END_DO_CHOICES;
    fd_decref(expanders);}}
  return fd_return_proper_choice(features);
}

FRAMERD_EXPORT
/* fd_get_slot_value_features:
     Arguments: a frame (or set of frames) and a slot (or set of slots), and a set of values
     Returns: a set of features based on those slots and those values

   Gets the features of FRAME on SLOTS. */
fd_lisp fd_get_slot_value_features(fd_lisp slots,fd_lisp values)
{
  lisp features=FD_EMPTY_CHOICE;
  DO_CHOICES(slotid,slots) {
    lisp slot_features=make_slot_features(slotid,values);
    ADD_TO_CHOICE(features,slot_features);
    decref(values);}
  END_DO_CHOICES;
  return fd_return_proper_choice(features);
}

FRAMERD_EXPORT
/* fd_get_slot_features:
     Arguments: a frame (or set of frames) and a slot (or set of slots) 
     Returns: a set of features based on those slots of those frames

   Gets the features of FRAME on SLOTS. */
lisp fd_get_slot_features(lisp frames,lisp slots)
{
  lisp features=FD_EMPTY_CHOICE;
  DO_CHOICES(frame,frames) {
    DO_CHOICES(slotid,slots) {
      lisp values=fd_prim_get(frame,slotid);
      lisp slot_features=make_slot_features(slotid,values);
      ADD_TO_CHOICE(features,slot_features);
      decref(values);}
    END_DO_CHOICES;}
  END_DO_CHOICES;
  return fd_return_proper_choice(features);
}

FRAMERD_EXPORT
/* fd_get_features:
     Arguments: a frame (or set of frames)
     Returns: a set of features based on those on the slots of FRAMES

  Gets the features of FRAME based on all of its slots. */
lisp fd_get_frame_features(lisp frames)
{
  lisp results=FD_EMPTY_CHOICE;
  DO_CHOICES(frame,frames) {
    DO_SLOTS(slotid,d,frame) {
      if (!(stop_slotp(slotid))) {
	lisp features=make_slot_features(slotid,d);
	ADD_TO_CHOICE(results,features);}}}
  END_DO_CHOICES;
  return fd_return_proper_choice(results);
}
  
static lisp make_features(lisp slots,lisp values)
{
  lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(slotid,slots) {
    DO_CHOICES(value,values) {
      lisp feature=FD_MAKE_PAIR(incref(slotid),incref(value));
      ADD_TO_CHOICE(answer,feature);}
    END_DO_CHOICES;}
  END_DO_CHOICES;
  return answer;
}

/** Indexing frames **/

FRAMERD_EXPORT
/* fd_index_frame:
     Arguments: a frame and an index
     Returns: nothing
   Indexes an object based on its slot values.
*/
void fd_index_frame(fd_index idx,lisp frame)
{
  lisp keys=fd_get_frame_features(frame);
  DO_CHOICES(key,keys) {
    fd_index_add(idx,key,frame);}
  END_DO_CHOICES;
  decref(keys);
}

FRAMERD_EXPORT
/* fd_index_slots:
      Arguments: a frame (or set of frames), an index, and a set of slots
      Returns: nothing
  Indexes the frame (or set of frames) based on the specified slots
*/
void fd_index_slots(fd_index idx,lisp frame,lisp slots)
{
  DO_CHOICES(slotid,slots) {
    lisp keys=fd_get_slot_features(frame,slotid);
    DO_CHOICES(key,keys)
      fd_index_add(idx,key,frame);
    END_DO_CHOICES;
    decref(keys);}
  END_DO_CHOICES;
}

FRAMERD_EXPORT
/* fd_index_slot_values:
      Arguments: a frame (or set of frames), an index, 
                 a set of slots, and a set of values
      Returns: nothing
  Indexes the frame (or set of frames) based on the specified slots
*/
void fd_index_slot_values
  (fd_index idx,lisp frames,lisp slotids,lisp values)
{
  FD_DO_CHOICES(slotid,slotids) {
    FD_DO_CHOICES(value,values) {
      fd_lisp key=FD_MAKE_PAIR(slotid,fd_incref(value));
      fd_index_add(idx,key,frames);
      fd_decref(key);}
    FD_END_DO_CHOICES;}
  FD_END_DO_CHOICES;
}

FRAMERD_EXPORT
/* fd_index_notice_slot_values:
      Arguments: a frame (or set of frames), an index, 
                 a set of slots, and a set of values
      Returns: nothing
  Indexes the frame (or set of frames) based on the specified slots
  This doesn't force slots to be indexed, so stop-features works.
*/
void fd_index_notice_slot_values
  (fd_index idx,lisp frames,lisp slots,lisp values)
{
  DO_CHOICES(slotid,slots)
    if (!(stop_slotp(slotid))) {
      lisp keys=make_slot_features(slots,values);
      DO_CHOICES(key,keys)
	fd_index_add(idx,key,frames);
      END_DO_CHOICES;
      decref(keys);}
  END_DO_CHOICES;
}

/** CNF_Tables **/

static struct FD_CNF_TABLE *make_cnf_table()
{
  struct FD_CNF_TABLE *consed=fd_malloc(sizeof(struct FD_CNF_TABLE));
  consed->n_disjoins=0; consed->disjoins=NULL;
  return consed;
}

static void free_cnf_table(struct FD_CNF_TABLE *table)
{
  int i=0, limit=table->n_disjoins;
  while (i < limit) {
    fd_cnf_disjoin dj=table->disjoins[i];
    int j=0, size=dj->n_choices; lisp *choices=dj->choices;
    if (dj->lookup) {
      fd_free_hashset(dj->lookup);
      fd_free(dj->lookup,sizeof(struct FD_HASHSET));}
    while (j < size) {decref(choices[j]); j++;}
    fd_free(dj->choices,dj->max_choices*sizeof(lisp));
    fd_free(dj,sizeof(struct FD_CNF_DISJOIN));
    i++;}
  fd_free(table->disjoins,
	  table->n_disjoins*sizeof(struct FD_CNF_DISJOIN *));
  fd_free(table,sizeof(struct FD_CNF_TABLE));
}

static fd_cnf_disjoin make_disjoin(struct FD_CNF_TABLE *in_table)
{
  fd_cnf_disjoin consed=fd_malloc(sizeof(struct FD_CNF_DISJOIN));
  consed->n_choices=0; consed->max_choices=32;
  consed->total=0; consed->closed=0;
  consed->choices=fd_malloc(sizeof(lisp)*32);
  consed->lookup=NULL;
  if (in_table->disjoins) {
    in_table->disjoins=fd_realloc
      (in_table->disjoins,
       (in_table->n_disjoins+1)*sizeof(struct FD_CNF_DISJOIN *),
       in_table->n_disjoins*sizeof(struct FD_CNF_DISJOIN *));
    in_table->disjoins[in_table->n_disjoins]=consed;
    in_table->n_disjoins++;}
  else {
    in_table->disjoins=fd_malloc(sizeof(struct FD_CNF_DISJOIN *));
    in_table->n_disjoins=1; in_table->disjoins[0]=consed;}
  return consed;
}

static void extend_disjoin(fd_cnf_disjoin dj,lisp choices)
{
  if (dj->n_choices >= dj->max_choices) {
    dj->choices=
      fd_realloc(dj->choices,(dj->max_choices+32)*sizeof(lisp),
		 dj->max_choices*sizeof(lisp));
    dj->max_choices=dj->max_choices+32;}
  dj->choices[dj->n_choices]=incref(choices);
  dj->total=dj->total+CHOICE_SIZE(choices);
  dj->n_choices++;
}

static void close_disjoin(fd_cnf_disjoin dj)
{
  int i=0, limit=dj->n_choices, use_lookup=0;
  if (dj->closed) return; dj->closed=1;
  while (i < limit) 
    if (!(CHOICEP(dj->choices[i]))) i++;
    else if (fd_sort_choice(dj->choices[i])) i++;
    else {use_lookup=1; i++;}
  if ((use_lookup) && (dj->total > 5)) {
    fd_hashset h=fd_make_hashset(2*dj->total);
    int i=0; while (i < limit) {
      DO_CHOICES(each,dj->choices[i]) fd_hashset_add(h,each); END_DO_CHOICES;
      i++;}
    dj->lookup=h;}
}

static int disjoin_containsp(lisp x,fd_cnf_disjoin dj)
{
  if (dj->lookup) return fd_hashset_get(dj->lookup,x);
  else {
    int i=0, limit=dj->n_choices; lisp *choices=dj->choices;
    while (i < limit)
      if (fd_choice_containsp(x,choices[i])) return 1;
      else i++;
    return 0;}
}

static lisp get_disjoin_elts(fd_cnf_disjoin dj)
{
  if (dj->lookup) return fd_hashset_elts(dj->lookup);
  else {
    lisp start=FD_EMPTY_CHOICE;
    int i=0, limit=dj->n_choices;
    while (i < limit) {
      DO_CHOICES(each,dj->choices[i]) {
	ADD_TO_CHOICE(start,incref(each));}
      END_DO_CHOICES;
      i++;}
    return start;}
}      

static lisp cnf_search(struct FD_CNF_TABLE *table)
{
  int i=1, limit=table->n_disjoins;
  fd_cnf_disjoin *disjoins=table->disjoins;
  int smallest=0, smallest_size;
  if (limit == 0) return FD_EMPTY_CHOICE; /* Nothing to start with */
  else smallest_size=disjoins[0]->total;  
  /* Find the smallest disjoin */
  while (i < limit)
    if (disjoins[i]->total < smallest_size) {
      smallest=i; smallest_size=disjoins[i]->total; i++;}
    else i++;
  /* Note that this uses the ->total field, which may be
     incorrect if there are lots of duplicate elements among
     the choices.  But let's not worry about that. */

  /* If one of the disjoins has no elements, the intersection
     is trivially empty */
  if (smallest_size == 0) return FD_EMPTY_CHOICE;
  else if (limit == 1) /* No other conditions */
    return get_disjoin_elts(disjoins[smallest]);
  else { /* Otherwise, make a set out of that disjoin and
	    check for membership in all of the others. */
    lisp finish=FD_EMPTY_CHOICE;
    lisp start=get_disjoin_elts(disjoins[smallest]);
    DO_CHOICES(each,start) {
      int i=0, ok=1; while (i < limit)
	if (i == smallest) i++;
	else if (disjoin_containsp(each,disjoins[i])) i++;
	else {ok=0; break;}
      if (ok) {ADD_TO_CHOICE(finish,incref(each));}}
    END_DO_CHOICES;
    decref(start);
    return finish;}
}

/* Generating CNF tables */

static
/* make_cnf_from_va_list:
     Arguments: an index (or set/choice of indices) and 
                a stdarg va_list pointer
     Returns: a pointer to a CNF table
*/
struct FD_CNF_TABLE *
make_cnf_from_va_list(lisp indices,int strict,va_list args)
{
  struct FD_CNF_TABLE *table=make_cnf_table();
  while (1) {
    lisp slots=va_arg(args,lisp), values=va_arg(args,lisp);
    lisp features; fd_cnf_disjoin dj;
    if (FD_VOIDP(slots)) break;
    dj=make_disjoin(table);
    features=index_prefetch(indices,make_features(slots,values));
    {
      DO_CHOICES(feature,features) {
	DO_CHOICES(index,indices) {
	  fd_index ix=lisp2index(index);
	  lisp values=fd_index_get(ix,feature,FD_EMPTY_CHOICE);
	  if (!(FD_EMPTYP(values))) {
	    extend_disjoin(dj,values); decref(values);}}
	END_DO_CHOICES;}
      END_DO_CHOICES;
    }
    close_disjoin(dj); decref(features);
    if ((strict) && (dj->total == 0)) {
      free_cnf_table(table); return NULL;}}
  return table;
}

static
/* make_cnf_from_features:
     Arguments: an index (or set/choice of indices) and 
                a list of sets/choices of features
     Returns: a pointer to a CNF table
*/
struct FD_CNF_TABLE *make_cnf_from_spec(lisp indices,lisp spec)
{
  struct FD_CNF_TABLE *table=make_cnf_table();
  while (PAIRP(spec)) {
    fd_cnf_disjoin dj;
    lisp slots, values;
    if (PAIRP(CAR(spec))) {
      slots=CAR(CAR(spec)); values=CDR(CAR(spec));}
    else if (PAIRP(CDR(spec))) {
      slots=CAR(spec); values=CAR(CDR(spec));}
    else fd_raise_lisp_exception("Malformed search spec","",spec);
    dj=make_disjoin(table);
    {DO_CHOICES(slotid,slots) {
      lisp features=index_prefetch(indices,make_features(slotid,values));
      DO_CHOICES(feature,features) {
	DO_CHOICES(index,indices) {
	  fd_index ix=lisp2index(index);
	  lisp values=fd_index_get(ix,feature,FD_EMPTY_CHOICE);
	  if (!(FD_EMPTYP(values))) {
	    extend_disjoin(dj,values); decref(values);}}
	END_DO_CHOICES;}
      END_DO_CHOICES;
      decref(features);}
    END_DO_CHOICES;}
    if (PAIRP(CAR(spec))) spec=CDR(spec);
    else spec=CDR(CDR(spec)); 
    close_disjoin(dj);}
  return table;
}

static void extend_cnf
  (lisp indices,struct FD_CNF_TABLE *table,lisp slotid,lisp values)
{
  fd_cnf_disjoin dj=make_disjoin(table);
  lisp features=make_slot_features(slotid,values);
  DO_CHOICES(feature,features) {
    lisp candidates=fd_search_get(indices,feature);
    extend_disjoin(dj,candidates);
    decref(candidates);}
  END_DO_CHOICES;
  close_disjoin(dj);
  decref(features);
}

static
/* make_cnf_from_samples:
     Arguments: an index (or set/choice of indices) and 
                a set of frames and a set of slotids
     Returns: a pointer to a CNF table
*/
struct FD_CNF_TABLE *make_cnf_from_samples(lisp indices,lisp samples,lisp slots)
{
  struct FD_CNF_TABLE *table=make_cnf_table();
  DO_CHOICES(frame,samples)
    if (FD_FALSEP(slots)) {
      DO_SLOTS(slotid,value,frame) {
	extend_cnf(indices,table,slotid,value);}}
    else {
      DO_CHOICES(slotid,slots) {
	lisp values=fd_prim_get(frame,slotid);
	if (!(FD_EMPTYP(values))) extend_cnf(indices,table,slotid,values);
	decref(values);}
      END_DO_CHOICES;}
  END_DO_CHOICES;
  return table;
}

/** Strict searches **/

FRAMERD_EXPORT
/* fd_find_frames:
    Arguments: a set of indices and a series of slot and value sets 
               terminated by a FD_VOID
    Returns: a set of frames

  This is a C version of the FDSCRIPT strict searching function
  It returns those frames indexed as having each of the slots with
   at least of the subsequent values
*/
lisp fd_find_frames(lisp indices,...)
{
  struct FD_CNF_TABLE *table; lisp result;
  va_list args; va_start(args,indices);
  table=make_cnf_from_va_list(indices,1,args);
  if (table == NULL) return FD_EMPTY_CHOICE;
  result=cnf_search(table);
  free_cnf_table(table);
  return result;
}

FRAMERD_EXPORT
/* fd_find_similar:
    Arguments: an index, a set of frames, and a set of slots
    Returns: a set of frames

  Finds all frames in <index> that have some <slots> in common with <frames>
*/
lisp fd_find_similar(lisp indices,lisp frames,lisp slots)
{
  struct FD_CNF_TABLE *table=make_cnf_from_samples(indices,frames,slots);
  lisp result=cnf_search(table);
  free_cnf_table(table);
  return result;
}

FRAMERD_EXPORT
/* fd_strict_search:
     Arguments: an index (or set/choice of indices) and 
                a list of sets/choices of features
     Returns: a set of objects
  Returns all objects recorded in indices which have
one of each set of features in the provided list
*/
lisp fd_strict_search(lisp indices,lisp spec)
{
  struct FD_CNF_TABLE *table=make_cnf_from_spec(indices,spec);
  lisp result=cnf_search(table);
  free_cnf_table(table);
  return result;
}

/** Fuzzy searches **/

static int score_with_cnf(lisp frame,struct FD_CNF_TABLE *table)
{
  int i=0, limit=table->n_disjoins; int score=0;
  fd_cnf_disjoin *disjoins=table->disjoins;
  while (i < limit)
    if (disjoin_containsp(frame,disjoins[i])) {score++; i++;}
    else i++;
  return score;
}

static fd_hashtable score_frames(lisp frames,struct FD_CNF_TABLE *table)
{
  fd_hashtable h=fd_make_hashtable(CHOICE_SIZE(frames)*2);
  DO_CHOICES(frame,frames) {
    int score=score_with_cnf(frame,table);
    fd_hashtable_set(h,frame,LISPFIX(score));}
  END_DO_CHOICES;
  return h;
}

static fd_hashtable score_all_frames(struct FD_CNF_TABLE *table)
{
  fd_hashtable h=fd_make_hashtable(2048);
  int i=0, ilimit=table->n_disjoins;
  while (i < ilimit) {
    int j=0, jlimit=table->disjoins[i]->n_choices;
    lisp *choices=table->disjoins[i]->choices;
    while (j < jlimit) {
      DO_CHOICES(frame,choices[j]) {
	lisp current=fd_hashtable_get(h,frame,FD_VOID);
	if (FD_VOIDP(current)) {
	  int score=score_with_cnf(frame,table);
	  fd_hashtable_set(h,frame,LISPFIX(score));}
	else fd_decref(current);}
      END_DO_CHOICES;
      j++;}
    i++;}
  return h;
}

FRAMERD_EXPORT
/* fd_score_from_samples:
     Arguments: an index, a set of frames, a set of samples, and a set
                 of slotids
     Returns: a hashtable of frames and scores
 Computes similarity scores to <samples> based on slotids and index.
 If the set of frames argument is void, all candidates are scored;
  otherwise, only the specified frames are scored. */
fd_hashtable fd_score_from_samples
   (lisp indices,lisp frames,lisp samples,lisp slots) 
{
  struct FD_CNF_TABLE *table=make_cnf_from_samples(indices,samples,slots);
  fd_hashtable result;
  if ((FD_FALSEP(frames)) || (FD_EMPTYP(frames))) result=score_all_frames(table);
  else result=score_frames(frames,table);
  free_cnf_table(table);
  return result;
}

FRAMERD_EXPORT
/* fd_score_from_spec:
     Arguments: an index, a set of frames, and a list of feature sets
     Returns: a hashtable of frames and scores
 Computes similarity scores to <samples> based on the feature sets
 If the set of frames argument is void, all candidates are scored;
  otherwise, only the specified frames are scored. */
fd_hashtable fd_score_from_spec(lisp indices,lisp frames,lisp spec) 
{
  struct FD_CNF_TABLE *table=make_cnf_from_spec(indices,spec);
  fd_hashtable result;
  if (FD_FALSEP(frames)) result=score_all_frames(table);
  else result=score_frames(frames,table);
  free_cnf_table(table);
  return result;
}

/** Initialization procedures **/

void fd_initialize_search_c()
{
  expanders_symbol=fd_make_symbol("%EXPANDERS");
  default_expanders_symbol=fd_make_symbol("%DEFAULT-EXPANDERS");
  stop_slots_symbol=fd_make_symbol("%STOP-SLOTS");
  FD_SET_SYMBOL_VALUE(stop_slots_symbol,FD_EMPTY_CHOICE);
  FD_SET_SYMBOL_VALUE(expanders_symbol,FD_EMPTY_CHOICE);
  FD_SET_SYMBOL_VALUE(default_expanders_symbol,FD_TRUE);

  fd_register_source_file("search",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: search.c,v $
   Revision 1.19  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.18  2004/08/19 12:06:19  haase
   Update search code to favor sorted choices

   Revision 1.17  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.16  2003/11/21 17:48:19  haase
   Made indexing with explicit values not do automatic expansion

   Revision 1.15  2003/10/01 09:02:18  haase
   Added exception preface function

   Revision 1.14  2003/09/30 19:08:52  haase
   Wrapped locks around pool/index lookup/creation

   Revision 1.13  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.12.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.12.2.1  2003/01/26 20:43:16  haase
   Misc. fixes especially some GC

   Revision 1.12  2002/06/15 14:53:14  haase
   Remove interface to deleted search_max functionality

   Revision 1.11  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
