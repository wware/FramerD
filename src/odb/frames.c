/* C Mode */

/* frames.c
   Implements the FramerD frame system including slot operations and
      slot-based inference.
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

static char vcid[] = "$Id: frames.c,v 1.27 2005/01/14 16:48:48 haase Exp $";

/** Initial definitions **/
/** Automatic frame indexing **/
/** Maintaining the slot stack **/
/** Frame Construction: creation, annotation, and copying **/
/** Primitives: getting, putting, stripping **/
/** Slot caching **/
/** Computed slot access **/
/** RLL Operations **/
/** Evaluating demons **/
/** Useful inference support procedures **/
/** Iterating over slots and values **/
/** Describing slots and oids **/
/** Frame export and import to/from text **/
/** Special OBJ-NAME printing for frames **/
/** Initializing frame functions **/

#include "framerd.h"

/* Contents */
/* Initial definitions */
/* Automatic frame indexing */
/* Maintaining the slot stack */
/* Frame Construction: creation and copying */
/* Primitives: getting, putting, stripping */
/* Slot caching */
/* RLL Operations */
/* Evaluating demons */
/* Iterating over slots and values */
/* Describing slots and oids */
/* Frame export exprs */
/* Special OBJ-NAME printing for frames */
/* Initializing frame functions */


/** Initial definitions **/

fd_exception
  fd_Non_Atomic_SlotID=_("SlotID isn't an ATOM"),
  fd_Invalid_Frame=_("Frame is not an oid"),
  fd_Invalid_SlotID=_("SlotID is not an oid or symbol"),
  fd_NoSuchValue=_("Slot doesn't have the value to remove"),
  fd_FSVInvalid=_("slot value assignment is invalid"),
  InvalidSlotCache=_("Invalid slot cache object (internal error)");

int fd_ignore_homeless_frames=1;

static fd_exception Unpopped_slot_stack=_("Unpopped slot stack");

static lisp equals_symbol;
static lisp get_methods_symbol, compute_methods_symbol, test_methods_symbol,
            add_effects_symbol, remove_effects_symbol,
            validators_symbol;

/* Prototypes of note */
static int validate_value(lisp frame,lisp slotid,lisp value,int signal);
static lisp compute_slot_value(lisp methods,lisp frame,lisp slotid);
static int test_slot_value(lisp methods,lisp frame,lisp slotid,lisp value);
static void eval_effects
   (lisp frame,lisp slotid,lisp new,lisp e_symbol,slot_op op);

FRAMERD_EXPORT
lisp fd_eval_slot_method(lisp methods,lisp frame,lisp slot,lisp value);

static lisp autoindex_slots_symbol;
static fd_index autoindex=NULL;

FASTOP int containsp(lisp x,lisp set)
{
  if (FD_CHOICEP(set))
    return fd_choice_containsp(x,set);
  else return FD_LISP_EQUAL(x,set);
}

/* Adjunct store */

static struct FD_ADJUNCT_STORE {
  fd_lisp slotid; FD_OID base; unsigned int cap;
  fd_index store;} adjstores[128];
static int n_adjstores=0;
#if FD_THREADS_ENABLED
fd_mutex adjstore_lock;
#endif

static fd_index get_adjunct_store(fd_lisp oid,fd_lisp slotid)
{
  FD_OID addr=FD_OID_ADDR(oid);
  int i=0; while (i < n_adjstores)
    if ((FD_LISP_EQ(slotid,adjstores[i].slotid)) &&
	(FD_OID_IN_RANGE(addr,adjstores[i].base,adjstores[i].cap)))
      return adjstores[i].store;
    else i++;
  return NULL;
}

FRAMERD_EXPORT
void fd_register_adjunct_store
  (fd_lisp slotid,fd_index ix,FD_OID base,unsigned int cap)
{
  int i=0;
  fd_lock_mutex(&adjstore_lock);
  adjstores[n_adjstores].base=base; adjstores[n_adjstores].cap=cap;
  adjstores[n_adjstores].slotid=slotid;
  adjstores[n_adjstores].store=ix; n_adjstores++;
  fd_unlock_mutex(&adjstore_lock);
}


/** Automatic frame indexing **/

static void autoindex_slot_values
  (fd_index ix,lisp frame,lisp slotid,lisp value)
{
  lisp slots=SYMBOL_VALUE(autoindex_slots_symbol);
  if ((FD_EMPTYP(slots)) || (FD_VOIDP(slots)))
    fd_index_notice_slot_values(ix,frame,slotid,value);
  else if (FD_TRUEP(slots))
    fd_index_notice_slot_values(ix,frame,slotid,value);
  else if (containsp(slotid,slots))
    fd_index_notice_slot_values(ix,frame,slotid,value);
}

FASTOP void note_slot_values_for_index(lisp frame,lisp slotid,lisp value)
{
  if (autoindex != NULL)
    autoindex_slot_values(autoindex,frame,slotid,value);
}

FRAMERD_EXPORT
/* fd_use_auto_index:
     Arguments: a pointer to an index
     Returns: void

 Sets up a particular index for automatically recording
  changed and new slot values. */
void fd_use_autoindex(fd_index ix)
{
  autoindex=ix;
}

/** Maintaining the slot stack **/

#if FD_THREADS_ENABLED
static fd_tld_key slot_stack_key;
static void push_slot_stack(struct FD_SLOT_STACK *s)
{
  struct FD_SLOT_STACK *ss=fd_tld_get(slot_stack_key);
  s->next=ss;
  fd_tld_set(slot_stack_key,s);
}
static void pop_slot_stack(struct FD_SLOT_STACK *s)
{
  struct FD_SLOT_STACK *ss=fd_tld_get(slot_stack_key);
  if (ss) 
    if (ss == s) fd_tld_set(slot_stack_key,ss->next);
    else fd_raise_exception(Unpopped_slot_stack);
}
static struct FD_SLOT_STACK *get_slot_stack()
{
  return fd_tld_get(slot_stack_key);
}
#else
struct FD_SLOT_STACK *slot_stack=NULL;
static void push_slot_stack(struct FD_SLOT_STACK *s)
{
  s->next=slot_stack; slot_stack=s;
}
static void pop_slot_stack(struct FD_SLOT_STACK *s)
{
  if (s != slot_stack) fd_raise_exception(Unpopped_slot_stack);
  slot_stack=s->next;
}
static struct FD_SLOT_STACK *get_slot_stack()
{
  return slot_stack;
}
#endif

#define SETUP_SLOT_STACK(new,opn,u,s,v) \
  new.op=opn; new.frame=u; new.slotid=s; new.value=v; new.goal=FD_VOID; \
  push_slot_stack(&new)

/* already_doing_p
   fd_already_doing_p
   Arguments: a slot op, a frame, slotid, and value 
   Returns: 1 or 0
    Returns true (1) if the specified slot operation is already
     in progress.
*/
FASTOP int already_doing_p(slot_op op,lisp frame,lisp slotid,lisp value)
{
  struct FD_SLOT_STACK *ptr=get_slot_stack();
  while (ptr)
    if ((ptr->op == op) &&
	(LISP_EQ(ptr->frame,frame)) &&
	(LISP_EQ(ptr->slotid,slotid)) && 
	(LISP_EQ(ptr->value,value)))
      return 1;
    else ptr=ptr->next;
  return 0;
}
FRAMERD_EXPORT
/* fd_already_doing_p:
    Arguments: a slot operation, a frame, slot, and value
    Returns: 1 or 0
 Returns 1 if the frame system is already doing a slot operation.
(This causes most slot operations to return the empty choice). */
int fd_already_doing_p(slot_op op,lisp frame,lisp slotid,lisp value)
{ return already_doing_p(op,frame,slotid,value); }

/** Frame Construction: creation, annotation, and copying **/

/* Slotmaps are a data structure mapping slotids to values
   Frames are OIDs whose values are slotmaps. */

FRAMERD_EXPORT
/* fd_frame_create:
     Arguments: a pool
     Returns: a pointer to a frame (oid)

   This creates a new oid in the pool and initializes
    its value to be a slotmap, thus making it a frame.
*/
lisp fd_frame_create(fd_pool x)
{
  lisp frame=fd_new_oid(x); 
  lisp slotmap=fd_make_slotmap(2);
  fd_slotmap sm=FD_SLOTMAP_PTR(slotmap);
  sm->modified=1; fd_set_oid_value(frame,slotmap);
  fd_decref(slotmap);
  return frame;
}
  
/* get_slotmap:
   Arguments: a slotmap or 
   Returns: the slotmap
*/
static lisp get_slotmap(lisp oid)
{
  if (SLOTMAPP(oid)) return incref(oid);
  else if (OIDP(oid)) {
    lisp values=fd_oid_value(oid), result=FD_VOID;
    if (SLOTMAPP(values)) return values;
    else if (FD_EMPTYP(values))
      fd_ctype_error("get_slotmap",_("OID value is EMPTY"),oid);
    else {
      DO_CHOICES(v,values) 
	if (SLOTMAPP(v)) {result=incref(v);}
      END_DO_CHOICES;
      /* Done with that... */
      decref(values);
      if (FD_VOIDP(result)) 
	if (fd_get_pool(oid) == NULL)
	  if (fd_ignore_homeless_frames)
	    return FD_EMPTY_CHOICE;
	  else fd_raise_lisp_exception(fd_Homeless_OID,_("get_slotmap"),oid);
	else
	  fd_ctype_error("get_slotmap",_("OID is not a frame"),oid);
      else return result;}}
  else fd_ctype_error("get_slotmap",_("neither slotmap nor OID"),oid);
}
FRAMERD_EXPORT
/* fd_get_slotmap:
     Arguments: a lisp pointer
     Returns: a lisp pointer
  If arg is a slotmap, it is cref'd and returned;
  if arg is an OID whose value is a slotmap, that slotmap is returned
  if arg is an OID whose value is a choice, one of which is a slotmap,
     that slotmap is returned */
lisp fd_get_slotmap(lisp arg)
{
  return get_slotmap(arg);
}

FRAMERD_EXPORT
/* fd_copy_frame:
     Arguments: a frame and a pool
     Returns: a pointer to a new frame (oid)

   This creates a new object in the pool and initializes
    its value to be a slotmap copying another object's stotmap,
    thus making it a frame.
*/
lisp fd_copy_frame(lisp original,fd_pool x)
{
  lisp object=fd_new_oid(x);
  lisp smap=fd_oid_value(original);
  lisp smcopy=fd_copy_lisp(smap);
  fd_set_oid_value(object,smcopy);
  if (autoindex) {
    DO_SLOTS(s,v,object)
      note_slot_values_for_index(object,s,v);}
  fd_decref(smcopy); fd_decref(smap);
  return object;
}
  
/** Primitives: getting, putting, stripping **/

FRAMERD_EXPORT
/* fd_prim_get:
     Arguments: a frame and an attribute name (a lisp pointer)
     Returns: a lisp pointer
 Returns either the value associated with the attribute
 or FD_EMPTY_CHOICE if no such attribute exists; this value is
 computed from the attribute value if the slot is an object identifier.

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an object pointer
*/
lisp fd_prim_get(lisp frame,lisp slotid)
{
  if (!(ATOMICP(slotid)))
    fd_ctype_error("fd_prim_get",_("invalid slotid"),slotid);
  else if (SLOTMAPP(frame))
    return fd_slotmap_get(SLOTMAP_PTR(frame),slotid,FD_EMPTY_CHOICE);
  else if (!(OIDP(frame)))
    fd_ctype_error("fd_prim_get",_("neither slotmap nor OID"),frame);
  else {
    fd_index store=(((FD_OIDP(frame)) && (FD_OIDP(slotid))) ?
		    (get_adjunct_store(frame,slotid)) : (NULL));
    if (store) return fd_index_get(store,frame,FD_EMPTY_CHOICE);
    else {
      lisp smap=get_slotmap(frame), answer;
      if (FD_EMPTYP(smap)) return FD_EMPTY_CHOICE;
      answer=fd_slotmap_get(SLOTMAP_PTR(smap),slotid,FD_EMPTY_CHOICE);
      decref(smap);
      return answer;}}
}

FRAMERD_EXPORT
/* fd_prim_set:
     Arguments: a frame, a slotid (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   If the frame already possess the named attribute, its value is replaced
    with the new one; otherwise, the attribute is added to the frame
    with the corresponding value.

   Side effects:
     May replace the slot/value vector of a slotmap to add an attribute
     Frees the previous value of the attribute (if it exists)
     Refcounts or copies the value given it (sets are copied)

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an object
*/
void fd_prim_set(lisp frame,lisp slotid,lisp value)
{
  if (!(ATOMICP(slotid)))
    fd_ctype_error("fd_prim_set",_("invalid slotid"),slotid);
  else if (SLOTMAPP(frame))
    fd_slotmap_set(SLOTMAP_PTR(frame),slotid,value);
  else if (!(OIDP(frame)))
    fd_ctype_error("fd_prim_set",_("neither slotmap nor OID"),frame);
  else {
    fd_index store=(((FD_OIDP(frame)) && (FD_OIDP(slotid))) ?
		    (get_adjunct_store(frame,slotid)) : (NULL));
    if (store) return fd_index_set(store,frame,value);
    else {
      lisp smap=get_slotmap(frame);
      if (FD_EMPTYP(smap)) return;
      if (!(SLOTMAP_PTR(smap)->modified)) {
	fd_mark_modified(frame); decref(smap); smap=get_slotmap(frame);}
      if (FD_EMPTYP(smap)) return;
      fd_slotmap_set(SLOTMAP_PTR(smap),slotid,value);
      decref(smap);}}
}
FRAMERD_EXPORT
/* fd_prim_set_consed:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   Just like fd_prim_set, but frees the value it was passed (the
 frame still keeps its pointer.  This is a convenience function
 for passing consed values as arguments. */
void fd_prim_set_consed(lisp frame,lisp slotid,lisp value)
{
  fd_prim_set(frame,slotid,value);
  decref(value);
}


FRAMERD_EXPORT
/* fd_prim_add:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   If the frame already possess the named attribute, the specified
    value is added to the current one; otherwise, the attribute is
    added to the frame with the corresponding value.

   Side effects:
     May replace the slot/value vector of a slotmap to add an attribute
     Refcounts or copies the value given it (sets are copied)

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an object
*/
void fd_prim_add(lisp frame,lisp slotid,lisp value)
{
  if (!(ATOMICP(slotid)))
    fd_ctype_error("fd_prim_add",_("invalid slotid"),slotid);
  else if (SLOTMAPP(frame))
    fd_slotmap_add(SLOTMAP_PTR(frame),slotid,value);
  else if (!(OIDP(frame)))
    fd_ctype_error("fd_prim_add",_("neither slotmap nor OID"),frame);
  else {
    fd_index store=(((FD_OIDP(frame)) && (FD_OIDP(slotid))) ?
		    (get_adjunct_store(frame,slotid)) : (NULL));
    if (store) return fd_index_add(store,frame,value);
    else {
      lisp smap=get_slotmap(frame);
      if (FD_EMPTYP(smap)) return;
      if (!(SLOTMAP_PTR(smap)->modified)) {
	fd_mark_modified(frame); decref(smap); smap=get_slotmap(frame);}
      fd_slotmap_add(SLOTMAP_PTR(smap),slotid,value);
      decref(smap);}}
}
FRAMERD_EXPORT
/* fd_prim_add_consed:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   Just like fd_prim_add, but frees the value it was passed (the
 frame still keeps its pointer.  This is for passing consed values
 as arguments. */
void fd_prim_add_consed(lisp frame,lisp slotid,lisp value)
{
  fd_prim_add(frame,slotid,value);
  decref(value);
}

FRAMERD_EXPORT
/* fd_prim_drop:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   Removes the designated value from the designated attribute of a frame

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an object
*/
void fd_prim_drop(lisp frame,lisp slotid,lisp value)
{
  if (!(ATOMICP(slotid)))
    fd_ctype_error("fd_prim_drop",_("invalid slotid"),slotid);
  else if (SLOTMAPP(frame))
    fd_slotmap_remove(SLOTMAP_PTR(frame),slotid,value);
  else if (!(OIDP(frame)))
    fd_ctype_error("fd_prim_drop",_("neither slotmap nor OID"),frame);
  else {
    fd_index store=(((FD_OIDP(frame)) && (FD_OIDP(slotid))) ?
		    (get_adjunct_store(frame,slotid)) : (NULL));
    if (store) return fd_index_drop(store,frame,value);
    else {
      lisp smap=get_slotmap(frame);
      if (FD_EMPTYP(smap)) return;
      if (!(SLOTMAP_PTR(smap)->modified)){
	fd_mark_modified(frame); decref(smap); smap=get_slotmap(frame);}
      fd_slotmap_remove(SLOTMAP_PTR(smap),slotid,value);
      decref(smap);}}
}

FRAMERD_EXPORT
/* fd_prim_test:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: 1 or 0

   Returns 1 if the named attribute of the frame contains a value
*/
int fd_prim_test(lisp frame,lisp slotid,lisp value)
{
  if (!(ATOMICP(slotid)))
    fd_ctype_error("fd_prim_test",_("invalid slotid"),slotid);
  else if (SLOTMAPP(frame))
    return fd_slotmap_test(SLOTMAP_PTR(frame),slotid,value);
  else if (!(OIDP(frame)))
    fd_ctype_error("fd_prim_test",_("neither slotmap nor OID"),frame);
  else {
    fd_index store=(((FD_OIDP(frame)) && (FD_OIDP(slotid))) ?
		    (get_adjunct_store(frame,slotid)) : (NULL));
    if (store) {
      fd_lisp values=fd_index_get(store,frame,FD_EMPTY_CHOICE);
      int answer=fd_choice_containsp(value,values);
      fd_decref(values); return answer;}
    else {
      lisp smap=get_slotmap(frame), values; int answer;
      if (FD_EMPTYP(smap)) return 0;
      values=fd_slotmap_get(SLOTMAP_PTR(smap),slotid,FD_EMPTY_CHOICE);
      if (containsp(value,values)) answer=1; else answer=0;
      decref(values); decref(smap);
      return answer;}}
}

/** Slot caching **/

static struct FD_HASHTABLE slot_cache;

#if FD_THREADS_ENABLED
static fd_mutex slot_cache_lock;
#endif

FRAMERD_EXPORT
/* fd_enable_slot_cache:
      Arguments: slotid
      Returns: void
  Enables caching on the slot SLOTID */
void fd_enable_slot_cache(fd_lisp slotid,fd_lisp cache)
{
  fd_lisp current_cache;
  fd_lock_mutex(&slot_cache_lock);
  current_cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(current_cache)) {
    if (FD_PRIM_TYPEP(cache,index_type)) cache=fd_incref(cache);
    else if (FD_PRIM_TYPEP(cache,hashtable_type)) cache=fd_incref(cache);
    else cache=fd_make_hashtable_for_lisp(1024);
    fd_hashtable_set(&slot_cache,slotid,cache);
    fd_decref(cache);}
  fd_unlock_mutex(&slot_cache_lock);
}

FRAMERD_EXPORT
/* fd_disable_slot_cache:
      Arguments: slotid
      Returns: void
  Disables caching on the slot SLOTID */
void fd_disable_slot_cache(lisp slotid)
{
  lisp cache;
  lock_mutex(&slot_cache_lock);
  cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (!(FD_EMPTYP(cache)))
    fd_hashtable_set(&slot_cache,slotid,FD_EMPTY_CHOICE);
  fd_decref(cache);
  unlock_mutex(&slot_cache_lock);
}

FRAMERD_EXPORT
/* fd_reset_slot_cache:
      Arguments: slotid
      Returns: void
  Resets the slot cache. */
void fd_reset_slot_cache()
{
  lisp cache;
  lock_mutex(&slot_cache_lock);
  fd_free_hashtable(&slot_cache);
  fd_init_hashtable(&slot_cache,17);
  unlock_mutex(&slot_cache_lock);
}

/* clear_slot_cache:
      Arguments: slotid
      Returns: void
  Removes all the cached values for SLOTID */
static void clear_slot_cache(lisp slotid)
{
  lisp cache;
  lock_mutex(&slot_cache_lock);
  cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (FD_PRIM_TYPEP(cache,hashtable_type)) {
    fd_lisp ncache;
    fd_hashtable s=(fd_hashtable)CPTR_DATA(cache); int size=s->n_slots;
    fd_hashtable_set(&slot_cache,slotid,FD_EMPTY_CHOICE);
    ncache=fd_make_hashtable_for_lisp(size-1);
    fd_hashtable_set(&slot_cache,slotid,ncache);
    fd_decref(cache); fd_decref(ncache);}
  unlock_mutex(&slot_cache_lock);
}

/* clear_slot_cache_entry:
      Arguments: slotid, frame
      Returns: void
  Removes all cached values for SLOTID of FRAME */
static void clear_slot_cache_entry(lisp slotid,lisp frame)
{
  lisp cache;
  lock_mutex(&slot_cache_lock);
  cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (FD_PRIM_TYPEP(cache,hashtable_type))
    fd_hashtable_set((fd_hashtable)CPTR_DATA(cache),frame,FD_VOID);
  fd_decref(cache);
  unlock_mutex(&slot_cache_lock);
}

FRAMERD_EXPORT
/* fd_clear_slot_cache:
     Arguments: slotid, frame
     Returns: void
  Clears cached entries for slotid and frame, clearing
  all entries if frame is FD_VOID */
void fd_clear_slot_cache(fd_lisp slotid,fd_lisp frame)
{
  if (FD_VOIDP(frame)) clear_slot_cache(slotid);
  else clear_slot_cache_entry(slotid,frame);
}

FASTOP fd_lisp slot_cache_get(fd_lisp frame,fd_lisp slotid)
{
  fd_lisp cache, answer;
  lock_mutex(&slot_cache_lock);
  cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(cache)) answer=FD_VOID;
  else if (FD_PRIM_TYPEP(cache,index_type))
    answer=fd_index_get((fd_index)CPTR_DATA(cache),frame,FD_VOID);
  else if (FD_PRIM_TYPEP(cache,hashtable_type))
    answer=fd_hashtable_get((fd_hashtable)CPTR_DATA(cache),frame,FD_VOID);
  else {
    char buf[64]; sprintf(buf,"get:@%x/%x",FD_OID_ADDR_HIGH(slotid),FD_OID_ADDR_LOW(slotid));
    fd_decref(cache);
    fd_raise_lisp_exception(InvalidSlotCache,buf,cache);}
  fd_decref(cache);
  unlock_mutex(&slot_cache_lock);
  return answer;
}

static void slot_cache_add(fd_lisp frame,fd_lisp slotid,fd_lisp value)
{
  fd_lisp cache;
  lock_mutex(&slot_cache_lock);
  cache=fd_hashtable_get(&slot_cache,slotid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(cache)) {}
  else if (FD_PRIM_TYPEP(cache,index_type))
    fd_index_add((fd_index)CPTR_DATA(cache),frame,value);
  else if (FD_PRIM_TYPEP(cache,hashtable_type))
    fd_hashtable_add((fd_hashtable)CPTR_DATA(cache),frame,value);
  else {
    char buf[64]; sprintf(buf,"add:@%x/%x",FD_OID_ADDR_HIGH(slotid),FD_OID_ADDR_LOW(slotid));
    fd_decref(cache);
    fd_raise_lisp_exception(InvalidSlotCache,buf,cache);}
  fd_decref(cache);
  unlock_mutex(&slot_cache_lock);
}

/* Computed slot access */

FRAMERD_EXPORT
/* fd_frame_get:
     Arguments: a frame and an attribute name (a lisp pointer)
     Returns: either the value associated with the attribute
       or FD_EMPTY_CHOICE if no such attribute exists
       (The return value is a copy which may need to be freed).

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an oid pointer
*/
lisp fd_frame_get(lisp frame,lisp slotid)
{
  if (!(OIDP(slotid))) return fd_prim_get(frame,slotid);
  else if (already_doing_p(slot_get,frame,slotid,FD_VOID))
    return FD_EMPTY_CHOICE;
  else {
    lisp methods=fd_prim_get(slotid,get_methods_symbol);
    if (FD_EMPTYP(methods)) {
      fd_lisp values=fd_prim_get(frame,slotid);
      if (FD_EMPTYP(values)) {
	fd_lisp methods=fd_prim_get(slotid,compute_methods_symbol);
	if (FD_EMPTYP(methods)) return values;
	else {
	  lisp cached=slot_cache_get(frame,slotid);
	  if (!(FD_VOIDP(cached))) {
	    decref(methods); return cached;}
	  else {
	    fd_lisp computed;
	    validate_value(frame,slotid,FD_EMPTY_CHOICE,1);
	    computed=compute_slot_value(methods,frame,slotid);
	    slot_cache_add(frame,slotid,computed);
	    decref(methods);
	    return computed;}}}
      else return values;}
    else {
      lisp cached=slot_cache_get(frame,slotid);
      if (!(FD_VOIDP(cached))) {
	decref(methods); return cached;}
      else {
	lisp computed;
	validate_value(frame,slotid,FD_EMPTY_CHOICE,1);
	computed=compute_slot_value(methods,frame,slotid);
	slot_cache_add(frame,slotid,computed);
	decref(methods);
	return computed;}}}
}

FRAMERD_EXPORT
/* fd_frame_test:
     Arguments: a frame, a slotid, and a value
     Returns: 1 if the value is on the slot, 0 otherwise

   If the slot is an oid, this may use the TEST-METHODS and 
    GET-METHODS of the slot.  Otherwise, it just checks for membership
    in the corresponding value.

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an oid pointer
*/
int fd_frame_test(lisp frame,lisp slotid,lisp value)
{
  if (FD_CHOICEP(slotid)) {
    int result=0;
    FD_DO_CHOICES(each,slotid) {
      result=fd_frame_test(frame,each,value);
      if (result) break;}
    FD_END_DO_CHOICES;
    return result;}
  else if (!(OIDP(slotid)))
    return fd_prim_test(frame,slotid,value);
  else if (already_doing_p(slot_test,frame,slotid,value))
    return 0;
  else {
    lisp methods=fd_prim_get(slotid,test_methods_symbol);
    if (FD_EMPTYP(methods)) {
      fd_lisp values=fd_frame_get(frame,slotid);
      int result=((FD_CHOICEP(value)) ? (fd_choice_overlapsp(value,values)) :
		  (fd_choice_containsp(value,values)));
      fd_decref(values);
      return result;}
    else {
      int result=test_slot_value(methods,frame,slotid,value);
      fd_decref(methods);
      return result;}}
}

FRAMERD_EXPORT
/* fd_frame_add:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   If a frame already possesses the named attribute, the value is added to
     its set of values (if it's value isn't a set, it is made one).
   If the slot is itself an oid and the value stored is new (not currently
          in the attribute's set of values),
     then the add-effects demons of the slot are evaluated.

   Side effects:
     May replace the slot/value vector of a slotmap to add an attribute
     Refcounts or copies the value given it (sets are copied)

   Note: the attribute name is compared with ==, meaning that
    it can be a symbol, fixnum, ascii or unicode character, boolean,
       or an oid
*/
void fd_frame_add(lisp frame,lisp slotid,lisp value)
{
  if ((OIDP(slotid)) && (already_doing_p(slot_add,frame,slotid,value)))
    return;
  else if (!(OIDP(slotid)))
    if (fd_prim_test(frame,slotid,value)) {}
    else {
      note_slot_values_for_index(frame,slotid,value);
      fd_prim_add(frame,slotid,value);}
  else if (fd_frame_test(frame,slotid,value)) {}
  else {
    lisp methods=fd_prim_get(slotid,add_effects_symbol);
    validate_value(frame,slotid,value,1);
    if (FD_EMPTYP(methods)) {
      note_slot_values_for_index(frame,slotid,value);
      fd_prim_add(frame,slotid,value);}
    else {
      decref(methods); 
      eval_effects(frame,slotid,value,add_effects_symbol,slot_add);
      note_slot_values_for_index(frame,slotid,value);}}
}

FRAMERD_EXPORT
/* fd_frame_remove:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   If a frame already possesses the named attribute whose value contains
     the given value, it is removed.  If that would make the attribute value
     empty, it is replaced with FD_EMPTY_CHOICE; if that would make the attribute
     value a singleton, it is replaced with just that value.
   If the slot is itself an oid and the value removed is actually present,
     then the remove-effects demons of the slot are evaluated.

*/
void fd_frame_remove(lisp frame,lisp slotid,lisp value)
{
  if ((OIDP(slotid)) && (already_doing_p(slot_remove,frame,slotid,value)))
    return;
  else if (!(OIDP(slotid))) fd_prim_drop(frame,slotid,value);
  else if (fd_frame_test(frame,slotid,value)) {
    lisp methods=fd_prim_get(slotid,remove_effects_symbol);
    if (FD_EMPTYP(methods)) {
      fd_prim_drop(frame,slotid,value);}
    else {
      fd_decref(methods);
      eval_effects(frame,slotid,value,remove_effects_symbol,slot_remove);}}
  else {}
}

FRAMERD_EXPORT
/* fd_frame_set:
     Arguments: a frame, an attribute name (a lisp pointer),
       and a value (a lisp pointer)
     Returns: void

   Modifies the given attribute (slotid) of frame so that it has values,
    removing and adding values as neccessary.
*/
void fd_frame_set(lisp frame,lisp slotid,lisp value)
{
  lisp current=fd_frame_get(frame,slotid);
  {DO_CHOICES(v,current)
    if (!(containsp(v,value))) fd_frame_remove(frame,slotid,v);
   END_DO_CHOICES;}
  {DO_CHOICES(v,value)
     if (!(containsp(v,current))) fd_frame_add(frame,slotid,v);
   END_DO_CHOICES;}
  decref(current);
}

/* fd_frame_validate:
     Arguments: a frame, a slotid (a lisp pointer),
       and a value (a lisp pointer)
     Returns: 1 or 0

   Returns 1 if the assorted frame, slotid, and value are valid
    according to the slotid's semantics.
*/
int fd_frame_validate(lisp frame,lisp slotid,lisp value)
{
  if (SYMBOLP(slotid)) return 1;
  else if (!(OIDP(slotid))) return 0;
  else return (validate_value(frame,slotid,value,0));
}

/** Evaluating demons **/

static int validate_value(lisp frame,lisp slotid,lisp value,int signal)
{
  struct FD_SLOT_STACK new; int fail=0;
  if (already_doing_p(slot_validate,frame,slotid,value)) return 1;
  else {
    lisp methods=fd_prim_get(slotid,validators_symbol);
    if (FD_EMPTYP(methods)) return 1;
    else {
      UNWIND_PROTECT {
	SETUP_SLOT_STACK(new,slot_validate,frame,slotid,value);
	{DO_CHOICES(method,methods) {
	  lisp method_result=fd_eval_slot_method(method,frame,slotid,value);
	  if (FD_FALSEP(method_result))
	    if (signal) {
	      fd_u8char *details=fd_object_to_string(method);
	      decref(method_result); decref(methods);
	      fd_raise_lisp_exception(fd_FSVInvalid,details,
				      FD_MAKE_LIST(3,frame,slotid,value));}
	    else {fail=1; break;}
	  else {decref(method_result);}}
	END_DO_CHOICES;}}
      ON_UNWIND {decref(methods); pop_slot_stack(&new);}
      END_UNWIND;}
    if (fail) return 0; else return 1;}
}

static int test_slot_value(lisp methods,lisp frame,lisp slotid,lisp value)
{
  volatile int answer=0;
  struct FD_SLOT_STACK new;
  UNWIND_PROTECT {
    SETUP_SLOT_STACK(new,slot_test,frame,slotid,value);
    validate_value(frame,slotid,value,1);
    {DO_CHOICES(method,methods) {
      lisp method_result=fd_eval_slot_method(method,frame,slotid,value);
      if (!(FD_FALSEP(method_result))) {
	decref(method_result); answer=1; break;}}
     END_DO_CHOICES;}}
  ON_UNWIND {pop_slot_stack(&new);}
  END_UNWIND;
  return answer;
}

static int slotid_cachedp(fd_lisp x)
{
  fd_lisp v=fd_hashtable_get(&slot_cache,x,FD_VOID);
  if (FD_VOIDP(v)) return 0;
  else {fd_decref(v); return 1;}
}

static lisp compute_slot_value(lisp methods,lisp frame,lisp slotid)
{
  struct FD_SLOT_STACK new;
  lisp answer=FD_EMPTY_CHOICE;
  UNWIND_PROTECT {
    SETUP_SLOT_STACK(new,slot_get,frame,slotid,FD_VOID);
    if (slotid_cachedp(slotid))
      new.goal=FD_MAKE_PAIR(fd_incref(frame),slotid);
    {DO_CHOICES(method,methods) {
      lisp method_result=fd_eval_slot_method(method,frame,slotid,FD_VOID);
      ADD_TO_CHOICE(answer,method_result);}
     END_DO_CHOICES;}}
  ON_UNWIND {fd_decref(new.goal); pop_slot_stack(&new);}
  END_UNWIND;
  return answer;
}

static void eval_effects
   (lisp frame,lisp slotid,lisp new_value,lisp e_symbol,slot_op op)
{
  struct FD_SLOT_STACK new; volatile lisp methods=FD_EMPTY_CHOICE;
  if (already_doing_p(op,frame,slotid,new_value)) return;
  else {
    UNWIND_PROTECT {
      SETUP_SLOT_STACK(new,op,frame,slotid,new_value);
      methods=fd_prim_get(slotid,e_symbol);
      {DO_CHOICES(method,methods) {
	lisp effect_result=fd_eval_slot_method(method,frame,slotid,new_value);
	decref(effect_result);}
       END_DO_CHOICES;}}
   ON_UNWIND
     {pop_slot_stack(&new); decref(methods);}
   END_UNWIND}
}

/** GET* implementation */

static void get_star_helper(fd_hashset h,fd_lisp frames,fd_lisp slotids)
{
  DO_CHOICES(frame,frames)
    if (fd_hashset_get(h,frame)) {}
    else {
      fd_hashset_add(h,frame);
      {DO_CHOICES(slotid,slotids) {
	fd_lisp values=fd_frame_get(frame,slotid);
	get_star_helper(h,values,slotids);
	fd_decref(values);}
      END_DO_CHOICES;}}
  END_DO_CHOICES;
}

FRAMERD_EXPORT
/* fd_frame_get_star:
     Arguments: a choice of frames and a choice of slotids
     Returns: the kleene star of the slotids on the frames
*/
fd_lisp fd_frame_get_star(fd_lisp frames,fd_lisp slotids)
{
  fd_lisp values;
  struct FD_HASHSET hs; fd_init_hashset(&hs,512);
  get_star_helper(&hs,frames,slotids);
  values=fd_hashset_elts(&hs); fd_free_hashset(&hs);
  return values;
}

/** Useful inference support procedures **/


static void inherit_values_helper(lisp root,lisp slotid,lisp through,fd_hashset seen,lisp *deposit)
{
  if (fd_hashset_get(seen,root)) return;
  else {
    lisp values=fd_prim_get(root,slotid);
    ADD_TO_CHOICE(*deposit,values);
    fd_hashset_add(seen,root);}
  {lisp next=fd_frame_get(root,through);
   DO_CHOICES(each,next)
     inherit_values_helper(each,slotid,through,seen,deposit);
   END_DO_CHOICES;
   decref(next);}
}

FRAMERD_EXPORT
/* fd_inherit_values:
     Arguments: a frame and two slotids
     Returns: a lisp pointer
  Searches for a value for the first slotid through the lattice
  defined by the second slotid. */
lisp fd_inherit_values(lisp root,lisp slotid,lisp through)
{
  lisp results=FD_EMPTY_CHOICE; fd_hashset seen=fd_make_hashset(64);
  inherit_values_helper(root,slotid,through,seen,&results);
  fd_free_hashset(seen); fd_free(seen,sizeof(struct FD_HASHSET));
  return results;
}

static int inherits_valuep_helper
   (fd_hashset seen,lisp root,lisp slotid,lisp through,lisp value)
{
  if (fd_hashset_get(seen,root)) return 0;
  else {
    lisp values=fd_prim_get(root,slotid);
    fd_hashset_add(seen,root);
    if (containsp(values,value)) {decref(values); return 1;}
    else decref(values);
    {lisp next=fd_frame_get(root,through); int result=0;
     DO_CHOICES(each,next)
       if (inherits_valuep_helper(seen,each,slotid,through,value)) {result=1; break;}
     END_DO_CHOICES;
     decref(next);
     if (result) return result;}
    return 0;}
}

FRAMERD_EXPORT
/* fd_inherit_values:
     Arguments: a frame, two slotids, and a value
     Returns: 1 or 0
  Returns 1 if the value can be inherited for the first slotid
  going through the lattice defined by the second slotid. */
int fd_inherits_valuep(lisp root,lisp slotid,lisp through,lisp value)
{
  fd_hashset seen=fd_make_hashset(64);
  int answer=inherits_valuep_helper(seen,root,slotid,through,value);
  fd_free_hashset(seen); fd_free(seen,sizeof(struct FD_HASHSET));
  return answer;
}

static int pathp_helper(fd_hashset seen,lisp node,lisp slotid,lisp target)
{
  if (fd_hashset_get(seen,node)) return 0;
  else if (LISP_EQ(node,target)) return 1;
  else {
    lisp next=fd_prim_get(node,slotid); int result=0;
    fd_hashset_add(seen,node);
    {DO_CHOICES(n,next)
       if (pathp_helper(seen,n,slotid,target)) {result=1; break;}
    END_DO_CHOICES;}
    decref(next);
    return result;}
}

FRAMERD_EXPORT
/* fd_pathp:
     Arguments: a frame, a slotid, and a frame
     Returns: 1 or 0
  Returns 1 if there is a path through slotid between the two frames */
int fd_pathp(lisp root,lisp slotid,lisp to)
{
  fd_hashset seen=fd_make_hashset(64);
  int answer=pathp_helper(seen,root,slotid,to);
  fd_free_hashset(seen); fd_free(seen,sizeof(struct FD_HASHSET));
  return answer;
}

/** Iterating over slots and values **/

FRAMERD_EXPORT
/* fd_for_slots:
     Arguments: a function on three lisp objects and a lisp object
     Returns: void

   Applies the function to the frame and each of its attributes and values.

*/
void fd_for_slots(void (*fcn)(lisp frame,lisp slotid,lisp value),lisp frames)
{
  DO_CHOICES(frame,frames) {
    DO_SLOTS(slotid,value,frame)
      fcn(frame,slotid,value);
  END_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_frame_slots:
     Arguments: a frame
     Returns: a set of slot ids

   Returns the current slot ids with associations on the frame

*/
fd_lisp fd_frame_slots(lisp frames)
{
  fd_lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(frame,frames) {
    if (OIDP(frame)) {
      lisp values=fd_oid_value(frame);
      DO_CHOICES(v,values) {
	DO_SLOTS(slotid,value,v) {
	  ADD_TO_CHOICE(answer,incref(slotid));}}
      END_DO_CHOICES;
      fd_decref(values);}
    else {
      DO_SLOTS(slotid,value,frame) {
	ADD_TO_CHOICE(answer,incref(slotid));}}}
  END_DO_CHOICES;
  return answer;
}

/** Describing slots and oids **/

FRAMERD_EXPORT
/* fd_describe_slot:
    Arguments: an attribute name and a value
    Returns: void

   Describes a slot and its value to a stream.
*/
void fd_describe_slot(FILE *stream,lisp slotid,lisp value)
{
  fprintf(stream,"    ");
  fd_print_lisp(slotid,stream);
  if (CHOICEP(value))
    {fprintf(stream,_(": (%d values)"),CHOICE_SIZE(value));
     {DO_CHOICES(v,value) {
       fprintf(stream,"\n       ");
       fd_print_lisp(v,stream);}
      END_DO_CHOICES;
      fprintf(stream,"\n");}}
  else {fprintf(stream,":  "); fd_print_lisp(value,stream);
	fprintf(stream,"\n");}
}

FRAMERD_EXPORT
void fd_describe_frame(lisp frame,FILE *stream)
{
  fprintf(stream,
	  "---------------------------------------------------------------\n");
  fprintf(stream,_("The frame ")); fd_print_lisp(frame,stream);
  fprintf(stream,":\n");
  {DO_SLOTS(s,v,frame)
     if (OIDP(s)) {
       lisp realv=fd_frame_get(frame,s);
       fd_describe_slot(stream,s,realv); decref(realv);}
     else fd_describe_slot(stream,s,v);}
}

/** Frame export and import to/from text **/

static lisp set2list(lisp set)
{
  lisp elts=FD_EMPTY_LIST;
  DO_CHOICES(r,set) elts=FD_MAKE_PAIR(incref(r),elts); END_DO_CHOICES;
  return elts;
}

static lisp list2set(lisp lst)
{
  lisp set=FD_EMPTY_CHOICE;
  DOLIST(e,lst) {ADD_TO_CHOICE(set,incref(e));}
  return set;
}

FRAMERD_EXPORT
lisp fd_export_frame(lisp frame,lisp focus_slots,int just_focus)
{
  lisp slot_entries=FD_EMPTY_LIST;
  fd_pool p=fd_get_pool(frame);
  if (p == NULL) return frame;
  if (!(FRAMEP(frame))) {
    fd_lisp values=fd_oid_value(frame);
    fd_lisp result=FD_MAKE_PAIR
      (fd_incref(frame),FD_MAKE_PAIR(equals_symbol,set2list(values)));
    fd_decref(values);
    return result;}
  if (!(just_focus)) {
    DO_SLOTS(slotid,values,frame) {
      if (!(containsp(slotid,focus_slots))) {
	lisp slot_entry=FD_MAKE_PAIR(slotid,set2list(values));
	slot_entries=FD_MAKE_PAIR(slot_entry,slot_entries);}}}
  {DO_CHOICES(slotid,focus_slots) {
    lisp values=fd_frame_get(frame,slotid);
    lisp slot_entry=FD_MAKE_PAIR(slotid,set2list(values));
    decref(values);
    slot_entries=FD_MAKE_PAIR(slot_entry,slot_entries);}
   END_DO_CHOICES;}
  return FD_MAKE_PAIR(fd_incref(frame),slot_entries);
}

FRAMERD_EXPORT
void fd_import_frame(lisp frame,lisp slots,int noisy)
{
  int additions=0, removals=0; 
  if ((PAIRP(slots)) && (LISP_EQ(CAR(slots),equals_symbol))) {
    lisp v=list2set(CDR(CDR(slots)));
    fd_set_oid_value(frame,v);
    fd_decref(v);}
  else {
    DOLIST(spec,slots) {
      lisp slotid=fd_car_noref(spec);
      lisp new_values=list2set(fd_cdr_noref(spec));
      lisp current_values=fd_frame_get(frame,slotid);
      {DO_CHOICES(v,current_values)
	 if (!(containsp(v,new_values))) {
	   removals++; fd_frame_remove(frame,slotid,v);}
       END_DO_CHOICES;}
      {DO_CHOICES(v,new_values)
	 if (!(containsp(v,current_values))) {
	   additions++; fd_frame_add(frame,slotid,v);}
       END_DO_CHOICES;}
      decref(new_values); decref(current_values);}}
  if (noisy) {
    if ((additions) || (removals))
      fd_notify(_("[%t] %q: %d additions, %d removals"),
		frame,additions,removals);
    else fd_notify(_("[%t] %q: no changes"),frame);}
}

/** Initializing frame functions **/

void fd_initialize_frames_c()
{
  fd_init_hashtable(&slot_cache,5);

#if FD_THREADS_ENABLED
  fd_new_tld_key(&slot_stack_key,NULL);
#endif

  equals_symbol=fd_make_symbol("=");

  get_methods_symbol=fd_make_symbol("GET-METHODS"); 
  compute_methods_symbol=fd_make_symbol("COMPUTE-METHODS"); 
  test_methods_symbol=fd_make_symbol("TEST-METHODS"); 
  add_effects_symbol=fd_make_symbol("ADD-EFFECTS"); 
  remove_effects_symbol=fd_make_symbol("DROP-EFFECTS");
  validators_symbol=fd_make_symbol("CHECK-METHODS");

  autoindex_slots_symbol=fd_make_symbol("AUTOINDEX-SLOTS");
  FD_SET_SYMBOL_VALUE(autoindex_slots_symbol,FD_EMPTY_CHOICE);

}


/* File specific stuff */

/* The CVS log for this file
   $Log: frames.c,v $
   Revision 1.27  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.26  2005/01/11 14:48:26  haase
   Fix initialization of compute_methods_symbol

   Revision 1.25  2005/01/07 19:04:44  haase
   Made frame-get use compute-methods if there aren't any get-methods or any stored values

   Revision 1.24  2004/10/27 17:25:41  haase
   Add FDSCript access to adjuncts

   Revision 1.23  2004/09/17 08:29:57  haase
   Added complete slotcache resets

   Revision 1.22  2004/08/26 19:15:46  haase
   Removed overlays and added adjunct stores

   Revision 1.21  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.20  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.19  2004/07/14 14:14:18  haase
   Made pick take non-deterministic slotids

   Revision 1.18  2004/06/10 15:35:53  haase
   Made fd_frame_test handle choice arguments correctly

   Revision 1.17  2004/04/18 15:31:38  haase
   Made get_slotmap conditionally forgiving of homeless OIDs

   Revision 1.16  2004/02/17 18:36:31  haase
   Indices can now serve as slot caches, allowing persistence

   Revision 1.15  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.14.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.14.2.2  2003/08/02 14:04:10  haase
   Handled possible case of get_slotmap returning empty

   Revision 1.14.2.1  2003/01/26 20:44:39  haase
   Fixed slot stack handling and GC problems

   Revision 1.14  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.13  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.12  2002/04/13 01:33:35  haase
   Fixed bug in testing for remove methods

   Revision 1.11  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
