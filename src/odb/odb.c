/* C Mode */

/* odb.c

   Implements the FramerD object and frame system including
      object retrieval, reversion, and storing, slot-based
      inference, and object locking/unlocking.
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

static char vcid[] = "$Id: odb.c,v 1.29 2005/08/04 23:37:21 haase Exp $";

/** Initial definitions **/
/** Utility functions **/
/** Getting OID values **/
/** Locking and modifying OIDs **/
/** Storing values in OIDs **/
/** Individual OID operations **/
/** Committing and reverting pools **/
/** Swapping out **/
/** OID printing and parsing **/
/** Initialization and termination functions **/

/*

Objects are uniquely associated with 64-bit ids and this association
is maintained by procedures in "data.c".  Logically, objects have
"values" which are arbitrary LISP objects and are retrieved from files
and servers on demand.  

This file implements the object database which does the retrieval on
demand.

*/

/** Initial definitions **/

#define FD_INLINE_OIDS 1

#include "framerd.h"
#include "framerd/dtcodes.h"
#include <limits.h>

static int never_save;
int fd_loaded_oids=0, fd_oids_loaded=0, fd_new_oids=0;

static lisp obj_name_symbol;

fd_exception 
   fd_Read_Only_OID=_("Attempt to modify Read-Only OID"),
   fd_Cant_Lock_Remote_OID=_("Can't lock remote OID on server"),
   fd_Homeless_OID=_("Can't find pool for OID");

#if FD_THREADS_ENABLED
fd_mutex oid_counter_lock;
#endif

/** Utility functions **/

FRAMERD_EXPORT
/* fd_never_save:
     Arguments: none
     Returns: nothing

  Declares the current process to not be saving things.  Irreversible.
*/
void fd_never_save()
{
  never_save=1;
}

FRAMERD_EXPORT
/* fd_ephemeralp:
     Arguments: none
     Returns: 1 if the current process is saving its changes
*/
int fd_ephemeralp()
{
  return never_save;
}

/** Getting OID values **/

/* resolve_oid_value:
     Arguments: a pointer to a pool and a lisp pointer to an oid
     Returns: the value associated with the oid, fetched if neccessary.

   Side effects:
     (Usually) sets the object's value field and OBJECT_PRESENT_BIT
     (Sometimes) opens a pool in order to get the value out
*/
static lisp resolve_oid_value(fd_pool p,lisp x)
{
  if (p == NULL) return FD_VOID;
  else {
    lisp v;
    WITH_OID_LOCKED(x) {
      v=_fd_oid_current_value_nolock(x);
      if (!(FD_VOIDP(v))) {}
      else if (p->handler->lookup_oid) {
	lisp value=p->handler->lookup_oid(p,x);
	_fd_store_oid_value_nolock(x,value); v=value;
	lock_mutex(&oid_counter_lock);
	fd_loaded_oids++; fd_oids_loaded++;
	unlock_mutex(&oid_counter_lock);}
      else  fd_raise_exception(_("Unknown pool type"));}
    END_WITH_OID_LOCKED(x);
    return v;}
}
      
static void try_prefetch(fd_pool p,void *vps)
{
  struct FD_PREFETCH_STRUCT *ps=(struct FD_PREFETCH_STRUCT *)vps;
  if (p->handler->bulk_fetch)
    p->handler->bulk_fetch(p,ps->oids,ps->values,ps->n);
}

static int get_size(fd_lisp arg)
{
  if (FD_CHOICEP(arg)) return FD_CHOICE_SIZE(arg);
  else if (FD_VECTORP(arg)) return FD_VECTOR_LENGTH(arg);
  else fd_type_error(_("not vector or choice"),arg);
}

FRAMERD_EXPORT
void fd_prefetch_oids(lisp oids)
{
  if (FD_EMPTYP(oids)) return;
  else if (OIDP(oids)) {
    fd_lisp v=fd_oid_value(oids);
    fd_decref(v);}
  else if ((FD_VECTORP(oids)) || (FD_CHOICEP(oids))) {
    int i=0, size=get_size(oids), n=0, oid_loads=0;
    fd_lisp *oidv=fd_malloc(sizeof(fd_lisp)*size);
    fd_lisp *values=fd_malloc(sizeof(fd_lisp)*size);
    struct FD_PREFETCH_STRUCT ps;
    if (FD_CHOICEP(oids)) { 
      FD_DO_CHOICES(elt,oids)
	if ((OIDP(elt)) && (!(fd_oid_loadedp(elt)))) 
	  oidv[n++]=elt; 
	else {}
      END_FD_DO_CHOICES;}
    else if (FD_VECTORP(oids)) {
      int j=0; while (j < size) {
	fd_lisp elt=FD_VECTOR_REF(oids,j);
	if ((FD_OIDP(elt)) && (!(fd_oid_loadedp(elt)))) 
	  oidv[n++]=elt;
	j++;}}
    if (n == 0) {
      fd_free(oidv,sizeof(fd_lisp)*size);
      fd_free(values,sizeof(fd_lisp)*size);
      return;}
    ps.oids=oidv; ps.values=values; ps.n=n;
    i=0; while (i < n) values[i++]=(FD_VOID);
    fd_for_pools(try_prefetch,&ps);
    i=0; while (i < n)
      if (FD_VOIDP(values[i])) i++;
      else {
	fd_store_oid_value(oidv[i],values[i]);
	fd_decref(values[i]);
	oid_loads++; i++;}
    lock_mutex(&oid_counter_lock);
    fd_oids_loaded=fd_oids_loaded+oid_loads;
    fd_loaded_oids=fd_loaded_oids+oid_loads;
    unlock_mutex(&oid_counter_lock);
    fd_free(oidv,sizeof(fd_lisp)*size);
    fd_free(values,sizeof(fd_lisp)*size);}
  else fd_type_error(_("PREFETCH: an OID vector/choice"),oids);
}

FRAMERD_EXPORT
void fd_bulk_store_oid_values(fd_lisp *oidv,fd_lisp *values,int n)
{
  int i=0, oid_loads=0; while (i < n)
    if (FD_VOIDP(values[i])) i++;
    else {
      fd_store_oid_value(oidv[i],values[i]);
      fd_decref(values[i]);
      oid_loads++; i++;}
  lock_mutex(&oid_counter_lock);
  fd_oids_loaded=fd_oids_loaded+oid_loads;
  fd_loaded_oids=fd_loaded_oids+oid_loads;
  unlock_mutex(&oid_counter_lock);
}

FRAMERD_EXPORT
/* fd_get_oid_value:
     Arguments: a lisp pointer to an oid
     Returns: a lisp pointer
 Gets the value of an OID, doing fetching from files or network as
neccessary. */
lisp fd_get_oid_value(lisp oid)
{
  fd_pool p=FD_GET_POOL(oid);
  return resolve_oid_value(p,oid);
}

FRAMERD_EXPORT
/* fd_try_oid_value:
     Arguments: a lisp pointer to an oid
     Returns: a lisp pointer
 Gets the value of an OID, doing fetching from files or network as
neccessary.  If an error occurs, this will return FD_VOID rather
than signalling the error. */
lisp fd_try_oid_value(lisp oid)
{
  lisp value=FD_VOID;
  WITH_HANDLING {
    fd_pool p=FD_GET_POOL(oid);
    value=resolve_oid_value(p,oid);}
  ON_EXCEPTION
    {fd_clear_exception();}
  END_HANDLING;
  return value;
}

/** Locking and modifying OIDs **/

static void make_oid_writable(lisp obj,fd_pool p)
{
  if (p->read_only == FD_POOL_WRITABLE) return;
  else if (p->read_only == FD_POOL_LOCKABLE)
    if (p->handler->oid_locker)
      p->handler->oid_locker(p,obj,FD_POOL_LOCK_OID);
    else fd_raise_detailed_exception(fd_UnlockablePool,p->id);
  else fd_raise_detailed_exception(fd_ReadOnlyPool,p->id);
}

/* mark_modified:
     Arguments: a lisp pointer to an object
     Returns: none

   Errors:
     Cannot modify oid

   Side effects:
*/
FASTOP void mark_modified(lisp oid,fd_pool p)
{
  if (p == NULL) 
    fd_raise_lisp_exception(fd_Homeless_OID,"",oid);
  else if (fd_hashset_get(&(p->modified),oid)) return;
  else if (p->read_only) make_oid_writable(oid,p);
  fd_hashset_add(&(p->modified),oid);
  p->modifiedp=1;
}

FRAMERD_EXPORT
/* fd_mark_modified:
     Arguments: a lisp pointer to an oid
     Returns: void
 Marks the designated OID as modified and signals an error if it cannot
be modified. */
void fd_mark_modified(lisp oid) 
{
  WITH_OID_LOCKED(oid) {
    mark_modified(oid,FD_GET_POOL(oid));}
  END_WITH_OID_LOCKED(oid);
} 

FRAMERD_EXPORT
/* fd_get_modified:
     Arguments: a pointer to a pool, a result pointer to an int, and a flag
     Returns: a pointer to an array of lisp pointers
Returns an array of all the modified OIDs 
 Marks the designated OID as modified and signals an error if it cannot
be modified. */
fd_lisp *fd_get_modified(fd_pool p,int *n_oids,int reset)
{
  fd_hashset mods=&(p->modified); int size=0;
  fd_lisp *result, *write, *wlimit, *scan, *slimit; 
  fd_lock_mutex(&(mods->lock));
  scan=mods->table; slimit=scan+mods->n_slots; size=mods->n_keys;
  write=result=fd_malloc(sizeof(fd_lisp)*size); wlimit=write+size;
  while ((scan < slimit) && (write < wlimit)) {
    if (FD_OIDP(*scan)) *write++=*scan++;
    else scan++;}
  if (reset) fd_reinit_hashset(mods,reset,1);
  fd_unlock_mutex(&(mods->lock));
  if (write != wlimit)
    fd_warn("inconsistent modifications table for %s",p->id);
  *n_oids=size;
  return result;
} 

/** Storing values in OIDs **/

FRAMERD_EXPORT
/* fd_set_oid_value:
     Arguments: a lisp pointer to an oid and a lisp pointer to a value
     Returns: none

   This sets the value of an oid (its first argument) 
       to a new value (its second argument).
    fd_set_oid_value refcounts or copies the second argument (if it's a choice)

   Errors:
     Cannot modify oid

   Side effects:
     Frees the old value of the oid
     Changes the value field of the oid
     Adds the oid to its pools "modified objects"
*/
void fd_set_oid_value(lisp oid,lisp value)
{
  if (!(OIDP(oid)))
    fd_ctype_error("fd_set_oid_value",_("not an oid"),oid);
  else {
    fd_pool p=FD_GET_POOL(oid);
    WITH_OID_LOCKED(oid) {
      if ((p) && (p->read_only)) make_oid_writable(oid,p);
      if (CHOICEP(value)) {
	fd_lisp copy=copy_lisp(value); 
	_fd_store_oid_value_nolock(oid,copy); fd_decref(copy);}
      else {
	if (FD_SLOTMAPP(value)) {
	  struct FD_SLOTMAP *sm=FD_SLOTMAP_PTR(value); set_sticky(sm,1);}
	_fd_store_oid_value_nolock(oid,value);}
      mark_modified(oid,p);}
    END_WITH_OID_LOCKED(oid);}
}

/** Individual OID operations **/

FRAMERD_EXPORT
/* fd_new_oid:
     Arguments: a pointer to a pool structure
     Returns: a new oid in the pool

   Errors:
    Pool is used up

   Side effects:
    Increments the load of a file pool
     or
    Updates the load on a remote server
*/
lisp fd_new_oid(fd_pool p)
{
  lisp oid;
  if (p->handler->new_oid) oid=p->handler->new_oid(p);
  else return FD_VOID;
  lock_mutex(&oid_counter_lock);
  fd_new_oids++; fd_loaded_oids++; p->modifiedp=1;
  unlock_mutex(&oid_counter_lock);
  return oid;
}

FRAMERD_EXPORT
/* fd_revert_oid:
     Arguments: a lisp pointer to an oid
     Returns: 1 if successful, 0 otherwise

  Erases any uncommited changes to the designated oid. */
int fd_revert_oid(lisp oid)
{
  lisp old_value;
  fd_pool p=FD_GET_POOL(oid);
  int modified=fd_hashset_get(&(p->modified),oid);
  WITH_OID_LOCKED(oid) {
    _fd_store_oid_value_nolock(oid,FD_VOID);
    if (modified)
      p->handler->oid_locker(p,oid,FD_POOL_UNLOCK_OID);}
  END_WITH_OID_LOCKED(oid);
  fd_hashset_drop(&(p->modified),oid);
  return modified;
}

FRAMERD_EXPORT
/* fd_commit_oid:
     Arguments: a lisp pointer to an oid
     Returns: 1 if successful, 0 otherwise

  Commits any changes to an oid using pool specific methods, as appropriate. */
int fd_commit_oid(lisp oid)
{
  lisp old_value;
  fd_pool p=FD_GET_POOL(oid);
  if (p == NULL) 
    fd_raise_lisp_exception("Homeless OID","",oid);
  else if (p->handler->commit_oid == NULL)
    fd_raise_detailed_exception
      ("Pool does not support individual OID commitments",p->id);
  else {
    int modified=fd_hashset_get(&(p->modified),oid);
    if (modified) {
      WITH_OID_LOCKED(oid) {
	p->handler->commit_oid(p,oid);}
      END_WITH_OID_LOCKED(oid);
      fd_hashset_drop(&(p->modified),oid);
      return modified;}
    else return 0;}
}

/** Reverting pools **/

FRAMERD_EXPORT
/* fd_revert_pool:
     Arguments: a pointer to a pool
     Returns: void

  Erases any uncommited changes to the oids in the pool. */
void fd_revert_pool(fd_pool p)
{
  WITH_MUTEX_LOCKED(&(p->modified.lock)) {
    lisp *scan=p->modified.table, *limit=scan+p->modified.n_slots;
    while (scan < limit)
      if (!((FD_VOIDP(*scan)) || (FD_EMPTYP(*scan)))) {
	fd_store_oid_value(*scan,FD_VOID);
	scan++;}
      else scan++;}
  p->modifiedp=0;
  END_WITH_MUTEX_LOCKED(&(p->modified.lock));
  fd_free_hashset(&(p->modified));
}

FRAMERD_EXPORT
/* fd_close_pool:
     Arguments: a pointer to a pool
     Returns: void

  Closes open file pointers or network connections underlying
the pool. */
void fd_close_pool(fd_pool p)
{
  if (p->handler->close_pool) p->handler->close_pool(p);
}


/** Swapping out **/

FRAMERD_EXPORT
/* fd_modifiedp:
     Arguments: a lisp pointer to an oid
     Returns: 1 or 0

   Returns 1 if the oid has usaved modifications.
*/
int fd_oid_modifiedp(lisp oid)
{
  lisp v=fd_oid_current_value(oid);
  if (FD_VOIDP(v)) return 0;
  else if (SLOTMAPP(v)) {
    fd_slotmap smap=FD_SLOTMAP_PTR(v);
    int mod=smap->modified;
    fd_decref(v);
    return mod;}
  else {
    fd_pool p=FD_GET_POOL(oid); fd_decref(v);
    if (p)
      return (fd_hashset_get(&(p->modified),oid));
    else return 0;}
}

FRAMERD_EXPORT
/* fd_swap_out:
     Arguments: a lisp pointer to an oid
     Returns: none

   If this oid's value has not been modified, it is freed
    and the oid declared "non present", allowing it to be
    reloaded later if its value is needed.
*/
void fd_swap_out(lisp oid)
{
  fd_pool p=FD_GET_POOL(oid); int swapped_out=0;
  WITH_OID_LOCKED(oid) {
    fd_lisp v=_fd_oid_current_value_nolock(oid);
    /* Nothing to do if its not loaded. */
    if (FD_VOIDP(v)) {}
    /* Don't swap out if it's modified */
    else if (fd_hashset_get(&(p->modified),oid)) {
      swapped_out=0; fd_decref(v);}
    else if (FD_SLOTMAPP(v)) {
      if (SLOTMAP_PTR(v)->modified) /* check background assumption */
	fd_warn(_("Inconsistent modification info for %q in %s"),oid,p->id);
      else if (fd_refcount(v) == 2) {
	/* swap out if there aren't any other pointers except me and thee
	   (this procedure and the OID table) */
	swapped_out=1; set_sticky(SLOTMAP_PTR(v)->sticky,0); fd_decref(v);
	_fd_store_oid_value_nolock(oid,FD_VOID);}
      /* another strange case, where there are too few references */
      else if (fd_refcount(v) < 2) 
	fd_warn(_("Strange refcount for %q in %s"),oid,p->id);
      /* Final case, just leave as is, since there's another pointer somewhere,
	 but remember to decref the value. */
      else fd_decref(v);}
    /* Change but no need to GC */
    else if (FD_NPTR_TYPE(v) < FD_GC_LIMIT) {
      swapped_out=1; _fd_store_oid_value_nolock(oid,FD_VOID);}
    /* Change and GC the old value */
    else if (FD_PTR_DATA(v,acons)->n_refs == 2) {
      swapped_out=1; fd_decref(v);
      _fd_store_oid_value_nolock(oid,FD_VOID);}
    else if (fd_refcount(v) < 2)
      fd_warn(_("Strange refcount for %q in %s"),oid,p->id);
    /* Final case, just leave as is, since there's another pointer somewhere,
       but do remember to gc it. */
    else fd_decref(v);}
  END_WITH_OID_LOCKED(oid);
  if (swapped_out) {
    lock_mutex(&oid_counter_lock);
    fd_loaded_oids--;
    unlock_mutex(&oid_counter_lock);}
}

/* Swapping out en masse */

#if (FD_LIGHTWEIGHT_OIDS)
FRAMERD_EXPORT
/* fd_swap_out_pool:
    Arguments: a pointer to a pool
    Returns: void
  Frees any space being used for the values of unmodified OIDs
in pool.  It doesn't do anything about committing changes. */
void fd_swap_out_pool(fd_pool p)
{
  int i=0, keepers=0;
  while (i < FD_OID_BUCKETS) {
    WITH_MUTEX_LOCKED(&_fd_oid_buckets[i].lock) {
      struct FD_PAIR **scan, **limit;
      scan=_fd_oid_buckets[i].table; limit=scan+_fd_oid_buckets[i].n_slots;
      while (scan < limit)
	if (*scan) {
	  fd_pair e=*scan; fd_pool pl=FD_GET_POOL(e->car);
	  if (p != pl) {}
	  else if (FD_SLOTMAPP(e->cdr)) {
	    if ((FD_SLOTMAP_PTR(e->cdr))->modified) keepers++;
	    else if ((FD_SLOTMAP_PTR(e->cdr))->n_refs > 1) keepers++;
	    else {
	      set_sticky(FD_SLOTMAP_PTR(e->cdr)->sticky,0);
	      fd_decref(e->cdr); e->cdr=FD_VOID;
	      lock_mutex(&oid_counter_lock);
	      fd_loaded_oids--;
	      unlock_mutex(&oid_counter_lock);}}
	  else if (FD_VOIDP(e->cdr)) {}
	  else {
	    fd_pool p=FD_GET_POOL(e->car);
	    if (fd_hashset_get(&(p->modified),e->car)) keepers++;
	    else {
	      fd_decref(e->cdr); e->cdr=FD_VOID;
	      lock_mutex(&oid_counter_lock);
	      fd_loaded_oids--;
	      unlock_mutex(&oid_counter_lock);}}
	  scan++;}
	else scan++;}
    fd_cleanup_locked_hashtable(&_fd_oid_buckets[i]);
    END_WITH_MUTEX_LOCKED(&_fd_oid_buckets[i].lock);
    i++;}
}
    
FRAMERD_EXPORT
/* fd_swap_out_oids:
    Arguments: none
    Returns: void
  Frees any space being used for the values of unmodified OIDs.
  It doesn't do anything about committing changes. */
void fd_swap_out_oids()
{
  int i=0, keepers=0;
  while (i < FD_OID_BUCKETS) {
    WITH_MUTEX_LOCKED(&_fd_oid_buckets[i].lock) {
      struct FD_PAIR **scan, **limit;
      scan=_fd_oid_buckets[i].table; limit=scan+_fd_oid_buckets[i].n_slots;
      while (scan < limit)
	if (*scan) {
	  fd_pair e=*scan;
	  if (FD_SLOTMAPP(e->cdr)) {
	    if ((FD_SLOTMAP_PTR(e->cdr))->modified) keepers++;
	    else if ((FD_SLOTMAP_PTR(e->cdr))->n_refs < 1)
	      fd_warn("Dangling pointer in OID slotmap");
	    else if ((FD_SLOTMAP_PTR(e->cdr))->n_refs > 1) keepers++;
	    else {
	      set_sticky(FD_SLOTMAP_PTR(e->cdr)->sticky,0);
	      fd_decref(e->cdr); e->cdr=FD_VOID;
	      lock_mutex(&oid_counter_lock);
	      fd_loaded_oids--;
	      unlock_mutex(&oid_counter_lock);}}
	  /* Has already been swapped out */
	  else if (FD_VOIDP(e->cdr)) {}
	  else {
	    fd_pool p=FD_GET_POOL(e->car);
	    if (fd_hashset_get(&(p->modified),e->car)) keepers++;
	    else {
	      fd_decref(e->cdr); e->cdr=FD_VOID;
	      lock_mutex(&oid_counter_lock);
	      fd_loaded_oids--;
	      unlock_mutex(&oid_counter_lock);}}
	  scan++;}
	else scan++;}
    fd_cleanup_locked_hashtable(&_fd_oid_buckets[i]); 
    END_WITH_MUTEX_LOCKED(&_fd_oid_buckets[i].lock);
    i++;}
}
#else
FRAMERD_EXPORT
/* fd_swap_out_pool:
    Arguments: a pointer to a pool
    Returns: void
  Frees any space being used for the values of unmodified OIDs
in pool.  It doesn't do anything about committing changes. */
void fd_swap_out_pool(fd_pool p)
{
  fd_hashset oid_table=fd_oid_table();
  fd_notify(_("Swapping out pool %s"),p->id);
  {WITH_MUTEX_LOCKED(&(oid_table->lock)) {
    FD_OID base=p->base; unsigned int cap=p->capacity;
    lisp *scan=oid_table->table, *limit=scan+oid_table->n_slots;
    while (scan < limit)
      if ((OIDP(*scan)) &&
	  ((FD_COMPARE_OIDS((OID_ADDR(*scan)),base)) >= 0) &&
	  ((FD_OID_DIFFERENCE((OID_ADDR(*scan)),base)) < cap) &&
	  (fd_oid_loadedp(*scan))) {
	fd_swap_out(*scan); scan++;}
      else scan++;}
  END_WITH_MUTEX_LOCKED(&(oid_table->lock));}
}

static void swap_out_pool_proc(fd_pool p,void *iter_arg);

FRAMERD_EXPORT
/* fd_swap_out_oids: 
     Arguments: none
     Returns: none

   Side effects:
    Saves the values and reclaims space for all modified oids,
    also clearing the modified oids hashset.
*/
void fd_swap_out_oids()
{
  int message_output=0;
  fd_for_pools(swap_out_pool_proc,&message_output);
}
static void swap_out_pool_proc(fd_pool p,void *iter_arg)
{
  int *message_output=(int *) iter_arg;
  if (fd_oids_loaded) {
    if (*message_output == 0) {
      fd_notify(_("Swapping out all pools")); *message_output=1;}
    fd_swap_out_pool(p);}
}

#endif

/* Reverting pools */

/* This is called by fd_for_pools */
static void revert_pool_proc(fd_pool p,void *iter_arg);

FRAMERD_EXPORT
/* fd_revert_pools:  
     Arguments: none
     Returns: none

   Side effects:
    Reverts all pools to on-disk values.
*/
void fd_revert_pools()
{
  int message_output=0;
  fd_for_pools(revert_pool_proc,&message_output);
}
static void revert_pool_proc(fd_pool p,void *iter_arg)
{
  int *message_output=(int *) iter_arg;
  if (p->modifiedp) {
    if (*message_output == 0) {
      fd_notify(_("Reverting all pools")); *message_output=1;}
    fd_revert_pool(p);}
}

/** OID printing and parsing **/

static int show_poolids=0; /* This will make them "unreadable" a priori */
static int force_oid_names=0;
static int print_oid_names=1;

FRAMERD_EXPORT
/* fd_print_oid:
     Arguments: a lisp pointer (to an `oid') and a string stream
     Returns: nothing
    Outputs an external representation of the oid, consisting of
     its ID followed by its OBJ-NAME slot (if it exists).
*/
void fd_print_oid(lisp frame,fd_string_stream ss)
{
  FD_OID id=OID_ADDR(frame); fd_pool p;
  if ((show_poolids == 0) && (print_oid_names == 0)) {
    fd_printf(ss,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
    return;}
  else p=FD_GET_POOL(frame);
  if (p == NULL) {
    fd_printf(ss,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
    return;}
  else if ((p->prefix_id) && (show_poolids)) {
    int offset=FD_OID_LOW(id)-FD_OID_LOW(p->base);
    fd_printf(ss,"@/%s/%x",p->prefix_id,offset);}
  else fd_printf(ss,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
  if ((force_oid_names) || (oid_loadedp(frame))) {
    lisp oid_name_slot=fd_getenv("%OID-NAME");
    lisp name=FD_EMPTY_CHOICE;
    if ((FD_VOIDP(oid_name_slot)) || (FD_EMPTYP(oid_name_slot)))
      oid_name_slot=obj_name_symbol;
    name=fd_frame_get(frame,oid_name_slot);
    ss->fancy_oids=0;
    if (FD_EMPTYP(name)) {}
    else if (!((STRINGP(name)) || (PAIRP(name)) || (CHOICEP(name))))
      fd_printf(ss,"{%q}",name);
    else fd_print_lisp_to_string(name,ss);
    decref(name);
    ss->fancy_oids=1;}
}

FRAMERD_EXPORT
fd_lisp fd_parse_oid(fd_u8char *string_arg)
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
  else fd_raise_detailed_exception(_("Invalid OID format"),string_arg);
}

FRAMERD_EXPORT
/* fd_control_frame_printing
      Arguments: an integral print level
      Returns: void
  Controls the printing of oids
   If print level is 0, names are never printed
   If print level is 1, names are printed for loaded oids
   If print level is 2, names are always printed and
     oids are loaded when printed
*/
void fd_control_frame_printing(int level)
{
  if (level == 0) {
    print_oid_names=0; force_oid_names=0;}
  else if (level == 1) {
    print_oid_names=1; force_oid_names=0;}
  else if (level == 2) {
    print_oid_names=1; force_oid_names=1;}
  else fd_raise_exception(_("Oid printing level must be in [0,3)")); 
}

FRAMERD_EXPORT
/* fd_show_poolids
      Arguments: 1 or 0
      Returns: void
  Determines if pool ids are used to print OIDs.
*/
void fd_show_poolids(int use_ids)
{
  show_poolids=use_ids;
}

/** Initialization and termination functions **/

static int odb_initialized=0;

void fd_initialize_odb_c()
{
  if (odb_initialized) return;

#if FD_THREADS_ENABLED
  fd_init_mutex(&oid_counter_lock);
#endif

  obj_name_symbol=fd_make_symbol("%ID");

  fd_configure_oid_io(fd_print_oid,fd_parse_oid,NULL);

  fd_register_source_file("odb",__DATE__,vcid);

  odb_initialized=1;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: odb.c,v $
   Revision 1.29  2005/08/04 23:37:21  haase
   Changed obj-name to %id

   Revision 1.28  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.27  2004/09/28 23:38:46  haase
   Added non-symbol oid lookup

   Revision 1.26  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.25  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.24  2004/03/31 11:19:57  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.23  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.22  2003/10/05 06:44:40  haase
   Fixed OID printing to not print non-names as {{}}

   Revision 1.21  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.20.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.20.2.2  2003/08/02 13:58:14  haase
   Moved load counts out of bulk fetches

   Revision 1.20.2.1  2003/01/26 20:45:14  haase
   Misc. fixes and general cleanup

   Revision 1.20  2002/07/03 06:04:21  haase
   Added a C-level debugging feature for the GC where some slotmaps can be
   declared "sticky" meaning that an error is signalled when the are GCd.  This
   sticky bit is set whenever a slotmap is stored under an OID and cleared
   by the procedures for swapping out OIDs.

   Revision 1.19  2002/07/02 16:47:35  haase
   Indentation fix and brand new day

   Revision 1.18  2002/07/01 17:08:13  haase
   More swap out GC fixes

   Revision 1.17  2002/07/01 14:54:31  haase
   Check n_refs before swap out in per-pool swap outs

   Revision 1.16  2002/06/24 18:07:09  haase
   Made fd_parse_oid signal errors on malformed OIDs

   Revision 1.15  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.14  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.13  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.12  2002/04/20 19:47:48  haase
   Renamed fd_hashset_zap to fd_hashset_drop

   Revision 1.11  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
