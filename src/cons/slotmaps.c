/* Mode: C */

/* slotmaps.c
   This file implements slotmaps

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

static char vcid[] = "$Id: slotmaps.c,v 1.26 2005/01/14 16:48:44 haase Exp $";

#include "dtypes.h"

STATIC_INLINE check_slotmap_refcount(fd_slotmap s)
{
  if (s->n_refs <= 0) fd_raise_exception(fd_DanglerOp);
}

STATIC_INLINE int roundup_size(int j)
{
  if (j%FD_SLOTMAP_DELTA) return j+FD_SLOTMAP_DELTA;
  else return ((j/FD_SLOTMAP_DELTA)+2)*FD_SLOTMAP_DELTA;
}
static fd_lisp *copy_schema(fd_lisp *v,int size,int lim)
{
  fd_lisp *nv=fd_malloc(sizeof(fd_lisp)*lim);
  fd_lisp *write=nv, *read=v, *limit=read+size;
  while (read < limit) *write++=*read++;
  return nv;
}

/** Slotmaps **/

/* Slotmaps associate keys with values and are intended for relatively
   small sets of such associations. */

STATIC_INLINE int get_slotno(fd_slotmap sm,lisp key)
{
  fd_lisp *scan=sm->schema, *limit=scan+sm->size; 
  FD_CHECK_PTR(key);
  while (scan < limit)
    if (LISP_EQ(*scan,key)) return scan-(sm->schema);
    else scan++;
  return -1;
}

STATIC_INLINE int ensure_slotno(fd_slotmap sm,lisp key)
{
  fd_lisp *scan=sm->schema, *limit=scan+sm->size; FD_CHECK_PTR(key);
  while (scan < limit)
    if (LISP_EQ(*scan,key)) return scan-(sm->schema);
    else scan++;
  /* Make the schema private if neccessary */
  if (sm->shared_schema) {
    int limit=roundup_size(sm->size+1);
    sm->schema=copy_schema(sm->schema,sm->size,limit);
    sm->values=fd_realloc(sm->values,
			  sizeof(fd_lisp)*limit,
			  sizeof(fd_lisp)*sm->limit);
    sm->shared_schema=0; sm->limit=limit;}
  if (sm->limit == sm->size) {
    sm->schema=fd_realloc(sm->schema,
			  sizeof(fd_lisp)*(sm->limit+FD_SLOTMAP_DELTA),
			  sizeof(fd_lisp)*sm->limit);
    sm->values=fd_realloc(sm->values,
			  sizeof(fd_lisp)*(sm->limit+FD_SLOTMAP_DELTA),
			  sizeof(fd_lisp)*sm->limit);
    sm->limit=sm->limit+FD_SLOTMAP_DELTA;}
  sm->schema[sm->size]=fd_incref(key); sm->values[sm->size]=FD_EMPTY_CHOICE;
  return sm->size++;
}

STATIC_INLINE void remove_slotno(fd_slotmap sm,int slotno)
{
  if (sm->shared_schema) {
    int limit=roundup_size(sm->limit);
    sm->schema=copy_schema(sm->schema,sm->size,limit);
    sm->values=fd_realloc(sm->values,sizeof(fd_lisp)*limit,
			  sizeof(fd_lisp)*sm->limit);
    sm->shared_schema=0; sm->limit=limit;}
  /* decref(sm->values[slotno]); */
  if (slotno+1 < sm->size) {
    memmove(&(sm->schema[slotno]),&(sm->schema[slotno+1]),
	    sizeof(fd_lisp)*(sm->size-slotno-1));
    memmove(&(sm->values[slotno]),&(sm->values[slotno+1]),
	    sizeof(fd_lisp)*(sm->size-slotno-1));}
  sm->size--;
}


/* Basic operations */

DTYPES_EXPORT
/* fd_slotmap_get:
    Arguments: a slotmap, a lisp key, and a default value
    Returns: the value associated with the key in the slotmap
      or the default value otherwise. */
lisp fd_slotmap_get(fd_slotmap sm,lisp key,lisp dflt)
{
  int slotno;
  check_slotmap_refcount(sm);
  if (!(ATOMICP(key)))
    fd_type_error(_("fd_slotmap_get: non atomic key"),key);
  lock_mutex(&(sm->lock));
  slotno=get_slotno(sm,key);
  if (slotno < 0) {
    unlock_mutex(&(sm->lock)); return incref(dflt);}
  else {
    lisp v=sm->values[slotno];
    fd_incref(v);
    unlock_mutex(&(sm->lock));
    return v;}
}

DTYPES_EXPORT
/* fd_slotmap_test:
     Arguments: a slotmap, a lisp key, and a lisp value
     Returns: 1 or 0

 Returns 1 if the value can be found on the *key* slot of
the slotmap. */
int fd_slotmap_test(fd_slotmap sm,lisp key,lisp value)
{
  int slotno;
  check_slotmap_refcount(sm);
  if (!(ATOMICP(key)))
    fd_type_error(_("fd_slotmap_test: non atomic key"),key);
  lock_mutex(&(sm->lock));
  slotno=get_slotno(sm,key);
  if (slotno < 0) {unlock_mutex(&(sm->lock)); return 0;}
  else {
    int found=0;
    DO_CHOICES(v,value)
      if (fd_choice_containsp(v,sm->values[slotno])) {
	found=1; break;}
    END_DO_CHOICES;
    unlock_mutex(&(sm->lock));
    return found;}
}

DTYPES_EXPORT
/* fd_slotmap_set:
    Arguments: a slotmap, a lisp key, and a lisp value
    Returns: makes the value be associated with the key
     in the slotmap 
  Refcounts (if it's not a set) or copies (if it is) the value given it.
*/
void fd_slotmap_set(fd_slotmap sm,lisp key,lisp value)
{
  int slotno; lisp current;
  check_slotmap_refcount(sm); FD_CHECK_PTR(value);
  if (!(ATOMICP(key))) fd_type_error(_("fd_slotmap_set: non atomic key"),key);
  lock_mutex(&(sm->lock));
  slotno=ensure_slotno(sm,key);
  current=sm->values[slotno];
  if (CHOICEP(value)) value=copy_lisp(value); else incref(value);
  sm->modified=1;
  if (FD_EMPTYP(value)) {
    remove_slotno(sm,slotno);
    decref(current); decref(key);}
  else {decref(current); sm->values[slotno]=value;}
  unlock_mutex(&(sm->lock));
}

DTYPES_EXPORT
/* fd_slotmap_add:
    Arguments: a slotmap, a lisp key, and a lisp value
    Returns: makes the value be associated with the key
     in the slotmap, making the value  non deterministic if
     neccessary. 
  Refcounts (if it's not a set) or copies (if it is) the value given it.
*/
void fd_slotmap_add(fd_slotmap sm,lisp key,lisp value)
{
  int slotno, combined_size;
  check_slotmap_refcount(sm); FD_CHECK_PTR(value);
  if (!(ATOMICP(key))) fd_type_error(_("fd_slotmap_add: non atomic key"),key);
  lock_mutex(&(sm->lock));
  slotno=ensure_slotno(sm,key);
  combined_size=CHOICE_SIZE(value)+CHOICE_SIZE(sm->values[slotno]);
  /* For small slots, we try to keep things unique, but for larger slots,
     we just use ADD_TO_CHOICE which can generate improper sets (e.g. bags) */
  if (combined_size <= 12) {
    fd_lisp current=sm->values[slotno];
    DO_CHOICES(add,value) {
      if (fd_choice_containsp(add,current)) {}
      else {FD_ADD_TO_CHOICE(current,incref(add)); sm->modified=1;}}
    END_DO_CHOICES;
    sm->values[slotno]=current;}
  else {
    if (CHOICEP(value)) value=copy_lisp(value); else incref(value);
    ADD_TO_CHOICE(sm->values[slotno],value); sm->modified=1;}
  unlock_mutex(&(sm->lock));
}

DTYPES_EXPORT
/* fd_slotmap_remove:
    Arguments: a slotmap, a lisp key, and a lisp value
    Returns: removes the value from the values associated with 
     a particular key in the slotmap
  The value on the slotmap is freed.
*/
void fd_slotmap_remove(fd_slotmap sm,lisp key,lisp value)
{
  int slotno;
  check_slotmap_refcount(sm); FD_CHECK_PTR(value);
  if (!(ATOMICP(key)))
    fd_type_error(_("fd_slotmap_remove: non atomic key"),key);
  lock_mutex(&(sm->lock));
  slotno=get_slotno(sm,key);
  if (slotno < 0) unlock_mutex(&(sm->lock));
  else {
    lisp current=sm->values[slotno];
    lisp removed=fd_remove_from_choice(value,current);
    decref(current); sm->modified=1; sm->values[slotno]=removed;
    if (FD_EMPTYP(removed)) {
      remove_slotno(sm,slotno); decref(key);}
    unlock_mutex(&(sm->lock));}
}

DTYPES_EXPORT
/* fd_slotmap_zap:
    Arguments: a pointer to a slotmap and a key
    Returns: nothing
  Removes all values associated with the key in the slotmap. */
void fd_slotmap_zap(fd_slotmap sm,lisp key)
{
  fd_slotmap_set(sm,key,FD_EMPTY_CHOICE);
}

/* Data structure operations */

DTYPES_EXPORT
/* fd_make_slotmap:
    Arguments: a size (int)
    Returns: a slotmap with capacity for <size> slots */
lisp fd_make_slotmap(int size)
{
  fd_slotmap sm=fd_malloca(struct FD_SLOTMAP);
  sm->schema=fd_malloc(size*sizeof(fd_lisp));
  sm->values=fd_malloc(size*sizeof(fd_lisp));
  sm->n_refs=1; sm->limit=size; sm->size=0; set_sticky(sm,0);
  sm->modified=0; sm->shared_schema=0;
#if FD_THREADS_ENABLED
  fd_init_mutex(&(sm->lock));
#endif
  {RETURN_LISP(slotmap_type,slotmap,sm);}
}

/* Initializes a slotmap as read as a DType. */
static lisp init_slotmap(int isize,void *vdata)
{
  int size=isize/2, i=0, j=0;
  lisp *data=(lisp *) vdata, *schema, *values;
  fd_slotmap sm=fd_malloca(struct FD_SLOTMAP);
  sm->size=size; sm->limit=size; sm->n_refs=1; set_sticky(sticky,0);
  sm->modified=0; sm->shared_schema=0;
  sm->schema=schema=fd_malloc(size*sizeof(fd_lisp));
  sm->values=values=fd_malloc(size*sizeof(fd_lisp));
#if FD_THREADS_ENABLED
  fd_init_mutex(&(sm->lock));
#endif
  while (i < isize) {
    schema[j]=data[i++]; values[j]=data[i++]; j++;}
  fd_free(vdata,sizeof(lisp)*isize);
  {RETURN_LISP(slotmap_type,slotmap,sm);}
}

/* Copies a slotmap, copying the entries in it. */
static lisp copy_slotmap(lisp x)
{
  fd_slotmap sm=fd_malloca(struct FD_SLOTMAP);
  fd_slotmap original=PTR_DATA(x,slotmap);
  lisp *read, *limit, *write; 
  lock_mutex(&(original->lock));
  sm->size=original->size; sm->limit=original->limit; set_sticky(sm,0);
  sm->modified=0; sm->shared_schema=original->shared_schema;
#if FD_THREADS_ENABLED
  fd_init_mutex(&(sm->lock));
#endif
  /* Copy the schema */
  if (original->shared_schema)
    sm->schema=original->schema;
  else sm->schema=copy_schema(original->schema,sm->size,sm->limit);
  /* Copy the values */
  read=original->values; limit=read+sm->size;
  sm->values=write=fd_malloc(sizeof(fd_lisp)*sm->limit);
  while (read < limit) {
    lisp v=*read++;
    if (CHOICEP(v)) *write++=copy_lisp(v);
    else *write++=incref(v);}
  unlock_mutex(&(original->lock));
  {RETURN_LISP(slotmap_type,slotmap,sm);}
}

DTYPES_EXPORT
/* fd_copy_slotmap:
     Arguments: a slotmap
     Returns: a slotmap
     Copies a slotmap, deep copying any choices. */
fd_lisp fd_copy_slotmap(fd_lisp x)
{
  return copy_slotmap(x);
}

/* Freeing a slotmap, including its values. */
static void free_slotmap(lisp x)
{
  fd_slotmap original=SLOTMAP_PTR(x);
  lisp *scan, *limit;
  if (test_sticky(original))
    fd_raise_exception("Freeing sticky slotmap");
  lock_mutex(&(original->lock));
  scan=original->values; limit=scan+original->size;
  while (scan < limit) {decref(*scan); scan++;}
  unlock_mutex(&(original->lock));
#if FD_THREADS_ENABLED
  pthread_mutex_destroy(&(original->lock));
#endif
  if (original->shared_schema == 0)  
    fd_free(original->schema,(original->limit*sizeof(lisp)));
  fd_free(original->values,(original->limit*sizeof(lisp)));
  fd_qfree(original,sizeof(struct FD_SLOTMAP)); 
}

static void print_slotmap(lisp x,fd_string_stream s)
{
  lisp *data, *scan, *limit; int size;
  size=_fd_slotmap_data(x,(void **)&data); scan=data; limit=scan+size;
  fd_sputs(s,"#[");
  while (scan < limit) {
    fd_print_lisp_to_string(*scan++,s);
    if (scan != limit) fd_sputs(s," ");}
  fd_sputs(s,"]");
  _fd_done_with_slotmap_data(data,size);
}

static unsigned int compare_slotmaps(lisp s1,lisp s2)
{
  if ((FD_SLOTMAPP(s1)) && (FD_SLOTMAPP(s2))) {
    fd_slotmap sm1=PTR_DATA(s1,slotmap);
    fd_slotmap sm2=PTR_DATA(s2,slotmap);
    /*
     * Need to get the locks on both slotmaps but another thread could
     * deadlock if comparing in the other order so we always lock
     * in a fixed order.
     */
    if (sm1 < sm2) {
      lock_mutex(&(sm1->lock));
      lock_mutex(&(sm2->lock));}
    else {
      lock_mutex(&(sm2->lock));
      lock_mutex(&(sm1->lock));}
    if (sm1->size != sm2->size) {
      unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
      return 0;}
    else if (sm1->schema == sm2->schema) {
      fd_lisp *v1_scan=sm1->values, *v1_limit=v1_scan+sm1->size;
      fd_lisp *v2_scan=sm2->values;
      while (v1_scan < v1_limit)
	if (LISP_EQUAL(*v1_scan,*v2_scan)) {v1_scan++; v2_scan++;}
	else {
	  unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
	  return 0;}
      unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
      return 1;}
    else {
      int i=0, max_i=sm1->size;
      fd_lisp *schema1=sm1->schema, *v1=sm1->values, *v2=sm2->values;
      while (i < max_i) {
	lisp key=schema1[i];
	int slotno=get_slotno(sm2,key);
	if (slotno < 0) {
	  unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
	  return 0;}
	else if (LISP_EQUAL(v1[i],v2[slotno])) i++;
	else {
	  unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
	  return 0;}}
      unlock_mutex(&(sm1->lock)); unlock_mutex(&(sm2->lock));
      return 1;}}
  else return 0;
}

DTYPES_EXPORT
/* _fd_slotmap_data:
    Arguments: a lisp pointer (to a slotmap) and
                a pointer to a pointer to a vector of lisp pointers
    Returns: an int (the number of lisp pointers in the vector assigned

  Returns the data of the slotmap to use in producing a packaged
   DTYPE from it.
*/
int _fd_slotmap_data(lisp x,void **velts)
{
  if (FD_EMPTYP(x)) {
    *velts=NULL; return 0;}
  else {
    fd_slotmap sm=PTR_DATA(x,slotmap);
    fd_lisp *slotmap_vec, *write, *sread, *slimit, *vread, **elts;
    lock_mutex(&(sm->lock)); 
    slotmap_vec=fd_malloc(sizeof(fd_lisp)*sm->size*2);
    sread=sm->schema; slimit=sread+sm->size;
    write=slotmap_vec; vread=sm->values;
    while (sread < slimit) {
      fd_lisp slotid=*sread++;
      fd_lisp value=*vread++;
      FD_CHECK_LISP(slotid); FD_CHECK_LISP(value);
      *write++=slotid; *write++=incref(value);}
    elts=(lisp **) velts; *elts=slotmap_vec;
    unlock_mutex(&(sm->lock));
    return sm->size*2;}
}

DTYPES_EXPORT
/* _fd_done_with_slotmap_data:
      Arguments: a pointer to a vector of LISP pointers and a size
      Returns: nothing
  Frees a slotmap data vector. */
void _fd_done_with_slotmap_data(fd_lisp *elts,int size)
{
  int i=1;
  while (i < size) {fd_decref(elts[i]); i=i+2;}
  fd_free(elts,sizeof(lisp)*size);
}

static void done_with_slotmap_data(fd_lisp x,int size,void **d)
{
  _fd_done_with_slotmap_data((fd_lisp *)*d,size);
}

void fd_initialize_slotmaps_c()
{
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(slotmap_type);
    r->package_code=dt_framerd; r->subcode=dt_small_slotmap;
    r->print_fcn=print_slotmap; r->gc_fcn=free_slotmap;
    r->package_data_fcn=_fd_slotmap_data; r->copy_fcn=copy_slotmap;
    r->package_data_done_fcn=done_with_slotmap_data;
    r->compare_fcn=compare_slotmaps;
    r->package_restore_fcn=init_slotmap;}
}


/* File specific stuff */

/* The CVS log for this file
   $Log: slotmaps.c,v $
   Revision 1.26  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.25  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.24  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.23  2004/03/31 11:19:56  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.22  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.21  2004/03/30 19:16:26  haase
   Fixes to schema implementation

   Revision 1.20  2004/03/30 08:10:11  haase
   Support for using schemas

   Revision 1.19  2004/03/29 15:03:09  haase
   added fd_copy_slotmap

   Revision 1.18  2003/10/20 12:08:16  haase
   Added pointer checking

   Revision 1.17  2003/10/20 09:26:26  haase
   Added pointer checking to slotmap operations

   Revision 1.16  2003/10/05 06:39:48  haase
   Added primitive for getting interleaved slotmap data, since _fd_slotmap_data may return a vector for an OID-encoded schema

   Revision 1.15  2003/09/10 17:58:29  haase
   Made slotmaps grow linearly in size

   Revision 1.14  2003/08/31 16:57:16  haase
   Added first pass of shared schema support for slotmaps

   Revision 1.13  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.12.2.4  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.12.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.12.2.2  2003/08/02 13:41:08  haase
   Made _init_slotmap_data handle failure empty choice by providing empty slotmap data

   Revision 1.12.2.1  2003/01/26 20:35:34  haase
   Fixed atomicity/non-gc assumptions for slotmaps

   Revision 1.12  2002/07/03 06:04:21  haase
   Added a C-level debugging feature for the GC where some slotmaps can be
   declared "sticky" meaning that an error is signalled when the are GCd.  This
   sticky bit is set whenever a slotmap is stored under an OID and cleared
   by the procedures for swapping out OIDs.

   Revision 1.11  2002/07/02 16:45:49  haase
   Reordered locks and increfs to avoid thread race conditions

   Revision 1.10  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.9  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
