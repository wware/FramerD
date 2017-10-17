/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: oids.h,v 1.14 2005/12/05 04:09:39 haase Exp $

  This file is part of FramerD, a representation language and semantic
  database developed by Kenneth B. Haase and his students at the Media
  Laboratory at the Massachusetts Institute of Technology in Cambridge,
  Massachusetts.  Research at the Media Lab is supported by funds and
  equipment from a variety of corporations and government sponsors whose
  contributions are gratefully acknowledged.

    Use, modification, and redistribution of this program is permitted
    under the terms of either (at the developer's discretion) the GNU
    General Public License (GPL) Version 2, the GNU Lesser General Public
    License.

    This program is based on the FramerD library released in Fall 2001 by
    MIT under both the GPL and the LGPL licenses, both of which accompany
    this distribution.  Subsequent modifications by beingmeta, inc. are
    also released under both the GPL and LGPL licenses (at the developer's
    discretion).

*************************************************************************/

#ifndef FRAMERD_OIDS_H /* If defined, skip the file */
#define FRAMERD_OIDS_H

#include "framerd/common.h"

/* Are we optimizing these operations heavily? */

#ifndef FD_INLINE_OIDS
#define FD_INLINE_OIDS 0
#endif

/* Declare OID Types */

/* If longs have 8 bytes, we can use them to represent OIDs */
#ifndef FD_STRUCT_OIDS
#if (SIZEOF_LONG == 8)
#define FD_OIDS_ARE_SCALARS 1
typedef unsigned long FD_OID;
#elif (SIZEOF_LONG_LONG == 8)
#define FD_OIDS_ARE_SCALARS 1
typedef unsigned long long FD_OID;
#else
#define FD_OIDS_ARE_SCALARS 0
typedef struct FD_OID {unsigned int high, low;} FD_OID;
#endif
#else
#define FD_OIDS_ARE_SCALARS 0
typedef struct FD_OID {unsigned int high, low;} FD_OID;
#endif

DTYPES_EXPORT FD_OID fd_oid_addr(fd_lisp x);
DTYPES_EXPORT fd_lisp fd_make_oid(FD_OID id);

DTYPES_EXPORT fd_exception fd_PoolOverlap;

#define FD_OIDP(x) (FD_PRIM_TYPEP(x,object_type))

/* Operations on OIDS (the 64-bit addresses) */

#define fd_compare_ints(x,y) (((x) == (y)) ? 0 : (((x) > (y)) ? 1 : -1))

#if FD_OIDS_ARE_SCALARS
#define FD_OID_EQUAL(x,y) ((x)==(y))
#define FD_COMPARE_OIDS(x,y) (fd_compare_ints(x,y))
#define FD_OID_DIFFERENCE(x,y) (x-y)
#define FD_OID_PLUS(x,y) (x+y)
#define FD_OID_HIGH(x) ((unsigned int)((x)>>32))
#define FD_OID_LOW(x) ((unsigned int)((x)&0x00000000FFFFFFFF))
#define FD_SET_OID_HIGH(x,v) x=((x&(0x00000000FFFFFFFFLL)) | (((FD_OID)v)<<32))
#define FD_SET_OID_LOW(x,v)  x=((x&(0xFFFFFFFF00000000LL)) | ((FD_OID)v))
#define FD_OID_IN_RANGE(x,base,cap) ((x>base) && ((x-base)< cap))
#else
#define FD_OID_EQUAL(x,y) (((x.high)==(y.high)) && ((x.low)==(y.low)))
#define FD_COMPARE_OIDS(x,y) \
     ((x.high == y.high) ? (fd_compare_ints(x.low,y.low)) \
      : (fd_compare_ints(x.high,y.high)))
#define FD_OID_DIFFERENCE(x,y) (x.low-y.low)
#define FD_OID_HIGH(x) (x).high
#define FD_OID_LOW(x) (x).low
#define FD_SET_OID_HIGH(x,v) ((x).high)=v
#define FD_SET_OID_LOW(x,v)  ((x).low)=v
#define FD_OID_IN_RANGE(x,base,cap) \
   (((FD_OID_HIGH(x)) == (FD_OID_HIGH(base))) && \
    ((FD_OID_LOW(x)) >= (FD_OID_LOW(base))) && \
    ((FD_OID_LOW(x)) < (FD_OID_LOW(base)+(cap))))
static FD_OID FD_OID_PLUS(FD_OID x,int y)
{
  x.low=x.low+y;
  return x;
}
#endif

/* OID Locking */

#define FD_LOCK_OID(ptr) FD_LOCK_CELL(ptr)
#define FD_UNLOCK_OID(ptr) FD_UNLOCK_CELL(ptr)
#define FD_WITH_OID_LOCKED(ptr) FD_WITH_CELL_LOCKED(ptr)
#define FD_END_WITH_OID_LOCKED(ptr) FD_END_WITH_CELL_LOCKED(ptr)

/* Implementations of OIDS */
/* These are different for consed and unconsed oids */

#if (FD_LIGHTWEIGHT_OIDS)

#define FD_OID_BUCKETS 4

struct FD_POOL_STUB {
  FD_OID base; unsigned int capacity;};

DTYPES_EXPORT struct FD_POOL_STUB *_fd_get_pool_from_bucket(int pid,int off);

/* An array of these works for now */
#if OIDS_ARE_LONGS
struct FD_POOL_BUCKET {
  FD_OID base; int capacity;
  /* The fast case, one pool based at zero */
  struct FD_POOL_STUB *pool;};
#else
struct FD_POOL_BUCKET {
  FD_OID base; int id; int n_pools;
  /* The fast case, one pool based at zero */
  struct FD_POOL_STUB *pool; int capacity;
  /* The slower case, more than one pool or one pool
     not based at zero */
  struct FD_POOL_STUB **pools;};
#endif

DTYPES_EXPORT struct FD_POOL_BUCKET _fd_pool_buckets[256];
DTYPES_EXPORT int _fd_n_pool_buckets;

#if (FD_INLINE_OIDS)
STATIC_INLINE FD_OID FD_OID_ADDR(fd_lisp x) UNUSED;
STATIC_INLINE FD_OID FD_OID_ADDR(fd_lisp x) 
{
  unsigned int d=PTR_DATA(x,oidaddr);
  FD_OID addr=_fd_pool_buckets[d>>24].base;
#if FD_OIDS_ARE_SCALARS
  return addr+(d&0xFFFFFF);
#else
  FD_SET_OID_LOW(addr,FD_OID_LOW(addr)+(d&0xFFFFFF));
  return addr;
#endif
}
static fd_lisp FD_MAKE_OID(FD_OID x) UNUSED;
static fd_lisp FD_MAKE_OID(FD_OID x) 
{
  FD_OID base=x; int i=0, pid=-1, off=FD_OID_LOW(base)&0xFFFFFF;
  FD_SET_OID_LOW(base,FD_OID_LOW(base)&0xFF000000);
  while (i < _fd_n_pool_buckets)
    if (FD_COMPARE_OIDS(_fd_pool_buckets[i].base,base) == 0) {
      pid=i; break;}
    else i++;
  if (pid < 0) pid=_fd_get_pool_bucket(base);
  {FD_RETURN_LISP_IMMEDIATE(object_type,oidaddr,(pid<<24)|off);}
}
#else
#define FD_OID_ADDR fd_oid_addr
#define FD_MAKE_OID fd_make_oid
#endif /* not FD_INLINE_OIDS */

#ifndef FD_HASHTABLE_DEFINED
#define FD_HASHTABLE_DEFINED 1
typedef struct FD_HASHTABLE {
#if FD_THREADS_ENABLED
  fd_rwlock lock;
#endif
  unsigned int n_slots, n_keys; struct FD_PAIR **table;} *fd_hashtable;
#endif

DTYPES_EXPORT struct FD_HASHTABLE _fd_oid_buckets[FD_OID_BUCKETS];

/* Note that we don't have to do OID locking when saving values here because,
   the hashtables do it for us. */
#define _gob(x) (FD_PTR_DATA(x,oidaddr)%FD_OID_BUCKETS)
#define fd_oid_current_value(x) \
   (fd_hashtable_get(&_fd_oid_buckets[_gob(x)],x,FD_VOID))
#define _fd_oid_current_value_nolock(x) fd_oid_current_value(x)
#define fd_store_oid_value(x,v) \
  (fd_hashtable_set(&_fd_oid_buckets[_gob(x)],x,v))
#define _fd_store_oid_value_nolock fd_store_oid_value
#define fd_oid_loadedp(x) \
   (fd_hashtable_probe(&_fd_oid_buckets[_gob(x)],x))
#else /* FD_LIGHTWEIGHT_OIDS */

/* An FD_CONSOID is an OID with a value */
typedef struct FD_CONSOID {FD_OID id; fd_lisp value;} *FD_CONSOID;

#if (DTYPES_SOURCE)
#define OID_BLOCK_SIZE 512
#endif

DTYPES_EXPORT struct FD_CONSOID *_fd_not_an_oid(fd_lisp x);

#define FD_OID_PTR(x) \
  ((FD_PRIM_TYPEP((x),object_type)) ? \
   (FD_PTR_DATA((x),oid)) : (_fd_not_an_oid(x)))

DTYPES_EXPORT fd_lisp fd_make_oid(FD_OID id);
DTYPES_EXPORT fd_lisp fd_probe_oid(FD_OID id);
DTYPES_EXPORT fd_lisp _fd_oid_current_value(fd_lisp x);
DTYPES_EXPORT int fd_oid_loadedp(fd_lisp x);

#define FD_MAKE_OID(id) fd_make_oid(id)

#define FD_OID_VALUE(x) (((FD_OID_PTR(x)))->value)

#if FD_THREADS_ENABLED
#define fd_oid_loadedp(x)       _fd_oid_loadedp(x)
#define fd_oid_current_value(x) _fd_oid_current_value(x)
#define _fd_oid_current_value_nolock(x) (fd_incref(FD_OID_VALUE(x)))
#define fd_store_oid_value(x,v)   _fd_store_oid_value(x,v)
#define _fd_store_oid_value_nolock(x,v) \
 (fd_incref(v),fd_decref(((FD_OID_PTR(x))->value)),((FD_OID_PTR(x))->value)=v)
#else /* FD_THREADS_ENABLED */
#define fd_oid_loadedp(x)       (!(FD_VOIDP(FD_OID_VALUE(x))))
#define fd_oid_current_value(x) (fd_incref(FD_OID_VALUE(x)))
#define _fd_oid_current_value_nolock(x) (fd_incref(FD_OID_VALUE(x)))
#define fd_store_oid_value(x,v)  \
  (fd_incref(v),fd_decref(((FD_OID_PTR(x))->value)),((FD_OID_PTR(x))->value)=v)
#define _fd_store_oid_value_nolock(x,v) \
  (fd_incref(v),fd_decref(((FD_OID_PTR(x))->value)),((FD_OID_PTR(x))->value)=v)
#endif /* not FD_THREADS_ENABLED */
#endif /* not FD_LIGHTWEIGHT_OIDS */

/* Common to both */
#define FD_OID_ADDR_HIGH(x) FD_OID_HIGH(FD_OID_ADDR(x))
#define FD_OID_ADDR_LOW(x) FD_OID_LOW(FD_OID_ADDR(x))


#if (FD_SOURCE)
#define OIDP FD_OIDP
#define OID_PTR FD_OID_PTR
#define OID_ADDR(x) FD_OID_ADDR(x)

#define OID_HIGH FD_OID_HIGH
#define OID_LOW FD_OID_LOW
#define SET_OID_HIGH FD_SET_OID_HIGH
#define SET_OID_LOW FD_SET_OID_LOW
#define COMPARE_OIDS FD_COMPARE_OIDS
#define OID_DIFFERENCE FD_OID_DIFFERENCE
#define OID_ADDR_HIGH FD_OID_ADDR_HIGH
#define OID_ADDR_LOW FD_OID_ADDR_LOW

#define LOCK_OID(o) FD_LOCK_OID(o)
#define UNLOCK_OID(o) FD_UNLOCK_OID(o)
#define WITH_OID_LOCKED(o) FD_WITH_OID_LOCKED(o) 
#define END_WITH_OID_LOCKED(o) FD_END_WITH_OID_LOCKED(o) 

#define oid_loadedp fd_oid_loadedp
#define oid_current_value fd_oid_current_value
#define store_oid_value fd_store_oid_value

#endif /*  FD_SOURCE */

#endif /* FRAMERD_OIDS_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: oids.h,v $
   Revision 1.14  2005/12/05 04:09:39  haase
   Updates for compiling with gcc 4.0

   Revision 1.13  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.12  2004/10/28 15:31:11  haase
   Added FD_OID_EQUAL

   Revision 1.11  2004/09/15 12:39:12  haase
   Added FD_STRUCT_OID override

   Revision 1.10  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.9  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.8  2004/07/16 18:07:01  haase
   Fix bug in long long OIDs

   Revision 1.7  2004/07/16 17:53:55  haase
   Removed old FD_OID_ADDR definition

   Revision 1.6  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.5  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.4.2.1  2002/08/09 16:42:35  haase
   Moving towards more direct FD_RETURN_LISP

   Revision 1.4  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
