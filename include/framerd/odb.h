/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: odb.h,v 1.35 2005/01/14 16:48:44 haase Exp $

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

#ifndef FRAMERD_ODB_H
#define FRAMERD_ODB_H

#include "framerd/common.h"
#include "framerd/cons.h"
#include "framerd/os.h"


/* FramerD Exceptions */

FRAMERD_EXPORT fd_exception fd_PoolNOOP;

FRAMERD_EXPORT fd_exception
  fd_Non_Atomic_Slot, fd_Read_Only_OID,
  fd_Cant_Lock_Remote_OID, fd_Homeless_OID,
  fd_Invalid_Frame, fd_Invalid_Slot, fd_NoSuchValue,
  fd_FSVInvalid,
  fd_UnknownPool,
  fd_FilePoolExhausted,
  fd_UnlockablePool,
  fd_ReadOnlyPool,
  fd_NotAFilePool,
  fd_NotASuperPool,
  fd_UnregisteredPool,
  fd_BadPoolSpec,
  fd_UnWritablePool,
  fd_NotASuperPool,
  fd_UnallocatedOID,
  fd_InvalidOID,
  fd_FSVInvalid,
  fd_AmbiguousFrameName,
  fd_UnknownFrameName;

FRAMERD_EXPORT int fd_ignore_homeless_frames;


/* Pools */

#define FD_SUPER_POOL_MAGIC_NUMBER 320147474
#define FD_REGISTERED_SUPER_POOL_MAGIC_NUMBER 320147475
#define FD_FILE_POOL_MAGIC_NUMBER  67179521
#define FD_FILE_POOL_SNAPSHOT_MAGIC_NUMBER 269684240

#define FD_POOL_WRITABLE 0
#define FD_POOL_LOCKABLE 1
#define FD_POOL_READ_ONLY 2

struct FD_PREFETCH_STRUCT {fd_lisp *oids, *values; int n;};

/* This is how all pools start */
#define FD_POOL_FIELDS() \
  FD_OID base; unsigned int capacity;                              \
  enum FD_POOL_TYPE type; int serial;                              \
  fd_u8char *id, *prefix_id; fd_lisp label;                        \
  int modifiedp; int read_only;                                    \
  struct FD_HASHSET modified;                                      \
  struct FD_POOL_HANDLER *handler

#define FD_POOL_LOCK_OID 1
#define FD_POOL_UNLOCK_OID 2

typedef struct FD_POOL { 
  FD_POOL_FIELDS();} *fd_pool;

struct FD_POOL_HANDLER {
  fd_lisp (*new_oid)(struct FD_POOL *p);
  fd_lisp (*lookup_oid)(struct FD_POOL *p,fd_lisp id);
  void (*commit_pool)(struct FD_POOL *p);
  void (*commit_oid)(struct FD_POOL *p,fd_lisp oid);
  int (*bulk_fetch)(struct FD_POOL *p,fd_lisp *o,fd_lisp *v,int n);
  void (*close_pool)(struct FD_POOL *p);
  void (*oid_locker)(struct FD_POOL *p,fd_lisp oid,int action);
  unsigned int (*get_load)(struct FD_POOL *p);
  void (*spend_memory)(struct FD_POOL *p);
  fd_lisp (*read_metadata)(FILE *,int *,fd_u8char **,time_t *,time_t *,time_t *);
  int (*sync_pool)(struct FD_POOL *p);
};

/* File pools are stored in the file system */
typedef struct FD_FILE_POOL {
  FD_POOL_FIELDS();
  char *buf; /* cached big buffer */
  char *filename;
  FILE *store;
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif
  unsigned int load;
  off_t end_pos;
  unsigned int offsets_size;
  unsigned int *offsets;} *fd_file_pool;

/* Network pools are accessed over the network */
typedef struct FD_NETWORK_POOL {
  FD_POOL_FIELDS();
  fd_lisp syncstamp;
  fd_server conn;} *fd_network_pool;

/* An array of these works for now */
typedef struct FD_POOL_TABLE_ENTRY {
  FD_OID base; unsigned int capacity;
  fd_pool p;} *pool_entry;

FRAMERD_EXPORT struct FD_POOL_TABLE_ENTRY *_fd_pool_table;
FRAMERD_EXPORT int fd_n_pools;

FRAMERD_EXPORT
void fd_register_file_pool_opener
 (unsigned int prefix_code,struct FD_POOL_HANDLER *,fd_pool (*opener)(fd_u8char *c));

struct FD_FILE_POOL_OPENER {
  int magic_number; struct FD_POOL_HANDLER *handler;
  fd_pool (*opener)(fd_u8char *);
  struct FD_FILE_POOL_OPENER *next;};


/* Accessing OIDs */

FRAMERD_EXPORT fd_lisp fd_get_oid_value(fd_lisp oref);

#if (FD_THREADS_ENABLED)
FASTOP fd_lisp fd_oid_value(fd_lisp) UNUSED;
FASTOP fd_lisp fd_oid_value(fd_lisp obj)
{
  fd_lisp v;
  FD_LOCK_OID(obj);
  v=_fd_oid_current_value_nolock(obj);
  FD_UNLOCK_OID(obj);
  if (FD_VOIDP(v)) return fd_get_oid_value(obj);
  else return v;
}
#else
#define fd_oid_value(obj) \
  ((FD_OIDP(obj)) ? \
   ((fd_oid_loadedp(obj)) ? (fd_oid_current_value(obj)) : \
    (fd_get_oid_value(obj))) : \
   (fd_type_error(_("fd_oid_value: not an OID"),obj),(FD_VOID)))
#endif


/* Pool Functions */

#if FD_THREADS_ENABLED
static fd_mutex _fd_pool_table_lock;
#endif

FRAMERD_EXPORT
  void fd_init_pool_data
  (fd_pool,enum FD_POOL_TYPE,FD_OID,unsigned int,int,
   fd_u8char *,fd_u8char *,fd_lisp);

FRAMERD_EXPORT fd_pool fd_use_pool(fd_u8char *c);
FRAMERD_EXPORT 
  void fd_for_pools(void (*fcn)(fd_pool p,void *arg),void *arg);
FRAMERD_EXPORT int fd_get_pool_count(void);
FRAMERD_EXPORT fd_pool fd_interpret_pool(fd_lisp spec);
FRAMERD_EXPORT struct FD_POOL *fd_get_pool(fd_lisp x);

FRAMERD_EXPORT int fd_register_pool(fd_pool p);
FRAMERD_EXPORT fd_pool fd_locate_pool(fd_lisp oid);

FRAMERD_EXPORT fd_pool fd_find_pool_named(fd_u8char *string);

FRAMERD_EXPORT fd_pool fd_use_file_pool(fd_u8char *filename);
FRAMERD_EXPORT
fd_network_pool fd_use_network_pool(char *hostname,int port,fd_u8char *id);
FRAMERD_EXPORT int fd_lock_file_pool(fd_file_pool p);

FRAMERD_EXPORT int _fd_auto_cache_file_pools;
FRAMERD_EXPORT void fd_auto_cache_file_pools(void);
FRAMERD_EXPORT void fd_maybe_cache_file_pool(fd_file_pool fp,int already_locked);
FRAMERD_EXPORT void fd_make_file_pool
   (char *filename,FD_OID base,unsigned int capacity,
    int major_version,fd_lisp metadata);
FRAMERD_EXPORT FD_OID fd_allocate_pool
   (char *super_pool,unsigned int capacity,char *label);
FRAMERD_EXPORT FD_OID fd_recovered_pool
   (char *super_pool,FD_OID base,unsigned int capacity,fd_lisp tag);
FRAMERD_EXPORT void fd_new_file_pool
   (char *filename,unsigned int capacity,char *super_pool);
FRAMERD_EXPORT void fd_cache_file_pool(fd_file_pool p);
FRAMERD_EXPORT fd_lisp fd_read_file_pool_metadata
  (FILE *f,int *revmajor,fd_u8char **revminor,time_t *make,time_t *repack,time_t *change);
FRAMERD_EXPORT void fd_store_file_pool_metadata(FILE *f,fd_lisp metadata);
FRAMERD_EXPORT fd_lisp fd_read_std_file_pool_metadata
  (FILE *f,int *revmajor,fd_u8char **revminor,time_t *make,time_t *repack,time_t *change);


/* Declarations */

FRAMERD_EXPORT int fd_loaded_oids;
FRAMERD_EXPORT int fd_oids_loaded;
FRAMERD_EXPORT int fd_new_oids;

FRAMERD_EXPORT void fd_never_save(void);
FRAMERD_EXPORT void fd_homeless_ok(void);
FRAMERD_EXPORT int fd_ephemeralp(void);

FRAMERD_EXPORT void fd_for_file_pool(void (*fcn)(fd_lisp obj),fd_file_pool fp);

/* Maintenance functions */

FRAMERD_EXPORT void fd_repack_file_pool(char *origin,char *destination);
FRAMERD_EXPORT unsigned int fd_make_super_pool
  (char *filename,unsigned int base,unsigned int load);
FRAMERD_EXPORT unsigned int fd_make_new_super_pool(char *filename);
FRAMERD_EXPORT void fd_report_framerd_stats(FILE *to);

FRAMERD_EXPORT unsigned int fd_file_pool_load(fd_u8char *filename);
FRAMERD_EXPORT unsigned int fd_file_pool_capacity(fd_u8char *filename);
FRAMERD_EXPORT unsigned int fd_file_pool_freespace(fd_u8char *filename);

FRAMERD_EXPORT FD_OID fd_super_pool_base(char *id);
FRAMERD_EXPORT FD_OID fd_super_pool_top(char *id);
FRAMERD_EXPORT float fd_super_pool_loading(char *id);

FRAMERD_EXPORT void fd_make_pool_snapshot(char *filename,char *snapshot);
FRAMERD_EXPORT void fd_restore_pool_snapshot(char *snapshot,char *filename);

/* Autoindexing declarations */

FRAMERD_EXPORT void fd_use_autoindex(struct FD_INDEX *ix);

FRAMERD_EXPORT int fd_pool_load(fd_pool p);
FRAMERD_EXPORT fd_lisp fd_random_oid(fd_pool p);

FRAMERD_EXPORT void fd_label_file_pool(char *filename,fd_lisp label);

FRAMERD_EXPORT fd_lisp fd_try_oid_value(fd_lisp oref);
FRAMERD_EXPORT void fd_mark_modified(fd_lisp oref);
FRAMERD_EXPORT fd_lisp *fd_get_modified(fd_pool p,int *n_oids,int reset);
FRAMERD_EXPORT void fd_set_oid_value(fd_lisp oref,fd_lisp value);
FRAMERD_EXPORT fd_lisp fd_new_oid(fd_pool p);

/* Saving, reversion, and swapping */

FRAMERD_EXPORT int fd_revert_oid(fd_lisp oref);
FRAMERD_EXPORT void fd_swap_out(fd_lisp oref);

FRAMERD_EXPORT void fd_close_pool(fd_pool p);
FRAMERD_EXPORT void fd_commit_pool(fd_pool p);
FRAMERD_EXPORT void fd_revert_pool(fd_pool p);
FRAMERD_EXPORT void fd_swap_out_pool(fd_pool p);
FRAMERD_EXPORT int fd_sync_pool(fd_pool p);

FRAMERD_EXPORT void fd_commit_pools(void);
FRAMERD_EXPORT void fd_revert_pools(void);
FRAMERD_EXPORT void fd_swap_out_oids(void);
FRAMERD_EXPORT void fd_sync_pools(void);
/* For old code */
#define fd_swap_out_pools fd_swap_out_oids

FRAMERD_EXPORT void fd_save_all_and_exit(void);

FRAMERD_EXPORT void fd_commit_file_pool(fd_file_pool p);
FRAMERD_EXPORT void fd_revert_file_pool(fd_file_pool p);

FRAMERD_EXPORT void fd_prefetch_oids(fd_lisp oids);
FRAMERD_EXPORT void fd_bulk_store_oid_values(fd_lisp *oidv,fd_lisp *values,int n);


/* Frame operations */

#define FD_FRAMEP(x) ((FD_OIDP(x)) && (FD_SLOTMAPP(fd_oid_value(x))))

typedef enum SLOT_OP 
  { slot_get, slot_add, slot_remove, slot_test , slot_validate} slot_op;
struct FD_SLOT_STACK {
  slot_op op;
  fd_lisp frame, slotid, value, goal; 
  struct FD_SLOT_STACK *next;};
extern struct FD_SLOT_STACK *fd_slot_stack;

FRAMERD_EXPORT fd_lisp fd_prim_get(fd_lisp frame,fd_lisp slot);
FRAMERD_EXPORT void fd_prim_set(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_prim_set_consed(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT int fd_prim_test(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_prim_add(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_prim_add_consed(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_prim_drop(fd_lisp frame,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_prim_rename_slot
  (fd_lisp frame,fd_lisp old_name,fd_lisp new_name);

FRAMERD_EXPORT fd_lisp fd_frame_create(fd_pool p);

FRAMERD_EXPORT fd_lisp fd_copy_frame (fd_lisp f,fd_pool p);
FRAMERD_EXPORT fd_lisp fd_frame_get  (fd_lisp f,fd_lisp s);
FRAMERD_EXPORT void fd_frame_add     (fd_lisp f,fd_lisp s,fd_lisp v);
FRAMERD_EXPORT int fd_frame_validate (fd_lisp f,fd_lisp s,fd_lisp v);
FRAMERD_EXPORT int fd_frame_test     (fd_lisp f,fd_lisp s,fd_lisp v);
FRAMERD_EXPORT void fd_frame_set     (fd_lisp f,fd_lisp s,fd_lisp v);
FRAMERD_EXPORT void fd_frame_remove  (fd_lisp f,fd_lisp s,fd_lisp v);
FRAMERD_EXPORT fd_lisp fd_frame_get_star  (fd_lisp f,fd_lisp s);

FRAMERD_EXPORT
void fd_register_adjunct_store
  (fd_lisp slotid,fd_index ix,FD_OID base,unsigned int cap);

/* Caching computed slot values */

FRAMERD_EXPORT void fd_clear_slot_cache(fd_lisp slot,fd_lisp frame);
FRAMERD_EXPORT void fd_enable_slot_cache(fd_lisp slot,fd_lisp cache);
FRAMERD_EXPORT void fd_disable_slot_cache(fd_lisp slot);
FRAMERD_EXPORT void fd_reset_slot_cache();

/* Useful predicates */

FRAMERD_EXPORT fd_lisp fd_inherit_values(fd_lisp root,fd_lisp slotid,fd_lisp through);
FRAMERD_EXPORT int fd_inherits_valuep(fd_lisp root,fd_lisp slotid,fd_lisp through,fd_lisp value);
FRAMERD_EXPORT int fd_pathp(fd_lisp root,fd_lisp slotid,fd_lisp to);

/* I/O related functions */

FRAMERD_EXPORT void fd_control_frame_printing(int level);
FRAMERD_EXPORT void fd_print_oid(fd_lisp oid,fd_string_stream ss);
FRAMERD_EXPORT fd_lisp fd_parse_oid(fd_u8char *atomic_string);
FRAMERD_EXPORT void fd_show_poolids(int use_ids);

FRAMERD_EXPORT void fd_describe_slot(FILE *stream,fd_lisp slot,fd_lisp value);
FRAMERD_EXPORT void fd_describe_frame(fd_lisp frame,FILE *stream);

FRAMERD_EXPORT void fd_import_frame(fd_lisp frame,fd_lisp slots,int noisy);
FRAMERD_EXPORT fd_lisp fd_export_frame(fd_lisp frame,fd_lisp focus_slots,int only);


/* Dealing with slots */

FRAMERD_EXPORT void fd_for_slots
  (void (*fcn)(fd_lisp frame,fd_lisp slot,fd_lisp value),fd_lisp frame);
FRAMERD_EXPORT void fd_for_values
  (void (*fcn)(fd_lisp frame,fd_lisp slot,fd_lisp value),fd_lisp frame);
FRAMERD_EXPORT fd_lisp fd_frame_slots(fd_lisp frame);
FRAMERD_EXPORT fd_lisp fd_get_slotmap(fd_lisp oid);

#define FD_SLOTP(x) ((FD_SYMBOLP(x)) || (FD_OIDP(x)))

#define FD_DO_SLOTS(s,v,obj) \
  fd_lisp _doslots_u=obj; \
  fd_lisp _doslots_ov=fd_get_slotmap(_doslots_u);  \
  fd_lisp s, v, *_doslots_slots;  \
  int _size=_fd_slotmap_data \
             (_doslots_ov,(void **)&_doslots_slots); \
  fd_lisp *_doslots_scan=_doslots_slots; \
  fd_lisp *_doslots_limit=_doslots_slots+_size; \
  while ((_doslots_scan<_doslots_limit) ? \
	 (s=*_doslots_scan++,v=*_doslots_scan++,1) : \
	 (fd_decref(_doslots_ov),_fd_done_with_slotmap_data(_doslots_slots,_size),0))

#if FD_INLINE_OIDS
#if (FD_LIGHTWEIGHT_OIDS)
STATIC_INLINE fd_pool FD_GET_POOL(fd_lisp x) 
{
  if (FD_OIDP(x)) {
    unsigned int d=FD_PTR_DATA(x,oidaddr);
    unsigned int pid=(d>>24);
    unsigned int off=(d&0xFFFFFF);
    if (_fd_pool_buckets[pid].pool)
      if (off < _fd_pool_buckets[pid].capacity)
	return (fd_pool) _fd_pool_buckets[pid].pool;
      else return (fd_pool) _fd_get_pool_from_bucket(pid,off);
    else return (fd_pool) _fd_get_pool_from_bucket(pid,off);}
  else {
    fd_type_error(_("not an OID"),x);
    return NULL;}
}
#else /* FD_LIGHTWEIGHT_OIDS */
FASTOP fd_pool FD_GET_POOL(lisp oid)
{
  if (FD_OIDP(oid)) {
    fd_pool p=NULL;
    if (fd_n_pools) {
      FD_OID id=FD_OID_ADDR(oid);
      int bot=0, top=fd_n_pools-1, direction;
      unsigned int hi=FD_OID_HIGH(id), lo=FD_OID_LOW(id);
      fd_lock_mutex(&_fd_pool_table_lock);
      while (top >= bot) {
	int i=bot+(top-bot)/2;
	if (FD_OID_HIGH(_fd_pool_table[i].base) < hi) direction=1;
	else if (FD_OID_HIGH(_fd_pool_table[i].base) > hi) direction=-1;
	else if (FD_OID_LOW(_fd_pool_table[i].base) > lo) direction=-1;
	else if (lo < FD_OID_LOW(_fd_pool_table[i].base)+_fd_pool_table[i].capacity) {
	  p=_fd_pool_table[i].p; break;}
	else direction=1;
	if (direction < 0) top=i-1;
	else bot=i+1;}
      fd_unlock_mutex(&_fd_pool_table_lock);}
    return p;}
  else {
    fd_type_error(_("not an OID"),oid);
    return NULL;}
}
#endif /* not FD_LIGHTWEIGHT_OIDS */
#else /* FD_INLINE_OIDS */
#define FD_GET_POOL(x) fd_get_pool(x)
#endif

#if (FD_SOURCE)
#define OID_ADDR_HIGH FD_OID_ADDR_HIGH
#define OID_ADDR_LOW FD_OID_ADDR_LOW
#define oid_value fd_oid_value
#define oid_current_value fd_oid_current_value
#define store_oid_value fd_store_oid_value

#define FRAMEP FD_FRAMEP
#define SLOTP FD_SLOTP
#define DO_SLOTS FD_DO_SLOTS
#endif


#endif /* ndef FRAMERD_ODB_H */




/* File specific stuff */

/* The CVS log for this file
   $Log: odb.h,v $
   Revision 1.35  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.34  2004/10/27 17:26:20  haase
   Add FDSCript access to adjuncts

   Revision 1.33  2004/09/17 08:29:25  haase
   Added complete slotcache resets

   Revision 1.32  2004/08/26 19:16:22  haase
   Removed overlays

   Revision 1.31  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.30  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.29  2004/07/16 13:37:21  haase
   Made file pools use off_t

   Revision 1.28  2004/07/04 01:38:27  haase
   Made auto_cache variables be exported

   Revision 1.27  2004/04/18 15:31:38  haase
   Made get_slotmap conditionally forgiving of homeless OIDs

   Revision 1.26  2004/03/31 11:19:55  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.25  2004/03/31 03:13:10  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.24  2004/02/17 18:36:25  haase
   Indices can now serve as slot caches, allowing persistence

   Revision 1.23  2004/02/09 12:47:30  haase
   Added implementation of database syncing for pools and indices

   Revision 1.22  2003/12/05 14:45:07  haase
   Prepared the way for 64bit file pools

   Revision 1.21  2003/10/05 15:35:52  haase
   Added declarations for fd_print_oid/fd_parse_oid

   Revision 1.20  2003/10/05 06:37:48  haase
   Made FD_DO_SLOTS use interleaved data function

   Revision 1.19  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.18.2.6  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.18.2.5  2003/08/15 13:29:35  haase
   Various extensions to file pools and indices to handle custom extensions

   Revision 1.18.2.4  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.18.2.3  2003/08/02 13:13:21  haase
   Added serial numbers to pools, moved prefect structure into common header

   Revision 1.18.2.2  2003/06/29 19:41:04  haase
   Syntactic changes connected to slot iteration

   Revision 1.18.2.1  2003/01/26 20:31:08  haase
   Made pool fields be utf-8

   Revision 1.18  2002/07/24 02:05:47  haase
   Removed 'new' symbols from include files to allow inclusion in C++ files

   Revision 1.17  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.16  2002/06/21 13:46:03  haase
   Made file pools use an offset table limited to the load of the pool, growing it when neccessary.

   Revision 1.15  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.14  2002/04/22 14:23:07  haase
   Added extended metadata to file pools and indices

   Revision 1.13  2002/04/11 00:26:23  haase
   Removed API for naming pools

   Revision 1.12  2002/04/10 18:57:08  haase
       Orthogonalized associations between pool names and pools and between
   OIDs and their pools.
       Made canonicalization of paths or server specs be done directly
   at pool creation time rather than kludged through the name mechanism.
       Added use of dotted syntax in pool labels.

   Revision 1.11  2002/04/10 03:01:50  haase
   Added version information to file pools and indices

   Revision 1.10  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
