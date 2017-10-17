/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: index.h,v 1.28 2006/06/27 11:41:06 haase Exp $

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

#ifndef FRAMERD_INDEX_H
#define FRAMERD_INDEX_H

#include "framerd/common.h"
#include "framerd/cons.h"
#include "framerd/os.h"

/* Exceptions */

FRAMERD_EXPORT fd_exception
  fd_NotFileIndex,
  fd_ReadOnlyIndex,
  fd_NoKeysMethod,
  fd_NoHashMethod,
  fd_BadIndexSpec;

#define FD_FILE_INDEX_MAGIC_NUMBER 151913496
#define FD_MULT_FILE_INDEX_MAGIC_NUMBER 151913497
#define FD_MULT_FILE3_INDEX_MAGIC_NUMBER 151913498
#define FD_FILE_INDEX_SNAPSHOT_MAGIC_NUMBER 152243728

#if FD_THREADS_ENABLED 
#define FD_INDEX_FIELDS() \
  fd_u8char *id; enum FD_INDEX_TYPE type; int serial; \
  struct FD_INDEX_HANDLER *handler; \
  int read_only; int zipf_threshold; \
  struct FD_HASHTABLE cache, adds, drops, sizes; \
  struct FD_HASHSET interned_values; \
  fd_lisp for_slotid; \
  int cache_size, adds_size, drops_size, sizes_size; \
  struct FD_INDEX *next; \
  fd_mutex lock
#else
#define FD_INDEX_FIELDS() \
  fd_u8char *id; enum FD_INDEX_TYPE type; int serial; \
  struct FD_INDEX_HANDLER *handler;  \
  int read_only; int zipf_threshold; \
  struct FD_HASHTABLE cache, adds, drops, sizes; \
  struct FD_HASHSET interned_values; \
  fd_lisp for_slotid; \
  int cache_size, adds_size, drops_size, sizes_size; \
  struct FD_INDEX *next
#endif

/* All index structures start like this */
typedef struct FD_INDEX {
  FD_INDEX_FIELDS();} *fd_index;

struct FD_FILE_INDEX {
  FD_INDEX_FIELDS();
  char *filename; char *buf; FILE *store;
  unsigned int size; unsigned int *offsets;
  int at_end; int hashv; int preloaded;};
typedef struct FD_FILE_INDEX *fd_file_index;

struct FD_NETWORK_INDEX {
  FD_INDEX_FIELDS();
  fd_lisp syncstamp;
  fd_server conn;
  fd_lisp xname;};
typedef struct FD_NETWORK_INDEX *fd_network_index;

struct FD_COMPOUND_INDEX {
  FD_INDEX_FIELDS();
  fd_index *indices; int n_indices;};
typedef struct FD_COMPOUND_INDEX *fd_compound_index;

struct FD_INDEX_HANDLER {
  fd_lisp (*ix_get)(struct FD_INDEX *,fd_lisp key);
  int  (*ix_get_size)(struct FD_INDEX *,fd_lisp key);
  void  (*ix_add)(struct FD_INDEX *,fd_lisp key,fd_lisp values);
  void  (*ix_drop)(struct FD_INDEX *,fd_lisp key,fd_lisp values);
  fd_lisp (*ix_fetch)(struct FD_INDEX *,fd_lisp key);
  int  (*ix_fetch_size)(struct FD_INDEX *,fd_lisp key);
  void (*ix_commit)(struct FD_INDEX *);
  void (*ix_prefetch)(struct FD_INDEX *,fd_lisp keys);
  fd_lisp (*ix_dir)(struct FD_INDEX *);
  void (*ix_spend_memory)(struct FD_INDEX *);
  void (*ix_close)(struct FD_INDEX *);
  int (*ix_sync)(struct FD_INDEX *);
  int (*ix_preload)(struct FD_INDEX *,int);
};

FRAMERD_EXPORT fd_index fd_all_indices;

FRAMERD_EXPORT void fd_register_index(fd_index ix);

FRAMERD_EXPORT
void fd_register_file_index_opener
 (int prefix_code,fd_index (*opener)(fd_u8char *c));

struct FD_FILE_INDEX_OPENER {
  int magic_number; fd_index (*opener)(fd_u8char *);
  struct FD_FILE_INDEX_OPENER *next;};

/* These structures are used when searching for a key in a file index.
   <probe> is the slot in the hash table found and <point> is the beginning of
   the key entry to which it points. */

struct INDEX_LOC {off_t point, data; unsigned int probe, n_elts;};
struct INDEX_LOC_CACHE {fd_lisp key; off_t point, data; unsigned int n_elts;};


/* Prototype definitions */

FRAMERD_EXPORT fd_index fd_open_index(fd_u8char *name);
FRAMERD_EXPORT fd_index fd_find_index(fd_u8char *name);
FRAMERD_EXPORT fd_index fd_interpret_index(fd_lisp spec);

FRAMERD_EXPORT fd_index fd_open_file_index(fd_u8char *name);
FRAMERD_EXPORT fd_network_index fd_open_network_index(char *,int,fd_lisp,fd_u8char *);

FRAMERD_EXPORT int fd_set_search_max(int new_value);

FRAMERD_EXPORT void fd_intern_index_values(fd_index x);
FRAMERD_EXPORT void fd_cache_file_index(fd_file_index x);
FRAMERD_EXPORT void fd_maybe_cache_file_index(fd_file_index x,int already_locked);

FRAMERD_EXPORT fd_lisp fd_index_get(fd_index x,fd_lisp key,fd_lisp dflt);
FRAMERD_EXPORT unsigned int fd_index_get_size(fd_index x,fd_lisp key);
FRAMERD_EXPORT void fd_index_add(fd_index x,fd_lisp key,fd_lisp value);
FRAMERD_EXPORT void fd_index_drop(fd_index x,fd_lisp key,fd_lisp value);
FRAMERD_EXPORT void fd_index_set(fd_index x,fd_lisp key,fd_lisp value);
FRAMERD_EXPORT void fd_index_zap(fd_index x,fd_lisp key);
FRAMERD_EXPORT void fd_index_remove(fd_index x,fd_lisp key,fd_lisp to_remove);

FRAMERD_EXPORT void fd_index_prefetch(fd_index ix,fd_lisp oids);

FRAMERD_EXPORT fd_lisp fd_index_keys(fd_index idx);
FRAMERD_EXPORT void fd_commit_index(fd_index x);
FRAMERD_EXPORT int fd_sync_index(fd_index x);
FRAMERD_EXPORT void fd_index_set_sizes
  (fd_index x,int cache_size,int adds_size,int drops_size,int sizes_size);
FRAMERD_EXPORT void fd_revert_index(fd_index x);
FRAMERD_EXPORT void fd_swap_out_index(fd_index idx);
FRAMERD_EXPORT
void fd_set_index_zipf_threshold(fd_index ix,int threshold);

FRAMERD_EXPORT int _fd_auto_cache_file_indices;
FRAMERD_EXPORT void fd_auto_cache_file_indices(void);
FRAMERD_EXPORT void fd_commit_indices(void);
FRAMERD_EXPORT void fd_revert_indices(void);
FRAMERD_EXPORT void fd_swap_out_indices(void);
FRAMERD_EXPORT void fd_sync_indices(void);

FRAMERD_EXPORT void fd_close_index(fd_index x);

FRAMERD_EXPORT void fd_for_indices
   (void (*fcn)(fd_index p,void *arg),void *arg);
FRAMERD_EXPORT int fd_get_index_count(void);
FRAMERD_EXPORT void fd_index_report_stats(FILE *stream);

FRAMERD_EXPORT
struct FD_COMPOUND_INDEX *fd_make_compound_index(int,fd_index *);
FRAMERD_EXPORT void
  fd_add_to_compound_index(struct FD_COMPOUND_INDEX *cix,fd_index ix);
FRAMERD_EXPORT void fd_trim_compound_index(struct FD_COMPOUND_INDEX *cix);


/* Internal functions for external use */

FRAMERD_EXPORT unsigned int fd_hash_dtype(fd_lisp x); 
FRAMERD_EXPORT unsigned int fd_hash_dtype2(fd_lisp x); 
FRAMERD_EXPORT unsigned int fd_hash_dtype3(fd_lisp x); 
FRAMERD_EXPORT int fd_dtype_compare (fd_lisp key,FILE *stream);

/* File index specific functions */

FRAMERD_EXPORT fd_lisp fd_file_index_collect_values
  (fd_file_index ix,unsigned int loc,unsigned int size);
FRAMERD_EXPORT void fd_repack_file_index
  (char *ofile,char *dfile,unsigned int nsize);
FRAMERD_EXPORT void fd_preload_file_index(fd_index idx_arg);
FRAMERD_EXPORT void fd_unpreload_file_index(fd_index idx_arg);
FRAMERD_EXPORT fd_lisp fd_read_file_index_metadata
   (FILE *f,int *revnum,off_t *size,time_t *,time_t *,time_t *);
FRAMERD_EXPORT void fd_store_file_index_metadata(FILE *f,fd_lisp metadata);

#endif /* ndef FRAMERD_INDEX_H */


/* File specific stuff */

/* The CVS log for this file
   $Log: index.h,v $
   Revision 1.28  2006/06/27 11:41:06  haase
   Added experimental v3 hash function

   Revision 1.27  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.26  2004/10/18 15:24:58  haase
   Added compound indices

   Revision 1.25  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.24  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.23  2004/07/16 13:59:46  haase
   Made file indices use off_t

   Revision 1.22  2004/07/04 01:38:27  haase
   Made auto_cache variables be exported

   Revision 1.21  2004/05/14 14:41:55  haase
   Made preloading be an option for all kinds of indices

   Revision 1.20  2004/03/31 21:04:11  haase
   Various unthreaded compile fixes

   Revision 1.19  2004/03/30 11:32:14  haase
   Renamed mult_hash functions

   Revision 1.18  2004/03/12 20:30:22  haase
   Added new kind of index with multiplicative hash function more appropriate to very large indices

   Revision 1.17  2004/02/09 12:47:30  haase
   Added implementation of database syncing for pools and indices

   Revision 1.16  2003/11/26 13:51:54  haase
   Added index subservers

   Revision 1.15  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.14.2.4  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.14.2.3  2003/08/15 13:29:35  haase
   Various extensions to file pools and indices to handle custom extensions

   Revision 1.14.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.14.2.1  2003/08/02 13:44:34  haase
   Reorganized pool and index initialization, added serial numbers

   Revision 1.14  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.13  2002/06/26 18:56:39  haase
   Remove leftover declaration for fd_unzipf_file_index

   Revision 1.12  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.11  2002/04/22 14:23:07  haase
   Added extended metadata to file pools and indices

   Revision 1.10  2002/04/10 03:01:50  haase
   Added version information to file pools and indices

   Revision 1.9  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
