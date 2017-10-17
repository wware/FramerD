/* C Mode */

/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: fdmalloc.h,v 1.26 2005/03/07 23:56:59 haase Exp $

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

  This is the file fdmalloc.h supporting a layer over malloc which
  maintains free lists.

*************************************************************************/

/* Hair for debugging */

#if (FD_LOG_MALLOC)
#define fd_malloc(sz) fd_malloc_debug(sz,__FILE__,__LINE__)
#define fd_xmalloc(sz) fd_xmalloc_debug(sz,__FILE__,__LINE__)
#define fd_realloc(p,newsz,oldsz) fd_realloc_debug(p,newsz,oldsz,__FILE__,__LINE__)
#define fd_xrealloc(p,newsz) fd_xrealloc_debug(p,newsz,__FILE__,__LINE__)
#define fd_free(p,oldsz) fd_free_debug(p,oldsz,__FILE__,__LINE__)
#define fd_xfree(p) fd_xfree_debug(p,__FILE__,__LINE__)
#define fd_strdup(p) fd_strdup_debug(p,__FILE__,__LINE__)
DTYPES_EXPORT void *fd_malloc_debug(size_t bytes,char *,int);
DTYPES_EXPORT void *fd_xmalloc_debug(size_t bytes,char *,int);
DTYPES_EXPORT void *fd_realloc_debug(void *ptr,size_t new_size,size_t old_size,char *,int);
DTYPES_EXPORT void *fd_xrealloc_debug(void *,size_t bytes,char *,int);
DTYPES_EXPORT void fd_xfree_debug(void *ptr,char *,int);
DTYPES_EXPORT void fd_free_debug(void *ptr,size_t bytes,char *,int);
DTYPES_EXPORT char *fd_strdup_debug(const char *,char*,int);
#else
DTYPES_EXPORT void *fd_malloc(size_t bytes);
DTYPES_EXPORT void *fd_xmalloc(size_t bytes);
DTYPES_EXPORT void *fd_realloc(void *ptr,size_t new_size,size_t old_size);
DTYPES_EXPORT void *fd_xrealloc(void *,size_t bytes);
DTYPES_EXPORT void fd_xfree(void *ptr);
DTYPES_EXPORT void fd_free(void *ptr,size_t bytes);
DTYPES_EXPORT void *fd_xrealloc(void *ptr,size_t bytes);
DTYPES_EXPORT char *fd_strdup(const char *);
#endif

/* Function prototypes */

/* Memory allocation */


DTYPES_EXPORT char *fd_mallocize(char *ptr,size_t size);

DTYPES_EXPORT char *fd_memdup(const char *,size_t sz);
DTYPES_EXPORT char *fd_xmemdup(const char *,size_t sz);

DTYPES_EXPORT void fd_malloc_adjust(int delta);
DTYPES_EXPORT void fd_free_int_array(unsigned int *ptr,size_t bytes);

DTYPES_EXPORT void _fd_record_malloc_block(void *start,int size);
DTYPES_EXPORT int fd_check_qptr(void *ptr);
DTYPES_EXPORT void fd_invalid_qptr(void *ptr);

#if (HAVE_VALGRIND_H)
DTYPES_EXPORT void fd_vcheck_range(char *tag,void *start,int size);
#else
#define fd_vcheck_range(t,m,n)
#endif

#if (HAVE_MMAP)
DTYPES_EXPORT fd_exception fd_MmapFailed, fd_UnmapFailed;
#endif

/* This file implements a layer over malloc which supports free lists */

struct FD_REFCOUNTED_STRUCT {fd_refcount n_refs;};
struct FD_FREE_LIST {
  fd_refcount n_refs;
  struct FD_FREE_LIST *next;};
struct FD_MALLOC_BUCKET {
#if FD_THREADS_ENABLED
  fd_mutex lock; int shared;
#endif
  int malloc_size, n_chunks;
  int n_used, n_free;
  struct FD_FREE_LIST *used;
  void *fresh, *last_fresh;};
struct FD_MALLOC_DATA {
  struct FD_MALLOC_BUCKET *buckets[16];
  long other;
  struct FD_MALLOC_DATA *next;};

DTYPES_EXPORT struct FD_QMALLOC_RECORD {
  void *ptr; int size; char *file; int line;
  struct FD_QMALLOC_RECORD *next;} *_fd_qmalloc_records;
DTYPES_EXPORT int _fd_recording_qmallocs;

DTYPES_EXPORT void _fd_record_qmalloc(void *p,int size,char *file,int line);
DTYPES_EXPORT void _fd_record_qfree(void *p);

struct FD_MALLOC_BLOCK_RECORD { void *start; int size; };
DTYPES_EXPORT int _fd_debugging_memory;

#if FD_THREADS_ENABLED
DTYPES_EXPORT fd_tld_key _fd_malloc_data_key;
DTYPES_EXPORT void fd_use_threadlocal_malloc(void);
#else
#define fd_use_threadlocal_malloc() 
#endif

DTYPES_EXPORT struct FD_MALLOC_DATA _fd_global_malloc_data;

DTYPES_EXPORT void *_fd_qmalloc(size_t n);
DTYPES_EXPORT void *_fd_qmalloc_cons(size_t n);
DTYPES_EXPORT void _fd_qfree(void *p,size_t n);

#if (FD_WITH_BLOCK_MALLOC == 0)
#define fd_qmalloc(size) fd_malloc(size)
static void *fd_qmalloc_cons(size_t size)
{
  struct FD_REFCOUNTED_STRUCT *answer=fd_qmalloc(size);
  answer->n_refs=1;
  return (void *) answer;
}
#define fd_qfree(ptr,size) fd_free(ptr,size)
#elif (FD_INLINE_FDMALLOC && FD_LOG_MALLOC)
#if FD_THREADS_ENABLED
STATIC_INLINE struct FD_MALLOC_DATA *get_malloc_data()
{
  struct FD_MALLOC_DATA *d=fd_tld_get(_fd_malloc_data_key);
  if (d == NULL) return &_fd_global_malloc_data;
  else return d;
}
#else
STATIC_INLINE struct FD_MALLOC_DATA *get_malloc_data()
{
  return &_fd_global_malloc_data;
}
#endif
STATIC_INLINE void *fd_qmalloc_debug(size_t size,char *file,int line)
{
  if (size == 0) return NULL;
  else {
    struct FD_MALLOC_DATA *d=get_malloc_data();
    if ((size >= sizeof(struct FD_FREE_LIST)) &&
	(size%4 == 0) && (size < 64) &&
	(d->buckets[size/4])) {
      struct FD_MALLOC_BUCKET *b=d->buckets[size/4]; void *answer;
      if (size != b->malloc_size)
	fd_raise_exception("in the wrong bucket");
#if FD_THREADS_ENABLED
      if (b->shared) fd_lock_mutex(&(b->lock));
#endif
      if (b->used) {
	answer=b->used; b->used=b->used->next; b->n_free--;}
      else if (b->fresh == NULL) {
	b->fresh=fd_xmalloc(b->malloc_size*b->n_chunks);
	_fd_record_malloc_block(b->fresh,b->malloc_size*b->n_chunks);
	b->last_fresh=((char *)b->fresh)+b->malloc_size*(b->n_chunks-1);
	answer=b->fresh; b->fresh=((char *)b->fresh)+b->malloc_size;}
      else if (b->fresh == b->last_fresh) {
	answer=b->fresh; b->fresh=NULL; b->last_fresh=NULL;}
      else {
	answer=b->fresh; b->fresh=((char *)b->fresh)+size;}
      b->n_used++;
#if FD_THREADS_ENABLED
      if (b->shared) fd_unlock_mutex(&(b->lock));
#endif
      if (_fd_recording_qmallocs) _fd_record_qmalloc(answer,size,file,line);
      return answer;}
    else {
      void *ptr=fd_malloc(size);
      if (_fd_recording_qmallocs) _fd_record_qmalloc(ptr,size,file,line);
      return ptr;}}
}
STATIC_INLINE void *fd_qmalloc_cons_debug(size_t size,char *file,int line)
{
  struct FD_REFCOUNTED_STRUCT *answer=fd_qmalloc_debug(size,file,line);
  answer->n_refs=1;
  return (void *) answer;
}
STATIC_INLINE void fd_qfree_debug(void *x,size_t size,char *file,int line)
{
  if (size == 0) return;
  else {
    struct FD_MALLOC_DATA *d=get_malloc_data();    
    if ((size >= sizeof(struct FD_FREE_LIST)) &&
	(size%4 == 0) && (size < 64) &&
	(d->buckets[size/4])) {
      struct FD_MALLOC_BUCKET *b=d->buckets[size/4];
      struct FD_FREE_LIST *fl;
      if ((_fd_debugging_memory) && (fd_check_qptr(x))) fd_invalid_qptr(x);
#if FD_THREADS_ENABLED
      if (b->shared) fd_lock_mutex(&(b->lock));
#endif
      if (_fd_recording_qmallocs) _fd_record_qfree(x);
      /* Fill the struct with 1's, especially to screw up anything
	 else that is (erroneously) depending on it. */
      memset(x,0xFF,size); 
      fl=x; fl->next=b->used; fl->n_refs=0;
      b->used=fl; b->n_free++; b->n_used--;
#if FD_THREADS_ENABLED
      if (b->shared) fd_unlock_mutex(&(b->lock));
#endif
    }
    else {
      if (_fd_recording_qmallocs) _fd_record_qfree(x);
      fd_free(x,size);}}
}
#define fd_qmalloc(sz) fd_qmalloc_debug(sz,__FILE__,__LINE__)
#define fd_qmalloc_cons(sz) fd_qmalloc_cons_debug(sz,__FILE__,__LINE__)
#define fd_qfree(p,sz) fd_qfree_debug(p,sz,__FILE__,__LINE__)
#elif FD_INLINE_FDMALLOC
#if FD_THREADS_ENABLED
STATIC_INLINE struct FD_MALLOC_DATA *get_malloc_data()
{
  struct FD_MALLOC_DATA *d=fd_tld_get(_fd_malloc_data_key);
  if (d == NULL) return &_fd_global_malloc_data;
  else return d;
}
#else
STATIC_INLINE struct FD_MALLOC_DATA *get_malloc_data()
{
  return &_fd_global_malloc_data;
}
#endif
STATIC_INLINE void *fd_qmalloc(size_t size)
{
  if (size == 0) return NULL;
  else {
    struct FD_MALLOC_DATA *d=get_malloc_data();
    if ((size >= sizeof(struct FD_FREE_LIST)) &&
	(size%4 == 0) && (size < 64) &&
	(d->buckets[size/4])) {
      struct FD_MALLOC_BUCKET *b=d->buckets[size/4]; void *answer;
      if (size != b->malloc_size)
	fd_raise_exception("in the wrong bucket");
#if FD_THREADS_ENABLED
      if (b->shared) fd_lock_mutex(&(b->lock));
#endif
      if (b->used) {
	answer=b->used; b->used=b->used->next; b->n_free--;}
      else if (b->fresh == NULL) {
	b->fresh=fd_xmalloc(b->malloc_size*b->n_chunks);
	_fd_record_malloc_block(b->fresh,b->malloc_size*b->n_chunks);
	b->last_fresh=((char *)b->fresh)+b->malloc_size*(b->n_chunks-1);
	answer=b->fresh; b->fresh=((char *)b->fresh)+b->malloc_size;}
      else if (b->fresh == b->last_fresh) {
	answer=b->fresh; b->fresh=NULL; b->last_fresh=NULL;}
      else {
	answer=b->fresh; b->fresh=((char *)b->fresh)+size;}
      b->n_used++;
#if FD_THREADS_ENABLED
      if (b->shared) fd_unlock_mutex(&(b->lock));
#endif
      return answer;}
    else {
      void *ptr=fd_malloc(size);
      return ptr;}}
}
STATIC_INLINE void *fd_qmalloc_cons(size_t size)
{
  struct FD_REFCOUNTED_STRUCT *answer=fd_qmalloc(size);
  answer->n_refs=1;
  return (void *) answer;
}
STATIC_INLINE void fd_qfree(void *x,size_t size)
{
  if (size == 0) return;
  else {
    struct FD_MALLOC_DATA *d=get_malloc_data();    
    if ((size >= sizeof(struct FD_FREE_LIST)) &&
	(size%4 == 0) && (size < 64) &&
	(d->buckets[size/4])) {
      struct FD_MALLOC_BUCKET *b=d->buckets[size/4];
      struct FD_FREE_LIST *fl;
      if ((_fd_debugging_memory) && (fd_check_qptr(x))) fd_invalid_qptr(x);
#if FD_THREADS_ENABLED
      if (b->shared) fd_lock_mutex(&(b->lock));
#endif
      /* Fill the struct with 1's, especially to screw up anything
	 else that is (erroneously) depending on it. */
      memset(x,0xFF,size); 
      fl=x; fl->next=b->used; fl->n_refs=0;
      b->used=fl; b->n_free++; b->n_used--;
#if FD_THREADS_ENABLED
      if (b->shared) fd_unlock_mutex(&(b->lock));
#endif
    }
    else {
      fd_free(x,size);}}
}
#elif FD_LOG_MALLOC
DTYPES_EXPORT void *_fd_qmalloc_debug(size_t size,char *file,int line);
DTYPES_EXPORT void *_fd_qmalloc_cons_debug(size_t size,char *file,int line);
DTYPES_EXPORT void *_fd_qfree_debug(void *p,size_t size,char *file,int line);
#define fd_qmalloc(sz) _fd_qmalloc_debug(sz,__FILE__,__LINE__)
#define fd_qmalloc_cons(sz) _fd_qmalloc_cons_debug(sz,__FILE__,__LINE__)
#define fd_qfree(p,sz) _fd_qfree_debug(p,sz,__FILE__,__LINE__)
#else
DTYPES_EXPORT void *_fd_qmalloc(size_t size);
DTYPES_EXPORT void *_fd_qmalloc_cons(size_t size);
DTYPES_EXPORT void _fd_qfree(void *p,size_t size);
#define fd_qmalloc(size) _fd_qmalloc(size)
#define fd_qmalloc_cons(size) _fd_qmalloc_cons(size)
#define fd_qfree(x,size) _fd_qfree(x,size)
#endif

#define fd_malloca(spec) fd_qmalloc_cons(sizeof(spec))
DTYPES_EXPORT unsigned long fd_cons_usage();
DTYPES_EXPORT unsigned long fd_malloc_usage();

DTYPES_EXPORT void fd_describe_mallocation();

DTYPES_EXPORT void *fd_huge_malloc(size_t);
DTYPES_EXPORT void fd_huge_free(void *,size_t);



/* Full free functions: declaring and using */

DTYPES_EXPORT void fd_full_free(void);
DTYPES_EXPORT void fd_add_full_free_fn(void (*fn)(void));

/* File specific stuff */

/* The CVS log for this file
   $Log: fdmalloc.h,v $
   Revision 1.26  2005/03/07 23:56:59  haase
   Fixed mmap calls to have right size and offset

   Revision 1.25  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.24  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.23  2004/09/26 00:56:23  haase
   Added support for more detailed qmalloc debugging

   Revision 1.22  2004/09/17 08:28:23  haase
   Added recording of qmallcsc

   Revision 1.21  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.20  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.19  2003/10/20 12:08:50  haase
   Interface renaming (mostly) for malloc logging

   Revision 1.18  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.17  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.16.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.16.2.2  2002/09/26 02:12:50  haase
   Cleanups and addition of FD_QSTRINGP

   Revision 1.16.2.1  2002/08/09 16:39:35  haase
   Added option for compiling without block malloc

   Revision 1.16  2002/07/24 02:05:47  haase
   Removed 'new' symbols from include files to allow inclusion in C++ files

   Revision 1.15  2002/05/27 18:16:32  haase
   Added abstraction layer for thread-local data

   Revision 1.14  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.13  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.12  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
