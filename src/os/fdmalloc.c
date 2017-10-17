/* Mode: C */

/* fdmalloc.c
   This file implements malloc support functions.

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

static char vcid[] = "$Id: fdmalloc.c,v 1.45 2007/06/30 15:25:13 haase Exp $";

#include "dtypes.h"
#include <time.h>

#if ((HAVE_MMAP) && (HAVE_UNISTD_H))
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#endif

#if (HAVE_MMAP)
fd_exception fd_MmapFailed=_("mmap() call failed");
fd_exception fd_UnmapFailed=_("munmap() call failed");
#endif

#if HAVE_VALGRIND_H
#include <valgrind.h>
#endif

#define C256KBYTES (256*1024)
#define HUGE_MALLOC_SIZE C256KBYTES

#if ((!(defined(MAP_ANONYMOUS))) && (defined(MAP_ANON)))
#define MAP_ANONYMOUS MAP_ANON
#endif

static struct FD_MALLOC_BLOCK_RECORD *block_records=NULL;
static int n_blocks=0, max_blocks=0;
#if FD_THREADS_ENABLED
static fd_mutex block_lock;
#endif
int _fd_debugging_memory=0;

/* Support for malloc debugging */

#if (FD_LOG_MALLOC)
static FILE *memlog=NULL;
#if HAVE_FTIME
#include <sys/timeb.h>
static long int elapsed_time()
{
  struct timeb tb; ftime(&tb);
  return tb.time+tb.millitm*1000;
}
#else
static long int elapsed_time()
{
  return clock();
}
#endif
#endif

/* Recording malloc blocks */

DTYPES_EXPORT
void _fd_record_malloc_block(void *start,int size)
{
  fd_lock_mutex(&block_lock);	
  if (block_records == NULL) {
    block_records=
      fd_xmalloc(64*sizeof(struct FD_MALLOC_BLOCK_RECORD)); max_blocks=64;}
  else if (n_blocks >= max_blocks) {
    block_records=
      fd_xrealloc
      (block_records,2*max_blocks*sizeof(struct FD_MALLOC_BLOCK_RECORD));
    max_blocks=max_blocks*2;}
  block_records[n_blocks].start=start; block_records[n_blocks].size=size;
  n_blocks++;
  fd_unlock_mutex(&block_lock);	
}

DTYPES_EXPORT
int fd_check_qptr(void *ptr)
{
  int i=0;
  fd_lock_mutex(&block_lock);	
  while ((i < n_blocks) && (ptr>block_records[i].start) && (ptr-block_records[i].start<block_records[i].size)) i++;
  fd_unlock_mutex(&block_lock);
  return (i == n_blocks);
}

DTYPES_EXPORT void fd_invalid_qptr(void *ptr)
{
  fd_warn("Invalid qptr %lx",(long)ptr);
}

#if (FD_LOG_MALLOC)
static FILE *trace_malloc=NULL;
void _fd_start_malloc_trace(char *fname)
{
  if (fname == NULL)
    if (trace_malloc) trace_malloc=NULL;
    else trace_malloc=stderr;
  else trace_malloc=fopen(fname,"w");
}
#endif

#if FD_THREADS_ENABLED
static fd_mutex global_malloc_lock;
fd_tld_key _fd_malloc_data_key;
#endif

struct FD_MALLOC_DATA _fd_global_malloc_data;
#if FD_THREADS_ENABLED
static struct FD_MALLOC_DATA *all_malloc_data=NULL;
#endif

fd_exception
  fd_Out_Of_Memory=_("Malloc failed (probably out of memory)"),
  fd_HugeMalloc=_("Unreasonably huge malloc argument"),
  fd_ReallocFailed=_("Call to realloc failed");

#ifndef FD_MAX_MALLOC
#define FD_MAX_MALLOC 1000000000
#endif

/* Huge malloc and free, using memmap */

/* huge_malloc:
     Arguments: a size_t argument
     Returns: an array of size_t

  This may use OS specific methods (rather than the malloc library)
  to allocate a big chunk of memory. */
static void *huge_malloc(size_t sz)
{
#if ((HAVE_MMAP) && (HAVE_UNISTD_H) && (FD_WITH_MMAP_MALLOC) && (defined(MAP_ANONYMOUS)))
  void *result=
    mmap(NULL,sz,(PROT_READ|PROT_WRITE),(MAP_PRIVATE|MAP_ANONYMOUS),-1,0);
  if (result) return result;
  else fd_raise_exception(_("huge malloc failed"));
#elif ((HAVE_MMAP) && (HAVE_UNISTD_H) && (FD_WITH_MMAP_MALLOC))
  int fd=open("/dev/zero",O_RDWR,S_IRWXU);
  void *result=
    mmap(NULL,sz,(PROT_READ|PROT_WRITE),MAP_PRIVATE,fd,0);
  close(fd);
  if (result) return result;
  else fd_raise_exception(_("huge malloc failed"));
#else
  return malloc(sz);
#endif
}

/* huge_free:
     Arguments: a pointer and a size_t argument
     Returns: an array of size_t

   Frees a block of memory allocated by fd_huge_malloc.  */
static void huge_free(void *ptr,size_t sz)
{
#if ((HAVE_MMAP) && (HAVE_UNISTD_H) && (FD_WITH_MMAP_MALLOC))
  if (munmap(ptr,sz) < 0) {
    perror("munmap");
    fd_warn("huge_free %lx:%d failed",(long)ptr,(int)sz);}
#else
  free(ptr);
#endif
}

/* Maintaining malloc structures, especially per-thread */

static int global_malloc_copies_made=0;

DTYPES_EXPORT
/* fd_malloc_init:
     Arguments: two size_t pointers, struct_size and block_size
     Returns: void

Arranges for malloc tables to keep a free list of structs
with *struct_size* bytes and to allocate these structs
in blocks of *block_size* to reduce malloc overhead.

This will signal an error if any thread has already copied
the malloc data table.  This isn't neccessary, but just easy
to code right now.
*/
void fd_malloc_init(size_t sz,int chunk_size)
{
  struct FD_MALLOC_BUCKET *b;
  fd_lock_mutex(&(global_malloc_lock));
  if (_fd_global_malloc_data.buckets[sz/4]) {
    fd_unlock_mutex(&(global_malloc_lock));
    return;}
  if (global_malloc_copies_made) {
    fd_unlock_mutex(&(global_malloc_lock));
    fd_raise_exception("Can't add new malloc buckets");}
  b=fd_xmalloc(sizeof(struct FD_MALLOC_BUCKET));
#if FD_THREADS_ENABLED
  fd_init_mutex(&(b->lock)); b->shared=1;
#endif
  _fd_global_malloc_data.buckets[sz/4]=b;
  b->malloc_size=sz; b->n_chunks=chunk_size; b->n_used=0; b->n_free=0;
  b->used=NULL; b->fresh=NULL; b->last_fresh=NULL;
  fd_unlock_mutex(&(global_malloc_lock));
}

#if (FD_THREADS_ENABLED)
DTYPES_EXPORT
/* fd_use_threadlocal_malloc:
     Arguments: none
     Returns: nothing
  Arranges for the current thread to use its own malloc table.
This can greatly improve performance on SMP machines, since processors
do not have to share information on mallocs. */
void fd_use_threadlocal_malloc()
{
  struct FD_MALLOC_DATA *d=fd_tld_get(_fd_malloc_data_key);
  int i=0;
  if (d) return;
  fd_lock_mutex(&(global_malloc_lock));
  global_malloc_copies_made=1;
  fd_unlock_mutex(&(global_malloc_lock));
  d=fd_xmalloc(sizeof(struct FD_MALLOC_DATA));
  d->other=0; 
  /* Copy all the buckets */
  while (i < 16) 
    if (_fd_global_malloc_data.buckets[i]) {
      struct FD_MALLOC_BUCKET *b, *gb=_fd_global_malloc_data.buckets[i];
      fd_lock_mutex(&(gb->lock));
      b=fd_xmalloc(sizeof(struct FD_MALLOC_BUCKET)); b->shared=0;
      fd_init_mutex(&(b->lock));
      b->malloc_size=gb->malloc_size; b->n_chunks=gb->n_chunks;
#if 0
      b->n_used=0; b->n_free=gb->n_free; b->used=gb->used;
      gb->n_free=0; gb->used=NULL;
      b->fresh=NULL; b->last_fresh=NULL;
#else
      b->n_used=0; b->n_free=0; b->used=NULL;
      b->fresh=NULL; b->last_fresh=NULL;
#endif
      d->buckets[i]=b;
      fd_unlock_mutex(&(gb->lock));
      i++;}
    else d->buckets[i++]=NULL;
  fd_tld_set(_fd_malloc_data_key,d);
  fd_lock_mutex(&(global_malloc_lock));
  d->next=all_malloc_data; all_malloc_data=d; 
  fd_unlock_mutex(&(global_malloc_lock));
}
#endif

#if (FD_THREADS_ENABLED)
static void use_up_bucket(struct FD_MALLOC_BUCKET *b)
{
  if (b == NULL) return;
  else {
    int size=b->malloc_size;
    if (b->fresh)
      if (_fd_debugging_memory)
	while (b->fresh < b->last_fresh) {
	  struct FD_FREE_LIST *answer;
	  answer=(struct FD_FREE_LIST *) b->fresh;
	  b->fresh=((char *)b->fresh)+size;
	  b->n_free++; answer->next=b->used;
	  if (fd_check_qptr(answer))
	    fd_invalid_qptr(answer);
	  b->used=answer;}
      else while (b->fresh < b->last_fresh) {
	struct FD_FREE_LIST *answer;
	answer=(struct FD_FREE_LIST *) b->fresh;
	b->fresh=((char *)b->fresh)+size;
	b->n_free++; answer->next=b->used;
	b->used=answer;}}
}

static void _free_local_malloc(void *dv)
{
  struct FD_MALLOC_DATA *d=dv; int i;
  lock_mutex(&global_malloc_lock);
  i=0; while (i < 16) use_up_bucket(d->buckets[i++]);
  i=0; while (i < 16) 
    if (_fd_global_malloc_data.buckets[i]) {
      struct FD_MALLOC_BUCKET *b=_fd_global_malloc_data.buckets[i];
      struct FD_FREE_LIST *scan, *last;
      lock_mutex(&(b->lock));
      scan=_fd_global_malloc_data.buckets[i]->used; last=NULL;
      while (scan) {last=scan; scan=last->next;}
      if (last) last->next=d->buckets[i]->used;
      else _fd_global_malloc_data.buckets[i]->used=d->buckets[i]->used;
      _fd_global_malloc_data.buckets[i]->n_free=
	_fd_global_malloc_data.buckets[i]->n_free+d->buckets[i]->n_free;
      _fd_global_malloc_data.buckets[i]->n_used=
	_fd_global_malloc_data.buckets[i]->n_used+d->buckets[i]->n_used;
      unlock_mutex(&(b->lock));
      i++;}
    else i++;
  i=0; while (i < 16)
    if (d->buckets[i]) {fd_xfree(d->buckets[i]); i++;}
    else i++;
  {
    struct FD_MALLOC_DATA *scan=all_malloc_data, **last=&all_malloc_data;
    while (scan)
      if (scan == d) {*last=scan->next; break;}
      else {last=&(scan->next); scan=scan->next;}}
  _fd_global_malloc_data.other=_fd_global_malloc_data.other+d->other;
  unlock_mutex(&global_malloc_lock);  
  fd_xfree(d);
}
#endif

/* Mallocing while counting bytes */

#if FD_THREADS_ENABLED
STATIC_INLINE void malloc_adjust(int delta)
{
  struct FD_MALLOC_DATA *d=fd_tld_get(_fd_malloc_data_key);
  if (d) d->other=d->other+delta;
  else {
    lock_mutex(&global_malloc_lock);
    _fd_global_malloc_data.other=_fd_global_malloc_data.other+delta;
    unlock_mutex(&global_malloc_lock);}
}
#else
STATIC_INLINE void malloc_adjust(int delta)
{
  _fd_global_malloc_data.other=_fd_global_malloc_data.other+delta;
}
#endif

#if FD_CODDLE_MALLOC
STATIC_INLINE size_t roundup_size(size_t bytes)
{
  if (bytes < 16) return bytes;
  else if (bytes < 128) 
    if (bytes%4 == 0) return bytes;
    else return 4*((bytes/4)+1);
  else if (bytes <= 1024)
    if (bytes%32 == 0) return bytes;
    else return 32*((bytes/32)+1);
  else if (bytes <= 16384)
    if (bytes%1024 == 0) return bytes;
    else return 1024*((bytes/1024)+1);
  else 
    if (bytes%16384 == 0) return bytes;
    else return 16384*((bytes/16384)+1);
}
#else
#define roundup_size(x) (x)
#endif

#if (FD_LOG_MALLOC)
#include "fdmalloc-debug.c"
#else
DTYPES_EXPORT
/* fd_malloc:
     Arguments: number of bytes
     Returns: allocated memory to at least that many bytes

  This signals an exception if malloc fails.
*/
void *fd_malloc(size_t bytes)
{
  void *malloc_result; int malloc_size;
  if (bytes > FD_MAX_MALLOC) fd_raise_exception(fd_HugeMalloc);
  else if (bytes == 0) return NULL;
  else malloc_size=roundup_size(bytes);
  if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE))
    malloc_result=huge_malloc(malloc_size);
  else malloc_result=malloc(malloc_size);
  if (malloc_result) {
    malloc_adjust(bytes);
    return malloc_result;}
  /* else */ 
  else {
    perror("fd_malloc");
    fd_raise_exception(fd_Out_Of_Memory);}
  /* Should never be reached */
  return NULL;
}

DTYPES_EXPORT
/* fd_xmalloc:
     Arguments: number of bytes
     Returns: allocated memory to at least that many bytes

  This signals an exception if malloc fails.
  It doesn't count the malloc'd bytes, though...
*/
void *fd_xmalloc(size_t bytes)
{
  void *malloc_result; int malloc_size;
  if (bytes == 0) return NULL;
  else malloc_size=roundup_size(bytes);
  malloc_result=malloc(malloc_size);
  if (malloc_result) {
    return malloc_result;}
  else {
    perror("fd_xmalloc");
    fd_raise_exception(fd_Out_Of_Memory);
    /* Should never be reached */
    return NULL;}
}

DTYPES_EXPORT
/* fd_realloc:
     Arguments: a pointer and a number of bytes
     Returns: void

 Frees the pointer and updates the memory usage count
*/
void *fd_realloc(void *ptr,size_t new_size,size_t old_size)
{
  size_t malloc_size=roundup_size(new_size);
  size_t old_malloc_size=roundup_size(old_size);
  void *nptr;
  if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE)) {
    /* There may be a better way to do this */
    nptr=huge_malloc(malloc_size);
    if (nptr) memcpy(nptr,ptr,old_size);
    if (old_malloc_size >= HUGE_MALLOC_SIZE)
      huge_free(ptr,old_malloc_size);
    else free(ptr);}
  else nptr=realloc(ptr,malloc_size);
  if (nptr) {
    malloc_adjust(new_size-old_size);
    return nptr;}
  else fd_raise_exception(fd_ReallocFailed);
}

DTYPES_EXPORT
/* fd_xrealloc:
     Arguments: a pointer and a size
     Returns: returns a pointer to a memory chunk with at least size bytes
       which includes the data of the argument passed in

  This signals an exception if malloc fails.
  It doesn't count the malloc'd bytes
*/
void *fd_xrealloc(void *oldptr,size_t bytes)
{
  int new_size=roundup_size(bytes);
  void *malloc_result=realloc(oldptr,new_size);
  if (malloc_result) {
    return malloc_result;}
  /* else */ 
  else {
    perror("fd_xremalloc");
    fd_raise_exception(fd_Out_Of_Memory);}
  /* Should never be reached */
  return NULL;
}

DTYPES_EXPORT
/* fd_free:
     Arguments: a pointer and a number of bytes
     Returns: void

 Frees the pointer and updates the memory usage count
*/
void fd_free(void *ptr,size_t bytes)
{
  if (ptr == NULL)
    if (bytes == 0) return;
    else fd_raise_exception("Freeing NULL pointer");
  else {
    int malloc_size=roundup_size(bytes);
    if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE))
      huge_free(ptr,malloc_size);
    else free(ptr);
    malloc_adjust(-bytes);}
}

DTYPES_EXPORT
/* fd_xfree:
     Arguments: a pointer 
     Returns: void

 Doesn't signal an error of pointer is NULL
*/
void fd_xfree(void *ptr)
{
  if (ptr) free(ptr);
}
DTYPES_EXPORT
/* fd_qmalloc:
     Arguments: number of bytes
     Returns: allocated memory

  This maintains a free list for certain memory sizes and
   allocates them blocks at a time.
*/
void *_fd_qmalloc(size_t bytes)
{
  return fd_qmalloc(bytes);
}

DTYPES_EXPORT
/* _fd_qmalloc_cons:
     Arguments: number of bytes
     Returns: allocated memory

  This also initializes the reference count, assuming
that the result will be a struct whose first int field
is the reference count.
*/
void *_fd_qmalloc_cons(size_t bytes)
{
  return fd_qmalloc_cons(bytes);
}

DTYPES_EXPORT
/* fd_qfree:
     Arguments: a pointer and a number of bytes
     Returns: void

  This frees a cons allocated by fd_qmalloc which tries to
do free list maintainance.
*/
void _fd_qfree(void *p,size_t bytes)
{
  fd_qfree(p,bytes);
}

#endif /* else (FD_LOG_MALLOC) */

DTYPES_EXPORT
/* fd_malloc_adjust:
     Arguments: an int
     Returns: void

  Bumps up the malloc count but doesn't really malloc anything
*/
void fd_malloc_adjust(int delta)
{
  malloc_adjust(delta);
}

DTYPES_EXPORT
/* fd_free_int_array:
     Arguments: a pointer and number of bytes
     Returns: void

  This frees the pointer and bumps the malloc pointer down
   by a number rounded up to the size to FD_STRING_CHUNK.
*/
void fd_free_int_array(unsigned int *ptr,size_t size)
{
  fd_free(ptr,size*sizeof(unsigned int));
}

DTYPES_EXPORT
/* fd_cons_usage:
     Arguments: none
     Returns: an unsigned long
     Returns the amount of spaced used by consed structures
*/
unsigned long fd_cons_usage()
{
  unsigned long count=0; int i=0;
#if FD_THREADS_ENABLED
  struct FD_MALLOC_DATA *scan;
#endif
  struct FD_MALLOC_BUCKET **buckets=_fd_global_malloc_data.buckets;
  while (i < 16) {
    if (buckets[i]) {
      count=count+buckets[i]->n_used*buckets[i]->malloc_size; i++;}
    else i++;}
#if FD_THREADS_ENABLED
  scan=all_malloc_data;
  while (scan) {
    buckets=scan->buckets;
    i=0; while (i < 16)
      if (buckets[i]) {
	count=count+buckets[i]->n_used*buckets[i]->malloc_size; i++;}
      else i++;
    scan=scan->next;}
#endif
  return count;
}

DTYPES_EXPORT
/* fd_malloc_usage:
     Arguments: none
     Returns: an unsigned long
     Returns the amount of spaced used by fd_malloc'd structures
*/
unsigned long fd_malloc_usage()
{
  unsigned long count=_fd_global_malloc_data.other;
#if FD_THREADS_ENABLED
  struct FD_MALLOC_DATA *scan=all_malloc_data;
  while (scan) {count=count+scan->other; scan=scan->next;}
#endif
  return count;
}

static void describe_malloc_data(struct FD_MALLOC_DATA *d)
{
  int i=0;
  fprintf(stderr,";;; Malloc data %lx\n",(long)d);
  while (i < 16)
    if (d->buckets[i]) {
      fprintf(stderr,";;; %d-byte chunks: %d used, %d free\n",
	      d->buckets[i]->malloc_size,
	      d->buckets[i]->n_used,
	      d->buckets[i]->n_free);
      i++;}
    else i++;
  fprintf(stderr,";;; Other data: %ld bytes\n",d->other);
}

DTYPES_EXPORT
/* fd_describe_mallocation
     Arguments: none
     Returns: nothing
  Outputs a report on the current malloc structures
*/
void fd_describe_mallocation()
{
#if FD_THREADS_ENABLED
  struct FD_MALLOC_DATA *scan=all_malloc_data;
  describe_malloc_data(&_fd_global_malloc_data);
  while (scan) {
    describe_malloc_data(scan);
    scan=scan->next;}
#else
  describe_malloc_data(&_fd_global_malloc_data);
#endif
}

#if (HAVE_VALGRIND_H)
DTYPES_EXPORT
/* fd_vcheck_range
     Arguments: a tag (string), a pointer and a number of bytes
     Returns: nothing
  When running under valgrind, reports if the designated range
is either unwritable or uninitialized.
*/
void fd_vcheck_range(char *tag,void *data,int size)
{
  unsigned long unreadable, unwritable;
  unreadable=VALGRIND_CHECK_READABLE(data,size);
  unwritable=VALGRIND_CHECK_WRITABLE(data,size);
  if ((unreadable) && (unwritable) && (unreadable != unwritable)) 
    fprintf(stderr,"%s: %lx:%d uninitialized @%lx, unallocated @%lx\n",
	    tag,data,size,unreadable,unwritable);
  else if (unwritable)
    fprintf(stderr,"%s: %lx:%d unallocated @%lx\n",tag,data,size,unwritable);
  else if (unreadable)
    fprintf(stderr,"%s: %lx:%d uninitialized @%lx\n",tag,data,size,unreadable);
}
#endif

/* Error-checking versions of strdup and realloc */

#if (FD_LOG_MALLOC)
DTYPES_EXPORT
/* fd_strdup:
   Arguments: a null terminated string
   Returns: a null terminated string
 Allocates a new string which is a copy of its argument.
 This does not count towards the global memory counts.
*/
char *fd_strdup_debug(const char *string,char *filename,int lineno)
{
  char *copy=fd_xmalloc_debug(strlen(string)+1,filename,lineno);
  strcpy(copy,string);
  return copy;
}
#elif (!(HAVE_STRDUP))
DTYPES_EXPORT
/* fd_strdup:
   Arguments: a null terminated string
   Returns: a null terminated string
 Allocates a new string which is a copy of its argument.
 This does not count towards the global memory counts.
*/
char *fd_strdup(const char *string)
{
  char *copy=fd_xmalloc(strlen(string)+1);
  strcpy(copy,string);
  return copy;
}
#else
DTYPES_EXPORT
/* fd_strdup:
   Arguments: a null terminated string
   Returns: a null terminated string
 Allocates a new string which is a copy of its argument.
 This does not count towards the global memory counts.
*/
char *fd_strdup(const char *string)
{
  char *copy=strdup(string);
  if (copy) return copy;
  else {
    perror("fd_strdup");
    fd_raise_exception(fd_Out_Of_Memory);}
}
#endif

DTYPES_EXPORT
/* fd_mallocize:
   Arguments: a pointer to a chunk of memory and a size
   Returns: a pointer
 Returns a pointer which can be used with the fd_malloc/realloc/free
functions.  This does two things: if the size is large enough to for
calling huge_malloc, it is called, the strings contents is copied, and
the original chunk is freed; in addition, in either the case of a copy or a
pass-through fd_malloc_adjust is called to record the memory take up
by the chunk of memory.
*/
char *fd_mallocize(char *data,size_t sz)
{
  int malloc_size=roundup_size(sz);
  if (malloc_size >= HUGE_MALLOC_SIZE) {
    char *copy=fd_malloc(sz);
    memcpy(copy,data,sz); free(data);
    return copy;}
  else {
    fd_malloc_adjust(sz); return data;}
}

DTYPES_EXPORT
/* fd_memdup:
   Arguments: a pointer to a chunk of memory and a size
   Returns: a string
 Allocates a new string which is a copy of its argument
 and has the corresponding size.  This will work with
 strings that contain NULLs.
 This will count towards global memory counts and use huge_malloc
  if neccessary. */
char *fd_memdup(const char *data,size_t sz)
{
  char *copy=fd_malloc(sz);
  memcpy(copy,data,sz);
  return copy;
}

DTYPES_EXPORT
/* fd_xmemdup:
   Arguments: a pointer to a chunk of data and a size
   Returns: a string
 Allocates a new string which is a copy of its argument
 and has the corresponding size.  This will work with
 strings that contain NULLs.
 This does not count towards the global memory counts.
*/
char *fd_xmemdup(const char *data,size_t sz)
{
  char *copy=fd_xmalloc(sz+1);
  memcpy(copy,data,sz+1);
  return copy;
}

/* fd_huge_malloc:
     Arguments: a size_t argument
     Returns: an array of size_t

  This may use OS specific methods (rather than the malloc library)
  to allocate a big chunk of memory. */
DTYPES_EXPORT void *fd_huge_malloc(size_t sz)
{
  void *result;
  if (sz == 0) return NULL;
  else result=huge_malloc(sz);
  if (result) return result;
  else fd_raise_exception(_("fd_huge_malloc failed"));
}

/* fd_huge_free:
     Arguments: a pointer and a size_t argument
     Returns: an array of size_t

   Frees a block of memory allocated by fd_huge_malloc.  */
DTYPES_EXPORT void fd_huge_free(void *ptr,size_t sz)
{
  void *result;
  if (sz == 0) return;
  else huge_free(ptr,sz);
}

/* Full Free */

struct FULL_FREE_FN {
  void (*fn)(void);
  struct FULL_FREE_FN *next;};
static struct FULL_FREE_FN *full_free_fns=NULL;

DTYPES_EXPORT
/* fd_full_free:
     Arguments: none
     Returns: void
  Tries to free all data structures to help with leak detection. */
void fd_full_free()
{
  struct FULL_FREE_FN *scan=full_free_fns, *last=NULL;
  while (scan) {
    if (last) free(last);
    scan->fn(); last=scan; scan=scan->next;}
}

DTYPES_EXPORT
/* fd_add_full_free_fn:
     Arguments: a free function
     Returns: void
  Tries to free all data structures to help with leak detection. */
void fd_add_full_free_fn(void (*fn)(void))
{
  struct FULL_FREE_FN *nfn=malloc(sizeof(struct FULL_FREE_FN));
  nfn->fn=fn; nfn->next=full_free_fns; full_free_fns=nfn;
}

/* Keeps track of in use qmallocs, supposedly over a short period. */

struct FD_QMALLOC_RECORD *_fd_qmalloc_records=NULL;
int _fd_recording_qmallocs=0;

DTYPES_EXPORT
void _fd_record_qmalloc(void *p,int size,char *file,int line)
{
  struct FD_QMALLOC_RECORD *r=malloc(sizeof(struct FD_QMALLOC_RECORD));
  r->ptr=p; r->size=size; r->file=file; r->line=line;
  r->next=_fd_qmalloc_records;
  _fd_qmalloc_records=r;
}

DTYPES_EXPORT
void _fd_record_qfree(void *p)
{
  struct FD_QMALLOC_RECORD *scan=_fd_qmalloc_records, *prev;
  if (scan->ptr == p) {
    _fd_qmalloc_records=scan->next;
    free(scan);}
  else {
    prev=scan; scan=scan->next;
    while (scan)
      if (scan->ptr == p) break; else {prev=scan; scan=scan->next;}
    if (scan) {prev->next=scan->next; free(scan);}}
}

/* fd_initialize_fdmalloc_c
     Arguments: none
     Returns: nothing
  Initializes fdmalloc structures
*/
void fd_initialize_fdmalloc_c ()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&global_malloc_lock);
  fd_init_mutex(&block_lock);
  fd_new_tld_key(&_fd_malloc_data_key,_free_local_malloc);
#endif  
  _fd_global_malloc_data.other=0;

  fd_malloc_init(sizeof(struct FD_SLOTMAP),1024);
  fd_malloc_init(sizeof(struct FD_STRING),1024);
  fd_malloc_init(sizeof(struct FD_PAIR),4096);
  fd_malloc_init(sizeof(struct FD_CHOICE),1024);
  fd_malloc_init(sizeof(struct FD_SPROC),1024);
  fd_malloc_init(sizeof(struct FD_SYMBOL),256);
#if (!(FD_OIDS_ARE_SCALARS))
  fd_malloc_init(sizeof(struct FD_OID),4096);
#endif
  fd_malloc_init(sizeof(struct FD_CPTR),4096);
  fd_malloc_init(sizeof(struct FD_RECORD),2048);
  fd_malloc_init(sizeof(struct FD_LRECORD),2048);


  if (getenv("FD_MEMDEBUG")) _fd_debugging_memory=1;

#if (FD_LOG_MALLOC)
  {
    char *malloc_trace_file=getenv("FD_MALLOC_LOG");
    if (malloc_trace_file) {
      memlog=fopen(malloc_trace_file,"w");
      fd_warn("Writing malloc trace data to %s",malloc_trace_file);}
  }
#endif

  fd_register_source_file("fdmalloc",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdmalloc.c,v $
   Revision 1.45  2007/06/30 15:25:13  haase
   Bumped up the max malloc

   Revision 1.44  2005/03/07 23:57:33  haase
   Fixed mmap calls to have right size and offset

   Revision 1.43  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.42  2005/01/14 15:15:45  haase
   Fix combo declaration and binding

   Revision 1.41  2004/10/19 22:12:24  haase
   Added malloc unlock

   Revision 1.40  2004/09/26 00:56:23  haase
   Added support for more detailed qmalloc debugging

   Revision 1.39  2004/09/17 08:28:28  haase
   Added recording of qmallcsc

   Revision 1.38  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.37  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.36  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.35  2004/05/04 23:58:22  haase
   Upped the max malloc

   Revision 1.34  2003/10/20 12:08:51  haase
   Interface renaming (mostly) for malloc logging

   Revision 1.33  2003/10/06 11:05:26  haase
   Added support for detailed malloc debugging

   Revision 1.32  2003/10/05 06:42:43  haase
   Moved setting of all_malloc_data inside global malloc loc

   Revision 1.31  2003/10/01 09:50:13  haase
   Fixed missplaced lock in memory allocation

   Revision 1.30  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.29.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.29.2.1  2002/08/09 16:39:35  haase
   Added option for compiling without block malloc

   Revision 1.29  2002/07/24 02:05:47  haase
   Removed 'new' symbols from include files to allow inclusion in C++ files

   Revision 1.28  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.27  2002/05/19 13:22:20  haase
   Make more platforms use anonymous mmap

   Revision 1.26  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.25  2002/05/13 15:20:36  haase
   Better diagnostic on huge_free failure

   Revision 1.24  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.23  2002/04/29 20:17:38  haase
   Fixed error in last commit to fdmalloc.c

   Revision 1.22  2002/04/29 20:14:45  haase
   Added roundup of old size to fd_realloc

   Revision 1.21  2002/04/29 17:57:58  haase
   Fixed huge leak with huge realloc

   Revision 1.20  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.19  2002/04/03 19:07:28  haase
   Fixed huge malloc for Darwin by using MAP_ANON

   Revision 1.18  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
