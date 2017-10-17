/* C Mode */

/* file-pool.c
   Implements the disk files which store pools of OIDs.
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

static char vcid[] = "$Id: file-pool.c,v 1.69 2005/05/21 18:04:47 haase Exp $";

/** OVERVIEW **/
/** Declarations and inclusions **/
/** Variables and control functions **/
/** Opening and closing file pools **/
/** Locking and Unlocking file pools **/
/** Creating file pools **/
/** Caching File Pools **/
/** Making new OIDs **/
/** Commiting file pools **/
/** Fetching from file pools **/
/** Iterating over file pools **/
/** Maintenance functions **/
/** Initialization **/

/** OVERVIEW **/

/*
   File pools are a binary file format for storing FRAMERD OIDs,
   supporting fast retrieval of FD_OID vlues and efficient generation of new
   OIDs.

   FRAMERD OIDs are uniquely identified by a 64 bit identifier; a file
   pool stores values associated some contiguous range of identifiers.
   The lowermost identifier is called the "base" of the pool; the number
   of identifiers in the range is called the "capacity" of the pool.

   Usually, only a subset of a pool's identifiers are allocated; the allocated
   identifiers also form a contiguous block, starting at the base of the
   pool.  The number of allocated identifers is the "load" of the pool.

   The format of a file pool (on disk) consists of three segments:
   a header, an offset index, and a data segment.  They are laid
   out as follows (X=one byte):

   0   XXXX      "Magic number" for file pools      |
   4   XXXXXXXX  The pool's "base" identifier       |
   12  XXXX      Capacity of the pool (= k)         |- header
   16  XXXX      Load of the pool                   |
   20  XXXX      Offset (in the file)               |
                  of a descriptive header           |

   24  (XXXX)                                       |
       (XXXX)                                       |- offset
       (XXXX)                                       |- index
       (XXXX)                                       |- table
       (XXXX)                                       |
       ......                                       .
       ......                                       .
       ......                                       .

24+4K  FFFFF-n   metadatablock start code,          |
                   where n indicates version        |
       XXXX   length (in bytes of metadata block)   |
       XXXX   repack serial number                  |
       XXXX   time of creation (8 bytes)            |
       XXXX                                         |- meta data
       XXXX   time of last repack (8 bytes)         |- block
       XXXX                                         |
       XXXX   time of modification (8 bytes)        |
       XXXX                                         |
       XXXX   pointer to metdata dtype              |
       
24+4k  <dtype representation>                       |
       <dtype representation>                       |- data
       <dtype representation>                       |- segment
       ......................                       .
       ......................                       .
       ......................                       .

   Basic operations on file pools are fetching an OIDs value, storing
   an OIDs value, and allocating a new FD_OID.

*/

#include "framerd.h"

/** Declarations and inclusions **/

#include <assert.h>
#include <limits.h>

#ifdef WIN32 /* For getcwd */
#include <direct.h>
#include <process.h>
#endif
#ifndef PATH_MAX
#define PATH_MAX 512
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#if HAVE_MMAP
#include <sys/mman.h>
#define MMAP_FLAGS (MAP_SHARED|MAP_NORESERVE)
#endif

#if FD_THREADS_ENABLED
static fd_mutex _file_pool_lookup_lock;
#endif

static struct FD_FILE_POOL_OPENER *pool_openers=NULL;

#define get_store(p) ((p->store) ? (p->store) : open_store(p))

#if (!(WORDS_BIGENDIAN))
#define offget(offsets,i) (fd_flip_word(offsets[i]))
#else
#define offget(offsets,i) (offsets[i])
#endif

static off_t fread_off_t(FILE *f)
{
  return fd_fread_4bytes(f);
}

static struct FD_POOL_HANDLER *get_pool_handler(unsigned int magic_no)
{
  struct FD_FILE_POOL_OPENER *scan=pool_openers;
  while (scan)
    if (scan->magic_number == magic_no)
      return scan->handler;
    else scan=scan->next;
  return NULL;
}

static struct FD_POOL_HANDLER file_pool_handler;

/* Utility function */
static int in_poolp(FD_OID id,fd_pool p)
{
  return FD_OID_IN_RANGE(id, p->base, p->capacity);
}

/** Variables and control functions **/

fd_exception File_Not_Readable=_("Couldn't open file for reading");
fd_exception File_Pool_Overflow=_("File pool size overflow");
FRAMERD_EXPORT int _fd_auto_cache_file_pools=0;

static fd_exception FutureMetaData=
  _("File pool metadata is from far future (time_t overflow)");

#define CHECK_POS(pos,id) \
  if (pos > UINT_MAX) fd_raise_detailed_exception(File_Pool_Overflow,id)

FRAMERD_EXPORT
/* fd_auto_cache_file_pools:
     Arguments: none
     Returns: void

 Sets flag causing file pools to automatically cache their
 offset tables. */
void fd_auto_cache_file_pools()
{
  _fd_auto_cache_file_pools=1;
}

static lisp file_pool_new_oid(fd_pool fp);
static lisp file_pool_fetch(fd_pool fp,lisp oid);
static unsigned int file_pool_get_load(fd_pool fp);
static void file_pool_commit(fd_pool fp);
static void file_pool_commit_oid(fd_pool fp,lisp obj);
static void file_pool_close(fd_pool fp);
static void file_pool_locker(fd_pool p,lisp oid,int action);
static void lock_file_pool(fd_file_pool p);
static int file_pool_prefetch(fd_pool,fd_lisp *,fd_lisp *,int);
static void grow_offsets(struct FD_FILE_POOL *fp);
static void unlock_file_pool(fd_file_pool p);
static void cache_file_pool_unlocked(fd_file_pool fp);

static void write_size_as_revminor(FILE *f,fd_u8char **revminor)
{
  char buf[128]; off_t file_size, opos=ftello(f);
  fseek(f,0,SEEK_END); file_size=ftello(f); fseeko(f,opos,SEEK_SET);
  sprintf(buf,"%llu",file_size);
  *revminor=fd_strdup(buf);
}

FRAMERD_EXPORT
/* fd_read_file_pool_metadata:
     Arguments: an open file stream to a file pool and pointers to two ints
     Returns: a lisp object (or the empty choice) 
  Returns metadata and version information for a file pool.
     The version information, consisting of a serial repack ID and
  a file length, are written into the two integer  pointers.
     Since modifications always write at the end of the file,
  the repack serial number and the length of the file uniquely
  identify a moment in time for the file.  */
fd_lisp fd_read_file_pool_metadata
   (FILE *f,int *revmajor,fd_u8char **revminor,time_t *make,time_t *repack,time_t *change)
{
  unsigned int probe, magic_no;
  int n_slots, c; off_t opos=ftello(f);
  struct FD_POOL_HANDLER *h;
  fseek(f,0,SEEK_SET);
  magic_no=fd_fread_4bytes(f);
  fseeko(f,opos,SEEK_SET);
  if (h=get_pool_handler(magic_no))
    return h->read_metadata(f,revmajor,revminor,make,repack,change);
  else fd_raise_exception(fd_NotAFilePool);
}

FRAMERD_EXPORT
/* fd_read_std_file_pool_metadata:
     Arguments: an open file stream to a file pool and pointers to two ints
     Returns: a lisp object (or the empty choice) 
  Returns metadata and version information for a file pool.
     The version information, consisting of a serial repack ID and
  a file length, are written into the two integer  pointers.
     Since modifications always write at the end of the file,
  the repack serial number and the length of the file uniquely
  identify a moment in time for the file.  */
fd_lisp fd_read_std_file_pool_metadata
   (FILE *f,int *revnum,fd_u8char **revminor,time_t *make,time_t *repack,time_t *change)
{
  int n_slots, c, fileptr_size=4;
  off_t opos=ftello(f);
  unsigned int probe, magic_no;
  fseek(f,0,SEEK_SET); magic_no=fd_fread_4bytes(f);
  fseek(f,12,SEEK_SET); n_slots=fd_fread_4bytes(f);
  fseek(f,(24+n_slots*fileptr_size),SEEK_SET); c=fgetc(f);
  /* If there isn't any metadata, just return the size. */
  if ((c < 0) && (feof(f))) {
    if (revnum) *revnum=0; 
    if (revminor) write_size_as_revminor(f,revminor);
    fseeko(f,opos,SEEK_SET);
    return FD_EMPTY_CHOICE;}
  /* Go to the beginning of the metadata */
  fseeko(f,(off_t)(24+n_slots*fileptr_size),SEEK_SET);
  probe=fd_fread_4bytes(f);
  if (probe == 0xFFFFFFFF) { /* Version 1 */
    off_t md_loc; time_t notime=(time_t) -1;
    fd_lisp metadata=FD_EMPTY_CHOICE;
    /* Write the time information (actually, its absence in this version). */
    if (make) *make=notime;
    if (repack) *repack=notime;
    if (change) *change=notime;
    md_loc=fread_off_t(f); if (md_loc) {
      fseeko(f,md_loc,SEEK_SET); metadata=fd_fread_dtype(f);}
    if (revnum) *revnum=fd_fread_4bytes(f); else {fd_fread_4bytes(f);}
    if (revminor) write_size_as_revminor(f,revminor);
    fseeko(f,opos,SEEK_SET);
    if (FD_VOIDP(metadata)) return FD_EMPTY_CHOICE;
    else return metadata;}
  else if (probe == 0xFFFFFFFE) { /* Version 2 */
    fd_lisp metadata=FD_EMPTY_CHOICE;
    unsigned int block_size;
    off_t md_loc;
    block_size=fd_fread_4bytes(f);
    if (revnum) *revnum=fd_fread_4bytes(f); else fd_fread_4bytes(f);
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (make) *make=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (repack) *repack=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (change) *change=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    md_loc=fread_off_t(f);
    if (md_loc) {
      fseeko(f,md_loc,SEEK_SET); metadata=fd_fread_dtype(f);}
    if (revminor) write_size_as_revminor(f,revminor);
    fseeko(f,opos,SEEK_SET);
    if (FD_VOIDP(metadata)) return FD_EMPTY_CHOICE;
    else return metadata;}
  else {
    time_t notime=(time_t) -1;
    if (revnum) *revnum=0;
    if (revminor) write_size_as_revminor(f,revminor);
    /* Write the time information (actually, its absence in this version). */
    if (make) *make=notime;
    if (repack) *repack=notime; if (change) *change=notime;
    fseeko(f,opos,SEEK_SET);
    return FD_EMPTY_CHOICE;}
}

static void update_file_pool_change_date(fd_file_pool fp)
{
  FILE *f; int code;
  if (fp->store == NULL) {
    return;}
  else f=fp->store;
  fseek(f,24+4*fp->capacity,SEEK_SET);
  if (fread_4bytes(f) == 0xFFFFFFFF) {
    fd_warn("File pool version doesn't change dates");
    return;}
  fseek(f,24+4*fp->capacity+7*4,SEEK_SET);
  fwrite_4bytes(0,f); fwrite_4bytes((int)time(NULL),f);
  fflush(f);
}

FRAMERD_EXPORT
/* fd_store_file_pool_metadata:
     Arguments: an open file stream to a file index and lisp pointer
     Returns: void
*/
void fd_store_file_pool_metadata(FILE *f,fd_lisp metadata)
{
  int magic_no, fileptr_size=4; time_t make, repack, change;
  int n_slots, c; unsigned int probe; off_t md_loc;
  fseek(f,0,SEEK_END); md_loc=ftello(f); fd_fwrite_dtype(metadata,f);
  fseek(f,0,SEEK_SET);
  magic_no=fd_fread_4bytes(f); n_slots=fd_fread_4bytes(f);
  if (get_pool_handler(magic_no) == NULL)
    fd_raise_exception(_("Not a valid file pool"));
  fseek(f,24+n_slots*4,SEEK_SET); probe=fd_fread_4bytes(f);
  if (probe == 0xFFFFFFFF) { /* Version 1 */
    fseek(f,4,SEEK_CUR); /* Skip repack number */
    fd_fwrite_4bytes(md_loc,f);}
  else if (probe == 0xFFFFFFFE) { /* Version 2 */
    fseek(f,20,SEEK_CUR); /* Skip block size, repack number, and time info */
    fd_fwrite_4bytes(md_loc,f);}
}

/** Searching for an existing file pool **/

struct FP_SEARCH {char *filename; fd_pool result;};

static void search_for_current_pool(fd_pool p,void *ptr)
{
  struct FP_SEARCH *fps=ptr;
  if (fps->result) return;
  else if (p->type == file_pool) {
    fd_file_pool fp=(fd_file_pool)p;
    if ((strcmp(fp->filename,fps->filename)) == 0)
      fps->result=p;}
}

static fd_pool find_existing_file_pool(fd_u8char *filename)
{
  struct FP_SEARCH fps;
  fps.filename=fd_get_real_pathname(filename); fps.result=NULL;
  fd_for_pools(search_for_current_pool,(void *)&fps);
  fd_xfree(fps.filename);
  return fps.result;
}

/** Opening and closing file pools **/

/* open_file_pool:
      arguments: a string encoding a filename
      returns: a file pool
  This calls the opening function associated with the first word
  of the file. */
static fd_pool open_file_pool(fd_u8char *filename)
{
  FILE *f; int magic_no; 
  struct FD_FILE_POOL_OPENER *scan=pool_openers;
  if (fd_directoryp(filename))
    fd_raise_detailed_exception(fd_NotAFilePool,filename);
  f=fd_fopen(filename,"rb"); magic_no=fread_4bytes(f); fd_fclose(f);
  while (scan)
    if (scan->magic_number == magic_no)
      return scan->opener(filename);
    else scan=scan->next;
  fd_raise_detailed_exception(fd_NotAFilePool,filename);
}

FRAMERD_EXPORT
/* fd_register_file_pool_opener:
      Arguments: a magic number and an opening function
      Returns: void
  This associates an opening function with a number which
  is the first word of the file to use this opener.
*/
void fd_register_file_pool_opener
  (unsigned int magic_no,struct FD_POOL_HANDLER *h,fd_pool (*opener)(fd_u8char *c))
{
  struct FD_FILE_POOL_OPENER *new=fd_xmalloc(sizeof(struct FD_FILE_POOL_OPENER));
  new->magic_number=magic_no; new->handler=h; new->opener=opener;
  new->next=pool_openers;
  pool_openers=new;
}

/* open_std_file_pool
      arguments: a string encoding a filename
      Returns: a pointer to a structure describing a file pool

   Note: this does not add the file pool to the table of pools.
*/
static fd_pool open_std_file_pool(fd_u8char *filename)
{
  fd_file_pool new = fd_xmalloc(sizeof(struct FD_FILE_POOL));
  FD_OID base;
  int magic_number, read_only;
  off_t label_offset;
  FILE *store; 

  TIDY_ERRNO("open_file_pool preamble");
  if (fd_file_writablep(filename))
    read_only=FD_POOL_LOCKABLE;
  else read_only=FD_POOL_READ_ONLY;
  if ((store = fd_fopen (filename, "rb")) == NULL) 
    fd_raise_detailed_exception(File_Not_Readable,filename);

  /*check FD_FILE_POOL_MAGIC_NUMBER*/
  magic_number=fread_4bytes (store);
  if (magic_number == (unsigned) FD_FILE_POOL_MAGIC_NUMBER) {}
  else fd_raise_detailed_exception(fd_NotAFilePool,filename);

  FD_SET_OID_HIGH(base,fread_4bytes(store));
  FD_SET_OID_LOW(base,fread_4bytes(store));

  fd_init_pool_data((fd_pool)new,file_pool,
		    base,fread_4bytes(store),read_only,
		    fd_strdup(filename),NULL,FD_VOID);
  new->load=fread_4bytes(store); 
  label_offset=fread_4bytes(store);
  if (label_offset != 0) {
    fseeko(store,label_offset,SEEK_SET); new->label=fd_fread_dtype(store);}
  else new->label=FD_EMPTY_CHOICE;

#if FD_THREADS_ENABLED
  fd_init_mutex(&(new->lock));
#endif

  new->filename=fd_get_real_pathname(filename);
  /* reading stops at the beginning of table of pointers to data*/
  new->offsets = NULL; new->store=NULL; fclose(store); new->buf=NULL;
  TIDY_ERRNO("open_file_pool");

  new->handler=&file_pool_handler;

  fd_register_pool((fd_pool)new);

  return (fd_pool) new;
}

/* file_pool_close
      Arguments: a pointer to a file pool structure
      Returns: void
   Closes a file pool, writing out any changes to its cache
    and closing the corresponding stream.
*/
static void file_pool_close (fd_pool p)
{
  fd_file_pool fp=(fd_file_pool)p;
  lock_mutex(&(fp->lock));
  if (!(fp->modifiedp)) {
    if (fp->store) {
      if (fp->read_only == FD_POOL_WRITABLE) fflush(fp->store);
      fclose(fp->store); fp->store=NULL;}
    unlock_mutex(&(fp->lock));
    return;}
  /*rewrite the head of the file_pool*/
  fseek (fp->store, (off_t)0, SEEK_SET);
  fwrite_4bytes (FD_FILE_POOL_MAGIC_NUMBER, fp->store);
  fwrite_4bytes (FD_OID_HIGH(fp->base), fp->store);
  fwrite_4bytes (FD_OID_LOW(fp->base), fp->store);
  fwrite_4bytes (fp->capacity, fp->store);
  fwrite_4bytes (fp->load, fp->store);
  if (fp->offsets)
#if HAVE_MMAP
    if (fp->read_only == FD_POOL_READ_ONLY) {
      int retval=munmap(fp->offsets-6,(sizeof(unsigned int)*fp->load)+24);
      if (retval<0)
	fd_raise_detailed_exception(fd_UnmapFailed,strerror(errno));
      fp->offsets=NULL;} else
#endif
	{
	  unsigned int *offsets=fp->offsets, i=0, load=fp->load;
	  FILE *stream=fp->store;
	  fseek(stream,24,SEEK_SET);
	  fwrite(offsets,sizeof(unsigned int),load,stream);}
  if (fp->buf) free(fp->buf);
  fp->buf=NULL; fp->modifiedp=0;
  fflush(fp->store); fclose(fp->store); fp->store=NULL;
  unlock_mutex(&(fp->lock));
}

static FILE *open_store(fd_file_pool p)
{
  if (p->store) return p->store;
  else {
    FILE *store; unsigned int load_on_disk; int bigbuf;
    TIDY_ERRNO("file_pool open_store preamble");
    if ((p->read_only == FD_POOL_READ_ONLY) || (p->read_only == FD_POOL_LOCKABLE))
      store=fd_fopen(p->filename,"rb");
    else if ((store=fd_fopen(p->filename,"r+b")) == NULL) {
      fd_warn("Couldn't write to file pool %s",p->filename);
      p->read_only=FD_POOL_READ_ONLY; store=fd_fopen(p->filename,"rb");
      CLEAR_ERR();}
    if (errno) {perror("File Pool open"); CLEAR_ERR();}
    if (store == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,p->filename);
    p->buf=fd_get_big_buffer(&bigbuf);
    if (p->buf) setvbuf(store,p->buf,_IOFBF,bigbuf);
    if (p->read_only) {
      fseek(store,16,SEEK_SET); load_on_disk=fread_4bytes(store);
      if (load_on_disk < p->load)
	fd_raise_exception("In memory load too big");
      p->load=load_on_disk;}
    if (p->offsets) {
      fseek(store,0,SEEK_END);
      p->end_pos=ftello(store);}
    p->store=store;
    TIDY_ERRNO("file_pool open_store");
    return store;}
}

/** Locking and Unlocking file pools **/

static void file_pool_locker(fd_pool p,lisp id,int action)
{
  fd_file_pool fp=(fd_file_pool)p;
  if (action == FD_POOL_UNLOCK_OID) {
    if (p->modified.n_keys)
      fd_raise_detailed_exception(_("Can't unlock modified pool"),p->id);
    else return;}
  else if (action == FD_POOL_LOCK_OID) {
    if (p->read_only == FD_POOL_WRITABLE) return;
    else if (p->read_only == FD_POOL_READ_ONLY)
      fd_raise_detailed_exception(fd_FileLockFailed,p->id);
    else {
      lock_mutex(&(fp->lock));
      lock_file_pool(fp);
      unlock_mutex(&(fp->lock));}}
}

/* lock_file_pool:
     Arguments: a pointer to a file pool
     Returns: void
  Attempts to lock the file storing pool.
  This signals an error if the file cannot be locked.
  *THIS IS NOT THREADSAFE*
  The file pool's mutex (->lock) must be locked before it is called. 
*/
static void lock_file_pool(fd_file_pool p)
{
  int load_on_disk, bufsiz; 
  TIDY_ERRNO("lock_file_pool preamble");
  if (p->read_only == FD_POOL_WRITABLE) return;
  else if (p->read_only == FD_POOL_READ_ONLY) {
    unlock_mutex(&(p->lock));
    fd_raise_detailed_exception(fd_FileLockFailed,p->filename);}
  if (p->store) fd_fclose(p->store);
  p->store=NULL; p->store=fd_fopen_locked(p->filename,"r+b",1);
  if (p->store == NULL) {
    unlock_mutex(&(p->lock));    
    fd_raise_detailed_exception(fd_FileLockFailed,p->filename);}
  else p->read_only=FD_POOL_WRITABLE;
  /* Set the file buffer to be big */
  p->buf=fd_get_big_buffer(&bufsiz);
  if (p->buf) setvbuf(p->store,p->buf,_IOFBF,bufsiz);
  /* Since we are reopening and didn't have the lock before, values
     that we had cached before (load,offsets, etc) may have changed. */
  fseek(p->store,16,SEEK_SET);
  /* Read the new load */
  load_on_disk=fread_4bytes(p->store);
  if (p->load > load_on_disk)
    fd_raise_exception("In memory load too big");
  else p->load=load_on_disk;
  /* If you have offsets cached, reload them and erase any OIDs whose
     offsets in the file have changed. */
  if ((p->offsets) && (p->read_only != FD_POOL_READ_ONLY)) {
    int offsize=((p->load>p->offsets_size) ? (p->load) : (p->offsets_size));
    unsigned int *new_offsets=fd_malloc(sizeof(unsigned int)*offsize);
    unsigned int *old_offsets=p->offsets;
    int i=0, limit=p->load;
    /* Zero out the offsets */
    memset(new_offsets,0,sizeof(unsigned int)*offsize);
    /* Read the new offsets, and normalize them */
    fseek(p->store,24,SEEK_SET); 
    fread(new_offsets,sizeof(unsigned int),limit,p->store);
    /* If any offsets have changed, figure out what FD_OID it would be
       and reset its value (if any) */
    i=0; while (i < limit) {
      if ((new_offsets[i]) && (new_offsets[i] != old_offsets[i])) {
	FD_OID new=p->base; lisp oid;
	FD_SET_OID_LOW(new,FD_OID_LOW(new)+i);
#if (FD_LIGHTWEIGHT_OIDS)	
	oid=fd_make_oid(new);
#else
	oid=fd_probe_oid(new);
#endif
	fd_warn("Erasing value of %q",oid);
	if (OIDP(oid)) {
	  fd_store_oid_value(oid,FD_VOID);}}
      i++;}
    /* Clean up */
    fd_free_int_array(p->offsets,p->offsets_size);
    p->offsets=new_offsets; p->offsets_size=offsize;
    /* Move to the end and set the end_pos field */
    fseek(p->store,0,SEEK_END); p->end_pos=ftello(p->store);}
  TIDY_ERRNO("lock_file_pool");
}

FRAMERD_EXPORT
/* fd_lock_file_pool:
     Arguments: a pointer to a file pool
     Returns: 1 on success
  Attempts to lock the file storing pool. */
int fd_lock_file_pool(fd_file_pool p)
{
  lock_mutex(&(p->lock));
  lock_file_pool(p);
  unlock_mutex(&(p->lock));
  return 1;
}

/* unlock_file_pool:
     Arguments: a pointer to a file pool
     Returns: void
  Unlocks the designated file pool.
  *THIS IS NOT THREADSAFE*
  The file pool's mutex (->lock) must be locked before it is called. 
*/
static void unlock_file_pool(fd_file_pool p)
{
  if (p->read_only == FD_POOL_LOCKABLE) return;
  else if (p->read_only == FD_POOL_READ_ONLY) return;
  if (p->store) fd_fclose(p->store);
  p->read_only=FD_POOL_LOCKABLE; p->store=NULL;
}

/** Using file pools (external entry point) **/

FRAMERD_EXPORT
/* fd_use_file_pool: 
     Arguments: a string naming a file
     Returns: a pointer to a file pool structure

   Errors:
    Cannot open pool (signalled by fd_open_file_pool)

   Side effects:
    Creates a file pool structure for the named file
    Adds a pointer to the pool structure to _fd_pool_table
*/
fd_pool fd_use_file_pool(fd_u8char *fname)
{
  /* Most of what this does is making sure that the designated file
     pool does not already exist.  If it doesn't it hands things off
     to open_file_pool to do the actual work. */
  fd_pool p=NULL;
  FD_WITH_MUTEX_LOCKED(&_file_pool_lookup_lock) {
    if ((fd_file_existsp(fname)) && (!(fd_directoryp(fname)))) {
      p=find_existing_file_pool(fname);
      if (p == NULL) p=open_file_pool(fname);}
    else {
      fd_u8char *with_suffix=fd_xmalloc(strlen(fname)+8);
      strcpy(with_suffix,fname); strcat(with_suffix,".pool");
      if (fd_file_existsp(with_suffix)) {
	p=find_existing_file_pool(with_suffix);
	if (p == NULL) p=open_file_pool(with_suffix);
	fd_xfree(with_suffix);}
      else fd_xfree(with_suffix);}}
  FD_END_WITH_MUTEX_LOCKED(&_file_pool_lookup_lock);
  return p;
}

/** Creating file pools **/

FRAMERD_EXPORT
/* Creates an empty file pool
      Arguments: pointer to an object ID (oid)
                 capacity (unsigned int)
                 filename (string pointer)
      Returns: void

   Creates an empty file pool on disk which can be subsequently
    opened or added.
*/
void fd_make_file_pool
  (char *filename,FD_OID base, unsigned int capacity,
   int major_version,fd_lisp metadata)
{
    FILE * f = fd_fopen (filename, "wb");
    time_t now= time(NULL);

    if (f == NULL)
      fd_raise_detailed_exception(fd_FileWriteFailed,filename);

    fwrite_4bytes (FD_FILE_POOL_MAGIC_NUMBER, f);
    fwrite_4bytes (FD_OID_HIGH(base), f);
    fwrite_4bytes (FD_OID_LOW(base), f);
    fwrite_4bytes (capacity, f);
    fwrite_4bytes (0, f);  /*load =0 for the time being*/
    fwrite_4bytes (0, f);  /*pointer to header (0 by default) */

    {             /* set up the table of pointers to data */
      unsigned int i=0;
      while (i++ < capacity) {
        fwrite_4bytes (0, f); /*pointer = 0, 4 or 8 bytes? */
      }
    }
    /* Write metadata */
    fwrite_4bytes(0xFFFFFFFE,f);
    fwrite_4bytes(40,f);
    fwrite_4bytes(major_version,f);
    /* Write time information as 8 byte values (remember Y2K) */
    fwrite_4bytes(0,f); fwrite_4bytes((int)now,f);
    fwrite_4bytes(0,f); fwrite_4bytes(0,f);
    fwrite_4bytes(0,f); fwrite_4bytes(0,f);
    /* Now write the metadata, if it exists */
    if (FD_EMPTYP(metadata)) {
      fwrite_4bytes(0x0,f);}
    else {
      fd_fwrite_4bytes(24+capacity*4+40,f);
      fd_fwrite_dtype(metadata,f);}
    /* Close the stream */
    fclose(f);
}

FRAMERD_EXPORT
/* fd_new_file_pool:
     Arguments: a filename (localized string), a capacity (unsigned int),
                and a super pool id (localized string)
     Returns: void

 Creates a new, empty, file pool with a particular capacity from a
specified super pool. */
void fd_new_file_pool
  (char *filename,unsigned int capacity,char *super_pool)
{
  FD_OID base=fd_allocate_pool(super_pool,capacity,filename); 
  fd_make_file_pool(filename,base,capacity,1,FD_EMPTY_CHOICE);
}

/** Basic functions **/

static lisp file_pool_new_oid(fd_pool p)
{
  fd_file_pool fp=(fd_file_pool)p;
  lock_mutex(&(fp->lock));
  lock_file_pool(fp);
  if (fp->load < fp->capacity) {
    unsigned int increment=fp->load++; FD_OID new; lisp o;
#if FD_OIDS_ARE_SCALARS	
    new=fp->base+increment;
#else
    new.high=FD_OID_HIGH(fp->base); new.low=FD_OID_LOW(fp->base)+increment;
#endif
    o = fd_make_oid(new);
    fp->modifiedp=1;
    unlock_mutex(&(fp->lock));
    return o;}
  else {
    unlock_mutex(&(fp->lock));
    fd_raise_exception(fd_FilePoolExhausted);}
}

/* file_pool_fetch:
      Arguments: a pointer to a file pool
                 an object identifier offset (unsigned int)
      Returns: a lisp object
   Returns the object associated with a particular identifer.
*/
lisp file_pool_fetch(fd_pool p,lisp oid)
{
  fd_file_pool fp=(fd_file_pool)p;
  unsigned int offset; lisp value; FILE *store; off_t loc=0;
  FD_OID id=FD_OID_ADDR(oid);
  offset=FD_OID_LOW(id)-FD_OID_LOW(fp->base);
  lock_mutex(&(fp->lock));
  store=get_store(fp);
  if (offset >= fp->load) {
    char buf[20];
    sprintf(buf,"@%x/%x",FD_OID_HIGH(fp->base),FD_OID_LOW(fp->base)+offset);
    unlock_mutex(&(fp->lock));
    fd_raise_detailed_exception(fd_UnallocatedOID,buf);}
  if (fp->offsets)
    if (offset < fp->offsets_size) loc=offget(fp->offsets,offset);
    else loc=0;
  else if (_fd_auto_cache_file_pools) {
    unlock_mutex(&(fp->lock));
    fd_cache_file_pool(fp);
    lock_mutex(&(fp->lock));
    if (offset < fp->offsets_size) loc=offget(fp->offsets,offset);
    else loc=0;}
  else {
    fseek(store,24+4*offset,SEEK_SET);
    loc=(off_t)(fd_fread_4bytes(store));}
  if (loc == 0) {unlock_mutex(&(fp->lock)); return FD_EMPTY_CHOICE;}
  fseeko(store,loc,SEEK_SET);
  value=fd_fread_dtype(store);
  if (fp->offsets) fseek(store,0,SEEK_END);
  unlock_mutex(&(fp->lock));
  return value;
}

struct FD_OID_FETCH_SCHEDULE {
  fd_lisp oid; off_t oidpos, filepos;};
static int sort_by_filepos(const void *p1,const void *p2)
{
  /* This does something a little strange in that a zero filepos is
     interpreted as infinity, so that qsort will push all of the completed
     schedule items to the end of the array. */
  struct FD_OID_FETCH_SCHEDULE *ofs1=(struct FD_OID_FETCH_SCHEDULE *)p1;
  struct FD_OID_FETCH_SCHEDULE *ofs2=(struct FD_OID_FETCH_SCHEDULE *)p2;
  if (ofs1->filepos == 0)
    if (ofs2->filepos) return 1;
    else return 0;
  else if (ofs2->filepos == 0) return -1;
  else if (ofs1->filepos > ofs2->filepos) return 1;
  /* We also know that no two filepos's will be identical, so we don't have
     to worry about about the == case. */
  else return -1;
}

/* file_pool_prefetch:
      Arguments: a pointer to a file pool
                 a lisp pointer to a choice of OIDs
      Returns: void
   Loads a set of oids into memory at once.
*/
static int file_pool_prefetch(fd_pool p,fd_lisp *oids,fd_lisp *values,int n)
{
  fd_file_pool fp=(fd_file_pool)p;
  unsigned int offset; off_t loc; lisp value; FILE *store;
  if ((fp->offsets == NULL) && (_fd_auto_cache_file_pools)) {
    fd_cache_file_pool(fp);}
  {
    WITH_MUTEX_LOCKED(&(fp->lock)) {
      unsigned int *offsets=fp->offsets;
      unsigned int base=FD_OID_LOW(fp->base);
      struct FD_OID_FETCH_SCHEDULE *schedule=
	fd_malloc(sizeof(struct FD_OID_FETCH_SCHEDULE)*n),
	*scan=schedule;
      FILE *store=get_store(fp);
      int i=0, n_local=0;
      while (i < n) {
	fd_lisp oid=oids[i];
	FD_OID id=FD_OID_ADDR(oid);
	if (in_poolp(id,p)) {
	  fd_lisp value=fd_oid_current_value(oid);
	  int offset=FD_OID_LOW(id)-base;
	  if ((FD_VOIDP(value)) && ((offsets) ? (offset < fp->offsets_size) : (offset < fp->load))) {
	    schedule[n_local].oidpos=i;
	    schedule[n_local].oid=oids[i];
	    if (offsets)
	      schedule[n_local].filepos=(off_t)offget(offsets,offset);
	    else schedule[n_local].filepos=(off_t)24+offset*4;
	    i++; n_local++;}
	  else values[i++]=value;}
	else i++;}
      if (offsets == NULL) {
	qsort(schedule,n_local,sizeof(struct FD_OID_FETCH_SCHEDULE),sort_by_filepos);
	i=0; while (i < n_local) {
	  fseeko(store,(off_t)(schedule[i].filepos),SEEK_SET);
	  schedule[i].filepos=fd_fread_4bytes(store);
	  i++;}}
      qsort(schedule,n_local,sizeof(struct FD_OID_FETCH_SCHEDULE),sort_by_filepos);
      i=0; while (i < n_local)  {
	fseeko(store,(off_t)(schedule[i].filepos),SEEK_SET);
	values[schedule[i].oidpos]=fd_fread_dtype(store);
	i++;}
      fd_free(schedule,sizeof(struct FD_OID_FETCH_SCHEDULE)*n);}
    END_WITH_MUTEX_LOCKED(&(fp->lock));
    return 1;}
}

static unsigned int file_pool_get_load(fd_pool fp)
{
  return ((fd_file_pool)fp)->load;
}

/** Commiting file pools **/

/* file_pool_commit_oid:
     Arguments: a pointer to a file pool and an FD_OID (a lisp pointer)
     Returns: void

   Changes the lisp object associated with a particular identifier.
   If the file pool is cached, the new assignment will not be permanent
    until the file pool is saved or closed.
   The inner function commit_oid_to_fp is used by the whole pool commit
procedure and doesn't lock the file pool.  The outer function
file_pool_commit_oid locks the file pool before doing its stuff.
*/
static void commit_oid_to_fp(fd_file_pool fp,lisp obj)
{
  FILE *stream; FD_OID id=FD_OID_ADDR(obj); 
  unsigned int offset=FD_OID_LOW(id)-FD_OID_LOW(fp->base); off_t new_loc;
  /* Not threadsafe (but should only be called with the FD_OID locked). */
  lisp v;
  if (fd_ephemeralp()) return;
  stream=get_store(fp);
  if (fp->offsets) new_loc=fp->end_pos;
  else {
    fseek(stream,0,SEEK_END);
    new_loc=ftello(stream);}

  CHECK_POS(new_loc,fp->filename);

  v=_fd_oid_current_value_nolock(obj); 

  if (SLOTMAPP(v)) {
    fd_slotmap sm=SLOTMAP_PTR(v);
    fd_lock_mutex(&(sm->lock));
    sm->modified=0;
    fd_unlock_mutex(&(sm->lock));}

  if (fp->offsets) 
    fp->end_pos=fp->end_pos+fd_fwrite_dtype(v,stream);
  else fd_fwrite_dtype(v,stream);
  if (fp->offsets) {
    grow_offsets(fp);
    fp->offsets[offset]=fd_net_order(new_loc);}
  fseek(stream,24+offset*4,SEEK_SET);
  fwrite_4bytes(new_loc,stream);
  fflush(stream);
  fd_decref(v);
  update_file_pool_change_date(fp);
}
static void file_pool_commit_oid(fd_pool p,lisp obj)
{
  fd_file_pool fp=(fd_file_pool)p;
  WITH_MUTEX_LOCKED(&(fp->lock))
    commit_oid_to_fp(fp,obj);
  END_WITH_MUTEX_LOCKED(&(fp->lock));
}

/* file_pool_commit:
      Arguments: a file pool
      Returns: nothing
  Saves a file pool to disk.  When the file pool is cached, this
   is clever in trying to write everything out in order without
   doing any seeks or ftells.  This allows buffering to really help.
*/
void file_pool_commit(fd_pool p)
{
  fd_file_pool fp=(fd_file_pool)p;
  FILE *store; unsigned int load, *offsets; off_t  pos;
  TIDY_ERRNO("commit_file_pool preamble");
  if ((fd_ephemeralp()) || (fp->modifiedp == 0)) return;
  FD_WITH_MUTEX_LOCKED(&(fp->lock)) {
    int n_modified=0;
    fd_lisp *mods=fd_get_modified(p,&n_modified,64);
    fd_lisp *scan=mods, *limit=scan+n_modified;
    if (fp->offsets) grow_offsets(fp);
    else if (_fd_auto_cache_file_pools) cache_file_pool_unlocked(fp);
    load=fp->load; offsets=fp->offsets; store=get_store(fp);
    fd_notify(_("Saving %d OIDs from file pool %s"),n_modified,fp->id); 
    if (offsets) {
      fseek(store,16,SEEK_SET); fwrite_4bytes(load,store);
      fseek(store,0,SEEK_END); pos=ftello(store);
      while (scan < limit)  {
	lisp elt=*scan++, v;
	int offset=FD_OID_ADDR_LOW(elt)-FD_OID_LOW(fp->base);
	v=fd_oid_current_value(elt);
	CHECK_POS(pos,fp->filename);
	offsets[offset]=fd_net_order(pos);
	if (SLOTMAPP(v)) {
	  fd_slotmap sm=SLOTMAP_PTR(v);
	  lock_mutex(&(sm->lock)); sm->modified=0; unlock_mutex(&(sm->lock));}
	pos=pos+fd_fwrite_dtype(v,store);
	decref(v);}
      /* Write out the offsets table.  Note that we don't need to check for
	 whether the offets are mmap'd because that only happens when
	 FD_POOL_READ_ONLY is set, which means we can't be committing the pool.
      */
      fseek(store,24,SEEK_SET);
      fwrite(offsets,sizeof(unsigned int),load,store);}
    else {
      if (fp->read_only)
	fd_raise_detailed_exception
	  (_("File pool was never locked!"),fp->filename);
      fseek(fp->store,16,SEEK_SET); fwrite_4bytes(fp->load,store);
      fd_notify(_("Saving pool %s"),fp->id);
      while (scan < limit) {
	lisp elt=*scan++;
	UNWIND_PROTECT {
	  LOCK_OID(elt); commit_oid_to_fp(fp,elt);}
	ON_UNWIND {
	  FD_UNLOCK_OID(elt);}
	END_UNWIND;}}
    fd_free(mods,sizeof(fd_lisp)*n_modified); fp->modifiedp=0;
    update_file_pool_change_date(fp);
    fflush(fp->store); fclose(fp->store); fp->store=NULL;
    if (fp->buf) free(fp->buf); fp->buf=NULL;
    unlock_file_pool(fp);
    TIDY_ERRNO("commit_file_pool");
    fd_notify(_("Saved file pool %s"),fp->id);}
  FD_END_WITH_MUTEX_LOCKED(&(fp->lock));
}

/** Caching File Pools **/

static void cache_file_pool_unlocked(fd_file_pool fp)
{
  if (fp->offsets) return;
#if HAVE_MMAP
  else if (fp->read_only == FD_POOL_READ_ONLY) {
    FILE *store=get_store(fp); unsigned int *newmmap;
    int fd=fileno(store);
    newmmap=
      mmap(NULL,sizeof(unsigned int)*fp->capacity+24,
	   PROT_READ,MMAP_FLAGS,fd,0);
    if (newmmap)
      fp->offsets=newmmap+6;
    else fd_raise_detailed_exception(fd_MmapFailed,strerror(errno));
    fp->offsets_size=fp->capacity;
    fseek(store,0,SEEK_END);
    fp->end_pos=ftello(store);}
#endif
  else {
    unsigned int i=0, cap=fp->capacity, load=fp->load,
      *offsets=fd_malloc((load)*sizeof(unsigned int));
    FILE *stream=get_store(fp);
    fp->offsets_size=load;
    fd_notify(_("Caching file pool %s"),fp->filename);
    fseek(stream,24,SEEK_SET);
    fread(offsets,sizeof(unsigned int),load,stream);
    fseek(stream,0,SEEK_END);
    fp->end_pos=ftello(stream);
    fp->offsets=offsets;}
}

FRAMERD_EXPORT
/* fd_cache_file_pool:
      Arguments: pointer to a file pool structure
      Returns: void

   Initializes a cache for the file pool, reducing the
    need for disk access and repositioning.
*/
void fd_cache_file_pool(fd_file_pool fp)
{
  TIDY_ERRNO("fd_cache_file_pool preamble");
  lock_mutex(&(fp->lock));
  if (fp->offsets) {unlock_mutex(&(fp->lock));}
  else if (fp->load == 0) {unlock_mutex(&(fp->lock));}
  else {
    cache_file_pool_unlocked(fp);
    unlock_mutex(&(fp->lock));}
  TIDY_ERRNO("fd_cache_file_pool");
}

FRAMERD_EXPORT
/* fd_maybe_cache_file_pool:
      Arguments: pointer to a file pool structure
      Returns: void

   May initialize a cache for the file pool, reducing the
    need for disk access and repositioning.
*/
void fd_maybe_cache_file_pool(fd_file_pool fp,int already_locked)
{
  if (already_locked)
    if (fp->offsets) return;
    else if (fp->load == 0) return;
    else if (_fd_auto_cache_file_pools)
      cache_file_pool_unlocked(fp);
    else {}
  else {
    TIDY_ERRNO("fd_cache_file_pool preamble");
    lock_mutex(&(fp->lock));
    if (fp->offsets) {unlock_mutex(&(fp->lock));}
    else if (fp->load == 0) {unlock_mutex(&(fp->lock));}
    else if (_fd_auto_cache_file_pools) {
      cache_file_pool_unlocked(fp);
      unlock_mutex(&(fp->lock));}
    else {}
    TIDY_ERRNO("fd_cache_file_pool");}
}

static void grow_offsets(struct FD_FILE_POOL *fp)
{
  if (fp->offsets == NULL) return;
  else if (fp->load > fp->offsets_size) {
    int old_size=fp->offsets_size, load=fp->load, cap=fp->capacity;
    int new_size, i;
    if (load+load/2 >= cap) new_size=cap; else new_size=load+load/2;
    fp->offsets=fd_realloc
      (fp->offsets,new_size*sizeof(unsigned int),
       old_size*sizeof(unsigned int));
    i=old_size; while (i < new_size) fp->offsets[i++]=0;
    fp->offsets_size=new_size;}
}

/** Maintenance functions **/

extern void process_pool_label(fd_pool p,fd_lisp label);

FRAMERD_EXPORT
/* fd_label_file_pool
    Arguments: a string, and a lisp object
    Returns: void

  Modifies a file pool to have a given label.
*/
void fd_label_file_pool(char *filename,lisp label)
{
  FILE *f; off_t end_pos; 
  fd_pool p=find_existing_file_pool(filename);
  fd_file_pool fp=(fd_file_pool)p;
  if (fp) {
    fd_lock_mutex(&(fp->lock));
    if (fp->store) {
      fd_unlock_mutex(&(fp->lock));
      fd_raise_exception("Can't label an open file pool");}
    fd_decref(fp->label);
    fp->label=fd_incref(label);
    process_pool_label(p,label);}
  f=fd_fopen_locked(filename,"r+b",1);
  if (f == NULL) {
    fd_unlock_mutex(&(fp->lock));
    fd_raise_detailed_exception(fd_FileLockFailed,filename);}
  fseek(f,0,SEEK_END); end_pos=ftello(f);
  CHECK_POS(end_pos,filename);
  fd_fwrite_dtype(label,f);
  fseek(f,20,SEEK_SET); fwrite_4bytes(end_pos,f);
  fflush(f); fclose(f);
  if (fp) fd_unlock_mutex(&(fp->lock));
}

FRAMERD_EXPORT
/* fd_make_super_pool:
    Arguments: a filename, a base id, and a load
    Returns: the base id

 Creates a new super pool file.  The base id is specifies
the high half of the base of the super pool and the load declares
how many OIDs are already "pre allocated" from the super pool. */
unsigned int fd_make_super_pool
  (char *filename,unsigned int base,unsigned int load)
{
  FILE *s=fd_fopen(filename,"wb");
  if (s == NULL)
    fd_raise_detailed_exception(fd_FileWriteFailed,filename);
  fwrite_4bytes(FD_SUPER_POOL_MAGIC_NUMBER,s);
  fwrite_4bytes(base,s); 
  fwrite_4bytes(0,s);
  fwrite_4bytes(load,s);
  fd_fwrite_dtype(FD_EMPTY_LIST,s);
  fclose(s);
  return base;
}

FRAMERD_EXPORT
/* fd_new_super_pool:
    Arguments: a filename
    Returns: the base id

 Creates a new super pool file with a random base id (based
on the time and process id).  This will not allocated in the first
thousand super pools. */
unsigned int fd_make_new_super_pool(char *filename)
{
  unsigned int random=
    ((unsigned int)time(NULL)+(unsigned int)getpid());
  unsigned int base=((random+1024)&(0x3FFFFFFF));
  return fd_make_super_pool(filename,base,0);
}

FRAMERD_EXPORT
/* fd_make_pool_snapshot:
    Arguments: two filenames
    Returns: the base id

 Creates a "snapshot" of a specified file pool.  This is basically
a copy of the pools offset table and load information, which can be used
to reconstitute the state of the pool.  This takes advantage of the fact
that pools don't write over values until they are repacked. */
void fd_make_pool_snapshot(char *filename,char *snapshot)
{
  FILE *fp=fd_fopen(filename,"rb");
  FILE *sn=fd_fopen(snapshot,"wb");
  unsigned int code=fread_4bytes(fp);
  unsigned int base_high=fread_4bytes(fp);
  unsigned int base_low=fread_4bytes(fp);
  unsigned int cap=fread_4bytes(fp);
  unsigned int load=fread_4bytes(fp);
  unsigned int *offsets=fd_malloc(sizeof(unsigned int)*load);
  off_t data_start=24+cap*4, data_size;
  unsigned int i=0, limit=16;
  if (code != FD_FILE_POOL_MAGIC_NUMBER) 
    fd_raise_detailed_exception(fd_NotAFilePool,filename);
  fread_4bytes(fp); /* Ignore header */
  fread(offsets,sizeof(unsigned int),load,fp);
  fseek(fp,0,SEEK_END); data_size=ftello(fp)-data_start;
  fwrite_4bytes(FD_FILE_POOL_SNAPSHOT_MAGIC_NUMBER,sn);
  fwrite_4bytes(base_high,sn);
  fwrite_4bytes(base_low,sn);
  fwrite_4bytes(cap,sn);
  fwrite_4bytes(load,sn);
  fwrite(offsets,sizeof(unsigned int),load,sn);
  fwrite_4bytes(data_size,sn);
  fwrite_4bytes(limit,sn);
  if (data_size > 4)
    while (i < limit) {
      unsigned int pos=rand()%data_size, datum;
      fseek(fp,(data_start+pos),SEEK_SET);
      datum=fread_4bytes(fp);
      fwrite_4bytes(pos,sn);
      fwrite_4bytes(datum,sn);
      i++;}
  else {
    fseeko(fp,data_start,SEEK_SET);    
    while (i < data_size) {
      unsigned int byte=fread_byte(fp);
      if (byte > 0) fwrite_byte(byte,fp); i++;}}
  fclose(fp); fclose(sn);
}

FRAMERD_EXPORT
void fd_restore_pool_snapshot(char *filename,char *snapshot)
/* fd_restore_pool_snapshot:
    Arguments: two filenames (a file pool and a snapshot)
    Returns: the base id

 Restores a "snapshot" for the specified file pool.  This also
checks that the snapshot is in fact valid for the pool being restored. */
{
  FILE *fp=fd_fopen(filename,"r+b");
  FILE *sn=fd_fopen(snapshot,"rb");
  unsigned int code=fread_4bytes(fp);
  unsigned int base_high=fread_4bytes(fp);
  unsigned int base_low=fread_4bytes(fp);
  unsigned int cap=fread_4bytes(fp);
  unsigned int load=fread_4bytes(fp), new_load;
  unsigned int *offsets;
  unsigned int data_start=24+cap*4; off_t data_size;
  unsigned int i=0, limit=16;
  fd_notify(_("Validating snapshot %s against file pool %s"),snapshot,filename);
  if (code != FD_FILE_POOL_MAGIC_NUMBER) 
    fd_raise_detailed_exception(fd_NotAFilePool,filename);
  fread_4bytes(fp); /* Ignore header */
  if (fread_4bytes(sn) != FD_FILE_POOL_SNAPSHOT_MAGIC_NUMBER)
    fd_raise_detailed_exception(_("Not a file pool snapshot"),snapshot);
  if ((fread_4bytes(sn) != base_high) ||
      (fread_4bytes(sn) != base_low) ||
      (fread_4bytes(sn) != cap))
    fd_raise_detailed_exception(_("Snapshot of different pool"),snapshot);
  new_load=fread_4bytes(sn);
  offsets=fd_malloc(sizeof(unsigned int)*new_load);
  fread(offsets,sizeof(unsigned int),new_load,sn);  
  data_size=fread_4bytes(sn);
  fseek(fp,0,SEEK_END);
  if (ftello(fp) < data_size)
    fd_raise_exception(_("File pool is smaller than snapshot"));
  limit=fread_4bytes(sn);
  if (data_size > 4)
    while (i < limit) {
      unsigned int pos=fread_4bytes(sn);
      unsigned int datum=fread_4bytes(sn);
      fseeko(fp,pos+data_start,SEEK_SET);
      if (fread_4bytes(fp) != datum)
	fd_raise_exception(_("Pool/snapshot data conflict"));
      i++;}
  else {
    fseeko(fp,data_start,SEEK_SET);
    i=0; while (i < data_size) {
      int byte=fread_byte(fp);
      if (byte != fread_byte(sn))
	fd_raise_exception(_("Pool/snapshot data conflict"));
      else i++;}}
  fd_notify(_("Snapshot %s validated against file pool %s; load %d->%d"),
	    snapshot,filename,load,new_load);
  fseek(fp,16,SEEK_SET);
  fwrite_4bytes(new_load,fp);
  fseek(fp,24,SEEK_SET);
  fwrite(offsets,sizeof(unsigned int),new_load,fp);
  {unsigned int i=new_load; while (i < cap) {
	  fwrite_4bytes(0,fp); i++;}}
  fclose(fp); fclose(sn);
}

/** Functions for finding out about file pools without using them **/

FRAMERD_EXPORT
/* fd_file_pool_load:
     Arguments: a pathname (a string)
     Returns: an unsigned int
 Returns the number of allocated OIDs in the specified file pool.
This operates without actually "using the pool".
*/
unsigned int fd_file_pool_load(fd_u8char *filename)
{
  FILE *f=fd_fopen(filename,"rb");
  if (f == NULL) {
    char *longpath=fd_xmalloc(strlen(filename)+8);
    strcpy(longpath,filename); strcat(longpath,".pool");
    f=fd_fopen(longpath,"rb"); fd_xfree(longpath);}
  if (f) {
    unsigned int magic_no=fd_fread_4bytes(f), load;
    if (magic_no != FD_FILE_POOL_MAGIC_NUMBER)
      fd_raise_detailed_exception(fd_NotAFilePool,filename);
    fseek(f,16,SEEK_SET); load=fd_fread_4bytes(f);
    fclose(f);
    return load;}
  else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
}

FRAMERD_EXPORT
/* fd_file_pool_capacity:
     Arguments: a pathname (a string)
     Returns: an unsigned int
 Returns the total number of possible OIDs in the specified file pool.
This operates without actually "using the pool".
*/
unsigned int fd_file_pool_capacity(fd_u8char *filename)
{
  FILE *f=fd_fopen(filename,"rb");
  if (f == NULL) {
    char *longpath=fd_xmalloc(strlen(filename)+8);
    strcpy(longpath,filename); strcat(longpath,".pool");
    f=fd_fopen(longpath,"rb"); fd_xfree(longpath);}
  if (f) {
    unsigned int magic_no=fd_fread_4bytes(f), cap;
    if (magic_no != FD_FILE_POOL_MAGIC_NUMBER)
      fd_raise_detailed_exception(fd_NotAFilePool,filename);
    fseek(f,12,SEEK_SET); cap=fd_fread_4bytes(f);
    fclose(f);
    return cap;}
  else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
}

FRAMERD_EXPORT
/* fd_file_pool_freespace:
     Arguments: a pathname (a string)
     Returns: an unsigned int
 Returns the number of unallocated OIDs in the specified file pool.
This operates without actually "using the pool".
*/
unsigned int fd_file_pool_freespace(fd_u8char *filename)
{
  FILE *f=fd_fopen(filename,"rb");
  if (f == NULL) {
    char *longpath=fd_xmalloc(strlen(filename)+8);
    strcpy(longpath,filename); strcat(longpath,".pool");
    f=fd_fopen(longpath,"rb"); fd_xfree(longpath);}
  if (f) {
    unsigned int magic_no=fd_fread_4bytes(f), load, cap;
    if (magic_no != FD_FILE_POOL_MAGIC_NUMBER)
      fd_raise_detailed_exception(fd_NotAFilePool,filename);
    fseek(f,12,SEEK_SET);
    cap=fd_fread_4bytes(f); load=fd_fread_4bytes(f);
    fclose(f);
    return cap-load;}
  else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
}

/** Initialization **/

static struct FD_POOL_HANDLER file_pool_handler={
  file_pool_new_oid,
  file_pool_fetch,
  file_pool_commit,
  file_pool_commit_oid,
  file_pool_prefetch,
  file_pool_close,
  file_pool_locker,
  file_pool_get_load,
  NULL,
  fd_read_std_file_pool_metadata,
  NULL
};

void fd_initialize_file_pool_c()
{
  fd_register_file_pool_opener
    (FD_FILE_POOL_MAGIC_NUMBER,&file_pool_handler,open_std_file_pool);
  fd_register_source_file("file-pool",__DATE__,vcid);
#if FD_THREADS_ENABLED
  fd_init_mutex(&_file_pool_lookup_lock);
#endif
}


/* File specific stuff */

/* The CVS log for this file
   $Log: file-pool.c,v $
   Revision 1.69  2005/05/21 18:04:47  haase
   Made end_pos be off_t

   Revision 1.68  2005/03/13 22:45:51  haase
   MMAP fixes

   Revision 1.67  2005/03/07 23:57:20  haase
   Fixed mmap calls to have right size and offset

   Revision 1.66  2005/01/14 16:48:47  haase
   Updated copyrights to 2005

   Revision 1.65  2004/11/13 02:17:39  haase
   Fixed bugs in mmap offsets

   Revision 1.64  2004/11/13 01:30:23  haase
   Use mmap for read-only file pools

   Revision 1.63  2004/09/17 08:30:24  haase
   Fixed filepools to retain offset table over-allocation

   Revision 1.62  2004/09/16 10:35:51  haase
   Whitespace changes

   Revision 1.61  2004/09/08 18:33:40  haase
   Fix size/revminor output

   Revision 1.60  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.59  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.58  2004/07/17 19:06:24  haase
   Added checking for file overflows in pools and indices

   Revision 1.57  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.56  2004/07/16 13:37:25  haase
   Made file pools use off_t

   Revision 1.55  2004/07/04 01:50:48  haase
   Fixed minor bug in file pool fetches

   Revision 1.54  2004/07/04 01:41:07  haase
   Fixed error in declaration of _fd_auto_cache_file_pools

   Revision 1.53  2004/07/04 01:38:30  haase
   Made auto_cache variables be exported

   Revision 1.52  2004/05/15 18:56:02  haase
   Fixed over-eager file pool caching

   Revision 1.51  2004/04/06 09:56:32  haase
   Do autocaching before committing file pools

   Revision 1.50  2004/03/31 11:19:56  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.49  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.48  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.47  2004/01/09 13:50:23  haase
   Fix to set vbuf for file pools when committing

   Revision 1.46  2003/12/05 14:45:22  haase
   Prepared the way for 64bit file pools

   Revision 1.45  2003/11/29 22:24:25  haase
   Fixed leak in relabelling file pools

   Revision 1.44  2003/11/26 14:49:24  haase
   Fixed label file pool to update structures in memory

   Revision 1.43  2003/11/04 03:24:31  haase
   Fixed use-pool of a directory name

   Revision 1.42  2003/10/26 02:07:11  haase
   Special fix for case of file pool (or index) foo.pool and directory foo

   Revision 1.41  2003/10/24 13:29:06  haase
   Fixed metadata reading to repair erroneously void metadata

   Revision 1.40  2003/09/30 19:08:50  haase
   Wrapped locks around pool/index lookup/creation

   Revision 1.39  2003/09/30 11:16:15  haase
   Added extra locks to protect pool and index registries

   Revision 1.38  2003/08/31 16:55:55  haase
   Fixed prefetch of non cached file pools

   Revision 1.37  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.36.2.6  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.36.2.5  2003/08/15 13:29:36  haase
   Various extensions to file pools and indices to handle custom extensions

   Revision 1.36.2.4  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.36.2.3  2003/08/02 13:59:20  haase
   Reorganized pool initialization

   Revision 1.36.2.2  2003/02/17 12:28:09  haase
   Added file pool mutex unlock to empty pool case

   Revision 1.36.2.1  2002/09/26 02:08:37  haase
   Fix size and filepos declarations to be unsigned int

   Revision 1.36  2002/07/11 18:44:44  haase
   Fixed bug in file pool prefetch scheduling

   Revision 1.35  2002/07/03 06:16:49  haase
   Fixed return value for fd_lock_file_pool

   Revision 1.34  2002/07/03 06:02:12  haase
   Reorganized file pool locking to avoid some multithreaded race conditions.
   Made lock_file_pool signal an error when the pool is marked READ_ONLY,
   regardless of whether the underlying file is writable.
   Also added (here and elsewhere) some internal debugging support for the
   illegitimate freeing slotmaps which are the values of OIDs.

   Revision 1.33  2002/07/01 17:07:06  haase
   Fixed another nasty stunted offsets bug

   Revision 1.32  2002/07/01 14:51:49  haase
   Fixes to problems with pool fetches and stunted offsets tables

   Revision 1.31  2002/07/01 02:36:06  haase
   Fixed nasty bug in offsets overrun from previous space-saving optimizations

   Revision 1.30  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.29  2002/06/21 13:46:03  haase
   Made file pools use an offset table limited to the load of the pool, growing it when neccessary.

   Revision 1.28  2002/04/28 02:28:15  haase
   Fixed offset computation for pool metadata on reset

   Revision 1.27  2002/04/27 17:47:53  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.26  2002/04/24 20:26:19  haase
   Fixed changelog entries for various files

   Revision 1.25  2002/04/24 20:06:19  haase
   Catch error when trying to open a directory as a file pool

   Revision 1.24  2002/04/23 22:59:11  haase
   Changed metadaata warning to error

   Revision 1.23  2002/04/22 17:52:25  haase
   Fixed bug with single commits being lost on abort when the pool offset
   table was loaded.
   Made modifications (oid and pool level) update the change timestamp
   in the file pool metadata.

   Revision 1.22  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.21  2002/04/16 00:10:18  haase
   fd_label_file_pool now adds label to existing pool (but doesn't remove old label, which is a bug)

   Revision 1.20  2002/04/10 18:52:40  haase
   include/framerd/odb.h

   Revision 1.19  2002/04/10 16:01:57  haase
   Fixed some more problems with NULL size arguments to metadata functions

   Revision 1.18  2002/04/10 12:28:23  haase
   Fixed handling of NULL size pointer to metadata retrieval functions

   Revision 1.17  2002/04/10 03:02:11  haase
   Added version information to file pools and indices

   Revision 1.16  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
