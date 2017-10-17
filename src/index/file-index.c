/* C Mode */

/* file-index.c
   Implements disk-based hash tables for DType objects.
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

static char vcid[] =
  "$Id: file-index.c,v 1.75 2006/06/27 11:41:06 haase Exp $";

/** Initial includes and definitions **/
/** Opening file indices **/
/** Utilities **/
/** Opening file indices **/
/** Committing File indices **/
/** Caching File indices **/
/** Comparing objects to DTYPEs on disk **/
/** Collecting values for keys **/
/** Iterating over indices **/
/** Reporting hash table statistics **/
/** Initialization **/

/*

This file implements FramerD indices, which are either files on disk
or servers over the network (which are probably accessing files on disk).
The network side of things is pretty straightforward, but the index files
get tricky.  So most of the documentation in this file and in this overview
in particular is about Framerd *file indices*.

File indices are LISP hash tables stored on disk and using a portable
hash function across hardware and software platforms.

The format of a file index is:

0        XXXX    Magic number identifying this as an index file |
4        XXXX    Number of slots in the hash table (= k)      |-- index segment
8        ....    Slots in hash table (each takes 4 bytes)     |
         ....    (k of them)                                  |

         FFFF    optional prefix for metadata segment         |
         XXXX    revision (repack) serial number              |
         XXXX    metadata location (a dtype)                  |-- meta data segment
         XXXX    length of fully qualified name               |
         ....    characters of NAME in UTF-8                  |

8+4k     <key entry or value entry>                           |   
         <key entry or value entry>                           |-- data segment
         <key entry or value entry>....			      |   
         ....                                                 .
         ....                                                 .
         ....                                                 .

Each slot in the index segment points to a key entry in the data
segment.  This pointer is offset by the the size of the index table
(that is, 4*k) to make it possible to enlarge the index segment
without reorganizing the data segment.

THE DATA SEGMENT starts at position 8+4k (where k is the number of
slots in the hash table).  The data segment stores a linked list of
values for each hash key.  This is designed to make it efficient to
add just one value (for instance when maintaining an inverted index).
The data segment is comprised of two kinds of entries: key entries and
value entries.

A KEY ENTRY consists of a 4 byte count, a 4-byte data pointer, and a
dtype representation.  The count tells how many objects are associated
with this key; the data pointer is an offset (within the file, offset
by 4*k bytes) pointing to the first value.

A VALUE ENTRY consists of a dtype representation and a 4 byte pointer
(again, within the file offset by 4*k bytes) to another value (or zero
to indicate no more values).

The file starts with a table containing offsets pointing to key
entries in the data segment of the file.  The actual file position is
determined by adding 4*k to the stored offset (this makes it possible
to grow the hash table without reorganizing the data segment.

The implementation uses chaining with a dual hash function; an initial
hash value is computed and used to generate both an initial probe and
a chain width.  The initial probe is the hash modulo the number of
slots; the chain width is the hash modulo (the number of slots minus
2).  It first looks in the slot corresponding to the initial probe; if
this is zero, the fetch fails; if it is not zero, it looks at the key
stored at the location and checks if it matches what it is looking
for.  If it is, it just reads the associated value; if it is not, it
computes a new probe by adding the chain width to the previous probe
and taking the result modulo the table size; this continues until a
matching key or an empty slot is found.

*/

/** Initial includes and definitions **/

#include "framerd.h"
#include "framerd/dtcodes.h"
#include <limits.h>
#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#if HAVE_MMAP
#include <sys/mman.h>
#define MMAP_FLAGS (MAP_SHARED)
#endif

#define TRACE_CLEVER_COMMIT 0

#if TRACE_CLEVER_COMMIT
#define trace_commit_entry(x) describe_commit_entry(x)
#else
#define trace_commit_entry(x)
#endif

#if FD_THREADS_ENABLED
static fd_mutex _file_index_lookup_lock;
#endif

static struct FD_INDEX_HANDLER file_index_handler;

static fd_file_index find_file_index(char *);
static lisp file_index_fetch(fd_index,lisp);
static int file_index_fetch_size(fd_index,lisp);
static void file_index_commit(fd_index);
static lisp file_index_keys(fd_index);
static void file_index_close(fd_index);
static void file_index_prefetch(fd_index,fd_lisp);

static int valid_index_magic_numberp(unsigned int magic_no);

/* This is signalled when the function index_get (which wants to return a
  single element) is called on a key with multiple associations. */
fd_exception fd_NoSuchIndex=_("No such FramerD file index"),
  fd_NotFileIndex=_("File is not a FramerD file index"),
  File_Index_Overflow=_("File index size overflow");

static fd_exception FutureMetaData=
  _("File index metadata is from far future (time_t overflow)");

/* Variables used for bookeeping on hash table performance. */
static unsigned int n_fetches=0, n_misses=0, n_probes=0, max_n_probes=0;

/* Whether file indices automatically get their offset tables cached.
   This can speed up access particularly on slow-to-seek disks. */
FRAMERD_EXPORT int _fd_auto_cache_file_indices=0;

/** Utilities **/

#define get_store(i) ((i->store) ? (i->store) : open_store(i))

FASTOP fd_file_index to_file_index(fd_index in)
{
  if ((in->type == file_index) && (in->handler == &file_index_handler))
    return (fd_file_index) in;
  else fd_raise_detailed_exception(fd_NotFileIndex,in->id);
}

#define CHECK_POS(pos,id) \
  if (pos > UINT_MAX) fd_raise_detailed_exception(File_Index_Overflow,id)

/* Used in various places for file indices */
static lisp collect_values(fd_file_index ix,off_t loc,unsigned int size);
static void cache_file_index(fd_file_index idx,int towrite);

FASTOP unsigned int fileindex_hash(fd_file_index fx,fd_lisp key)
{
  switch (fx->hashv) {
  case 1: return fd_hash_dtype(key);
  case 2: return fd_hash_dtype2(key);
  case 3: return fd_hash_dtype3(key);
  default: fd_raise_detailed_exception(fd_NotFileIndex,fx->id);}
}

/** Opening file indices **/

/*
   open_std_file_index()
     Arguments: a filename
     Returns: a file index
   Opens an index file
*/
static fd_index open_std_file_index(fd_u8char *filename)
{
  struct FD_FILE_INDEX *ix=fd_xmalloc(sizeof(struct FD_FILE_INDEX));
  FILE *store=fd_fopen(filename,"rb");
  int magic_no=fread_4bytes(store);
  if (!(valid_index_magic_numberp(magic_no)))
    fd_raise_detailed_exception(fd_NotFileIndex,filename);
  ix->id=fd_basename(filename,0); ix->type=file_index; ix->zipf_threshold=0;
  if (fd_file_writablep(filename)) ix->read_only=0;
  else ix->read_only=1;
  ix->interned_values.n_slots=0;
  fd_init_hashtable(&(ix->cache),128);
  fd_init_hashtable(&(ix->adds),128);
  fd_init_hashtable(&(ix->drops),128);
  fd_init_hashtable(&(ix->sizes),128);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ix->lock));
#endif
  /* File index specific stuff */
  ix->size=fread_4bytes(store);
  ix->filename=fd_get_real_pathname(filename);
  ix->store=NULL; fclose(store);
  ix->offsets=NULL; ix->buf=NULL;
  ix->at_end=0; ix->preloaded=0;
  
  if (magic_no == FD_MULT_FILE_INDEX_MAGIC_NUMBER) ix->hashv=2;
  else if (magic_no == FD_MULT_FILE3_INDEX_MAGIC_NUMBER) ix->hashv=3;
  else ix->hashv=1;

  ix->handler=&file_index_handler;
  
  ix->cache_size=128; ix->sizes_size=128;
  ix->adds_size=128; ix->drops_size=128;
  
  fd_register_index((fd_index)ix);
  return (fd_index)ix;
}

static fd_file_index find_file_index(char *absname)
{
  fd_index scan=fd_all_indices;
  while (scan) 
    if (scan->type != file_index) scan=scan->next;
    else if (strcmp(((fd_file_index)scan)->filename,absname) == 0)
      return (fd_file_index)scan;
    else scan=scan->next;
  return NULL;
}

static struct FD_FILE_INDEX_OPENER *index_openers=NULL;

/* open_file_index:
      arguments: a string encoding a filename
      returns: a file index
  This calls the opening function associated with the first word
  of the file. */
static fd_index open_file_index(fd_u8char *filename)
{
  FILE *f=fd_fopen(filename,"rb"); int magic_no;
  struct FD_FILE_INDEX_OPENER *scan=index_openers;
  if (f == NULL)
    fd_raise_detailed_exception(fd_FileOpenFailed,filename);
  magic_no=fread_4bytes(f); fd_fclose(f);
  while (scan)
    if (scan->magic_number == magic_no)
      return scan->opener(filename);
    else scan=scan->next;
  fd_raise_detailed_exception(fd_NotFileIndex,filename);
}

static int valid_index_magic_numberp(unsigned int magic_no)
{
  struct FD_FILE_INDEX_OPENER *scan=index_openers;
  while (scan)
    if (scan->magic_number == magic_no) return 1;
    else scan=scan->next;
  return 0;
}

FRAMERD_EXPORT
/* fd_register_file_index_opener:
      Arguments: a magic number and an opening function
      Returns: void
  This associates an opening function with a number which
  is the first word of the file to use this opener.
*/
void fd_register_file_index_opener(int magic_no,fd_index (*opener)(fd_u8char *c))
{
  struct FD_FILE_INDEX_OPENER *new=fd_xmalloc(sizeof(struct FD_FILE_INDEX_OPENER));
  new->magic_number=magic_no; new->opener=opener; new->next=index_openers;
  index_openers=new;
}

FRAMERD_EXPORT
fd_index fd_open_file_index(fd_u8char *filename)
{
  fd_index result=NULL;
  FD_WITH_MUTEX_LOCKED(&_file_index_lookup_lock) 
    if ((fd_file_existsp(filename)) && (!(fd_directoryp(filename)))) {
      char *abspath=fd_get_real_pathname(filename);
      fd_file_index found=find_file_index(abspath);
      if (found) result=(fd_index)found;
      else result=open_file_index(filename);
      fd_xfree(abspath);}
    else {
      fd_u8char *with_suffix=fd_xmalloc(strlen(filename)+8);
      strcpy(with_suffix,filename); strcat(with_suffix,".index");
      if (fd_file_existsp(with_suffix)) {
	char *abspath=fd_get_real_pathname(with_suffix);
	fd_file_index found=find_file_index(abspath);
	if (found) result=(fd_index) found;
	else result=open_file_index(with_suffix);
	fd_xfree(abspath); fd_xfree(with_suffix);}
      else fd_xfree(with_suffix);}
  FD_END_WITH_MUTEX_LOCKED(&_file_index_lookup_lock);
  return result;
}

static FILE *open_store(fd_file_index i)
{
  if (i->store) return i->store;
  else {
    FILE *store; int bigbuf;
    TIDY_ERRNO("Dangling errno:");
    if (i->read_only) store=fd_fopen(i->filename,"rb");
    else if ((store=fd_fopen(i->filename,"r+b")) == NULL) {
      i->read_only=1; store=fd_fopen(i->filename,"rb");
      CLEAR_ERR();}
    if (errno) {perror("File Index open"); CLEAR_ERR();}
    if (store == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,i->filename);
    i->store=store; 
    i->buf=fd_get_big_buffer(&bigbuf);
    if (i->buf) setvbuf(store,i->buf,_IOFBF,bigbuf);
    return store;}
}

FRAMERD_EXPORT
/* fd_read_file_index_metadata:
     Arguments: an open file stream to a file index and pointers to two ints
     Returns: a lisp object (or the empty choice) 
  Returns metadata and version information for a file pool.
     The version information, consisting of a serial repack ID and
  a file length, are written into the two integer  pointers.
     Since modifications always write at the end of the file,
  the repack serial number and the length of the file uniquely
  identify a moment in time for the file.  */
fd_lisp fd_read_file_index_metadata
  (FILE *f,int *revnum,off_t *size,time_t *make,time_t *repack,time_t *change)
{
  int n_slots, c; off_t opos=ftello(f), probe, magic_no; 
  time_t notime=(time_t)-1;
  fseek(f,0,SEEK_SET);
  magic_no=fd_fread_4bytes(f); if (!(valid_index_magic_numberp(magic_no))) {
    fseeko(f,opos,SEEK_SET); return FD_VOID;}
  n_slots=fd_fread_4bytes(f);
  fseek(f,8+n_slots*4,SEEK_SET); c=fgetc(f);
  if ((c < 0)  && (feof(f))) {
    if (size) {
      fseek(f,0,SEEK_END); *size=ftello(f);}
    fseeko(f,opos,SEEK_SET); *revnum=0; return FD_EMPTY_CHOICE;}
  fseek(f,8+n_slots*4,SEEK_SET); probe=fd_fread_4bytes(f);
  if (probe == 0xFFFFFFFF) { /* Version 1 */
    off_t md_loc; time_t now=time(NULL);
    fd_lisp metadata=FD_EMPTY_CHOICE;
    /* Read the revision (repack) number */
    if (revnum) *revnum=fd_fread_4bytes(f); else {fd_fread_4bytes(f);}
    /* Write the time information (actually, its absence in this version). */
    if (make) *make=notime; if (repack) *repack=notime; if (change) *change=notime;
    /* Read the metadata pointer and metadata */
    md_loc=(off_t)(fd_fread_4bytes(f)); if (md_loc) {
      fseeko(f,md_loc,SEEK_SET); metadata=fd_fread_dtype(f);}
    /* Set the file size */
    if (size) {
      fseek(f,0,SEEK_END); *size=ftello(f);}
    fseek(f,opos,SEEK_SET);
    return metadata;}
  else if (probe == 0xFFFFFFFE) { /* Version 2 */
    off_t md_loc; unsigned int block_size;
    fd_lisp metadata=FD_EMPTY_CHOICE;
    block_size=fd_fread_4bytes(f);
    if (revnum) *revnum=fd_fread_4bytes(f); else fd_fread_4bytes(f);
    /* Read time metdata */
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (make) *make=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (repack) *repack=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    if (fd_fread_4bytes(f) != 0) fd_raise_exception(FutureMetaData);
    if (change) *change=(time_t)fd_fread_4bytes(f);
    else fd_fread_4bytes(f);
    /* Read the metadata dtype */
    md_loc=(off_t)(fd_fread_4bytes(f));
    if (md_loc) {
      fseeko(f,md_loc,SEEK_SET); metadata=fd_fread_dtype(f);}
    if (size) {fseek(f,0,SEEK_END); *size=ftello(f);}
    fseek(f,opos,SEEK_SET);
    return metadata;}
  else {
    if (revnum) *revnum=0;
    /* Write the time information (actually, its absence in this version). */
    if (make) *make=notime; if (repack) *repack=notime; if (change) *change=notime;
    if (size) {fseek(f,0,SEEK_END); *size=ftello(f);}
    fseek(f,opos,SEEK_SET);
    return FD_EMPTY_CHOICE;}
}

static void update_file_index_change_date(fd_file_index fi)
{
  FILE *f; int code;
  if (fi->store == NULL) {
    return;}
  else f=fi->store;
  fseek(f,8+4*fi->size,SEEK_SET);
  if (fread_4bytes(f) == 0xFFFFFFFF) {
    fd_warn("File index version doesn't change dates");
    return;}
  fseek(f,8+4*fi->size+7*4,SEEK_SET);
  fwrite_4bytes(0,f); fwrite_4bytes((int)time(NULL),f);
  fflush(f);
}

FRAMERD_EXPORT
/* fd_store_file_index_metadata:
     Arguments: an open file stream to a file index and lisp pointer
     Returns: void
*/
void fd_store_file_index_metadata(FILE *f,fd_lisp metadata)
{
  int magic_no, revnum, size; time_t make, repack, change;
  int n_slots, c; off_t md_loc; unsigned int probe;
  fseek(f,0,SEEK_END); md_loc=ftello(f); fd_fwrite_dtype(metadata,f);
  if (md_loc > UINT_MAX) fd_raise_exception(File_Index_Overflow);
  fseek(f,0,SEEK_SET);
  magic_no=fd_fread_4bytes(f); n_slots=fd_fread_4bytes(f);
  if (!(valid_index_magic_numberp(magic_no)))
    fd_raise_exception(fd_NotFileIndex);
  fseek(f,8+n_slots*4,SEEK_SET); probe=fd_fread_4bytes(f);
  if (probe == 0xFFFFFFFF) { /* Version 1 */
    fseek(f,4,SEEK_CUR); /* Skip repack number */
    fd_fwrite_4bytes(md_loc,f);}
  else if (probe == 0xFFFFFFFE) { /* Version 2 */
    fseek(f,20,SEEK_CUR); /* Skip block size, repack number, and time info */
    fd_fwrite_4bytes(md_loc,f);}
}

/** Finding keys in file indices **/

/* This macro uses the cache if possible */
#if ((HAVE_MMAP) && (!(WORDS_BIGENDIAN)))
#define probe_loc(idx,probe) \
   ((idx->offsets) ? (fd_flip_word(idx->offsets[probe])) : \
     ((fseek(idx->store,probe*4+8,SEEK_SET)),fread_4bytes(idx->store)))
#define offget(offsets,probe) (fd_flip_word(offsets[probe]))
#else
#define probe_loc(idx,probe) \
   ((idx->offsets) ? (idx->offsets[probe]) : \
     ((fseek(idx->store,probe*4+8,SEEK_SET)),fread_4bytes(idx->store)))
#define offget(offsets,probe) (offsets[probe])
#endif

FASTOP int compare_slot_key
   (lisp key,FILE *stream,unsigned int offset,struct INDEX_LOC *loc);

/* search_index_file is the heart of the hash table lookup.

   This finds an entry for a key. It takes an index and a key and does
   a series of probes to find the appropriate slot number.  It returns
   an INDEX_LOC structure consisting of a slot number and an offset to that
   slot's key entry.  If the key does not exist, this offset is zero
   and the slot number is the first empty slot in its probes for the key.
   If the key is then stored in the IDX file, this slot is where it will
   be placed.
*/
static void search_index_file(fd_file_index idx,lisp key,struct INDEX_LOC *loc)
{
  FILE *stream=get_store(idx);
  unsigned int hash = fileindex_hash(idx,key), size=idx->size;
  unsigned int first_probe = (hash%size);
  unsigned int chain_width = (hash%(size-2))+1, probes=0;
  n_fetches++; /* Statistics */
  if ((_fd_auto_cache_file_indices) && (idx->offsets == NULL))
    cache_file_index(idx,0);
  loc->probe=first_probe; loc->point=probe_loc(idx,first_probe);
  loc->data=0; loc->n_elts=0;
  if (loc->point) idx->at_end=0;
  while (loc->point)
    if (compare_slot_key(key,stream,size*4,loc)) {
      /* Keep statistics */
      if (probes > max_n_probes) max_n_probes=probes;
      n_probes=n_probes+probes;
      if (loc->probe != first_probe) n_misses++;
      return;}
    else { /* Chain to the next location */
      loc->probe=(loc->probe+chain_width)%size;
      loc->point=probe_loc(idx,loc->probe);
      if (probes > 1000)
	fd_raise_exception("Hashing takes forever!");
      if ((loc->probe == first_probe) && (probes))
	fd_raise_exception("Hash table has cycled!");
      probes++;}
}

/* This is an inline procedure which does key comparison for tables
   It uses fd_dtype_compare to compare the key with a dtype representation
   on disk.

   It is compiled inline and takes three arguments: 
     the key being compared against,
     the index in which it is being compared, and a slot number. */
FASTOP int compare_slot_key
   (lisp key,FILE *stream,unsigned int offset,struct INDEX_LOC *loc)
{
  unsigned int size, data;
  fseeko(stream,loc->point+offset,SEEK_SET);
  size=fread_4bytes(stream); data=fread_4bytes(stream);
  if (fd_dtype_compare(key,stream)) {
    loc->n_elts=size; loc->data=data; return 1;}
  else return 0;
}

/** Committing file indices **/

static void clever_commit_file_index(fd_file_index idx);
static void simple_commit_file_index(fd_file_index idx);;
static void commit_drops(fd_file_index idx);

static void file_index_commit(fd_index ix_arg)
{
  fd_file_index idx=(fd_file_index)ix_arg;
  fd_hashtable adds=&(idx->adds);
  FILE *stream=get_store(idx);
  UNWIND_PROTECT {
    if (idx->drops.n_keys) commit_drops(idx);
    lock_mutex(&(adds->lock));
    if (adds->n_keys) {
#if HAVE_MMAP
      if (idx->offsets) { /* Unmap offsets before closing file */
	int retval=munmap(idx->offsets-2,(sizeof(unsigned int)*idx->size)+8);
	if (retval<0) {
	  fd_unlock_mutex(&(adds->lock));
	  fd_raise_exception(fd_UnmapFailed);}
	idx->offsets=NULL;}
#endif
      if (idx->store) fd_fclose(idx->store);
      stream=idx->store=fd_fopen_locked(idx->filename,"r+b",0);
      if (stream == NULL) 
	fd_raise_detailed_exception(fd_FileOpenWFailed,idx->filename);
      /* If need to cache, call inner version without mutex locking */
      if ((idx->offsets == NULL) && (_fd_auto_cache_file_indices))
	cache_file_index(idx,1);
      if (idx->offsets) clever_commit_file_index(idx);
      else simple_commit_file_index(idx);}}
    ON_UNWIND {
#if HAVE_MMAP
      if (idx->offsets) { /* Unmap offsets before closing file */
	int retval=munmap(idx->offsets-2,(sizeof(unsigned int)*idx->size)+8);
	if (retval<0) {
	  fd_unlock_mutex(&(adds->lock));
	  fd_raise_exception(fd_UnmapFailed);}
	idx->offsets=NULL;}
#endif
      if (idx->store) fd_fclose(idx->store); idx->store=NULL;
      if (fd_normal_exit == 0) 
	fd_reinit_hashtable(adds,idx->adds_size,1);
      fd_unlock_mutex(&(adds->lock));}
    END_UNWIND;
}

/*
    simple_commit_file_index:
      arguments: a pointer to an IDX index
      Returns: void
    Writes out the changes to an idx index.
     This assumes that index and its adds hashtable are both locked.
*/
static void simple_commit_file_index(fd_file_index idx)
{
  FILE *stream=get_store(idx); off_t pos=0;
  unsigned int *offsets=idx->offsets, ptr_offset=idx->size*4;
  int threshold=idx->zipf_threshold; 
  fd_hashtable adds=&(idx->adds), sizes=&(idx->sizes);
  fd_pair *scan=adds->table, *limit=scan+adds->n_slots;
  if (stream == NULL)
    fd_raise_detailed_exception(fd_FileOpenWFailed,idx->filename);
  /* If we have an offsets table we can update, we keep track of whether
     we are at the end of the file.  This avoids too many fseeks. */
  if (offsets) {
    fseek(stream,0,SEEK_END); pos=ftello(stream); idx->at_end=1;
    CHECK_POS(pos,idx->id);}
  else idx->at_end=0;
  while (scan < limit)
    if (*scan) {
      fd_pair elt=*scan++; lisp key=elt->car, value=elt->cdr;
      struct INDEX_LOC where;
      off_t key_loc, value_loc;
      unsigned int new_elts;
      /* If the value is empty, we just continue, otherwise, we incref it. */
      if (FD_EMPTYP(value)) continue; else fd_incref(value);
      /* Make sure we've removed duplicates */
      if (CHOICEP(value)) value=fd_return_proper_choice(value);
      /* Is this value worth saving? */
      if ((threshold == 0) || (CHOICE_SIZE(value) > threshold)) {
	/* Find where any current value is stored. */
	search_index_file(idx,key,&where);
	if (CHOICEP(value)) new_elts=CHOICE_SIZE(value); else new_elts=1;
	/* If offsets are NULL, we don't bother keeping at_end up to date,
	   so we just seek. */
	if (offsets == NULL) {
	  fseek(stream,0,SEEK_END);
	  value_loc=pos=ftello(stream); CHECK_POS(pos,idx->id);}
	/* If we know we're at the end of the file, we use that information. */
	else if (idx->at_end) value_loc=pos;
	else {
	  /* Otherwise, we go to the end and save the location */
	  fseek(stream,0,SEEK_END); value_loc=pos=ftello(stream);
	  idx->at_end=1;}
	CHECK_POS(pos,idx->id);
	  
	/* Now, we write all of the values for this key */
	if (CHOICEP(value)) {
	  int n=CHOICE_SIZE(value);
	  DO_CHOICES(elt,value) {
	    pos=pos+fd_fwrite_dtype(elt,stream)+4; n--;
	    if (n) {fwrite_4bytes(1,stream);}
	    else {fwrite_4bytes(where.data,stream);}}
	  END_DO_CHOICES;}
	else {
	  pos=pos+fd_fwrite_dtype(value,stream)+4;
	  fwrite_4bytes(where.data,stream);}
	  
	/* Now, we write the new key entry */
	CHECK_POS(pos,idx->id);
	key_loc=pos;
	fwrite_4bytes(where.n_elts+new_elts,stream);
	fwrite_4bytes(value_loc-ptr_offset,stream);
	pos=pos+8+fd_fwrite_dtype(key,stream);
	/* Now we update the sizes cache.  If we're preloaded, we may
	   end up creating a new entry. */
	if (idx->preloaded)
	  fd_hashtable_increment(sizes,key,new_elts);
	else fd_hashtable_increment_existing(sizes,key,new_elts);
	/* And we update the offsets */
	if (offsets) {
#if ((HAVE_MMAP) && (!(WORDS_BIGENDIAN)))
	  offsets[where.probe]=fd_flip_word(key_loc-ptr_offset);
#else
	  offsets[where.probe]=key_loc-ptr_offset;
#endif
	}
	else {
	  fseek(stream,where.probe*4+8,SEEK_SET);
	  fwrite_4bytes(key_loc-ptr_offset,stream);}}
      fd_decref(value);}
    else scan++;
#if (!(HAVE_MMAP))
  if (idx->offsets) {
    unsigned int *scan=idx->offsets, *limit=scan+idx->size;
    fseek(stream,8,SEEK_SET);
    if (net_order(3) != 3)
      while (scan < limit) {*scan=net_order(*scan); scan++;}
    fwrite(idx->offsets,sizeof(unsigned int),idx->size,stream);
    scan=idx->offsets; limit=scan+idx->size;
    if (net_order(3) != 3)
      while (scan < limit) {*scan=host_order(*scan); scan++;}}
#else
  if (idx->offsets) {
    int retval=munmap(idx->offsets-2,(sizeof(unsigned int)*idx->size)+8);
    if (retval<0) {
      fd_unlock_mutex(&(adds->lock));
      fd_raise_exception(fd_UnmapFailed);}
    idx->offsets=NULL;}
#endif
  update_file_index_change_date(idx);
  fflush(stream); fclose(stream); idx->store=NULL; idx->at_end=0;
  if (idx->buf) fd_xfree(idx->buf);
  idx->buf=NULL;
}

/* This just works by getting all of the current values, removing the dropped values
    and writing out a whole new value.  This is expensive in terms of both time and
    space, but drops are supposed to be relatively rare. */
static void commit_drops(fd_file_index idx)
{
  FILE *stream=get_store(idx);
  unsigned int pos=0, *offsets=idx->offsets, ptr_offset=idx->size*4;
  fd_pair *scan, *limit;
  fd_hashtable drop_table=&(idx->drops);
  fd_hashtable adds_table=&(idx->adds);
  lock_mutex(&(drop_table->lock));
  scan=drop_table->table; limit=scan+drop_table->n_slots;
  if (stream == NULL) {
    unlock_mutex(&(drop_table->lock));
    fd_raise_detailed_exception(fd_FileOpenWFailed,idx->filename);}
  while (scan < limit)
    if (*scan) {
      fd_pair elt=*scan++; lisp key=elt->car, value=elt->cdr;
      lisp current;
      lisp adds=fd_hashtable_get(adds_table,key,FD_EMPTY_CHOICE);
      struct INDEX_LOC where; 
      search_index_file(idx,key,&where);
      if (where.point)
	current=collect_values(idx,where.data,where.n_elts);
      else current=FD_EMPTY_CHOICE;
      if ((FD_CHOICE_SIZE(value) > 5)) {
	fd_hashset h=fd_choice_to_hashset(current);
	DO_CHOICES(drop,value)
	  fd_hashset_drop(h,drop);
	END_DO_CHOICES;
	{DO_CHOICES(add,adds) fd_hashset_add(h,add); END_DO_CHOICES;}
	fd_decref(current); current=fd_final_hashset_elts(h);
	fd_free(h,sizeof(struct FD_HASHSET));}
      else {
	DO_CHOICES(drop,value)
	  current=fd_remove_from_choice(drop,current);
	END_DO_CHOICES;
	{DO_CHOICES(add,adds)
	   ADD_TO_CHOICE(current,fd_incref(add));
	END_DO_CHOICES;}}
      fd_decref(adds);
      { /* Now, write the value out */
	unsigned int key_loc, value_loc, new_elts=CHOICE_SIZE(current);
	fseek(stream,0,SEEK_END); value_loc=pos=ftello(stream);
	CHECK_POS(pos,idx->id);
	if (CHOICEP(current)) {
	  int n=CHOICE_SIZE(current);
	  DO_CHOICES(elt,current) {
	    pos=pos+fd_fwrite_dtype(elt,stream)+4; n--;
	    if (n) {fwrite_4bytes(1,stream);}
	    else {fwrite_4bytes(0,stream);}}
	  END_DO_CHOICES;}
	else if (FD_EMPTYP(current)) value_loc=0;
	else {
	  pos=pos+fd_fwrite_dtype(current,stream)+4;
	  fwrite_4bytes(0,stream);}
	CHECK_POS(pos,idx->id);	key_loc=pos;
	fwrite_4bytes(new_elts,stream);
	if (value_loc) {
	  fwrite_4bytes(value_loc-ptr_offset,stream);}
	else {fwrite_4bytes(0,stream);}
	pos=pos+8+fd_fwrite_dtype(key,stream);
#if ((HAVE_MMAP) && (!(WORDS_BIGENDIAN)))
	if (offsets) offsets[where.probe]=fd_flip_word(key_loc-ptr_offset);
#else
	if (offsets) offsets[where.probe]=key_loc-ptr_offset;
#endif
	else {
	  fseek(stream,where.probe*4+8,SEEK_SET);
	  fwrite_4bytes(key_loc-ptr_offset,stream);}}
      /* Update the sizes cache */
      if ((idx->preloaded) || (fd_hashtable_probe(&(idx->sizes),key)))
	fd_hashtable_set(&(idx->sizes),key,FD_LISPFIX(FD_CHOICE_SIZE(current)));
      /* Remove the entries from DROPS and ADD */
      decref(value); elt->cdr=FD_VOID;
      fd_hashtable_zap(&(idx->adds),key);}
    else scan++;
#if (!(HAVE_MMAP))
  if (idx->offsets) {
    unsigned int *scan=idx->offsets, *limit=scan+idx->size;
    fseek(stream,8,SEEK_SET);
    if (net_order(3) != 3)
      while (scan < limit) {*scan=net_order(*scan); scan++;}
    fwrite(idx->offsets,sizeof(unsigned int),idx->size,stream);
    scan=idx->offsets; limit=scan+idx->size;
    if (net_order(3) != 3)
      while (scan < limit) {*scan=host_order(*scan); scan++;}}
#endif
  fd_reinit_hashtable(drop_table,idx->drops_size,1);
  fd_unlock_mutex(&(drop_table->lock));
}

struct INDEX_KEY_FIND_ENTRY {
  fd_pair entry; int found, probe, chain_width; off_t kpos;};
struct INDEX_KEY_FOUND_ENTRY {
  fd_pair entry; int found, probe, n_values; off_t vpos;};
union INDEX_COMMIT_ENTRY {
  struct INDEX_KEY_FIND_ENTRY find;
  struct INDEX_KEY_FOUND_ENTRY found;};

static void describe_commit_entry(union INDEX_COMMIT_ENTRY *e)
{
  if (e->find.found) {
    struct INDEX_KEY_FOUND_ENTRY *x=&(e->found);
    fd_fprintf(stderr,"found key: %q; probe: %d; n_values: %d; vpos: %u\n",
	       x->entry->car,x->probe,x->n_values,(unsigned long)x->vpos);}
  else {
    struct INDEX_KEY_FIND_ENTRY *x=&(e->find);
    fd_fprintf(stderr,"seeking key: %q; probe: %d; chain_width: %d; kpos: %u\n",
	       x->entry->car,x->probe,x->chain_width,(unsigned long)x->kpos);}
}

static int sort_for_fseek(const void *x,const void *y)
{
  struct INDEX_KEY_FIND_ENTRY *ix=(struct INDEX_KEY_FIND_ENTRY *)x;
  struct INDEX_KEY_FIND_ENTRY *iy=(struct INDEX_KEY_FIND_ENTRY *)y;
  if ((ix->found) && (iy->found)) return 0;
  else if (ix->found) return 1;
  else if (iy->found) return -1;
  else if (ix->kpos<iy->kpos) return -1;
  else return 1;
}

static void clever_commit_file_index(fd_file_index idx)
{
  FILE *stream=get_store(idx);
  fd_hashtable adds=&(idx->adds), sizes=&(idx->sizes);
  int threshold=idx->zipf_threshold, n_slots=idx->size, ptr_offset=n_slots*4;
  int n_keys=adds->n_keys, n_saves=0, table_size=16, keys_found=0;
  int size=idx->size, load=0;
  unsigned int *offsets, pass_no=0, preloaded=idx->preloaded;
  off_t pos=0;
  union INDEX_COMMIT_ENTRY *entries, *escan;
  /* Get offsets, reading them from disk if neccessary */
  if (idx->offsets == NULL) cache_file_index(idx,1);
  offsets=idx->offsets;
  /* Scan over modified keys, counting the keys whose modifications
     are above the save threshold */
  {
    fd_pair *scan=adds->table, *limit=scan+adds->n_slots;
    while (scan < limit)
      if (*scan) {
	fd_pair entry=*scan++;
	lisp values=entry->cdr=fd_return_proper_choice(entry->cdr);
	if ((!(FD_EMPTYP(values))) &&
	    ((threshold == 0) || (CHOICE_SIZE(values)>threshold)))
	  n_saves++;}
      else scan++;}
  /* Compute the load */
  {
    int i=0; while (i < size)
      if (offsets[i]) {load++; i++;} else i++;
  }
  /* Announce what you're about to do */
  fd_notify(_("Commiting %d keys (out of %d) in file index %s"),
	    n_saves,n_keys,idx->filename);
  fd_notify(_("The file index %s is %d%% full (%d of %d slots used)"),
	    idx->filename,((load*100)/size),load,size);
  /* Increase the table size to be larger than n_saves */
  while (table_size < n_saves) table_size=table_size*2;
  entries=fd_malloc(sizeof(union INDEX_COMMIT_ENTRY)*table_size);
  /* In order to avoid seeking around in the file for each key we need,
     to save we build a table with per-key information, which we fill in
     several passes.
   */
  { 
    fd_pair *scan=adds->table, *limit=scan+adds->n_slots; escan=entries;
    /* The first pass attempts to find the offset for each key.  If the
       first try in the offset table is empty, we can just put the value
       there, so we reserve it and record UINT_MAX as the kpos.  Otherwise,
       that key data is the first one to look at.  Each key which gets its
       first slot can be considered `found' and we track the number of keys
       which we have `found'. */
    while (scan < limit)
      if (*scan) {
	fd_pair entry=*scan++; lisp values=entry->cdr;
	if (FD_EMPTYP(values)) continue;
	if ((threshold == 0) || (CHOICE_SIZE(values)>threshold)) {
	  lisp key=entry->car;
	  struct INDEX_KEY_FIND_ENTRY *find_entry=&(escan->find);
	  unsigned int hash=fileindex_hash(idx,key), probe=hash%n_slots;
	  unsigned int chain_width=((hash)%(n_slots-2))+1;
	  find_entry->entry=entry; find_entry->probe=probe;
	  find_entry->found=0; find_entry->chain_width=chain_width;
	  /* An UINT_MAX offset means that the slot will be used for a key
	     currently being committed.  It also means that the slot was empty before,
	     so the key can't have any current values.  So we advance the probe, skipping
	     over full and reserved slots.  */
	  if (offget(offsets,probe)== UINT_MAX) 
	    while (offsets[probe]) probe=(probe+chain_width)%n_slots;
	  /* If you have a filled slot, it is your next kpos to check
	     (which will be sorted on to order disk accesses) */
	  if (offsets[probe]) {
	    find_entry->kpos=offget(offsets,probe); find_entry->probe=probe;}
	  else { /* otherwise, this key can't be in the table at all, so
		    we just pick this location and reserve it */
	    struct INDEX_KEY_FOUND_ENTRY *found_entry=&(escan->found);
	    found_entry->found=1;
	    offsets[probe]=fd_net_order(UINT_MAX); /* reserve it */
	    find_entry->probe=probe; found_entry->vpos=0;
	    found_entry->n_values=0;
	    trace_commit_entry((union INDEX_COMMIT_ENTRY *)found_entry);
	    keys_found++;}
	  escan++;}}
      else scan++;}
  /* Now we find where all the keys really are by doing a series of
     scans.  We start by sorting the keys by the slot that they *might* be
     in. */
  qsort(entries,n_saves,sizeof(union INDEX_COMMIT_ENTRY),sort_for_fseek);
  /* Now we wait for everything to be resolved, checking each potential
     offset in order.  When there are no more unresolved keys, we're done. */
  while (keys_found < n_saves) {
    union INDEX_COMMIT_ENTRY *scan=entries, *limit=scan+(n_saves-keys_found);
    /* We scan all the entries */
    while (scan < limit) {
      int n_values, vpos;
      trace_commit_entry(scan);
      /* Go to the location, read the key data */
      fseeko(stream,(off_t)(scan->find.kpos+ptr_offset),SEEK_SET);
      n_values=fread_4bytes(stream); vpos=fread_4bytes(stream);
      /* do a comparison */
      if (fd_dtype_compare(scan->find.entry->car,stream)) {
	/* If success, switch the entry to an INDEX_KEY_FOUND_ENTRY
	   and init it. */
	struct INDEX_KEY_FOUND_ENTRY *kf=&(scan->found); keys_found++;
	kf->n_values=n_values; kf->vpos=vpos; kf->found=1;
	trace_commit_entry(scan);
	scan++;}
      else {
	struct INDEX_KEY_FIND_ENTRY *kf=&(scan->find);
	/* If you failed, chain forward, skipping over entries which
	   other keys have reserved */
	int chain=(kf->chain_width), nprobe=((kf->probe)+chain)%n_slots;
	/* Skip over slots already taken */
	while (offget(offsets,nprobe)==UINT_MAX) nprobe=(nprobe+chain)%n_slots;
	/* When you find a non-zero non-INT_MAX entry, it's the next kpos,
	   so save it */
	if (offsets[nprobe]) {kf->probe=nprobe; kf->kpos=offget(offsets,nprobe);}
	else {
	  /* If you run into an empty slot, this key must not be in the file,
	     so reserve it, and init the new value structure. */
	  struct INDEX_KEY_FOUND_ENTRY *kf=&(scan->found);
	  kf->probe=nprobe;
#if ((HAVE_MMAP) && (!(WORDS_BIGENDIAN)))
	  offsets[nprobe]=fd_flip_word(UINT_MAX);
#else
	  offsets[nprobe]=UINT_MAX;
#endif
	  kf->found=1; kf->n_values=0; kf->vpos=0; keys_found++;}
	trace_commit_entry(scan);
	scan++;}}
    /* When you're all done, sort by the new kpos, pushing negative probes to the back */
    qsort(entries,n_saves,sizeof(union INDEX_COMMIT_ENTRY),sort_for_fseek);
    pass_no++;
    fd_notify(_("Pass %d: Found %d of %d keys"),pass_no,keys_found,n_saves);}
  fd_notify(_("Found all keys (%d of %d); starting to write values"),
	    keys_found,n_saves);
  fseek(stream,0,SEEK_END); pos=ftello(stream);
  { 
    union INDEX_COMMIT_ENTRY *scan=entries, *limit=scan+n_saves; 
    while (scan < limit) {
      off_t vpos=pos;
      lisp key=scan->found.entry->car, values=scan->found.entry->cdr;
      CHECK_POS(pos,idx->id);
      /* At this point, we update sizes (since we are here). */
      if (preloaded) fd_hashtable_increment(sizes,key,CHOICE_SIZE(values));
      else fd_hashtable_increment_existing(sizes,key,CHOICE_SIZE(values));
      if (CHOICEP(values)) {
	int n_new=CHOICE_SIZE(values), n=n_new;
	DO_CHOICES(elt,values) {
	  pos=pos+fd_fwrite_dtype(elt,stream)+4; n--;
	  if (n) {fwrite_4bytes(1,stream);}
	  else {fwrite_4bytes(scan->found.vpos,stream);}}
	END_DO_CHOICES;
	scan->found.n_values=scan->found.n_values+n_new;}
	else {
	  pos=pos+fd_fwrite_dtype(values,stream)+4;
	  fwrite_4bytes(scan->found.vpos,stream);
	  scan->found.n_values=scan->found.n_values+1;}
      scan->found.vpos=vpos; scan++;}}
    fd_notify(_("Wrote all values, starting to write keys"));
    {
      union INDEX_COMMIT_ENTRY *scan=entries, *limit=scan+n_saves;
      while (scan < limit) {
	struct INDEX_KEY_FOUND_ENTRY *e=&(scan->found); off_t kpos=pos;
	trace_commit_entry(scan);
	CHECK_POS(pos,idx->id);
	fwrite_4bytes(e->n_values,stream);
	if (e->vpos) {fwrite_4bytes(e->vpos-ptr_offset,stream);}
	else {fwrite_4bytes(0,stream);}
	pos=pos+8+fd_fwrite_dtype(e->entry->car,stream);
#if TRACE_CLEVER_COMMIT
	fd_fprintf(stderr,"For %q, Writing %lu into slot %d\n",
		   e->entry->car,(unsigned long)(kpos-ptr_offset),e->probe);
#endif
#if ((HAVE_MMAP) && (!(WORDS_BIGENDIAN)))
	offsets[e->probe]=fd_flip_word(kpos-ptr_offset);
#else
	offsets[e->probe]=kpos-ptr_offset;
#endif
	scan++;}}
#if (!(HAVE_MMAP))
    fd_notify(_("Wrote all key entries, updating offsets table"));
    {
      fseek(stream,8,SEEK_SET);
      /* Fix byte order if neccessary */
      if (net_order(3) != 3) {
	unsigned int *scan=offsets, *limit=scan+n_slots;
	while (scan < limit) {*scan=net_order(*scan); scan++;}}
      fwrite(offsets,sizeof(unsigned int),idx->size,stream);
      /* Re-Fix byte order if neccessary */
      if (net_order(3) != 3) {
	unsigned int *scan=offsets, *limit=scan+n_slots;
	while (scan < limit) {*scan=host_order(*scan); scan++;}}}
    fd_notify(_("Done with offsets table, cleaning up"));
#else
    if (offsets) {
      int retval=munmap(offsets-2,(sizeof(unsigned int)*idx->size)+8);
      if (retval<0) fd_raise_exception(fd_UnmapFailed);
      idx->offsets=NULL;}
#endif
    if (fd_normal_exit == 0) 
      fd_reinit_hashtable(adds,idx->adds_size,1);
    update_file_index_change_date(idx);
    fflush(stream); fclose(stream); idx->store=NULL;
    if (idx->buf) fd_xfree(idx->buf); idx->buf=NULL;
    fd_free(entries,(sizeof(union INDEX_COMMIT_ENTRY)*table_size));
    fd_notify(_("Finished commiting file index %s"),idx->id);
}

void file_index_close(fd_index idx)
{
  fd_file_index index=(fd_file_index) idx;
  UNWIND_PROTECT {
    lock_mutex(&(index->lock));
#if HAVE_MMAP
    if (index->offsets) {
      int retval=munmap(index->offsets-2,(sizeof(int)*index->size)+8);
      if (retval<0) {
	fd_unlock_mutex(&(index->lock));
	fd_raise_exception(fd_UnmapFailed);}}
#else
    if (index->offsets) fd_xfree(index->offsets);
#endif
    index->offsets=NULL;
    if (index->store) fclose(index->store); index->store=NULL;
    if (index->buf) fd_xfree(index->buf);
    index->buf=NULL;}
  ON_UNWIND {
    unlock_mutex(&(index->lock));}
  END_UNWIND;
}

/** Caching File indices **/

/* Caching a file index loads a copy of its offset table into memory, saving
   one file seek+read operation. */

FRAMERD_EXPORT
void fd_auto_cache_file_indices() /* void? */
{
  _fd_auto_cache_file_indices=1;
}

static void cache_file_index_handler(fd_index idx) { cache_file_index((fd_file_index)idx,0); }
static void cache_file_index(fd_file_index idx,int write)
{
  if (idx->offsets == NULL) {
#if HAVE_MMAP
    FILE *f=get_store(idx);
    unsigned int size=idx->size, *off, *mmapblock; int fd;
    fd=fileno(f);
    if (write)
      mmapblock=
	mmap(NULL,sizeof(unsigned int)*size+8,
	     PROT_WRITE|PROT_READ,MMAP_FLAGS,fd,0);
    else mmapblock=
	   mmap(NULL,sizeof(unsigned int)*size+8,
		PROT_READ,MAP_SHARED|MAP_NORESERVE,fd,0);
    if (mmapblock==NULL) {
      fd_raise_exception(fd_MmapFailed);}
    else off=idx->offsets=mmapblock+2;
#else
    FILE *f; unsigned int size=idx->size, *off;
    fd_notify(_("Caching file index %s"),idx->id);
    off=idx->offsets=fd_xmalloc(sizeof(unsigned int)*size);
    f=get_store(idx);
    fseek(f,8,SEEK_SET);
    fread(off,sizeof(unsigned int),size,f);
    if (3 != net_order(3)) {
      unsigned int *scan=off, *limit=scan+size;
      while (scan < limit) {
	unsigned int v=*scan; *scan=host_order(v); scan++;}}
#endif
  }
}

FRAMERD_EXPORT
void fd_cache_file_index(fd_file_index idx)
{
  UNWIND_PROTECT {
    lock_mutex(&(idx->lock));
    cache_file_index(idx,0);}
  ON_UNWIND {
    unlock_mutex(&(idx->lock));}
  END_UNWIND;
}

FRAMERD_EXPORT
void fd_maybe_cache_file_index(fd_file_index idx,int already_locked)
{
  if (already_locked)
    if (idx->offsets) {}
    else if (_fd_auto_cache_file_indices) cache_file_index(idx,0);
    else {}
  else {
    UNWIND_PROTECT {
      lock_mutex(&(idx->lock));
      if (idx->offsets) {}
      else if (_fd_auto_cache_file_indices) cache_file_index(idx,0);
      else {}}
    ON_UNWIND {
      unlock_mutex(&(idx->lock));}
    END_UNWIND;}
}

/** Comparing objects to DTYPEs on disk **/

FRAMERD_EXPORT
/* fd_dtype_compare:
     Arguments: a lisp object and a file stream
     Returns: boolean (int)
   This returns true if the dtype representation on the file stream
    is equal to the lisp object it is handed.  If it returns true (1),
    the file is positioned at the end of the dtype representation.
*/
int fd_dtype_compare (lisp key,FILE *stream)
{
  int code=getc(stream);
  if (code == -1) fd_raise_exception(fd_Unexpected_EOF);
  if (FD_IMMEDIATEP(key)) 
    if (FD_EMPTY_LISTP(key))
      if (code == dt_null) return 1; else return 0;
    else if (FD_FALSEP(key))
      if (code != dt_bool) return 0; else if (getc(stream) != 0) return 0;
      else return 1;
    else if (FD_TRUEP(key))
      if (code != dt_bool) return 0;
      else if (getc(stream) != 1) return 0;
      else return 1;
    else if (FD_EMPTYP(key)) {
      int subcode=fread_byte(stream);
      if (code != dt_framerd) return 0;
      if (subcode == dt_small_set)
	if ((fread_byte(stream)) != 0) return 0;
	else return 1;
      else if (subcode == dt_set)
	if ((fread_4bytes(stream)) != 0) return 0;
	else return 1;
      else return 0;}
    else {
      fd_warn("Comparing with strange immediate (%q)",key);
      if (code == dt_void) return 1;
      else return 0;}
  else if (FIXNUMP(key))
    if (code != dt_fixnum) return 0;
    else if ((int)fread_4bytes(stream) == FIXLISP(key)) return 1;
    else return 0;
  else if (SYMBOLP(key))
    if (code == dt_symbol) {
      int size=fread_4bytes(stream);
      char *name=SYMBOL_NAME(key);
      if (size != (int) strlen(name)) return 0;
      while (*name != 0)
	if ((getc(stream)) != *name++) return 0;
      return 1;}
    else if (code == dt_extended_character) {
      lisp value_on_disk; int same;
      ungetc(code,stream);
      value_on_disk=fd_fread_dtype(stream);
      same=LISP_EQUAL(key,value_on_disk);
      decref(value_on_disk);
      return same;}
    else return 0;
  else if (STRINGP(key))
    if ((code == dt_string) || (code == dt_zstring)) {
      unsigned int size=fread_4bytes(stream);
      if (((int)size) != STRING_LENGTH(key)) return 0;
      else {char *name=STRING_DATA(key), *limit=name+size;
      while (name < limit) if ((getc(stream)) != *name++) return 0;
      return 1;}}
    else if (code == dt_extended_character) {
      lisp value_on_disk; int same;
      ungetc(code,stream);
      value_on_disk=fd_fread_dtype(stream);
      same=LISP_EQUAL(key,value_on_disk);
      decref(value_on_disk);
      return same;}
    else return 0;
  else if (CHOICEP(key)) {
    if (code != dt_framerd) return 0; else {
      int subcode=fread_byte(stream);
      unsigned int i=0, set_size=CHOICE_SIZE(key);
      struct FD_HASHSET hs;
      if (subcode == dt_set) 
	{if (set_size != (unsigned int) fread_4bytes(stream)) return 0;}
      else if (subcode == dt_small_set)
	{if (set_size != (unsigned int) fread_byte(stream)) return 0;}
      fd_init_hashset(&hs,set_size);
      {DO_CHOICES(r,key) fd_hashset_add(&hs,r); END_DO_CHOICES;}
      while (i < set_size) {
	lisp elt=fd_fread_dtype(stream); i++;
	if (fd_hashset_get(&hs,elt)) {decref(elt);}
	else {fd_free_hashset(&hs); decref(elt); return 0;}}
      fd_free_hashset(&hs);
      return 1;}}
  else if (PAIRP(key)) 
    if (code == dt_pair)
      if (fd_dtype_compare(CAR(key), stream))
	if (fd_dtype_compare(CDR(key), stream)) return 1;
	else return 0;
      else return 0;
    else return 0;
  else if (OIDP(key)) {
    FD_OID id=OID_ADDR(key);
    if (code == dt_oid)
      if ((FD_OID_HIGH(id))==fread_4bytes(stream))
	if ((FD_OID_LOW(id))==fread_4bytes(stream)) return 1;
    return 0;}
  else if (CHARACTERP(key)) {
    if (code == dt_extended_character) {
      int c=fread_byte(stream);
      int ccode=CHAR_CODE(key);
      if ((c == dt_ascii_char) || (c == dt_unicode_char)) {
	int n_bytes=fread_byte(stream);
	if ((ccode > 256) && (n_bytes == 1)) return 0;
	else if (n_bytes > 2) return 0; /*Signal error?*/
	else {
	  int on_disk=fread_byte(stream);
	  if (n_bytes == 2) on_disk=(on_disk<<8)+fread_byte(stream);
	  if (on_disk == ccode) return 1;
	  else return 0;}}
      else return 0;}
    return 0;}
  else if (VECTORP(key))
    if (code != dt_vector) return 0; else {
      int i=0, size=fread_4bytes(stream);
      if (size != ((int)VECTOR_LENGTH(key))) return 0;
      else while (i < size) {
	lisp elt=VECTOR_REF(key,i);
	if (fd_dtype_compare(elt,stream)) i++; else return 0;}
      return 1;}
  else {
    lisp value_on_disk; int same;
    ungetc(code,stream);
    value_on_disk=fd_fread_dtype(stream);
    same=LISP_EQUAL(key,value_on_disk);
    decref(value_on_disk);
    return same;}
}

/** Collecting values for keys **/

/* file_index_get: (static)
     Arguments: an index structure and a key
     Returns: a result set of objects associated with the key
   This is the chief fetch function.  It does a double chaining
    hash lookup to return the set of values.
*/
static lisp file_index_fetch(fd_index idx,lisp key)
{
  fd_file_index fix=(fd_file_index)idx;
  struct INDEX_LOC which_slot; lisp v=FD_VOID;
  if ((fix->preloaded) && (fd_hashtable_probe(&(idx->sizes),key) == 0))
    return FD_EMPTY_CHOICE;
  else {
    UNWIND_PROTECT {
      lock_mutex(&(fix->lock));
      search_index_file(fix,key,&which_slot);
      if (which_slot.point)
	v=collect_values(fix,which_slot.data,which_slot.n_elts);
      v=fd_return_sorted_choice(v);}
    ON_UNWIND
      unlock_mutex(&(fix->lock));
    END_UNWIND;
    return v;}
}

static int file_index_fetch_size(fd_index ix,lisp key)
{
  fd_file_index fix=(fd_file_index) ix;
  struct INDEX_LOC which_slot;
  if (fix->preloaded) return 0;
  lock_mutex(&(fix->lock));
  search_index_file(fix,key,&which_slot);
  unlock_mutex(&(fix->lock));
  if (which_slot.point) return which_slot.n_elts;
  else return 0;
}

/* This is an inline procedure which collects a set of values
   It takes an index file and a start pointer for the collection. */
FASTOP fd_lisp_type collect_values_inline
  (FILE *data,unsigned int offset,int n_elts,
   off_t next,fd_choice collection)
{
  int type=-1;
  while (next) {
    lisp elt;
    if (next != 1) fseeko(data,next+offset,SEEK_SET); 
    elt=fd_fread_dtype(data);
    if (FD_EMPTYP(elt))
      fd_warn(_("Empty element in index retrieval"));
    else {
      if (collection->size >= n_elts)
	fd_raise_exception(_("Index value size inconsistency"));
      if (type == 0) {}
      else if (type < 0) type=elt.type;
      else if (type != elt.type) type=0;
      collection->elements.lisp[collection->size]=elt; collection->size++;}
    next=fread_4bytes(data);}
  return type;
}

static lisp collect_values(fd_file_index ix,off_t loc,unsigned int size)
{
  FILE *store=get_store(ix);
  if (size == 0) return FD_EMPTY_CHOICE;
  else if (size == 1) {
    lisp v; fseeko(store,loc+ix->size*4,SEEK_SET);
    v=fd_fread_dtype(store);
    return v;}
  else {
    fd_choice ch=fd_malloca(struct FD_CHOICE);
    fd_lisp_type homogenous;
#if FD_THREADS_ENABLED
    fd_init_mutex(&(ch->lock));
#endif
    ch->sorted=0; ch->size=0; ch->limit=size; ch->elt_type=0; ch->busy=0;
    ch->elements.lisp=fd_malloc(sizeof(fd_lisp)*size);
    homogenous=collect_values_inline(store,ix->size*4,size,loc,ch);
    if (homogenous) {
      union FD_DATA *nelts=fd_malloc(sizeof(union FD_DATA)*size);
      lisp *scan=ch->elements.lisp, *limit=scan+size;
      union FD_DATA *write=nelts;
      while (scan < limit) *write++=(scan++)->data;
      ch->elt_type=homogenous;
      fd_free(ch->elements.lisp,sizeof(lisp)*size);
      ch->elements.data=nelts;
      /* Can we try to return a proper choice here? */
      {RETURN_LISP(choice_type,choice,ch);}}
    else {
      RETURN_LISP(choice_type,choice,ch);}}
}

FRAMERD_EXPORT
/* fd_file_index_collect_values:
     Arguments: a file stream
     Returns: a "result set"
    This reads a linked list stored in a binary file and generates a
     lisp list from its elements.  It returns a "result set" which
     consists of a size and a list of elements.
*/
lisp fd_file_index_collect_values
  (fd_file_index ix,unsigned int loc,unsigned int size)
{
  lisp v;
  lock_mutex(&(ix->lock));
  v=collect_values(ix,loc,size);
  unlock_mutex(&(ix->lock));
  return v;
}

/** Iterating over indices **/

FRAMERD_EXPORT
void fd_for_file_index(fd_file_index idx,void (*fcn)(lisp key,lisp value))
{
  get_store(idx);
  if (idx->offsets) {
    unsigned int i=0; lock_mutex(&(idx->lock)); while (i < idx->size)
      if (idx->offsets[i] == 0) i++; else {
	lisp key, value;
	unsigned int size; off_t data;
	fseek(idx->store,offget(idx->offsets,i)+4*idx->size,SEEK_SET); 
	size=fread_4bytes(idx->store);
	data=fread_4bytes(idx->store);
	key=fd_fread_dtype(idx->store);
	value=collect_values(idx,data,size);
	fcn(key,value); i++;
	decref(key); decref(value);}
    unlock_mutex(&(idx->lock));}
  else {
    unsigned int i=0; lock_mutex(&(idx->lock)); while (i < idx->size) {
      int loc; fseek(idx->store,i*4+8,SEEK_SET);
      loc=fread_4bytes(idx->store); 
      if (loc == 0) i++; else {
	lisp key, value;
	unsigned int size, data;
	fseeko(idx->store,loc+4*idx->size,SEEK_SET);
	size=fread_4bytes(idx->store);
	data=fread_4bytes(idx->store);
	key=fd_fread_dtype(idx->store);
	value=collect_values(idx,data,size);
	fcn(key,value); i++;
	decref(key); decref(value);}}
    unlock_mutex(&(idx->lock));}
}

static int compare_offsets_fn(const void *x,const void *y)
{
  unsigned int *ix=(unsigned int *)x, *iy=(unsigned int *)y;
  if (*ix<*iy) return -1;
  else if (*ix>*iy) return 1;
  else return 0;
}

/* file_index_keys:
     Arguments: a pointer to an index
     Returns: all the keys in the index
*/
static lisp file_index_keys(fd_index idx_arg)
{
  fd_file_index idx=(fd_file_index)idx_arg;
  lisp answer=FD_EMPTY_CHOICE;
  FILE *store; unsigned int *offsets; unsigned int i=0;
  if (idx->preloaded) {
    fd_hashtable sizes=&(idx->sizes);
    WITH_MUTEX_LOCKED((&(sizes->lock))) {
      fd_pair *scan, *limit;
      scan=sizes->table; limit=scan+sizes->n_slots;
      answer=fd_init_choice(sizes->n_keys);
      while (scan < limit)
	if (*scan) {
	  fd_lisp key=fd_incref((*scan)->car);
	  FD_ADD_TO_CHOICE(answer,key);
	  scan++;}
	else scan++;}
    END_WITH_MUTEX_LOCKED((&(sizes->lock)));}
  else {
    WITH_MUTEX_LOCKED(&(idx->lock)) {
      struct KEY_ENTRY *keys=NULL;
      store=get_store(idx);
      offsets=fd_malloc(sizeof(unsigned int)*idx->size);
      fseek(store,8,SEEK_SET);
      fread(offsets,sizeof(unsigned int),idx->size,store);
      if (3 != net_order(3)) {
	unsigned int *scan=offsets, *limit=scan+idx->size;
	while (scan < limit) {
	  unsigned int v=*scan; *scan=host_order(v); scan++;}}
      qsort(offsets,idx->size,sizeof(unsigned int),compare_offsets_fn);
      while (i < idx->size)
	if (offsets[i] == 0) i++;
	else {
	  lisp key;
	  fseeko(idx->store,(off_t)(offsets[i]+4*idx->size+8),SEEK_SET);
	  key=fd_fread_dtype(store);
	  ADD_TO_CHOICE(answer,key);
	  i++;}
      fd_free_int_array(offsets,idx->size);}
    END_WITH_MUTEX_LOCKED(&(idx->lock));}
  if (FD_CHOICEP(answer)) FD_SET_PRIM_TYPE(answer,proper_choice_type);
  return answer;
}

static int locate_uint(unsigned int k,unsigned int *v,int n)
{
  unsigned int *scan=v, *limit=v+n;
  while (scan < limit)
    if (*scan == k) return scan-v; else scan++;
  return -1;
}



struct KEY_ENTRY {fd_lisp key; int n_values;};

/*  This loads up (or unloads) the file indices size cache with all of the
    keys in the table.  In addition to providing a fast cache of frequency
    information, this allows fetching to determine if there is a value
    without actually going to disk. */
static file_index_preloader(fd_index idx_arg,int load_unload)
{
  if (load_unload) {
    FILE *store; int sizes_locked=0;
    fd_hashtable sizes=&(idx_arg->sizes);
    fd_file_index idx=to_file_index(idx_arg);
    unsigned int *offsets=NULL;
    if (idx->preloaded == 0) {
      UNWIND_PROTECT {
	struct KEY_ENTRY *entries=NULL;
	unsigned int i=0, n_keys=0;
	unsigned int *read, *write, *limit;
	lock_mutex(&(idx->lock));
	store=get_store(idx);
	offsets=fd_malloc(sizeof(unsigned int)*idx->size);
	fseek(store,8,SEEK_SET);
	fread(offsets,sizeof(unsigned int),idx->size,store);
	write=read=offsets; limit=offsets+idx->size;
	if (3 == net_order(3))
	  while (read < limit)
	    if (*read) *write++=*read++;
	    else read++;
	else while (read < limit) {
	  int v=*read++; if (v) *write++=host_order(v);}
	n_keys=write-offsets; entries=fd_malloc(sizeof(struct KEY_ENTRY)*n_keys);
	fd_grow_hashtable(sizes,n_keys*3);
	lock_mutex(&(sizes->lock)); sizes_locked=1;
	fd_notify("Preloading %d key entries from index %s",n_keys,idx->id);
	qsort(offsets,n_keys,sizeof(unsigned int),compare_offsets_fn);
	while (i < n_keys) {
	  lisp key; int n_values;
	  fseeko(idx->store,(off_t)(offsets[i]+4*idx->size),SEEK_SET);
	  entries[i].n_values=fread_4bytes(store); /* Read n values */
	  fread_4bytes(store); /* Skip value pointer */
	  entries[i].key=fd_fread_dtype(store); /* Read the key itself */
	  i++;}
	i=0; while (i < n_keys) {
	  _fd_hashtable_set_nolock
	    (sizes,entries[i].key,FD_LISPFIX(entries[i].n_values));
	  fd_decref(entries[i].key); i++;}
	fd_free(entries,sizeof(struct KEY_ENTRY)*n_keys);
	unlock_mutex(&(sizes->lock)); sizes_locked=0;
	fd_notify("Preloaded %d key entries from index %s",n_keys,idx->id);}
      ON_UNWIND {
	if (offsets) fd_free_int_array(offsets,idx->size);
	unlock_mutex(&(idx->lock));
	if (sizes_locked) unlock_mutex(&(sizes->lock));}
      END_UNWIND;
      /* This will keep the sizes table from being reset */
      idx->sizes_size=-(idx->sizes_size);
      /* The final declaration */
      idx->preloaded=1;}}
  else {
    fd_hashtable sizes=&(idx_arg->sizes);
    fd_file_index idx=to_file_index(idx_arg);
    UNWIND_PROTECT {
      fd_lock_mutex(&(idx->lock));
      idx->preloaded=0; idx->sizes_size=-(idx->sizes_size);
      fd_reinit_hashtable(sizes,idx->sizes_size,0);}
    ON_UNWIND 
      fd_unlock_mutex(&(idx->lock));
    END_UNWIND;}
}

/** File index prefetching **/

struct INDEX_SCHEDULE {
  fd_lisp key; int filepos;};
struct KEY_SEARCH_SCHEDULE {
  fd_lisp key; int filepos;
  unsigned int probe, chain;
  int n_values, vpos;};
struct VALUE_FETCH_SCHEDULE {
  fd_lisp key; int filepos;
  fd_lisp values; int n_values;};

static int sort_by_filepos(const void *p1,const void *p2)
{
  struct INDEX_SCHEDULE *i1=(struct INDEX_SCHEDULE *)p1;
  struct INDEX_SCHEDULE *i2=(struct INDEX_SCHEDULE *)p2;
  if (i1->filepos == i2->filepos) return 0;
  else if (i1->filepos == 0) return 1;
  else if (i2->filepos == 0) return -1;
  else if (i1->filepos < i2->filepos) return -1;
  else return 1;
}

static fd_lisp init_index_result(int n)
{
  fd_choice ch=fd_malloca(struct FD_CHOICE);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ch->lock));
#endif
  ch->sorted=0; ch->size=0; ch->limit=n;
  ch->elt_type=object_type; ch->busy=0;
  ch->elements.data = fd_malloc(sizeof(union FD_DATA)*n);    
  {RETURN_LISP(proper_choice_type,choice,ch);}
}

static int fetch_keydata
  (struct KEY_SEARCH_SCHEDULE *ks,fd_file_index fx,lisp keys,int *real_size)
{
  int i=0, j=0, active=0, recently_active=0;
  int *offsets=fx->offsets, size=fx->size, n_found=0;
  FILE *f=get_store(fx);
  fd_hashtable cache=&(fx->cache), size_cache=&(fx->sizes);
  /* We go through the keys and skip the ones which are already cached
     For the ones which aren't cached, we init an element of ks and
     increment j.  Also, if the key actually has values on disk
     (e.g. filepos != 0), we increment active. */
  DO_CHOICES(key,keys)
    if (fd_hashtable_probe(cache,key)) {}
    else if ((fx->preloaded) && (!(fd_hashtable_probe(size_cache,key)))) {
      /*In this case, we know that the key is not in the index */
    }
    else {
      unsigned int hash=fileindex_hash(fx,key), probe=hash%size;
      unsigned int chain=(hash%(size-2))+1, filepos=offget(offsets,probe);
      ks[j].key=key; ks[j].filepos=filepos;
      ks[j].probe=probe; ks[j].chain=chain;
      ks[j].n_values=0; ks[j].vpos=0; j++;
      if (filepos) active++;}
  END_DO_CHOICES;
  /* j is the number of entries we are actually using, which we pass up
     to *real_size.  It is also, for purposes of the loop below, the
     number of `recently active' entries which we will be sorting */
  *real_size=j; recently_active=j;
  /* This loop keeps going until we have found key entries (or
     evidence of their non-existence) for all of the keys.  On each
     pass, we sort the search schedule by filepos (in order to get
     some help from a buffering file system) and begin checking key
     entries in that order.  The sort function treats zero as
     infinity, so we can use a zero filepos to indicate that the
     search for that key has stopped and the sort procedure moves that
     entry to the end (past the value of active). */
  while (active) {
    int still_active=0;
    qsort(ks,recently_active,sizeof(struct KEY_SEARCH_SCHEDULE),
	  sort_by_filepos);
    i=0; while (i < active) {
      int n_vals, vpos;
      fseeko(f,(off_t)(ks[i].filepos+size*4),SEEK_SET);
      n_vals=fread_4bytes(f); vpos=fread_4bytes(f);
      if (fd_dtype_compare(ks[i].key,f)) {
	/* If we find our key, we fill in some more fields in the key entry
	   and set its filepos to zero.  We also increment n_found. */
	ks[i].filepos=0; ks[i].n_values=n_vals; ks[i].vpos=vpos; n_found++;}
      else {
	/* If we don't find our key, we find the next location to
	   check and change filepos to point to that.  We also
	   increment still_active, which tracks how many items there will
	   be to look for on the next pass. */
	int probe=(ks[i].probe+ks[i].chain)%size;
	if (offsets[probe]) {
	  ks[i].filepos=offget(offsets,probe);
	  ks[i].probe=probe; still_active++;}
	/* If there isn't a next location, the key has no values in
	   the file, so we're done.  Note that this doesn't increment
	   n_found, since that is the actual number of keys with values
	   which were found. */
	else {ks[i].filepos=0; ks[i].n_values=0; ks[i].vpos=0;}}
      i++;}
    /* Now we update the variables used to sort and iterate. */
    recently_active=active; active=still_active;}
  return n_found;
}

static void file_index_prefetch_unlocked(fd_file_index fx,lisp keys)
{
  int n_keys=CHOICE_SIZE(keys), ptr_offset=fx->size*4;
  int i, ks_size, vs_size, n_found;
  int active=0, recently_active, initial_active;
  struct KEY_SEARCH_SCHEDULE *ks=
    fd_malloc(sizeof(struct KEY_SEARCH_SCHEDULE)*n_keys);
  struct VALUE_FETCH_SCHEDULE *vs;
  FILE *f=get_store(fx);
  fd_hashtable cache=&(fx->cache), adds=&(fx->adds), drops=&(fx->drops);
  if (fx->offsets == NULL) cache_file_index(fx,0);
  n_found=fetch_keydata(ks,fx,keys,&ks_size);
  /* We make an entry in the cache for each item we prefetch.
     Note that items which are already cached will not be in the key
     schedule, so we will not be clobbering them. */
  i=0; while (i < ks_size)
    fd_hashtable_set(cache,ks[i++].key,FD_EMPTY_CHOICE);
  /* If there aren't really any values, we can exit now.  However, we
     do this after adding entries to the cache (above) because an
     empty entry will keep the system from going to disk to find
     something that's not there (got that?) */
  if (n_found) {
    /* Now we begin fetching values, which uses a different schedule.
       We initialize this structure with the information from the key
       schedule. We don't bother adding keys with no values to the schedule. */
    vs=fd_malloc(sizeof(struct VALUE_FETCH_SCHEDULE)*n_found);
    i=0; while (i < ks_size)
      if (ks[i].n_values) {
	vs[active].key=ks[i].key; vs[active].filepos=ks[i].vpos;
	vs[active].n_values=ks[i].n_values;
	vs[active].values=
	  ((ks[i].n_values>1) ? (init_index_result(ks[i].n_values)) : (FD_EMPTY_CHOICE));
	active++; i++;}
      else i++;
    vs_size=recently_active=active; 
    while (active) {
      int still_active=0;
      /* Now we sort all the entries we used in the previous pass,
	 since that will push the completed entries (filepos == 0) to the end
	 while ordering the rest of the entries. */
      qsort(vs,recently_active,sizeof(struct VALUE_FETCH_SCHEDULE),
	    sort_by_filepos);
      /* All of the active entries are now at the front, ordered by filepos */
      i=0; while (i < active) {
	lisp v; int next;
	/* Go to where the values are and read them as long as you get
	   the continuation value (1).  When you do finish up, set the
	   filepos to whatever you read from disk.  */
	fseeko(f,(off_t)(vs[i].filepos+ptr_offset),SEEK_SET);
	while (1) {
	  v=fd_fread_dtype(f); next=fread_4bytes(f);
	  ADD_TO_CHOICE(vs[i].values,v);
	  if (next != 1) {vs[i].filepos=next; break;}}
	/* If this entry still has a filepos, it is still active. */
	if (vs[i].filepos) still_active++;
	i++;}
      recently_active=active; active=still_active;}
    /* Now we actually put the values that we read into the cache */
    i=0; while (i < vs_size) {
      if (FD_PRIM_TYPEP(vs[i].values,choice_type)) {
	fd_choice ch=PTR_DATA(vs[i].values,choice);
	if ((ch->elt_type) && (ch->elt_type<FD_ATOMIC_LIMIT))
	  fd_sort_choice(vs[i].values);
	if (ch->size == 1) {
	  fd_lisp v; FD_DO_CHOICES(x,vs[i].values) v=fd_incref(x); FD_END_DO_CHOICES;
	  fd_decref(vs[i].values); vs[i].values=v;}
	else {
	  FD_SET_PRIM_TYPE(vs[i].values,proper_choice_type);}}
      fd_hashtable_set_nc(cache,vs[i].key,vs[i].values); i++;}}
  /* Now we have to deal with local adds and drops.  We need to do
     this for every entry in the key schedule, since we created an
     entry for them above. */
  i=0; while (i < ks_size) {
    fd_lisp plus=fd_hashtable_get(adds,ks[i].key,(FD_EMPTY_CHOICE));
    fd_lisp minus=fd_hashtable_get(drops,ks[i].key,(FD_EMPTY_CHOICE));
    if (!(FD_EMPTYP(plus)))
      fd_hashtable_add(&(fx->cache),ks[i].key,plus);
    if (!(FD_EMPTYP(minus)))
      fd_hashtable_drop(&(fx->cache),ks[i].key,minus);
    fd_decref(plus); fd_decref(minus);
    i++;}
  /* Now we can get rid of the data structures.  Note that we didn't
     reference count the keys, so we don't have to deref them. */
  fd_free(ks,sizeof(struct KEY_SEARCH_SCHEDULE)*n_keys);
  if (n_found) {
    i=0; while (i < vs_size) {decref(vs[i].values); i++;}
    fd_free(vs,sizeof(struct VALUE_FETCH_SCHEDULE)*n_found);}
}

static void file_index_prefetch(fd_index ix,lisp keys)
{
  fd_file_index fx=(fd_file_index)ix;
  UNWIND_PROTECT {
    lock_mutex(&(fx->lock));
    file_index_prefetch_unlocked(fx,keys);}
  ON_UNWIND {
    unlock_mutex(&(fx->lock));}
  END_UNWIND;
}

/** Reporting hash table statistics **/

FRAMERD_EXPORT
/* fd_index_report_stats:
    Arguments: none
    Returns: void
   Prints a report on hash table chaining behavior.
*/
void fd_index_report_stats(FILE *stream)
{
  if (n_fetches)
    fprintf(stream,_("In total, %d fetches resolved with %d misses (%2f%%)\n"),
	    n_fetches,n_misses,((double)n_misses*100)/((double) n_fetches));
  if (n_misses)
    {fprintf(stream,_("The misses were resolved by %d additional probes (mean=%f, max=%d)\n"),
	     n_probes,((double)n_probes)/((double)n_misses),max_n_probes);
     fprintf(stream,_("Thus, it took an average of %f probes for each access\n"),
	     ((double)(n_probes+n_fetches))/((double)n_fetches));}
  fflush(stream);
}

/** Initialization **/

static struct FD_INDEX_HANDLER file_index_handler={
  NULL,NULL,NULL,NULL, /* get, get_size, add , drop are all by default */
  file_index_fetch,
  file_index_fetch_size,
  file_index_commit,
  file_index_prefetch,
  file_index_keys,
  cache_file_index_handler,
  file_index_close,
  NULL,
  file_index_preloader};

void fd_initialize_file_index_c()
{
  fd_register_file_index_opener
    (FD_FILE_INDEX_MAGIC_NUMBER,open_std_file_index);
  fd_register_file_index_opener
    (FD_MULT_FILE_INDEX_MAGIC_NUMBER,open_std_file_index);
  fd_register_file_index_opener
    (FD_MULT_FILE3_INDEX_MAGIC_NUMBER,open_std_file_index);

#if FD_THREADS_ENABLED
  fd_init_mutex(&_file_index_lookup_lock);
#endif

  fd_register_source_file("file-index",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: file-index.c,v $
   Revision 1.75  2006/06/27 11:41:06  haase
   Added experimental v3 hash function

   Revision 1.74  2005/03/13 22:45:58  haase
   MMAP fixes

   Revision 1.73  2005/03/07 23:57:20  haase
   Fixed mmap calls to have right size and offset

   Revision 1.72  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.71  2005/01/11 14:47:53  haase
   Simplified offset compare function

   Revision 1.70  2004/11/10 17:22:02  haase
   Added memory mapped offsets to file indices

   Revision 1.69  2004/09/17 07:30:36  haase
   Made file index caching not copy the retrieved choice

   Revision 1.68  2004/08/02 21:15:01  haase
   Fixes to values sorting

   Revision 1.67  2004/07/31 14:07:17  haase
   Made file indices retrieve sorted choices

   Revision 1.66  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.65  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.64  2004/07/19 16:05:45  haase
   Made clever commit tracing be conditional

   Revision 1.63  2004/07/19 14:37:28  haase
   Cleanups, debugging for clever index commits

   Revision 1.62  2004/07/19 07:05:54  haase
   Fixes to clever file index commit

   Revision 1.61  2004/07/17 19:06:21  haase
   Added checking for file overflows in pools and indices

   Revision 1.60  2004/07/16 13:59:46  haase
   Made file indices use off_t

   Revision 1.59  2004/07/04 01:38:30  haase
   Made auto_cache variables be exported

   Revision 1.58  2004/06/25 16:58:43  haase
   Fix pager for cases where SCRIPT_URI of CGI requests is a choice

   Revision 1.57  2004/06/13 13:06:31  haase
   Fix file system link aliasing problem in getting file indices

   Revision 1.56  2004/05/14 14:41:57  haase
   Made preloading be an option for all kinds of indices

   Revision 1.55  2004/05/04 13:05:10  haase
   Fixed typo

   Revision 1.54  2004/05/04 12:59:15  haase
   Fix maybe_cache_file_index

   Revision 1.53  2004/05/03 20:31:51  haase
   Fix autocaching

   Revision 1.52  2004/04/27 17:36:28  haase
   Added verbosity control to file index repacking

   Revision 1.51  2004/03/30 11:38:53  haase
   Removed redundant unicode code from dtype_compare

   Revision 1.50  2004/03/30 11:32:15  haase
   Renamed mult_hash functions

   Revision 1.49  2004/03/12 20:33:14  haase
   Made mult indices be usable

   Revision 1.48  2004/03/12 20:30:26  haase
   Added new kind of index with multiplicative hash function more appropriate to very large indices

   Revision 1.47  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.46  2003/11/04 19:29:49  haase
   Fixed file index fetches of singletons

   Revision 1.45  2003/10/26 02:07:14  haase
   Special fix for case of file pool (or index) foo.pool and directory foo

   Revision 1.44  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.43  2003/09/30 19:08:52  haase
   Wrapped locks around pool/index lookup/creation

   Revision 1.42  2003/09/30 11:16:15  haase
   Added extra locks to protect pool and index registries

   Revision 1.41  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.40.2.5  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.40.2.4  2003/08/15 13:31:01  haase
   Fixed incomplete fix to last patch

   Revision 1.40.2.3  2003/08/15 13:29:35  haase
   Various extensions to file pools and indices to handle custom extensions

   Revision 1.40.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.40.2.1  2003/01/26 20:43:14  haase
   Misc. fixes especially some GC

   Revision 1.40  2002/07/03 21:50:04  haase
   Made index saving decline to reinit hashsets when invoked while exiting

   Revision 1.39  2002/07/03 02:12:42  haase
   Made commiting a file index decline to reinit its hashtable when the process is exiting, saving lots of GC time for large tables

   Revision 1.38  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.37  2002/05/01 22:03:13  haase
   Fixed bug in new fast dropping

   Revision 1.36  2002/04/28 02:24:18  haase
   Fixed comment on fd_read_file_index_metadata

   Revision 1.35  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.34  2002/04/23 22:59:11  haase
   Changed metadaata warning to error

   Revision 1.33  2002/04/23 22:56:32  haase
   Fixed order of repack serial number and metadata block size in file index metadata

   Revision 1.32  2002/04/22 17:50:59  haase
   Index commits now modify the change date in the index metadata

   Revision 1.31  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.30  2002/04/21 14:04:42  haase
   Fixed typo in commit_drops

   Revision 1.29  2002/04/20 19:46:51  haase
   Made large file index drops be more efficient

   Revision 1.28  2002/04/11 00:28:39  haase
   Added some casts

   Revision 1.27  2002/04/10 18:58:15  haase
       Made canonicalization of filenames and server specs use
   fd_get_real_pathname and fd_get_real_hostname, rather than
   trying special kludges.

   Revision 1.26  2002/04/10 16:01:56  haase
   Fixed some more problems with NULL size arguments to metadata functions

   Revision 1.25  2002/04/10 12:28:23  haase
   Fixed handling of NULL size pointer to metadata retrieval functions

   Revision 1.24  2002/04/10 03:02:11  haase
   Added version information to file pools and indices

   Revision 1.23  2002/04/03 18:16:17  haase
   Fixed failure to unlock after unpreloading an index

   Revision 1.22  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
