/* C Mode */

#include <framerd/indextools.h>

FASTOP fileindex_hash(int hashv,fd_lisp key)
{
  switch (hashv) {
  case 1: return fd_hash_dtype(key);
  case 2: return fd_hash_dtype2(key);
  case 3: return fd_hash_dtype3(key);
  default: fd_raise_exception(fd_NotFileIndex);}
}

struct OFFSET_ENTRY {int index; unsigned int offset;};

static int offset_compare_fn(const void *x,const void *y)
{
    struct OFFSET_ENTRY *ix=(struct OFFSET_ENTRY *)x;
    struct OFFSET_ENTRY *iy=(struct OFFSET_ENTRY *)y;
    if (ix->offset == iy->offset) return 0;
    else if (ix->offset == 0) return 1;
    else if (iy->offset == 0) return -1;
    else if (ix->offset<iy->offset) return -1;
    else return 1;
}

static int pos_compare_fn(const void *x,const void *y)
{
  struct FD_ASSOC *ax=(struct FD_ASSOC *)x, *ay=(struct FD_ASSOC *)y;
  if (ax->pos == ay->pos) return 0;
  else if (ax->pos < ay->pos) return -1;
  else return 1;
}

static int n_values_compare_fn(const void *x,const void *y)
{
  struct FD_ASSOC *ax=(struct FD_ASSOC *)x, *ay=(struct FD_ASSOC *)y;
  if (ax->n_values == ay->n_values) return 0;
  else if (ax->n_values > ay->n_values) return -1;
  else return 1;
}

static void read_values(FILE *f,struct FD_ASSOC *assocs,int size,int offset,int verbose)
{
  int i=0;
  while (i < size) {
    if (assocs[i].pos == UINT_MAX) break;
    else {
      fseeko(f,(off_t)(assocs[i].pos+offset),SEEK_SET);
      if (assocs[i].n_values == 1) {
	assocs[i].values=fd_fread_dtype(f);
	assocs[i].pos=UINT_MAX; i++;}
      else {
	fd_lisp value; int npos;
	if (FD_EMPTYP(assocs[i].values)) /* We'll assume that standard case of OID choices */
	  assocs[i].values=fd_init_oid_choice(assocs[i].n_values);
	value=fd_fread_dtype(f); npos=fd_fread_4bytes(f);
	FD_ADD_TO_CHOICE(assocs[i].values,value);
	while (npos == 1) {
	  value=fd_fread_dtype(f);
	  FD_ADD_TO_CHOICE(assocs[i].values,value);
	  npos=fd_fread_4bytes(f);}
	if (npos == 0) {
	  assocs[i].pos=UINT_MAX;
	  if (assocs[i].n_values != (FD_CHOICE_SIZE(assocs[i].values))) {
	    if (verbose)
	      fd_notify(_("Key %q had %d values, should have had %d"),
			assocs[i].key,FD_CHOICE_SIZE(assocs[i].values),assocs[i].n_values);
	    assocs[i].n_values=FD_CHOICE_SIZE(assocs[i].values);}}
	else assocs[i].pos=npos;
	i++;}}}
}

struct FD_ASSOC *fd_read_assocs_from_index
  (FILE *from,int *size,int *read_off,int *hashvp,
   int with_values,int value_min,int value_max,
   char *tag,int verbose)
{
  int n_slots, n_entries=0, n_keys=0, magic_number;
  unsigned int *offsets; struct OFFSET_ENTRY *offsets_table;
  struct FD_ASSOC *assocs; int i=0, hashv;
  fseek(from,0,SEEK_SET); magic_number=fd_fread_4bytes(from);
  if (magic_number == FD_FILE_INDEX_MAGIC_NUMBER) hashv=1;
  else if (magic_number == FD_MULT_FILE_INDEX_MAGIC_NUMBER) hashv=2;
  else if (magic_number == FD_MULT_FILE3_INDEX_MAGIC_NUMBER) hashv=3;
  else fd_raise_exception(fd_NotFileIndex);
  *hashvp=hashv;
  n_slots=fd_fread_4bytes(from);
  if (n_slots == 0) return NULL;
  if (read_off) *read_off=n_slots*4;
  offsets=fd_malloc(sizeof(unsigned int)*n_slots);
  offsets_table=fd_malloc(sizeof(struct OFFSET_ENTRY)*n_slots);
  fread(offsets,sizeof(unsigned int),n_slots,from);
  if (3 != fd_net_order(3)) {
    unsigned int *scan=offsets, *limit=scan+n_slots;
    struct OFFSET_ENTRY *write=offsets_table;
    while (scan < limit) {
	unsigned int v=*scan;
	write->index=scan-offsets;
	write->offset=fd_host_order(v);
	write++; scan++; if (v) n_entries++;}}
  else {
    unsigned int *scan=offsets, *limit=scan+n_slots;
    struct OFFSET_ENTRY *write=offsets_table;
    while (scan < limit) {
      unsigned int v=*scan;
      write->index=scan-offsets;
      write->offset=v;
      write++; scan++; if (v) n_entries++;}}
  assocs=fd_malloc(sizeof(struct FD_ASSOC)*n_entries);
  qsort(offsets_table,n_slots,sizeof(struct OFFSET_ENTRY),offset_compare_fn);
  if (tag)
    fd_notify(_("%s: Reading %d entries from %d slots"),
	      tag,n_entries,n_slots);
  else fd_notify(_("Reading %d entries from %d slots"),n_entries,n_slots);
  i=0; while (i < n_entries) {
    int n_values; 
    if ((n_entries<10) || (((i+1)%(n_entries/10)) == 0)) {
      char buf[16]; sprintf(buf,"%.2f",(i*100.0)/n_entries);
      if (tag)
	fd_notify(_("%s: %s%%: Read %d of %d/%d keys/slots"),
		  tag,buf,i,n_entries,n_slots);
      else fd_notify(_("%s%%: Read %d of %d/%d keys/slots"),
		     buf,i,n_entries,n_slots);}
    fseeko(from,(off_t)(offsets_table[i].offset+n_slots*4),SEEK_SET);
    n_values=fd_fread_4bytes(from);
    if ((n_values >= value_min) && (n_values > 0) &&
	((value_max<0) || (n_values<value_max))) {
      assocs[n_keys].index=offsets_table[i].index;
      assocs[n_keys].n_values=n_values;
      assocs[n_keys].pos=fd_fread_4bytes(from);
      assocs[n_keys].values=FD_EMPTY_CHOICE;
      assocs[n_keys].key=fd_fread_dtype(from);
      assocs[n_keys].hash=fileindex_hash(hashv,assocs[n_keys].key);
      n_keys++; i++;}
    else i++;}
  fd_free(offsets,n_slots*sizeof(int));
  fd_free(offsets_table,n_slots*sizeof(struct OFFSET_ENTRY));
  if ((with_values) && (n_keys != 0)) {
    int n_active=n_keys;
    qsort(assocs,n_active,sizeof(struct FD_ASSOC),pos_compare_fn);
    n_active=0; while ((n_active < n_keys) && (assocs[n_active].pos<UINT_MAX))
      n_active++;
    while (assocs[0].pos<UINT_MAX) {
      read_values(from,assocs,n_keys,n_slots*4,verbose);
      qsort(assocs,n_active,sizeof(struct FD_ASSOC),pos_compare_fn);
      n_active=0; while ((n_active < n_keys) &&
			 (assocs[n_active].pos<UINT_MAX))
	n_active++;}}
  *size=n_keys;
  return assocs;
}

int fd_start_file_index(FILE *to,int new_size,int hashv,
			fd_lisp metadata,int version,time_t make,time_t change)
{
  int i=0;
  if (hashv==3) {
    fd_fwrite_4bytes(FD_MULT_FILE3_INDEX_MAGIC_NUMBER,to);}
  else if (hashv==2) {
    fd_fwrite_4bytes(FD_MULT_FILE_INDEX_MAGIC_NUMBER,to);}
  else {fd_fwrite_4bytes(FD_FILE_INDEX_MAGIC_NUMBER,to);}
  fd_fwrite_4bytes(new_size,to);
  while (i < new_size) {fd_fwrite_4bytes(0,to); i++;}
  fd_fwrite_4bytes(0xFFFFFFFE,to); fd_fwrite_4bytes(40,to);
  fd_fwrite_4bytes(version,to);
  /* Copy creation timestamp */
  if (make < 0) {
    fd_fwrite_4bytes(0,to); fd_fwrite_4bytes((int)0,to);}
  else {
    fd_fwrite_4bytes(0,to); fd_fwrite_4bytes((int)make,to);}
  /* Write repack timestamp */
  fd_fwrite_4bytes(0,to); fd_fwrite_4bytes((int)time(NULL),to);
  /* Copy change timestamp */
  fd_fwrite_4bytes(0,to); fd_fwrite_4bytes((int)change,to);
  /* Write the metadata */
  if (FD_EMPTYP(metadata)) {fd_fwrite_4bytes(0,to);}
  else {
    fd_fwrite_4bytes(8+new_size*4+40,to);
    fd_fwrite_dtype(metadata,to);}
  return new_size;
}

void fd_sort_assocs_by_n_values(struct FD_ASSOC *assocs,int n_keys)
{
  qsort(assocs,n_keys,sizeof(struct FD_ASSOC),n_values_compare_fn);
}

static void read_values_from_index
  (FILE *from,struct FD_ASSOC *assocs,int n_keys,int offset,int verbose)
{
  int n_active=n_keys;
  qsort(assocs,n_active,sizeof(struct FD_ASSOC),pos_compare_fn);
  n_active=0; while ((n_active < n_keys) && (assocs[n_active].pos<UINT_MAX))
    n_active++;
  while (assocs[0].pos<UINT_MAX) {
    read_values(from,assocs,n_keys,offset,verbose);
    qsort(assocs,n_active,sizeof(struct FD_ASSOC),pos_compare_fn);
    n_active=0; while ((n_active < n_keys) &&
		       (assocs[n_active].pos<UINT_MAX))
      n_active++;}
}

static int write_values_to_index
  (FILE *to,struct FD_ASSOC *assocs,int n_keys,int pos,int verbose)
{
  int i=0; while (i < n_keys)
    if (assocs[i].n_values == 0) i++;
    else if (assocs[i].n_values == 1) {
      assocs[i].pos=pos;
      pos=pos+fd_fwrite_dtype(assocs[i].values,to)+4;
      fd_fwrite_4bytes(0,to); i++;}
    else {
      int n= FD_CHOICE_SIZE(assocs[i].values); assocs[i].pos=pos;
      if (n != assocs[i].n_values) {
	if (verbose)
	  fd_notify("The key %q had %d (%d stored, %d unique) redundant values",
		    assocs[i].key,assocs[i].n_values-n,assocs[i].n_values,n);
	assocs[i].n_values=n;}
      {FD_DO_CHOICES(elt,assocs[i].values) {
	pos=pos+fd_fwrite_dtype(elt,to)+4; n--;
	if (n) {fd_fwrite_4bytes(1,to);}
	else {fd_fwrite_4bytes(0,to);}}
      FD_END_DO_CHOICES;}
      i++;}
  return pos;
}

static void free_values_in_assocs(struct FD_ASSOC *assocs,int n_keys)
{
  int i=0; while (i < n_keys) {
    fd_decref(assocs[i].values); i++;}
}

FASTOP void sort_assoc_values(struct FD_ASSOC *assoc) 
{
  if (FD_CHOICEP(assoc->values)) {
    fd_choice ch=FD_PTR_DATA(assoc->values,choice);
    if (ch->elt_type) fd_sort_choice(assoc->values);}
}

static void sort_assocs_values(struct FD_ASSOC *assocs,int n_keys) 
{
  int i=0; while (i < n_keys) sort_assoc_values(&(assocs[i++]));
}

int fd_copy_assoc_values
  (struct FD_ASSOC *assocs,int n_keys,FILE *from,FILE *to,int pos,int r_off,int verbose) 
{
  int npos;
  read_values_from_index(from,assocs,n_keys,r_off,verbose);
  sort_assocs_values(assocs,n_keys);
  npos=write_values_to_index(to,assocs,n_keys,pos,verbose);
  free_values_in_assocs(assocs,n_keys);
  return npos;
}

void fd_write_keys_to_index(FILE *to,struct FD_ASSOC *assocs,
			    int n_keys,int hashv,int new_size,
			    int pos,char *tag)
{
  unsigned int *offsets, chunk=n_keys/10;
  int i=0;
  fd_notify(_("Writing key entries to new file"));
  while (i < n_keys) 
    if (assocs[i].n_values == 0) i++;
    else {
      int kpos=pos;
      if ((chunk==0) || (((i+1)%chunk) == 0)) {
	char buf[16]; sprintf(buf,"%.2f",(i*100.0)/n_keys);
	if (tag)
	  fd_notify(_("%s: %s%%: Wrote %d of %d keys"),tag,buf,i,n_keys);
	else fd_notify(_("%s%%: Wrote %d of %d keys"),buf,i,n_keys);}
      fd_fwrite_4bytes(assocs[i].n_values,to);
      fd_fwrite_4bytes(assocs[i].pos,to);
      pos=pos+8+fd_fwrite_dtype(assocs[i].key,to);
      assocs[i].pos=kpos; i++;}
  /* Sort by size to give more common keys priority */
  qsort(assocs,n_keys,sizeof(struct FD_ASSOC),n_values_compare_fn);
  offsets=fd_malloc(new_size*sizeof(unsigned int));
  fd_notify(_("Recomputing hash table for the new file"));
  i=0; while (i < new_size) offsets[i++]=0;
  i=0; while (i < n_keys) {
    unsigned int hash=fileindex_hash(hashv,assocs[i].key), probe=hash%new_size;
    unsigned int chain=(hash%(new_size-2))+1;
    if ((chunk==0) || (((i+1)%chunk) == 0)) {
      char buf[16]; sprintf(buf,"%.2f",(i*100.0)/n_keys);
      if (tag)
	fd_notify(_("%s: %s%%: Rehashed %d of %d keys"),tag,buf,i,n_keys);
      else fd_notify(_("%s%%: Rehashed %d of %d keys"),buf,i,n_keys);}
    while (offsets[probe]) {
      probe=(probe+chain)%new_size;}
    offsets[probe]=assocs[i].pos; i++;}
  fseek(to,8,SEEK_SET);
  fd_notify(_("Writing new offsets table"));
  i=0; while (i < new_size) {fd_fwrite_4bytes(offsets[i],to); i++;}
}

void fd_write_assocs_to_index
   (FILE *to,struct FD_ASSOC *assocs,int n_keys,int hashv)
{
  int new_size=fd_select_table_size(n_keys*2), pos=8;
  int i, *offsets;
  fd_fwrite_4bytes(IDX_MAGIC_NUMBER,to); fd_fwrite_4bytes(new_size,to);
  i=0; while (i < new_size) {fd_fwrite_4bytes(0,to); i++;}
  i=0; while (i < n_keys)
    if (assocs[i].n_values == 0) i++;
    else if (assocs[i].n_values == 1) {
      assocs[i].pos=pos;
      pos=pos+fd_fwrite_dtype(assocs[i].values,to)+4;
      fd_fwrite_4bytes(0,to); i++;}
    else {
      int n= FD_CHOICE_SIZE(assocs[i].values); assocs[i].pos=pos;
      {FD_DO_CHOICES(elt,assocs[i].values) {
	pos=pos+fd_fwrite_dtype(elt,to)+4; n--;
	if (n) {fd_fwrite_4bytes(1,to);}
	else {fd_fwrite_4bytes(0,to);}}
       FD_END_DO_CHOICES;}
      i++;}
  i=0; while (i < n_keys) 
    if (assocs[i].n_values == 0) i++;
    else {
      int kpos=pos;
      fd_fwrite_4bytes(assocs[i].n_values,to);
      fd_fwrite_4bytes(assocs[i].pos,to);
      pos=pos+8+fd_fwrite_dtype(assocs[i].key,to);
      assocs[i].pos=kpos; i++;}
  /* Sort by size to give more common keys priority */
  qsort(assocs,n_keys,sizeof(struct FD_ASSOC),n_values_compare_fn);
  offsets=fd_malloc(new_size*sizeof(unsigned int));
  i=0; while (i < new_size) offsets[i++]=0;
  i=0; while (i < n_keys) {
    unsigned int hash=fileindex_hash(hashv,assocs[i].key), probe=hash%new_size;
    unsigned int chain=(hash%(new_size-2))+1;
    while (offsets[probe]) {
      probe=(probe+chain)%new_size;}
    offsets[probe]=assocs[i].pos; i++;}
  fseek(to,8,SEEK_SET);
  i=0; while (i < new_size) {fd_fwrite_4bytes(offsets[i],to); i++;}
}


/* File specific stuff */

/* The CVS log for this file
   $Log: indextools.c,v $
   Revision 1.27  2007/06/30 16:21:06  haase
   Various 64 bit fixes, together with stuff for repacking indices with less than 10 keys

   Revision 1.26  2006/07/08 23:24:33  haase
   Verbosity controls for index maintenance functions

   Revision 1.25  2006/06/27 13:11:39  haase
   Fixed bug introduced in adding the new experimental hash function

   Revision 1.24  2006/06/27 11:41:06  haase
   Added experimental v3 hash function

   Revision 1.23  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.22  2004/07/19 14:39:23  haase
   Further largefile index changes

   Revision 1.21  2004/07/16 14:09:26  haase
   more off_t fixes

   Revision 1.20  2004/04/27 17:36:28  haase
   Added verbosity control to file index repacking

   Revision 1.19  2004/04/04 17:02:54  haase
   Warn about redundancies and fix when copying values

   Revision 1.18  2004/03/30 11:32:15  haase
   Renamed mult_hash functions

   Revision 1.17  2004/03/13 01:02:43  haase
   Fixed bug in checking file index type for indextools

   Revision 1.16  2004/03/12 21:18:50  haase
   Extend new indices to indextools etc.

   Revision 1.15  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.14.2.2  2003/01/26 20:43:56  haase
   Misc. fixes especially some GC

   Revision 1.14.2.1  2002/09/26 02:09:59  haase
   Add -d flag to analyze-index

   Revision 1.14  2002/06/03 22:14:18  haase
   Shortened lines of progress reports

   Revision 1.13  2002/06/03 21:51:21  haase
   Progress reports now provide more context

   Revision 1.12  2002/04/28 02:24:47  haase
   Write correct offset information for metadata dtype

   Revision 1.11  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.10  2002/04/16 16:14:39  haase
   Fixed some inconsistent returns

   Revision 1.9  2002/04/10 03:02:11  haase
   Added version information to file pools and indices

   Revision 1.8  2002/04/03 01:33:09  haase
   Moved indextools out of FD_SOURCE core

   Revision 1.7  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
