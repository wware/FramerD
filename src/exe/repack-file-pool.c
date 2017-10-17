/* -*- Mode: C; -*- */

/* Copyright (C) beingmeta inc, 2001-2005
   Implemented by Ken Haase as part of FramerD

   This implements optimized repacking of file pools including
   scheduling of disk accesses.

   $Id: repack-file-pool.c,v 1.23 2007/06/30 16:21:06 haase Exp $
*/

#include "framerd/indextools.h"
#include <limits.h>

static char *usage=_("Usage: repack-file-pool [-q|-v|-d] <in> [out]\n");
static char *not_readable=_("Couldn't open file for reading: %s");
static char *not_writable=_("Couldn't open file for writing: %s");

struct READ_SCHEDULE {unsigned int index; off_t offset;};

static int read_schedule_compare_fn(const void *x,const void *y)
{
    const struct READ_SCHEDULE *ix=(const struct READ_SCHEDULE *)x;
    const struct READ_SCHEDULE *iy=(const struct READ_SCHEDULE *)y;
    if (ix->offset == iy->offset) return 0;
    else if (ix->offset == 0) return 1;
    else if (iy->offset == 0) return -1;
    else if (ix->offset<iy->offset) return -1;
    else return 1;
}

static int read_schedule_index_compare_fn(const void *x,const void *y)
{
    const struct READ_SCHEDULE *ix=(const struct READ_SCHEDULE *)x;
    const struct READ_SCHEDULE *iy=(const struct READ_SCHEDULE *)y;
    if (ix->index == iy->index) return 0;
    else if (ix->index < iy->index) return -1;
    else return 1;
}

static void copy_binary_file(char *from,char *to)
{
  FILE *in=fd_fopen_locked(from,"r+b",0), *out;
  int  bufsize=65536, ret_value=0, bytes=0;
  char *buf=fd_xmalloc(bufsize), *realname=NULL;
  if (fd_symbolic_linkp(to)) {
    realname=fd_get_real_pathname(to);
    out=fd_fopen_locked(realname,"wb",0);}
  else out=fd_fopen_locked(to,"wb",0);
  if (errno) {perror("Start of binary copy"); FD_CLEAR_ERR();}
  if (in == NULL) 
    fd_raise_detailed_exception(fd_FileOpenFailed,from);
  else if (out == NULL) 
    fd_raise_detailed_exception(fd_FileOpenWFailed,to);
  else while ((ret_value=fread(buf,sizeof(char),bufsize,in)) ||
	      (!(feof(in)))) {
    bytes=bytes+ret_value; fwrite(buf,sizeof(char),ret_value,out);}
  fclose(out); fclose(in); fd_xfree(buf);
  if (realname) fd_xfree(realname);
}

int main(int argc,char *argv[])
{
  fd_lisp header, metadata;
  time_t make, repack, change;
  unsigned int major_version; fd_u8char *minor_version;
  unsigned int code, oid_hi, oid_lo, capacity, load;
  unsigned int chunk, base=1, n_args, need_copy=0, newcap=-1;
  FILE *in, *out=NULL;
  char tmpbuf[PATH_MAX]="/tmp/fdrfpXXXXXX";
  unsigned int i, errors=0; struct READ_SCHEDULE *read_schedule;
  off_t header_off, write_pos;
  int dump=0;
  if (getenv("FRAMERD_TMPDIR")) {
    strcpy(tmpbuf,getenv("FRAMERD_TMPDIR"));
    strcat(tmpbuf,"/fdrfpXXXXXX");}
  while (base < argc)
    if (strcmp(argv[base],"-d") == 0) {
      dump=1; base++;}
    else if ((strcmp(argv[base],"-q")) == 0) {
      fd_disable_notifications(); base++;}
    else break;
  if (base == argc) {
    fprintf(stderr,usage); fd_exit(1); return 1;}
  n_args=argc-base;
  fd_initialize_framerd();
  in=fd_fopen_locked(argv[base],"rb",0);
  if (in == NULL) {
    fprintf(stderr,not_readable,argv[base]); exit(1);}
  code=fd_fread_4bytes(in);
  if (code == FD_FILE_POOL_MAGIC_NUMBER) {
    metadata=fd_read_file_pool_metadata
      (in,&major_version,&minor_version,&make,&repack,&change);
    /* Fixup times if neccessary */
    fd_notify(_("Repacking file pool %s with version id %u:%s"),
	      argv[base],major_version,minor_version);
    if (n_args >= 2) {
      fd_notify(_("Writing new file pool to %s"),argv[base+1]);
      out=fd_fopen_locked(argv[base+1],"wb",0);
      if (out == NULL) {
	fprintf(stderr,not_writable,argv[base+1]);
	exit(1);}}
    else if (!dump) {
      fd_notify(_("Writing new file pool to temporary file %s"),tmpbuf);
      out=fd_fopen_tmpfile(tmpbuf,"wb"); need_copy=1;
      if (out == NULL) {
	fprintf(stderr,not_writable,argv[base+1]);
	exit(1);}}}
  else {
    fclose(in);
    fprintf(stderr,_("Error: %s is not a file pool\n"),argv[base]);
    fd_exit(1); return 1;}
  /* Read header information, set up read schedule */
  oid_hi=fd_fread_4bytes(in); oid_lo=fd_fread_4bytes(in);
  capacity=fd_fread_4bytes(in); load=fd_fread_4bytes(in);
  header_off=(off_t)fd_fread_4bytes(in);
  if (n_args==3) {
    newcap=strtol(argv[base+2],NULL,10);
    if (newcap<load) {
      fclose(in); fclose(out);
      fprintf(stderr,_("Error: new capacity is too small\n"),argv[base+2]);
      exit(1);}}
  else newcap=capacity;
  read_schedule=fd_malloc(sizeof(struct READ_SCHEDULE)*load);
  fd_notify(_("Reading and sorting seek schedule for %d OIDs"),load);
  i=0; while (i < load) {
    read_schedule[i].index=i;
    read_schedule[i].offset=fd_fread_4bytes(in);
    i++;}
  /* Sort the read schedule */
  qsort(read_schedule,load,sizeof(struct READ_SCHEDULE),
	read_schedule_compare_fn);
  if (dump)
    printf("Base OID @%x/%x Capacity 0x%x, Load 0x%x\n\n",
      oid_hi, oid_lo, capacity, load);
  if (out != NULL) {
    fd_notify(_("Writing new file pool"));
    /* Write header code, base OID, capacity, and load */
    fd_fwrite_4bytes(code,out);
    fd_fwrite_4bytes(oid_hi,out); fd_fwrite_4bytes(oid_lo,out);
    fd_fwrite_4bytes(newcap,out); fd_fwrite_4bytes(load,out);
    /* Get the LISP header when you're through with the data */
    fd_fwrite_4bytes(0,out); 
    /* Write an empty offset table (we'll rewrite it later) */
    i=0; while (i < newcap) {fd_fwrite_4bytes(0,out); i++;}
    write_pos=(6+newcap)*4;  chunk=load/10;
    /* Write out the metadata segment */
    fd_fwrite_4bytes(0xFFFFFFFE,out); write_pos=write_pos+4;
    fd_fwrite_4bytes(40,out); write_pos=write_pos+4;
    fd_fwrite_4bytes(major_version+1,out); write_pos=write_pos+4;
    /* Copy creation timestamp */
    if (make < 0) {
      fd_fwrite_4bytes(0,out); fd_fwrite_4bytes((int)0,out);
      write_pos=write_pos+8;}
    else {
      fd_fwrite_4bytes(0,out); fd_fwrite_4bytes((int)make,out);
      write_pos=write_pos+8;}
    /* Write repack timestamp */
    fd_fwrite_4bytes(0,out); fd_fwrite_4bytes((int)time(NULL),out);
    write_pos=write_pos+8;
    /* Copy change timestamp */
    fd_fwrite_4bytes(0,out); fd_fwrite_4bytes((int)change,out);
    write_pos=write_pos+8;
    /* Write out the metadata itself */
    if (FD_EMPTYP(metadata)) {
      fd_fwrite_4bytes(0,out); write_pos=write_pos+4;}
    else { 
      fd_fwrite_4bytes(write_pos+4,out); write_pos=write_pos+4;
      write_pos=write_pos+fd_fwrite_dtype(metadata,out);}}
  /* Read and write the OIDs themselves in chunks  */
  i=0; while (i < load) {
    fd_lisp entry; off_t value_pos=write_pos;
    if ((out != NULL) && (chunk>0) && (((i+1)%chunk) == 0)) {
      char buf[16]; sprintf(buf,"%.2f",(i*100.0)/load);
      fd_notify(_("%s: %s%%: Copied %d of %d OIDs (%u bytes)"),
		argv[base],buf,i,load,
		(unsigned int)(write_pos-(6+newcap)*4));}
    fseeko(in,read_schedule[i].offset,SEEK_SET);
    {FD_WITH_HANDLING 
       entry=fd_fread_dtype(in);
     FD_ON_EXCEPTION {
       fd_exception ex=fd_theException();
       fd_u8char *details=fd_exception_details();
       fd_notify(_("Error copying @%x/%x: %s (%s)"),
		 oid_hi,oid_lo+read_schedule[i].index,ex,details);
       errors=1;
       entry=fd_make_exception
	 (FD_MAKE_LIST(2,fd_copy_string(ex),fd_copy_string(details)));
       fd_clear_exception();}
     FD_END_HANDLING;}
    if (dump) {
      printf("@%x/%x:\n",oid_hi,oid_lo+read_schedule[i].index);
      fd_pprint_lisp(entry,stdout,80);
      fputc('\n',stdout);}
    if (out != NULL) {
      if (FD_PRIM_TYPEP(entry,mystery_type)) {
        fd_notify(_("Mystery copying @%x/%x"),
		oid_hi,oid_lo+read_schedule[i].index);
        write_pos=write_pos+fd_fwrite_dtype(FD_FALSE,out);}
      else write_pos=write_pos+fd_fwrite_dtype(entry,out);}
    read_schedule[i].offset=value_pos;
    fd_decref(entry); i++;}
  if (header_off) {
    fd_lisp header;
    fseeko(in,header_off,SEEK_SET); header=fd_fread_dtype(in);
    if (dump) {
      printf("Label: ");
      fd_pprint_lisp(header,stdout,80);
      fputc('\n',stdout);}
    if (out != NULL) fd_fwrite_dtype(header,out);}
  else write_pos=0;
  fclose(in); /* All done with it */
  if (out != NULL) {
    /* Put things back in order */
    qsort(read_schedule,load,sizeof(struct READ_SCHEDULE),
	read_schedule_index_compare_fn);
    fseek(out,20,SEEK_SET); fd_fwrite_4bytes(write_pos,out); 
    i=0; while (i < load) {
      fd_fwrite_4bytes(read_schedule[i].offset,out); i++;}
    fclose(out);
    if (need_copy) {
      if (errors) {
        fd_notify(_("Errors repacking file, output left in %s"),tmpbuf);}
      else {
        fd_notify(_("Copying output file back into original"));
        copy_binary_file(tmpbuf,argv[base]);
	remove(tmpbuf);}}}
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: repack-file-pool.c,v $
   Revision 1.23  2007/06/30 16:21:06  haase
   Various 64 bit fixes, together with stuff for repacking indices with less than 10 keys

   Revision 1.22  2006/04/20 14:13:51  haase
   Fixed various typos in stylesheets

   Revision 1.21  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.20  2004/10/18 23:04:24  haase
   Made repacking remove its temp files

   Revision 1.19  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.18  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.17  2004/06/22 16:17:41  haase
   Fix rounding with rationals

   Revision 1.16  2004/03/31 11:19:56  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.15  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.14  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.13  2003/10/24 18:52:45  haase
   Fixes to pool metadata accesss

   Revision 1.12  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.11  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.10.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.10.2.1  2003/01/26 20:40:08  haase
   Misc. fixes and added dump option

   Revision 1.10  2002/06/03 21:51:21  haase
   Progress reports now provide more context

   Revision 1.9  2002/05/28 08:16:36  haase
   Added better messages to repack-file-*

   Revision 1.8  2002/05/13 06:51:01  haase
   Fix exception alloc/free bug

   Revision 1.7  2002/04/22 17:56:32  haase
   Fixed divide by zero case for very small file pools

   Revision 1.6  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.5  2002/04/12 15:42:08  haase
   Repacking a linked file now copies into the target rather than overwriting the link; also moved code out of FD_SOURCE

   Revision 1.4  2002/04/10 03:02:10  haase
   Added version information to file pools and indices

   Revision 1.3  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
