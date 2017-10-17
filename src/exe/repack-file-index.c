/* -*- Mode: C; -*- */

/* Copyright (C) beingmeta inc, 2001-2005
   Implemented by Ken Haase as part of FramerD

   This implements optimized repacking of file indices with scheduling
   of file reads to minimize disk accesses.  It also removes duplicate
   values and sorts based on OID id.

   $Id: repack-file-index.c,v 1.36 2007/06/30 16:21:06 haase Exp $ */

#include <framerd/indextools.h>
#include <limits.h>
#include <math.h>
#ifndef PATH_MAX
#define PATH_MAX 1023
#endif

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

static void describe_usage()
{
  fprintf(stderr,
	  "Usage: repack-file-index <index file> [output file]\n");
  fprintf(stderr,
	  " Options: [minvals=n] [maxvals=n] [blocksize=n]\n");
  fprintf(stderr,
	  " Options: [newsize=0|n] [newhash=1|2|3] [verbose=1_0]\n");
  fd_exit(1);
}

int main(int argc,char *argv[])
{
  struct FD_ASSOC *assocs;
  time_t make, repack, change;
  fd_lisp metadata, size_arg; int major_version; off_t minor_version;
  int i=0, n_keys, min=0, max=-1, bsize_max=4*65536, newhashv=-1, hashv=0;
  int r_off,  base, n_args, need_copy=0, new_size=-1, magic_number, old_size;
  char *infile=NULL, *outfile=NULL, tmpbuf[PATH_MAX]="/tmp/fdrpiXXXXXX";
  fd_u8char **params; int param_count, verbose;
  char *filebase;
  FILE *in, *out;
  if (getenv("FRAMERD_TMPDIR")) {
    strcpy(tmpbuf,getenv("FRAMERD_TMPDIR"));
    strcat(tmpbuf,"/fdrfiXXXXXX");}
  params=fd_cmdline(&param_count,argv,argc);
  if ((param_count < 1) || (param_count > 2)) {
    describe_usage(); exit(1);}
  verbose=fd_int_getenv("VERBOSE",0);
  max=fd_int_getenv("MAXVALS",-1);
  min=fd_int_getenv("MINVALS",-1);
  bsize_max=fd_int_getenv("BLOCKSIZE",bsize_max);
  size_arg=fd_getenv("NEWSIZE");
  newhashv=fd_int_getenv("NEWHASH",-1);
  infile=params[0];
  if (param_count > 1) outfile=params[1];
  fd_initialize_framerd();
  filebase=fd_basename(infile,1);
  /* r+b is neccessary here */
  in=fd_fopen_locked(infile,"r+b",0);
  if (in == NULL) {
    fd_fprintf(stderr,"Error: file %s does not exist\n",infile);
    fd_exit(1); return 1;}
  if ((outfile) && (strcmp(outfile,infile) != 0))
    out=fd_fopen_locked(outfile,"wb",0);
  else {out=fd_fopen_tmpfile(tmpbuf,"wb"); need_copy=1;}
  magic_number=fd_fread_4bytes(in);
  if (magic_number == FD_FILE_INDEX_MAGIC_NUMBER) {}
  else if (magic_number == FD_MULT_FILE_INDEX_MAGIC_NUMBER) {}
  else {
    fprintf(stderr,"The file %s is not a file index!\n",infile);
    fclose(in); fclose(out); exit(1);}
  old_size=fd_fread_4bytes(in);
  metadata=fd_read_file_index_metadata
    (in,&major_version,&minor_version,&make,&repack,&change);
  fd_notify("Repacking file index %s, version %d:%u",
	    infile,major_version,(unsigned int)minor_version);
  assocs=fd_read_assocs_from_index
    (in,&n_keys,&r_off,&hashv,0,min,max,filebase,verbose);
  fd_sort_assocs_by_n_values(assocs,n_keys);
  if (FD_EMPTYP(size_arg))
    new_size=fd_select_table_size(2*n_keys);
  else if (FD_FIXNUMP(size_arg))
    if (FD_FIXLISP(size_arg) == 0) new_size=old_size;
    else {
      new_size=FD_FIXLISP(size_arg);
      if (new_size<n_keys)
	new_size=fd_select_table_size(n_keys);}
  else if (FD_FLONUMP(size_arg)) {
    double multiplier=fd_lisp2float(size_arg);
    new_size=fd_select_table_size((int)ceil(old_size*multiplier));}
  if (new_size < 0) 
    if (n_keys == 0) new_size=old_size;
    else if (n_keys < old_size/10) new_size=old_size;
    else new_size=fd_select_table_size(n_keys*3);
  else if (new_size == 0) /* Means keep the old size */
    new_size=old_size;
  else if (new_size < n_keys)
    new_size=fd_select_table_size(n_keys*4);
  else {} /* Use the specified size */  

  /* Copy the values in blocks */
  {
    int writehashv=((newhashv<0) ? (hashv) : (newhashv));
    int new_slots=
      fd_start_file_index
      (out,new_size,(writehashv),metadata,major_version+1,
       ((make<0) ? (0) : make),((change < 0) ? (0) : (change)));
    int n_vals=0, vals_copied=0, start=0; off_t pos;
    /* Compute the total number of values */
    int i=0; while (i < n_keys) n_vals=n_vals+assocs[i++].n_values;
    fd_notify("Coping %d values over %d keys into %d slots",n_vals,n_keys,new_size);
    /* The pos value is offset by new_slots*4, and we use
       ftell because metadata of arbitrary length may have
       been written by fd_start_file_index(). */
    pos=ftello(out)-new_slots*4; while (start < n_keys) {
      char sbuf[32];
      int finish=start+1, bsize=assocs[start].n_values; off_t npos;
      while ((finish < n_keys) &&
	     (bsize+assocs[finish].n_values < bsize_max)) {
	bsize=bsize+assocs[finish].n_values; finish=finish+1;}
      npos=fd_copy_assoc_values
	(assocs+start,finish-start,in,out,pos,r_off,verbose);
      vals_copied=vals_copied+bsize;
      sprintf(sbuf,"%.2f%%",((vals_copied*100.0)/n_vals));
      fd_notify("%s: %s: copied +%d\t+%d\t+%u keys/values/bytes",
		filebase,sbuf,finish-start,bsize,(unsigned int)(npos-pos));
      fd_notify("%s: %s: total  %d\t%d\t%u keys/values/bytes",
		filebase,sbuf,finish,vals_copied,(unsigned int)npos);
      start=finish; pos=npos;}
    /* Write out the keys */
    fd_write_keys_to_index
      (out,assocs,n_keys,writehashv,new_slots,pos,filebase);}
  fd_notify("New file index has version info %d:%d",
	    major_version+1,ftell(out));
  /* Close the files streams */
  fclose(in); fclose(out);
  if (need_copy) {
    fd_notify(_("Copying output file back onto original"));
    copy_binary_file(tmpbuf,infile);
    remove(tmpbuf);}
  fd_exit(0);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: repack-file-index.c,v $
   Revision 1.36  2007/06/30 16:21:06  haase
   Various 64 bit fixes, together with stuff for repacking indices with less than 10 keys

   Revision 1.35  2007/06/30 15:25:53  haase
   Made new_size be larger

   Revision 1.34  2006/07/08 23:24:33  haase
   Verbosity controls for index maintenance functions

   Revision 1.33  2006/06/27 13:11:39  haase
   Fixed bug introduced in adding the new experimental hash function

   Revision 1.32  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.31  2004/10/18 23:04:24  haase
   Made repacking remove its temp files

   Revision 1.30  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.29  2004/09/08 18:36:38  haase
   Fix some declaration bugs which led to weird printf behavior.

   Revision 1.28  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.27  2004/07/20 05:08:23  haase
   Fixed trace problem with repack-file-index based on not casting off_t back to unsigned int

   Revision 1.26  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.25  2004/07/16 14:09:26  haase
   more off_t fixes

   Revision 1.24  2004/07/05 21:37:08  haase
   Fixed repack-file-index bug which didn't write out correct file index type information when upgrading

   Revision 1.23  2004/06/18 20:00:31  haase
   Made repack-file-index default to non-verbosity

   Revision 1.22  2004/05/15 21:34:54  haase
   Fix args to repack-file-index

   Revision 1.21  2004/05/03 20:31:26  haase
   Fix usage message

   Revision 1.20  2004/04/27 17:36:28  haase
   Added verbosity control to file index repacking

   Revision 1.19  2004/04/04 17:03:40  haase
   Use fd_cmdline for repack-file-index

   Revision 1.18  2004/03/30 19:16:27  haase
   Fixes to schema implementation

   Revision 1.17  2004/03/22 14:38:38  haase
   Fixed repack-file-index to handle mult indices

   Revision 1.16  2004/03/12 21:18:50  haase
   Extend new indices to indextools etc.

   Revision 1.15  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.14  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.13.2.2  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.13.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.13  2002/06/03 22:14:18  haase
   Shortened lines of progress reports

   Revision 1.12  2002/06/03 21:51:21  haase
   Progress reports now provide more context

   Revision 1.11  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.10  2002/04/12 15:42:32  haase
   Repacking a linked file now copies into the target rather than overwriting the link

   Revision 1.9  2002/04/11 19:42:13  haase
   Fixed bug where repack-file-index copied its results back into argv[0] --- typically 'repack-file-index' --- rather than the input file

   Revision 1.8  2002/04/10 03:02:10  haase
   Added version information to file pools and indices

   Revision 1.7  2002/04/03 01:33:09  haase
   Moved indextools out of FD_SOURCE core

   Revision 1.6  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
