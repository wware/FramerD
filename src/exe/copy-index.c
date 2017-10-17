#include <framerd/indextools.h>
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 1023
#endif

static void copy_binary_file(char *from,char *to)
{
  FILE *in=fd_fopen_locked(from,"rb",0), *out=fd_fopen_locked(to,"wb",1);
  int bufsize=65536; char *buf=fd_xmalloc(bufsize);
  int ret_value=0;
  if (errno) {perror("Start of binary copy"); FD_CLEAR_ERR();}
  if (in == NULL) 
    fd_raise_detailed_exception(fd_FileOpenFailed,from);
  else if (out == NULL) 
    fd_raise_detailed_exception(fd_FileOpenWFailed,to);
  else while ((ret_value=fread(buf,sizeof(char),bufsize,in)) ||
	      (!(feof(in)))) {
    fwrite(buf,sizeof(char),ret_value,out);}
  fclose(out); fclose(in); fd_xfree(buf);
}

void copy_bunch(fd_index in,fd_index out,fd_lisp bunch,int min,int max)
{
  FD_DO_CHOICES(bkey,bunch) {
    fd_lisp v=fd_index_get(in,bkey,FD_VOID);
    if (FD_VOIDP(v)) {}
    else if ((max > 0) && (FD_CHOICE_SIZE(v) > max)) {}
    else if (FD_CHOICE_SIZE(v) < min) {}
    else fd_index_add(out,bkey,v);
    fd_decref(v);}
  END_FD_DO_CHOICES;
}

int main(int argc,char *argv[])
{
  fd_index in, out;
  fd_lisp keys, bunch=(FD_EMPTY_CHOICE);
  int i=1, chunk_size=10000, min_size=1, max_size=-1;
  fd_initialize_framerd();
  fd_set_notify_handler(NULL);
  if (argc < 3) {
    printf("Usage: copy-index <from> <to> [chunksize] [min] [max]\n");
    exit(1);}
  in=fd_open_index(argv[1]); out=fd_open_index(argv[2]);
  if (in->type == file_index) fd_cache_file_index((fd_file_index)in);
  if (out->type == file_index) fd_cache_file_index((fd_file_index)out);
  if (argc > 3) chunk_size=strtol(argv[3],NULL,10);
  if (argc > 4) min_size=strtol(argv[4],NULL,10);
  if (argc > 5) max_size=strtol(argv[5],NULL,10);
  keys=fd_index_keys(in); bunch=fd_init_choice(chunk_size+16);
  fd_fprintf(stdout,"[%t Copying %d keys in bunches of %d]",
	     FD_CHOICE_SIZE(keys),chunk_size);
  {
    FD_DO_CHOICES(key,keys) {
      if ((i%chunk_size) == 0) {
	char buf[128];
	FD_ADD_TO_CHOICE(bunch,key); i++;
	fd_index_prefetch(in,bunch);
	copy_bunch(in,out,bunch,min_size,max_size);
	fd_swap_out_index(in);
	fd_commit_index(out);
	sprintf(buf,"Copied %d/%d=%2f%% of keys",
		i,FD_CHOICE_SIZE(keys),
		(100.0*i)/(1.0*FD_CHOICE_SIZE(keys)));
	fd_fprintf(stdout,"[%t %s]",buf);
	fd_decref(bunch);
	bunch=fd_init_choice(chunk_size+16);}
      else {
	FD_ADD_TO_CHOICE(bunch,key); i++;}}
    END_FD_DO_CHOICES;
    fd_index_prefetch(in,bunch);
    copy_bunch(in,out,bunch,min_size,max_size);
    fd_commit_index(out);}
  fd_exit(0);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: copy-index.c,v $
   Revision 1.8  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.7  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.6  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/01/26 20:40:27  haase
   Misc. fixes

   Revision 1.5  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.4  2002/04/03 02:09:55  haase
   Fixed copy-index to deal with un FD_SOURCEd indextools

   Revision 1.3  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
