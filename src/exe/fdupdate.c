#include <framerd/dtypes.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <utime.h>

static char usage[]=
  "fdupdate <dest> <source> [source]..]\nfdupdate --overwrite <dest> <source> [source]...\n";

static void install_file(char *from,char *to,int update)
{
  FILE *in=fd_fopen_locked(from,"r+b",0), *out;
  int  bufsize=65536*4, ret_value=0, bytes=0, new_file=0, through_link=0;
  char *buf=fd_xmalloc(bufsize), *realname=NULL;
  struct stat from_info, to_info;
  if (fd_symbolic_linkp(to)) {
    char *realsrc=fd_get_real_pathname(from);
    realname=fd_get_real_pathname(to);
    if (strcmp(realsrc,realname) == 0) {
      fd_warn(_("Not copying since %s is already linked to %s"),to,from);
      fd_xfree(realsrc); fd_xfree(realname);
      return;}
    through_link=1;}
  else realname=fd_strdup(to);
  stat(from,&from_info); 
  new_file=(fd_file_existsp(realname) == 0);
  if ((update) && (new_file==0)) {
    stat(realname,&to_info);
    if (to_info.st_mtime >= from_info.st_mtime) {
      fd_warn(_("Not overwriting newer '%s'"),realname);
      return;}}
  if (through_link)
    fd_warn(_("Copying %s to %s (through link %s)"),from,realname,to);
  else fd_warn(_("Copying %s to %s"),from,realname);
  out=fopen(realname,"wb");
  if (errno) {perror("Start of binary copy"); FD_CLEAR_ERR();}
  if (in == NULL) 
    fd_raise_detailed_exception(fd_FileOpenFailed,from);
  else if (out == NULL) 
    fd_raise_detailed_exception(fd_FileOpenWFailed,to);
  else while ((ret_value=fread(buf,sizeof(char),bufsize,in)) ||
	      (!(feof(in)))) {
    bytes=bytes+ret_value; fwrite(buf,sizeof(char),ret_value,out);}
  fclose(out); fclose(in); fd_xfree(buf);
  if (new_file) {
    char *gname, *uname; gid_t gid; uid_t uid;
    gname=getenv("FRAMERD_GROUP"); if (gname == NULL) gname="framerd";
    gid=fd_get_gid(gname);
    uname=getenv("FRAMERD_USER"); if (uname == NULL) uname="framerd";
    uid=fd_get_uid(uname);
    if (gid < 0) gid=from_info.st_gid;
    chown(realname,uid,gid);}

  {struct utimbuf wbuf;
   wbuf.actime=from_info.st_atime; wbuf.modtime=from_info.st_mtime;
   if (utime(realname,&wbuf) < 0) perror("utime");}
  fd_xfree(realname);
}

static int ends_in_slashp(char *string)
{
  int len=strlen(string);
  if (len == 0) return 0;
  else if (string[len-1] == '/') return 1;
  else return 0;
}

int main(int argc,char *argv[])
{
  int update=1; char *target; int source_start;
  if (argc < 2) {fputs(usage,stderr); exit(1);}
  if (argv[1][0] == '-') {
    if (strcmp(argv[1],"--overwrite") == 0) update=0;
    target=argv[2]; source_start=3;}
  else {
    target=argv[1]; source_start=2;}
  fd_initialize_dtypes();
  if (argc <= source_start) {fputs(usage,stderr); exit(1);}
  else if ((argc-source_start == 1) && (!(fd_directoryp(target))))
    install_file(argv[source_start],target,update);
  else {
    int need_slashp=(ends_in_slashp(target) == 0), dirlen=strlen(target);
    int i=source_start; while (i < argc) {
      char *base=fd_basename(argv[i],1);
      char *dest_path=fd_xmalloc(strlen(base)+dirlen+2);
      strcpy(dest_path,target);
      if (need_slashp) strcat(dest_path,"/");
      strcat(dest_path,base);
      install_file(argv[i],dest_path,update);
      fd_xfree(dest_path); i++;}}
  return 0;
}
