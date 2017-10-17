/* C Mode */

/* filefns.c
   Implements utility functions for dealing with the file system.
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

static char vcid[] = "$Id: filefns.c,v 1.20 2005/01/14 16:48:49 haase Exp $";

/** Manipulating filenames **/
/** OS generic file opening **/
/** Flushing input buffers **/
/** Checking file status information **/
/** Finding files along a search path **/

/** Lots of OS specific stuff **/

#include "dtypes.h"

#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 1023
#endif

#if WIN32
#include <direct.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#if ((HAVE_PWD_H) && (HAVE_SYS_TYPES_H) && (HAVE_UNISTD_H))
#define USING_PWD 1
#include <pwd.h>
#else
#define USING_PWD 0
#endif

#if WIN32
#include <io.h>
#include <sys/locking.h>
static int lock_fd(int fd,int for_write)
{
#if 0
  return _locking(fd,_LK_LOCK,0);
#endif
  return 1;
}
static int unlock_fd(int fd)
{
#if 0
  return _locking(fd,_LK_UNLCK,0);
#endif
  return 1;
}
#elif FD_WITH_FILE_LOCKING
static int lock_fd(int fd,int for_write)
{
  struct flock lock_data;
  int retval;
  lock_data.l_whence=0; lock_data.l_start=0; lock_data.l_len=0;
  lock_data.l_type=((for_write) ? (F_WRLCK) : (F_RDLCK));
  lock_data.l_pid=getpid();
  retval=fcntl(fd,F_SETLK,&lock_data);
  if (retval == 0) {CLEAR_ERR();}
  return retval;
}
static int unlock_fd(int fd)
{
  struct flock lock_data;
  int retval;
  lock_data.l_whence=0; lock_data.l_start=0; lock_data.l_len=0;
  lock_data.l_type=F_UNLCK; lock_data.l_pid=getpid();
  retval=fcntl(fd,F_SETLK,&lock_data);
  CLEAR_ERR();
  return retval;
}
#else
static int lock_fd(int fd,int for_write)
{
  return 1;
}
static int unlock_fd(int fd)
{
  return 1;
}
#endif

/** Exceptions **/

fd_exception
  fd_UnknownUser=_("Unknown user name"),
  fd_FileWriteFailed=_("Can't write to file"),
  fd_FileOpenFailed=_("Can't open file for reading"),
  fd_FileOpenWFailed=_("Can't open file for writing"),
  fd_FileLockFailed=_("Can't lock file"),
  fd_CantFindFile=_("Can't find file in search path");

/** Utilities **/

#if USING_PWD
char *get_homedir(char *uname)
{
  struct passwd *entry=getpwnam(uname);
  if (errno) {CLEAR_ERR();}
  if (entry) return fd_strdup(entry->pw_dir);
  else return NULL;
}
#else
char *get_homedir(char *uname)
{
  return NULL;
}
#endif

/** Manipulating filenames **/

char *expand_tilde(fd_u8char *filename)
{
  char *expansion, *pathname, *homedir;
  if (filename[1] == '/') {
    pathname=fd_make_os_string(filename+1);
    homedir=fd_get_homedir();}
  else {
    char uname[65], *dir;
    fd_u8char *slash=strchr(filename,'/');
    if (slash-filename > 64)
      fd_raise_detailed_exception(fd_UnknownUser,filename);
    strncpy(uname,filename+1,(slash-(filename+1)));
    uname[(slash-(filename+1))]=NUL;
    pathname=fd_make_os_string(slash);
    homedir=get_homedir(uname);}
  if (homedir == NULL) homedir=fd_strdup("./");
  expansion=fd_xmalloc(strlen(pathname)+strlen(homedir)+1);
  strcpy(expansion,homedir); strcat(expansion,pathname);
  fd_xfree(pathname); fd_xfree(homedir);
  return expansion;
}

static char *expand_logical_host(fd_u8char *filename,fd_u8char *colon)
{
  char buf[32], *dirname;
  strncpy(buf,filename,colon-filename);
  strcpy(buf+(colon-filename),"_DIR");
  dirname=fd_string_getenv(buf);
  if (dirname) {
    fd_u8char *translation;
    char *full=fd_xmalloc(strlen(dirname)+1+strlen(colon+1)+1);
    strcpy(full,dirname); strcat(full,"/"); strcat(full,colon+1);
    translation=fd_make_os_string(full);
    fd_xfree(full); fd_xfree(dirname);
    return translation;}
  else return fd_make_os_string(filename);
}

static char *make_filename(char *dir,char *name)
{
  char *result=fd_xmalloc(strlen(dir)+strlen(name)+2);
  char *last=dir+(strlen(dir)-1);
  if ((*last == '/') || (*last == '\\'))
    sprintf(result,"%s%s",dir,name);
#if WIN32
  else sprintf(result,"%s\\%s",dir,name);
#else
  else sprintf(result,"%s/%s",dir,name);
#endif
  return result;
}

static char *interpret_relative_pathname(char *buf,char *filename)
{
  char *dirname=fd_dirname(filename), dbuf[PATH_MAX], *fullname;
  if (*dirname == '\0')
    dirname=fd_strdup(getcwd(dbuf,PATH_MAX));
  while ((strncmp(buf,"../",3) == 0) || (strncmp(buf,"./",2) == 0)) 
    if (strncmp(buf,"../",3) == 0) {
      char *scan=dirname+strlen(dirname)-1;
      while ((scan > dirname) && ((*scan != '/') || (*scan != '\\')))
	scan--;
      buf=buf+3; *scan=NUL;}
    else if (strncmp(buf,"./",2) == 0)  buf=buf+2;
    else break;
  fullname=make_filename(dirname,buf);
  fd_xfree(dirname);
  return fullname;
}

/*  fd_filename:
      Arguments: a utf8 string naming a file, possibly relative
      Returns: a string
 Generates a localized string which interprets the tilde as homedir and
 replaces ambiguous separator characters / or \ with the OS appropriate
 character.
 Caller should free the return value with free().
*/
char *fd_filename(fd_u8char *filename)
{
  char *translation; fd_u8char *colon;
  if (*filename == '~') translation=expand_tilde(filename);
  else if (((colon=(strchr(filename,':'))) != NULL) &&
	   ((colon-filename) < 16))
    translation=expand_logical_host(filename,colon);
  else translation=fd_make_os_string(filename);
#ifdef WIN32
  {
   char *scan=translation; while (*scan)
     if (*scan == '/') *scan++='\\'; else scan++;}
#endif
  return translation;
}

#ifdef WIN32
#define DIRSEP "\\"
#else
#define DIRSEP "/"
#endif

/*  fd_dirname:
      Arguments: a string naming a file, possibly relative
      Returns: a string containing the directory portion of the filename
*/
char *fd_dirname(char *filename)
{
  char *copy=fd_strdup(filename), *scan=copy+strlen(filename)-1;
  while (scan > copy)
    if ((*scan == '/') || (*scan == '\\')) {
      *(scan+1)='\0'; return copy;}
    else scan--;
  fd_xfree(copy); return fd_strdup("");
}

DTYPES_EXPORT
/*  fd_basename:
      Arguments: a string naming a file, possibly relative and an int flag
      Returns: a string containing just the last part of the filename,
excluding the suffix if with_suffix=0
*/
char *fd_basename(char *string,int with_suffix)
{
  char *last_slash=strrchr(string,'/');
  char *start=((last_slash) ? (last_slash+1) : string);
  char *copy=fd_strdup(start);
  if (with_suffix) return copy;
  else {
    char *last_dot=strrchr(copy,'.');
    if (last_dot) *last_dot=NUL;
    return copy;}
}

DTYPES_EXPORT
/* fd_make_absolute_pathname:
     Arguments: a filename and a directory name
     Returns: a new string which contains an absolute pathname
*/
char *fd_make_absolute_pathname(fd_u8char *filename,char *wrt)
{
  if ((*filename == '\\') || (*filename == '/') || (*filename == '~') ||
      (isalpha(*filename) && (filename[1] == ':')))
    return fd_filename(filename);
  else {
    fd_u8char *full_version=interpret_relative_pathname(filename,wrt);
    char *normalized=fd_filename(full_version);
    fd_xfree(full_version);
    return normalized;}
}

DTYPES_EXPORT
/* fd_absolute_pathname:
     Arguments: a filename
     Returns: a new string which contains an absolute pathname
*/
char *fd_absolute_pathname(fd_u8char *filename)
{
  if ((*filename == '\\') || (*filename == '/') || (*filename == '~') ||
      (isalpha(*filename) && (filename[1] == ':')))
    return fd_filename(filename);
  else {
    char *cwd, buf[PATH_MAX], *abs, *rel; int wd_len;
    cwd=getcwd(buf,PATH_MAX); 
    if (!(cwd)) fd_raise_exception(fd_GETCWDfailed);
    else {
      strcat(cwd,DIRSEP);
      return fd_make_absolute_pathname(filename,cwd);}}
}

DTYPES_EXPORT
/* fd_get_real_pathname:
     Arguments: a utf-8 filename string
     Returns: a localized string identifying the real path 
     of the designated file in the file system. */
#if HAVE_REALPATH
char *fd_get_real_pathname(fd_u8char *name)
{
  char *path=fd_filename(name);
  char *rpath=fd_xmalloc(PATH_MAX);
  if (realpath(path,rpath)) {
    fd_xfree(path); return rpath;}
  else {
    char *errstring=strerror(errno); CLEAR_ERR();
    fd_warn("REALPATH(%s) failed: %s",name,errstring);
    fd_xfree(path); fd_xfree(rpath);
    return NULL;}
}
#else
char *fd_get_real_pathname(fd_u8char *name)
{
  return fd_absolute_pathname(name);
}
#endif

/** OS generic file opening **/

DTYPES_EXPORT
/* fd_fopen
     Arguments: a filename and a mode specifier
     Returns: a FILE * pointer
  Normalizes the filename for the OS 
*/
FILE *fd_fopen(fd_u8char *filename,char *mode)
{
  char *copy=fd_filename(filename);
  FILE *f=fopen(copy,mode);
  fd_xfree(copy);
  return f;
}

DTYPES_EXPORT
/* fd_fopen_locked
     Arguments: a filename, a mode specifier, and a flag (1 or 0)
     Returns: a FILE * pointer
  Normalizes the filename for the OS and locks the returned stream
  (using fcntl).  If the integer flag is 1, the lock is only a write
  lock.  If it is 0, neither reads nor writes are permitted.
*/
FILE *fd_fopen_locked(fd_u8char *filename,char *mode,int allow_readers)
{
  char *copy=fd_filename(filename); FILE *f;
  int for_write=0;
  if ((strchr(mode,'w')) || (strchr(mode,'a')) || (strchr(mode,'+')))
    for_write=1;
  f=fopen(copy,mode); fd_xfree(copy);
  if (f == NULL) {TIDY_ERRNO("open locked"); return NULL;}
  else if (lock_fd(fileno(f),for_write)<0) {
    TIDY_ERRNO("file lock"); fclose(f); TIDY_ERRNO("file lock");
    return NULL;}
  else return f;
}

DTYPES_EXPORT
/* fd_fclose
     Arguments: a filename
     Returns: a FILE * pointer
  Closes the stream and cleans up XFILEs associated with it
*/
void fd_fclose(FILE *stream)
{
  unlock_fd(fileno(stream));
  fclose(stream); fd_free_xfile(stream);
  TIDY_ERRNO("fd_fclose");
}

/** Flushing input buffers **/

DTYPES_EXPORT
/* fd_flush_input_buffer
     Arguments: a FILE * pointer
     Returns: an integer
 Discards any pending input and returns the number of bytes discarded.
 Not currently implemented for WIN32 */
#if WIN32 /* Is there a way to do it here? */
int fd_flush_input_buffer(FILE *f)
{
  return 0;
}
#else
int fd_flush_input_buffer(FILE *f)
{
  int c, flags, fn=fileno(f), count=0; 
  flags=fcntl(fn,F_GETFL,NULL);
  fcntl(fn,F_SETFL,(O_NONBLOCK|flags));
  while ((c=getc(f)) >= 0) count++;
  fcntl(fn,F_SETFL,(flags)); CLEAR_ERR();
  TIDY_ERRNO("fd_flush_input_buffer");
  return count;
}
#endif

/** Checking file status information **/

DTYPES_EXPORT
/* fd_file_existsp
    Arguments: a string
    Returns: 1 or 0
    Returns 1 if the file exists, 0 otherwise. Uses stat.  */
int fd_file_existsp(fd_u8char *fname)
{
  struct stat status;
  char *filename=fd_filename(fname);
  if (stat(filename,&status) < 0) {
    CLEAR_ERR(); fd_xfree(filename); return 0;}
  else {fd_xfree(filename); return 1;}
}

DTYPES_EXPORT
/* fd_file_writablep
    Arguments: a string
    Returns: 1 or 0
    Returns 1 if the file can be written (whether it exists or not),
      0 otherwise. Actually opens it to try, rather than doing something
       clever with stat.  */
int fd_file_writablep(fd_u8char *filename)
{
  FILE *f;
  if (fd_file_existsp(filename)) f=fd_fopen(filename,"r+b");
  else f=fd_fopen(filename,"wb");
  if (f) {fclose(f); return 1;} else {CLEAR_ERR(); return 0;}
}

DTYPES_EXPORT
/* fd_directoryp
    Arguments: a string
    Returns: 1 or 0
    Returns 1 if the file is actually a directory. */
int fd_directoryp(fd_u8char *path)
{
  char *filename=fd_filename(path);
  struct stat status; int stat_result=stat(filename,&status);
  fd_xfree(filename);
  if (stat_result < 0) {CLEAR_ERR(); return 0;}
  else if (S_ISDIR(status.st_mode)) return 1;
  else return 0;
}

DTYPES_EXPORT
/* fd_regular_filep
    Arguments: a string
    Returns: 1 or 0
    Returns 1 if the file is a regular file. */
int fd_regular_filep(fd_u8char *path)
{
  char *filename=fd_filename(path);
  struct stat status; int stat_result=stat(filename,&status);
  fd_xfree(filename); 
  if (stat_result < 0) {CLEAR_ERR(); return 0;}
  else if (S_ISREG(status.st_mode)) return 1;
  else return 0;
}

DTYPES_EXPORT
/* fd_file_size:
    Arguments: a string
    Returns: a long
    Returns the size of a file */
off_t fd_file_size(fd_u8char *path)
{
  char *filename=fd_filename(path);
  struct stat status; int stat_result=stat(filename,&status);
  fd_xfree(filename); 
  if (stat_result < 0) {CLEAR_ERR(); return -1;}
  else return status.st_size;
}

DTYPES_EXPORT
/* fd_symbolic_linkp
    Arguments: a string
    Returns: 1 or 0
    Returns 1 if the file is a symbolic link to another file
     (currently always zero under WIN32). */
int fd_symbolic_linkp(fd_u8char *path)
{
#if WIN32
  return 0;
#else
  char *filename=fd_filename(path);
  struct stat status; int stat_result=lstat(filename,&status);
  fd_xfree(filename);
  if (stat_result < 0) {CLEAR_ERR(); return 0;}
  else if (S_ISLNK(status.st_mode)) return 1;
  else return 0;
#endif
}

DTYPES_EXPORT
/* fd_readlink
    Arguments: a string
    Returns: another string
    Returns the target of a symbolic link or the path itself
     of any other string (currently the identity under WIN32). */
#if (!(WIN32))
char *fd_readlink(fd_u8char *path)
{
  char *filename=fd_filename(path);
  char *buf=fd_xmalloc(128); 
  int size=128, code;
  if (fd_symbolic_linkp(path) == 0) {
    fd_xfree(buf); return filename;}
  code=readlink(filename,buf,size);
  /* Keep calling readlink and growing buf until it's big enough */
  while (code > size) {
    size=size+128; buf=fd_xrealloc(buf,size);
    code=readlink(filename,buf,size);}
  if (code < 0) {fd_xfree(filename); fd_xfree(buf); return NULL;}
  else buf[code]=0;
  if (buf[0] == '/') {
    buf[code]='\0'; fd_xfree(filename); return buf;}
  else {
    char *fullpath=interpret_relative_pathname(buf,filename);
    fd_xfree(buf); fd_xfree(filename);
    return fullpath;}
}
#else
char *fd_readlink(fd_u8char *path)
{
  return fd_filename(path);
}
#endif

/** Merging relative pathnames **/



/** Finding files along a search path **/

DTYPES_EXPORT
/* fd_getpath:
    Arguments: a string naming an extended environment variable
    Returns: a lisp pointer to a list of directories
  Interprets a path variable */
lisp fd_getpath(fd_u8char *name)
{
  lisp v=fd_getenv(name);
  if (FD_PAIRP(v)) return v;
  else if (FD_STRINGP(v)) {
    fd_u8char *data=FD_STRING_DATA(v);
    if (strchr(data,':')) {
      lisp head=FD_EMPTY_LIST, *tail=&head;
      fd_u8char *start=data, *end=strchr(data,':');
      while (end)
	if (start == end) {
	  start++; end=strchr(start,':');}
	else {
	  lisp new=FD_MAKE_PAIR(fd_make_substring(start,end),*tail);
	  *tail=new; tail=&(FD_CDR(new));
	  start=end+1; end=strchr(start,':');}
      if (*start) {
	lisp new=FD_MAKE_PAIR(fd_copy_string(start),*tail);
	*tail=new;}	
      return head;}
    else return FD_MAKE_LIST1(v);}
  else {fd_decref(v); return FD_EMPTY_LIST;}
}

DTYPES_EXPORT
/* fd_find_file
    Arguments: a string and a lisp pointer
    Returns: another string
  The lisp pointer is used as a search path to look for files.
  It first checks to see if the string exists as a file as given.
  It then searches along the directories in the search path.
  The search path can be a string (taken as a directory name)
   or a list of strings take as directory names. 
  It allocates a string for a result (but doesn't count it as fd_mallocd).
*/
fd_u8char *fd_find_file(fd_u8char *filename,lisp search_path)
{
  if (fd_file_existsp(filename)) return fd_strdup(filename);
  else if (STRINGP(search_path)) {
    char full_path[PATH_MAX];
    char *tail=STRING_DATA(search_path)+STRING_LENGTH(search_path)-1;
    strcpy(full_path,STRING_DATA(search_path));
    if (strcmp(tail,DIRSEP) != 0) strcat(full_path,DIRSEP);
    strcat(full_path,filename);
    if (fd_file_existsp(full_path)) return fd_strdup(full_path);
    else return NULL;}
  else if (PAIRP(search_path)) {
    DOLIST(elt,search_path) {
      char *answer=fd_find_file(filename,elt);
      if (answer) return answer;}
    return NULL;}
  else if ((FD_EMPTY_LISTP(search_path)) || (FD_EMPTYP(search_path)) ||
	   (FD_VOIDP(search_path)))
    return NULL;
  else fd_raise_detailed_exception
    ("Invalid search path",fd_object_to_string(search_path));
}

/** Finding the exec filename */

#if (!(HAVE_REALPATH))
static char *get_realpath(char *string,char *buf)
{
  if ((strncmp(string,"./",2) == 0) ||
      (strncmp(string,".\\",2) == 0)) {
    getcwd(buf,PATH_MAX);
    strcat(buf,DIRSEP);
    strcat(buf,string+2);}
  else if ((strncmp(string,"../",3) == 0) ||
	   (strncmp(string,"..\\",3) == 0)) {
    char *scan;
    getcwd(buf,PATH_MAX); scan=buf+strlen(buf)-1;
    while ((scan > buf) && ((*scan != '/') || (*scan != '\\')))
      scan--;
    if (scan > buf) *scan=0;
    strcat(buf,DIRSEP);
    strcat(buf,string+3);}
  else strcpy(buf,string);
  return buf;
}
#else
#define get_realpath realpath
#endif

static char *try_prefix(char *start,char *end,char *file)
{
  struct stat tmp;
  char buf[PATH_MAX], real_buf[PATH_MAX];
  if (end) {
    strncpy(buf,start,end-start); buf[end-start]=0;}
  else strcpy(buf,start);
  strcat(buf,"/"); strcat(buf,file);
  if (get_realpath(buf,real_buf)) {
    if (stat(real_buf,&tmp)>=0)
      return fd_strdup(real_buf);
    else return NULL;}
  else return NULL;
}

DTYPES_EXPORT
/* fd_get_exec_filename:
    Arguments: a string, typicallying argv[0]
    Returns: a malloc'd absolute filename or NULL
 Attempts to figure out the absolute pathname of an
 executable from the argv[0] parameter.  Used to find
 the FramerD configuration file. */
char *fd_get_exec_filename(char *argv0)
{
  char *binfile=NULL, *path, buf[PATH_MAX];
  if ((*argv0 == '/') || (*argv0 == '.')) {
    if (get_realpath(argv0,buf)) binfile=buf;
    else binfile=NULL;}
  else if ((path=getenv("PATH")) != NULL) {
    char *start=path, *end;
    while ((end=strchr(start,':')) != NULL) {
      char *file=try_prefix(start,end,argv0);
      if (errno) errno=0;
      if (file) {binfile=file; break;}
      start=end+1;}
    if (binfile == NULL)
      binfile=try_prefix(start,end,argv0);
    if (errno) errno=0;}     
  else binfile=NULL;
  if (binfile) return fd_strdup(binfile);
  else return NULL;
}

/** fd_open_tmpfile implementation **/

DTYPES_EXPORT
#if HAVE_MKSTEMP
FILE *fd_fopen_tmpfile(char *namebuf,char *mode)
{
  int fd=mkstemp(namebuf);
  return fopen(namebuf,mode);
}
#else /* This is safer if it is an option */
FILE *fd_fopen_tmpfile(char *buf,char *mode)
{
  tmpnam(buf);
  return fopen(buf,mode);
}
#endif

/** Initialization procedure **/

void fd_initialize_filefns_c()
{
  fd_register_source_file("filefns",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: filefns.c,v $
   Revision 1.20  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.19  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.18  2004/07/16 14:34:25  haase
   Added configuration option for largefiles

   Revision 1.17  2004/03/10 16:25:25  haase
   Added fd_make_absolute_pathname which takes a relative argument

   Revision 1.16  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.15  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.14.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.14.2.2  2003/01/26 20:47:27  haase
   Fix GC errors

   Revision 1.14.2.1  2002/08/09 19:07:31  haase
   Added FD_ to WITH_FILE_LOCKING

   Revision 1.14  2002/04/10 19:00:31  haase
   Defined fd_get_real_pathname

   Revision 1.13  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
