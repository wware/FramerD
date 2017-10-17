/* C Mode */

/* envfns.c
   Implements functions for different sorts of environment access from
     DType/FramerD applications.
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

static char vcid[] = "$Id: envfns.c,v 1.31 2005/01/14 16:48:48 haase Exp $";

/** Lots of OS specific stuff **/
/** Initial declarations **/
/** Getting time information **/
/** Timestamp Functions **/
/** Generating the session ID **/
/** WIN32 Registry access **/
/** Environment access **/
/** Threadsafe Random numbers **/
/** User information functions **/
/** Exit functions **/
/** Startup and Exit heralds **/
/** Signal handling **/
/** Init function **/

#include <signal.h>
#include <math.h>
#include <limits.h>

#include "dtypes.h"
#include "buildstamp.h"

#ifndef PATH_MAX
#define PATH_MAX 1023
#endif

#if WIN32
#include <direct.h>
#include <winsock.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#else
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
static char *get_login()
{
  uid_t me=getuid();
  struct passwd *entry=getpwuid(me);
  if (entry) return entry->pw_name;
  else return getenv("USER");
}
#elif (WIN32)
/* There should be a way to get a real value here... */
static char userid[256]; static long userid_size=256;
static char *get_login() {
  userid_size=256;
  if (GetUserName(userid,&userid_size)) return userid;
  else return getenv("USER");}
#else
#define USING_PWD 0
static char *get_login()
{
  return getenv("USER");
}
#endif

#if ((HAVE_GRP_H) && (HAVE_SYS_TYPES_H) && (HAVE_UNISTD_H))
#define USING_GRP 1
#include <grp.h>
#else
#define USING_GRP 0
#endif

#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

/** Globals **/

static lisp plus_symbol;

fd_exception
  fd_GETCWDfailed=_("Couldn't get working directory"),
  fd_NonStringEnvVar=_("Can't coerce environment variable to string"),
  fd_GetEnvFailed=_("enhanced GETENV failed"),
  fd_UnknownCmdLineArg=_("unknown command-line argument"),
  fd_SignalException=_("Signal caught");
  

/** Generating the session ID **/

#define MAX_HOSTNAME 256

static char session_id[256], *session_mnemonic=NULL, *build_date=__DATE__;
static int session_id_initialized=0;

DTYPES_EXPORT
/* fd_session_id
    Arguments: none
    Returns: a string
The session ID identifies the current FramerD session with user, host,
process, time, and other information.
*/
char *fd_session_id()
{
  if (session_id_initialized) return session_id;
  else {
    char hostname[MAX_HOSTNAME+1], *user;
    time_t now=time(NULL); char isotime[32];  
    struct tm *tptr=localtime(&now); 
    strftime(isotime,32,"%Y-%m-%dT%H:%M:%S",tptr);
    hostname[0]='\0'; gethostname(hostname,MAX_HOSTNAME);
    if (hostname[0] == '\0') strcpy(hostname,"nohost");
    user=get_login(); CLEAR_ERR(); /* In case getlogin set it */
    if (user == NULL) user=getenv("USER");
    if (user == NULL) user="kilroy";
    sprintf(session_id,"%s/U:%s@%s/P:%ld:%ld/V:%d.%d.%d-%d-%s/T:%s",
	    ((session_mnemonic == NULL) ? ("framerd") : (session_mnemonic)),
	    user,hostname,(long)getpid(),(long)getppid(),
	    FD_MAJOR_VERSION,FD_MINOR_VERSION,FD_RELEASE_VERSION,FD_BUILDSTAMP,FD_OSID,
	    isotime);
    session_id_initialized=1;
    fd_notify(_("Session id=%s"),session_id);
    return session_id;}
}

DTYPES_EXPORT
/* fd_set_session_mnemonic
     Arguments: a string
     Returns: nothing

Sets the string used to identify this kind of session (e.g. 'fdscript'
or more usefully, 'mailreader') */
void fd_set_session_mnemonic(char *mnemonic)
{
  if (session_id_initialized)
    fd_raise_exception(_("Session ID already set"));
  else session_mnemonic=fd_strdup(mnemonic);
}


DTYPES_EXPORT
/* fd_get_session_mnemonic
     Arguments: none
     Returns: a string

Returns the string used to identify this kind of session (e.g. 'fdscript'
or more usefully, 'mailreader') */
char *fd_get_session_mnemonic()
{
  if (session_mnemonic) return session_mnemonic;
  else return "framerd";
}

DTYPES_EXPORT
/* fd_get_build_date:
     Arguments: none
     Returns: a string
  Returns the build date set by fd_set_build_date */
char *fd_get_build_date()
{ return build_date;}

DTYPES_EXPORT
/* fd_get_build_date:
     Arguments: a string
     Returns: nothing
  Sets the build date to be returned by fd_get_build_date */
void fd_set_build_date(char *date) {build_date=fd_strdup(date);}

/** WIN32 Registry access **/

#ifdef WIN32
#define DONT_CREATE_KEY 0
#define CREATE_KEY_IF_NEEDED 1
DTYPES_EXPORT
lisp fd_registry_query(HANDLE root,char *path,char *name)
{
  HKEY key; DWORD code;
  if ((code=RegOpenKeyEx(root,path,0,KEY_READ,&key)) == ERROR_SUCCESS) {
    char *data=fd_xmalloc(1024); DWORD size=1024, entry_type;
	int code=RegQueryValueEx(key,name,NULL,&entry_type,data,&size);
    if (code == ERROR_MORE_DATA) {
      fd_xfree(data); data=fd_xmalloc(size); 
      code=RegQueryValueEx(key,name,NULL,&entry_type,data,&size);}
    RegCloseKey(key);
    if (code == ERROR_SUCCESS) 
      switch (entry_type) {
      case REG_SZ: {lisp v=fd_make_string(data); fd_xfree(data); return v;}
      case REG_DWORD:
	{lisp v=LISPFIX(*((int *) data)); fd_xfree(data); return v;}
      case REG_DWORD_BIG_ENDIAN: {
	int i=*((int *) data); lisp v=LISPFIX(flip_word(i));
	free(data); return v;}
      case REG_MULTI_SZ: {
	lisp head=FD_MAKE_LIST1(fd_make_string(data)), tail=head;
	char *scan=data+strlen(data)+1;
	while (*scan) {
	  lisp new=FD_MAKE_LIST1(fd_make_string(scan));
	  RPLACD(tail,new); tail=new; scan=scan+strlen(scan)+1;}
	free(data);
	return head;}
      case REG_BINARY:
	return fd_make_packet(size,fd_mallocize(data,size));
      default:
	fd_raise_exception(_("Can't handle WIN32 registry value"));
      }
    else return (FD_VOID);}
  else return (FD_VOID);
}
DTYPES_EXPORT
int
fd_registry_modify(HANDLE root,char *path,char *name,lisp value,int create)
{
  HKEY key; DWORD code;
  if ((code=RegOpenKeyEx(root,path,0,KEY_SET_VALUE,&key)) == ERROR_SUCCESS) {
    char *data; DWORD data_size, entry_type; int intval;
    if (FIXNUMP(value)) {
      intval=FIXLISP(value); data=(char *)&intval;
      data_size=4; entry_type=REG_DWORD;}
    else if (STRINGP(value)) {
      data=STRING_DATA(value); data_size=STRING_LENGTH(value);
      entry_type=REG_SZ;}
    else if (PAIRP(value)) {
      int size=0;
      DOLIST(x,value)
	if (STRINGP(x)) {size=size+STRING_LENGTH(x)+1;}
	else {size=-1; break;}
      if (size > 0) {
	char *scan=data=fd_xmalloc(size+1);
	DOLIST(x,value)
	  {strcpy(scan,STRING_DATA(x)); scan=scan+STRING_LENGTH(x)+1;}
	*scan='\0'; data_size=size; entry_type=REG_MULTI_SZ;}
      else {RegCloseKey(key);
      fd_raise_exception(_("Can't store value in WIN32 registry"));}}
    else if (PRIM_TYPEP(value,packet_type)) {
      fd_lisp_string  s=(fd_lisp_string)CPTR_DATA(value);
      data_size=s->length; data=s->data; entry_type=REG_BINARY;}
    else {
      RegCloseKey(key);
      fd_raise_exception(_("Can't write WIN32 registry value"));}
    RegSetValueEx(key,name,0,entry_type,data,data_size);
    RegFlushKey(key); RegCloseKey(key);
    return 1;}
  else if (create)
    fd_raise_exception(_("WIN32 Key creation not implemented"));
  else return -1;
}
DTYPES_EXPORT int fd_registry_create(HANDLE h,char *path)
{
  HKEY entry; DWORD disp;
  DWORD code=RegCreateKeyEx
    (h,path,0,"framerd",
     REG_OPTION_NON_VOLATILE,KEY_ALL_ACCESS,
     NULL,&entry,&disp);
  if (code == ERROR_SUCCESS) {
    RegCloseKey(entry); return 1;}
  else return 0;
}
DTYPES_EXPORT
lisp fd_registry_get(char *path,char *name)
{
  lisp v=fd_registry_query(HKEY_CURRENT_USER,path,name);
  if (!(FD_VOIDP(v))) return v;
  else return fd_registry_query(HKEY_LOCAL_MACHINE,path,name);
}
DTYPES_EXPORT
lisp fd_registry_set(char *path,char *name,lisp value)
{
  if (fd_registry_modify(HKEY_CURRENT_USER,path,name,value,DONT_CREATE_KEY)
      > 0)
    return (FD_TRUE);
  else if (fd_registry_modify
	   (HKEY_LOCAL_MACHINE,path,name,value,DONT_CREATE_KEY) > 0)
    return (FD_TRUE);
  else return (FD_FALSE);
}
#endif

/** Environment access **/

DTYPES_EXPORT
/* fd_getenv:
     Arguments: a string naming a variable
     Returns: a lisp object

 Gets a variable from the environment.  It first tries the
 top level LISP environment, then the Windows registry (under WIN32),
 and finally the "POSIX" environment used by getenv. When the value comes
 from anywhere besides the lisp environment, it is parsed as a LISP argument,
 so that numbers turn into numbers.
*/
lisp fd_getenv(char *var)
{
  lisp sym=fd_probe_symbol(var), val;
  if (SYMBOLP(sym)) {
    val=fd_symbol_value(sym);
    if (!(FD_VOIDP(val))) return fd_incref(val);}
#ifdef WIN32
  val=fd_registry_get("Software\\FramerD\\environment",var);
  if (!(FD_VOIDP(val))) return val;
#endif
  {
    char *simple=getenv(var);
    if (simple)
      return fd_parse_arg(simple);
    else return (FD_EMPTY_CHOICE);}
}

DTYPES_EXPORT
/* fd_string_getenv:
     Arguments: a string naming a variable
     Returns: a lisp string

 Gets a variable from the environment.  It first tries the
 top level LISP environment, then the Windows registry (under WIN32),
 and finally the "POSIX" environment.

  Unlike fd_getenv, this doesn't parse the string.
 */
fd_u8char *fd_string_getenv(char *var)
{
  lisp sym=fd_probe_symbol(var), val=FD_VOID;
  if (SYMBOLP(sym)) val=SYMBOL_VALUE(sym);
#ifdef WIN32
  if (FD_VOIDP(val))
    val=fd_registry_get("Software\\FramerD\\environment",var);
#endif
  if (FD_VOIDP(val)) {
    char *simple=getenv(var);
    if (simple) return fd_convert_os_string(simple);
    else return NULL;}
  else if (FD_EMPTY_LISTP(val)) return NULL;
  else if (FD_EMPTYP(val)) return NULL;
  else if (STRINGP(val)) {
    fd_u8char *s=fd_strdup(STRING_DATA(val));
    return s;}
  else fd_raise_exception(fd_NonStringEnvVar);
  return 0; /* Never reached */
}

DTYPES_EXPORT
/* fd_int_getenv:
     Arguments: a string naming a variable and an int default
     Returns: an int

 Gets a variable from the environment.  It first tries the
 top level LISP environment, then the Windows registry (under WIN32),
 and finally the "POSIX" environment.

  Unlike fd_getenv, this doesn't parse the string.
 */
int fd_int_getenv(char *var,int dflt)
{
  lisp sym=fd_probe_symbol(var), val=FD_VOID;
  if (SYMBOLP(sym)) val=SYMBOL_VALUE(sym);
#ifdef WIN32
  if (FD_VOIDP(val))
    val=fd_registry_get("Software\\FramerD\\environment",var);
#endif
  if (FD_VOIDP(val)) {
    char *simple=getenv(var); int val;
    if (simple)
      val=strtol(simple,NULL,10);
    else return dflt;
    if (errno) {CLEAR_ERR(); return dflt;}
    else return val;}
  else if (FD_EMPTY_LISTP(val)) return dflt;
  else if (FD_EMPTYP(val)) return dflt;
  else if (STRINGP(val)) {
    int ival = strtol(STRING_DATA(val),NULL,10);
    fd_decref(val);
    return ival;}
  else if (FD_FIXNUMP(val)) return FIXLISP(val);
  fd_decref(val);
  return dflt;
}

DTYPES_EXPORT
/* fd_bool_getenv:
     Arguments: a string naming a variable
     Returns: 1 or 0 depending on the interpreted truth value of the variable binding

 Gets a variable from the environment.  It first tries the
 top level LISP environment, then the Windows registry (under WIN32),
 and finally the "POSIX" environment.

*/
int fd_bool_getenv(char *var)
{
  lisp val=fd_getenv(var);
  if ((FD_EMPTYP(val)) || (FD_FALSEP(val)) || (FD_EMPTY_LISTP(val)) ||
      ((FD_STRINGP(val)) && (fd_strlen(val) == 0))) {
    fd_decref(val); return 0;}
  else {fd_decref(val); return 1;}
}

/** Threadsafe Random numbers **/

/* Some environments are random number challenged */
static unsigned int bigrand()
{
  if (RAND_MAX < 65536) {
    int r1=rand(), r2=rand();
    return ((r1<<16)+r2);}
  else return rand();
}

/* fd_random
    Arguments: none
    Returns: an unsigned int between 0 and 2^32

 This is a generic procedure which is both threadsafe and
will return a bigger range of random numbers on platforms where
RAND_MAX is pitiful. */
#if FD_THREADS_ENABLED
static fd_mutex random_mutex;
DTYPES_EXPORT
/* fd_random
    Arguments: none
    Returns: an unsigned int between 0 and 2^32

 This is a generic procedure which is both threadsafe and
will return a bigger range of random numbers on platforms where
RAND_MAX is pitiful. */
unsigned int fd_random()
{
  unsigned int answer;
  lock_mutex(&random_mutex);
  answer=bigrand();
  unlock_mutex(&random_mutex);
  return answer;
}
#else
DTYPES_EXPORT
unsigned int fd_random()
{
  return bigrand();
}
#endif

#if FD_THREADS_ENABLED
DTYPES_EXPORT
/* fd_set_random
    Arguments: an unsigned int
    Returns: void

 This sets the random seed. */
void fd_set_random(unsigned int seed)
{
  lock_mutex(&random_mutex);
  srand(seed);
  unlock_mutex(&random_mutex);
}
#else
DTYPES_EXPORT
void fd_set_random(unsigned int seed)
{
  srand(seed);
}
#endif

/** Group information functions **/

DTYPES_EXPORT
/* fd_get_gid
    Arguments: a string
    Returns: gets the group ID for a named group
*/
gid_t fd_get_gid(char *uname)
#if USING_GRP
{
  if (uname) {
    struct group *entry=getgrnam(uname);
    if (errno) {CLEAR_ERR();}
    if (entry) return entry->gr_gid;
    else return -1;}
  else return getgid();
}
#else
{
  fd_warn(_("Can't get gid, try another OS"));
  return -1;
}
#endif

DTYPES_EXPORT
/* fd_get_gname
    Arguments: a gid
    Returns: a string
 Gets the group name for a particular gid    
*/
char *fd_get_gname(gid_t id)
#if USING_GRP
{
  struct group *entry=getgrgid(id);
  if (errno) {CLEAR_ERR();}
  if (entry) return fd_strdup(entry->gr_name);
  else return NULL;
}
#else
{
  fd_warn(_("Can't get uid, try another OS"));
  return NULL;
}
#endif

/** User information functions **/

DTYPES_EXPORT
/* fd_get_uid
    Arguments: a string
    Returns: gets the user ID for a named user
*/
uid_t fd_get_uid(char *uname)
#if USING_PWD
{
  if (uname) {
    struct passwd *entry=getpwnam(uname);
    if (errno) {CLEAR_ERR();}
    if (entry) return entry->pw_uid;
    else return -1;}
  else return getuid();
}
#else
{
  fd_warn(_("Can't get uid, try another OS"));
  return -1;
}
#endif

DTYPES_EXPORT
/* fd_get_uname
    Arguments: a uid
    Returns: a string
 Gets the user name for a particular id    
*/
char *fd_get_uname(uid_t id)
#if USING_PWD
{
  struct passwd *entry=getpwuid(id);
  if (errno) {CLEAR_ERR();}
  if (entry) return fd_strdup(entry->pw_name);
  else return NULL;
}
#else
{
  fd_warn(_("Can't get uid, try another OS"));
  return NULL;
}
#endif

#if USING_PWD
static lisp convert_user_data(struct passwd *entry)
{
  lisp slotmap=fd_make_slotmap(8);
  fd_slotmap sm=SLOTMAP_PTR(slotmap);
  fd_slotmap_add(sm,fd_make_symbol("UID"),LISPFIX(entry->pw_uid));
  fd_slotmap_add(sm,fd_make_symbol("GID"),LISPFIX(entry->pw_gid));
  fd_slotmap_add(sm,fd_make_symbol("UNAME"),
		 fd_make_string(entry->pw_name));
  fd_slotmap_add(sm,fd_make_symbol("TEXT-DATA"),
		 fd_make_string(entry->pw_gecos));    
  fd_slotmap_add(sm,fd_make_symbol("HOMEDIR"),
		 fd_make_string(entry->pw_dir));    
  fd_slotmap_add(sm,fd_make_symbol("SHELL"),
		 fd_make_string(entry->pw_shell));
  return slotmap;
}
#endif

DTYPES_EXPORT
/* fd_get_user_data
    Arguments: a uid
    Returns: a lisp structure
 Gets the user password data for a particular id    
*/
lisp fd_get_user_data(uid_t id)
#if USING_PWD
{
  struct passwd *entry=getpwuid(id);
  if (errno) {CLEAR_ERR();}
  if (entry) return convert_user_data(entry);
  else return FD_EMPTY_CHOICE;
}
#else
{
  fd_warn(_("Can't get user data, try another OS"));
  return FD_EMPTY_CHOICE;
}
#endif

DTYPES_EXPORT
/* fd_set_uid
    Arguments: a uid
    Returns: changes the current user to be name, return 1 if successful
*/
int fd_set_uid(char *uname)
#if USING_PWD
{
  struct passwd *entry=getpwnam(uname);
  if (errno) {CLEAR_ERR();}
  if (entry)
    if (setuid(entry->pw_uid))
      fd_raise_detailed_exception(_("Can't set uid"),strerror(errno));
    else return 0;
  else fd_raise_detailed_exception(_("Can't interpret uid"),strerror(errno));
}
#else
{
  fd_warn(_("Can't set uid, try another OS"));
  return -1;
}
#endif

DTYPES_EXPORT
/* fd_set_gid
    Arguments: a gid
    Returns: changes the current group to be name, return 1 if successful
*/
int fd_set_gid(char *gname)
#if USING_GRP
{
  struct group *entry=getgrnam(gname);
  if (errno) {CLEAR_ERR();}
  if (entry)
    if (setgid(entry->gr_gid))
      fd_raise_detailed_exception(_("Can't set gid"),strerror(errno));
    else return 0;
  else fd_raise_detailed_exception(_("Can't interpret gid"),strerror(errno));
}
#else
{
  fd_warn(_("Can't set uid, try another OS"));
  return -1;
}
#endif

DTYPES_EXPORT
/* fd_get_homedir
    Arguments: none
    Returns: a string naming a directory

    Returns the home directory for the current user. */
char *fd_get_homedir()
{
#if USING_PWD
  uid_t id=getuid();
  struct passwd *entry=getpwuid(id);
  if (errno) {CLEAR_ERR();}
  if ((entry) && (entry->pw_dir))
    return fd_strdup(entry->pw_dir);
#endif
  if (getenv("HOME") == NULL) return NULL;
  else return fd_strdup(getenv("HOME"));
}

DTYPES_EXPORT
/* fd_get_current_uid
    Arguments: none
    Returns: a uid 

  Returns the id of the current user. */
uid_t fd_get_current_uid()
{
#if USING_PWD
  return getuid();
#else
  fd_warn(_("Can't get current UID, try another OS"));
  return -1;
#endif
  
}

/** Exit functions **/

int fd_normal_exit=0;

DTYPES_EXPORT
/* fd_exit
     Arguments: a status code (an int)
     Returns: no.

 This sets the variable fd_normal_exit to 1 if the status
 code is zero, which can be read by atexit handlers. */
void fd_exit(int status)
{
  if (status == 0) fd_normal_exit=1;
  exit(status);
}

/** Startup and Exit heralds **/

static int inhibit_herald=0, inhibit_anti_warranty=0;
static char *herald=NULL;
static char *anti_warranty
  =_("FramerD comes with absolutely NO WARRANTY, see http://framerd.org/LICENSE for details");

DTYPES_EXPORT
/* fd_inhibit_herald
     Arguments: none
     Returns: void

     Keeps the startup and shutdown heralds from being shown */
void fd_inhibit_herald(int inhibit)
{
  inhibit_herald=inhibit;
}

DTYPES_EXPORT
/* fd_inhibit_anti_warranty
     Arguments: none
     Returns: void

     Keeps the startup and shutdown heralds from being shown */
void fd_inhibit_anti_warranty(int inhibit)
{
  inhibit_anti_warranty=inhibit;
}

DTYPES_EXPORT
/* fd_show_startup_herald:
     Arguments: none
     Returns: nothing
  Outputs a notification describing the library version */
void fd_show_startup_herald()
{
  lisp given=fd_getenv("HERALD");
  if (FD_VOIDP(given)) {}
  else if (FD_EMPTYP(given)) {}
  else if (STRINGP(given))
    herald=fd_strdup(STRING_DATA(given));
  else inhibit_herald=1;
  if (inhibit_herald) {}
  else {
    fd_notify(_("FramerD %d.%d.%d (B%d) for %s built %s"),
	      FD_MAJOR_VERSION,FD_MINOR_VERSION,FD_RELEASE_VERSION,
	      FD_BUILDSTAMP,FD_OSID,__DATE__);
    fd_notify(_("Copyright (C) MIT 1994-2001, Copyright (C) beingmeta 2001-2005"));
    if (inhibit_anti_warranty == 0) fd_notify(anti_warranty);
    if (herald) fd_notify(herald);
    if (herald) fd_xfree(herald);}
}

/** Signal handling **/

/* This code turns signals into exceptions.  This may be a bad idea
    sometimes, but it turns out to be convenient for certain other
    kinds of errors.  Setting the environment variable
    FD_DIE_CONVENTIONALLY will have FramerD decline to handle
    signals which would normally terminate the program.  */

fd_exception fd_BrokenPipe=_("Broken pipe"),
             fd_KeyboardInterrupt=_("KeyboardInterrupt");

static void abort_handler(int signal)
{
  fprintf(stderr,_("Signal %d\n"),signal);
  sigsetmask(0);
  if (errno) {
    char err_buf[256], *err_string;
    err_string=strerror(errno); strcpy(err_buf,err_string);
    if (errno) perror("Aaargh! ");
    fd_raise_detailed_exception("Abort",err_buf);}
#ifdef SIGABRT
  else if (signal == SIGABRT)
    fd_raise_detailed_exception(fd_SignalException,"SIGABRT");
#endif
#ifdef SIGPIPE
  else if (signal == SIGPIPE)
    fd_raise_detailed_exception(fd_SignalException,"SIGPIPE");
#endif
#ifdef SIGILL
  else if (signal == SIGILL)
    fd_raise_detailed_exception(fd_SignalException,"SIGILL");
#endif
#ifdef SIGQUIT
  else if (signal == SIGQUIT)
    fd_raise_detailed_exception(fd_SignalException,"SIGQUIT");
#endif
#ifdef SIGSEGV
  else if (signal == SIGSEGV)
    fd_raise_detailed_exception(fd_SignalException,"SIGSEGV");
#endif
#ifdef SIGFPE
  else if (signal == SIGFPE)
    fd_raise_detailed_exception(fd_SignalException,"SIGFPE");
#endif
#ifdef SIGBUS
  else if (signal == SIGBUS)
    fd_raise_detailed_exception(fd_SignalException,"SIGBUS");
#endif
#ifdef SIGHUP
else if (signal == SIGHUP)
  fd_raise_detailed_exception(fd_SignalException,"SIGHUP");
#endif
  else fd_raise_exception("Abort");
  fd_close_all_connections();
  exit(1);
}

static void term_handler(int signal)
{
  fd_close_all_connections();
  if (errno) exit(1); else exit(0);
}

static void interrupt_handler(int signal)
{
  sigsetmask(0);
  fd_raise_exception(_("Keyboard Interrupt"));
}

static void pipe_handler(int signal)
{
  sigsetmask(0);
  fd_raise_exception(fd_BrokenPipe);
}

/* fd_setup_signal_handlers
    Arguments: none
    Returns: void

  Sets up signal handlers which turn signals into exceptions.
  If the environment FD_DIE_CONVENTIONALLY is defined, normally
  terminating errors do not get turned into exceptions. */
void fd_setup_signal_handlers()
{
  lisp dc=fd_getenv("FD_DIE_CONVENTIONALLY");
  if ((FD_FALSEP(dc)) || (FD_EMPTYP(dc))) {
#ifdef SIGABRT
    signal(SIGABRT,abort_handler);
#endif
#ifdef SIGPIPE
    signal(SIGPIPE,pipe_handler);
#endif
#ifdef SIGQUIT
    signal(SIGQUIT,abort_handler);
#endif
#ifdef SIGSEGV
    signal(SIGSEGV,abort_handler);
#endif
#ifdef SIGFPE
    signal(SIGFPE,abort_handler);
#endif
#ifdef SIGBUS
    signal(SIGBUS,abort_handler);
#endif
#ifdef SIGILL
    signal(SIGILL,abort_handler);
#endif
  }
#ifdef SIGHUP
  signal(SIGHUP,abort_handler);
#endif
#ifdef SIGINT
  signal(SIGINT,interrupt_handler);
#endif
#ifdef SIGTERM
  signal(SIGTERM,term_handler);
#endif
}

/** Clearing errno **/

DTYPES_EXPORT
int _fd_clear_errno()
{
#if defined(WIN32)
  SetLastError(0); errno=0;
#else
  errno=0;
#endif
  return 0;
}

/** Loading configuration files **/

DTYPES_EXPORT
/* fd_load_config:
     Arguments: a filename (a string)
     Returns: nothing

  Loads the variable bindings defined in the specified
  configuration file, setting the corresponding symbol values */
int fd_load_config(char *config_file)
{
  FILE *f=fd_fopen(config_file,"r");
  if (f) while (1) {
    lisp expr=fd_parse_lisp_from_stream(f);
    if (FD_EOF_OBJECTP(expr)) break;
    else if ((PAIRP(expr)) && (SYMBOLP(CAR(expr))) && (PAIRP(CDR(expr)))) {
      if (PAIRP(CDR(CDR(expr)))) {
	lisp sym=CAR(expr), val=CAR(CDR(expr)), op=CAR(CDR(CDR(expr)));
	if (LISP_EQ(op,plus_symbol)) {
	  lisp current_value=fd_symbol_value(sym);
	  if (FD_VOIDP(current_value)) current_value=FD_EMPTY_CHOICE;
	  ADD_TO_CHOICE(current_value,incref(val));
	  fd_set_symbol_value(sym,current_value);
	  fd_decref(current_value);}
	else {
	  fd_raise_lisp_exception("Bad config file entry",config_file,expr);}}
      else {
	lisp sym=CAR(expr), val=CAR(CDR(expr));
	fd_set_symbol_value(sym,val);}
      decref(expr);}
    else {
      fd_raise_lisp_exception(fd_ConfigSyntaxError,config_file,expr);}}
  else {CLEAR_ERR();}
  if (f) fd_fclose(f);
  return 0;
}

int *fd_argcp=NULL;
char ***fd_argvp=NULL;

DTYPES_EXPORT
void fd_cmd_args(int *argcp,char ***argvp)
{
  fd_argcp=argcp; fd_argvp=argvp;
}

static void process_config_assignment(char *start)
{
  fd_lisp symbol, value; int add=0;
  char *equals=strchr(start,'=');
  if (equals == NULL) return;
  symbol=fd_intern(start,equals-start);
  if (equals[1]=='+') {add=1; equals++;}
  if ((equals[1]==' ') || (equals[1]=='\0')) value=FD_VOID;
  else value=fd_parse_arg(equals+1);
  if (add) {
    fd_lisp val=fd_symbol_value(symbol);
    if (FD_VOIDP(val))
      fd_set_symbol_value(symbol,value);
    else {
      FD_ADD_TO_CHOICE(val,fd_incref(value));
      fd_set_symbol_value(symbol,val);
      fd_decref(val);}}
  else fd_set_symbol_value(symbol,value);      
  fd_decref(value);
}

DTYPES_EXPORT
/* fd_cmdline:
    Arguments: a pointer to an int, and argv and argc values
    Returns: a vector of string arguments
 Stores the size of the vector in the first argument.  This
proceeds down the given arguments (according to the argv and argc values)
and processes all configuration directives (e.g. FOO=3), putting the
other results in the returned vector of parameters. */
fd_u8char **fd_cmdline(int *param_count,char **argv,int argc)
{
  fd_u8char **params, **tmp; int n_params=0, i=1;
  fd_initialize_data();
  tmp=fd_malloc(sizeof(fd_u8char *)*argc);
  i=1; while (i < argc)
    if ((strncasecmp(argv[i],"config=",7)) == 0) {
      fd_load_config(argv[i]+7); i++;}
    else if (argv[i][0] == '-') {
      /* Pass through old style option args */
      tmp[n_params++]=fd_convert_os_string(argv[i]); i++;}
    else if (strchr(argv[i],'=')) {
      process_config_assignment(argv[i]); i++;}
    else {tmp[n_params++]=fd_convert_os_string(argv[i]); i++;}
  if (n_params == 0) {*param_count=0; return NULL;}
  else {
    params=fd_malloc(sizeof(fd_u8char *)*n_params);
    i=0; while (i < n_params) {params[i]=tmp[i]; i++;}
    fd_free(tmp,sizeof(fd_u8char *)*argc);
    *param_count=n_params;
    return params;}
}

#if HAVE_SYS_RESOURCE_H
DTYPES_EXPORT
/* fd_getrusage:
     Arguments: a pointer to an rusage structure
     Returns: void
 Gets rusage information, covering for holes in various
 implementations (currently just Linux) */
void fd_getrusage(struct rusage *r)
{
  getrusage(RUSAGE_SELF,r);
#if FD_PROC_RUSAGE_PATCH
  {
    FILE *f=fopen("/proc/self/statm","r");
    long total, res, shared, textsize, data, library, dirty_size;
    fscanf(f,"%ld %ld %ld %ld %ld %ld %ld",
	   &total,&res,&shared,
	   &textsize,&library,&data,&dirty_size);
    r->ru_idrss=total; /* ??? r->ru_isrss=data;  */
    r->ru_maxrss=res; /* Not entirely correct... */
    r->ru_ixrss=shared+library;
    fclose(f);
  }
#endif
}
#endif

/** Init function **/

void fd_initialize_envfns_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&random_mutex);
#endif

  plus_symbol=fd_make_symbol("+");

  fd_register_source_file("envfns",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: envfns.c,v $
   Revision 1.31  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.30  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.29  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.28  2004/04/04 17:38:03  haase
   Made fdscript use fd_cmdline

   Revision 1.27  2004/03/30 21:47:03  haase
   Added fd_cmdline to unify command line processing

   Revision 1.26  2003/12/21 14:12:40  haase
   Fixes to configuration and makefile

   Revision 1.25  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.24  2003/11/03 00:16:49  haase
   Fixed bug in signalling config file errors

   Revision 1.23  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.22  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.21.2.6  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.21.2.5  2003/08/02 13:56:53  haase
   Added bool and int getenvs

   Revision 1.21.2.4  2003/02/09 23:10:01  haase
   Fix module loading to do a search again

   Revision 1.21.2.3  2003/01/26 20:45:58  haase
   Fix GC errors

   Revision 1.21.2.2  2002/09/26 02:07:17  haase
   Various fixes (Thanks, Ralph)

   Revision 1.21.2.1  2002/08/09 16:59:29  haase
   Make abort copy the result of strerror

   Revision 1.21  2002/07/26 17:21:39  haase
   Added three part version to heralds

   Revision 1.20  2002/06/09 18:02:48  haase
   Fixes for WIN32

   Revision 1.19  2002/05/30 12:11:38  haase
   Fixed leak in config file loading

   Revision 1.18  2002/05/26 22:06:11  haase
   Made session-id use getuid() rather than getlogin()

   Revision 1.17  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.16  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.15  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.14  2002/04/26 03:55:10  haase
   Caught up with some new structs in registry functions

   Revision 1.13  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
