/* C Mode */

/* osprims.c
   I/O primitives for FDScript
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

static char vcid[] = "$Id: osprims.c,v 1.56 2005/01/14 16:48:46 haase Exp $";

/** Lots of OS specific stuff **/
/** File locking and unlocking **/
/** FramerD environment stuff **/
/** Internationalization procedures **/
/** Current Directory **/
/** Accessing environment/registry **/
/** Tracking System Resources **/
/** System commands and URL access **/
/** Finding files in FDPATH **/
/** File system access **/
/** Walking the file system **/
/** File operations **/
/** Time functions **/
/** Miscellaneous: errno, sleep */
/** Disabling notifications **/
/** Managing configuration files **/

/** Lots of OS specific stuff **/

#include "fdscript.h"

#include <locale.h>
#include <sys/stat.h>
#include <limits.h>
#include <fcntl.h>
#include <signal.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
# else
# include <time.h>
# endif
#endif

#if (HAVE_FTIME && (!(HAVE_GETTIMEOFDAY)))
#include <sys/timeb.h>
#endif

#ifdef WIN32
#include <sys/stat.h>
#include <io.h>
#include <direct.h>
#include <winuser.h>
#include <shellapi.h>
#else
#if defined(HAVE_SYS_STAT_H)
# include <sys/stat.h>
#endif
#if defined(HAVE_DIRENT_H)
# include <dirent.h>
#else
# define dirent direct
# if defined(HAVE_SYS_NDIR_H)
#  include <sys/ndir.h>
# endif
# if defined(HAVE_SYS_DIR_H)
#  include <sys/dir.h>
# endif
#endif
#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif
#endif

#if HAVE_NETDB_H
# include <netdb.h>
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif

#include <framerd/fdscript.h>

#ifndef PATH_MAX
#define PATH_MAX 512
#endif

/** Initializing **/

static lisp timestamp_symbol;
static lisp year_symbol, month_symbol, day_symbol, hour_symbol, minute_symbol;
static lisp second_symbol, millisecond_symbol, microsecond_symbol, nanosecond_symbol;
fd_lispenv fd_osprims_env;

fd_exception
  fd_Invalid_IS08601=_("Invalid ISO8601 time specifier");

/** FramerD environment stuff **/

static lisp lisp_get_session_id_cproc()
{
  return fd_copy_string(fd_session_id());
}
static lisp lisp_id_session_cproc(lisp string) {
  if (STRINGP(string))
    fd_set_session_mnemonic(STRING_DATA(string));
  else if (SYMBOLP(string))
    fd_set_session_mnemonic(SYMBOL_NAME(string));
  else fd_type_error(_("session mnemonic is not a string or symbol"),string);
  return FD_TRUE;
}

static lisp lisp_report_framerd_stats_cproc()
{
  fd_report_framerd_stats(stderr);
  return FD_TRUE;
}

static lisp lisp_report_malloc_stats_cproc()
{
  fd_describe_mallocation(stderr);
  return FD_TRUE;
}

static lisp lisp_report_index_stats_cproc()
{
  fd_index_report_stats(stderr);
  return FD_TRUE;
}

static lisp lisp_never_save_cproc()
{
  fd_never_save();
  return FD_TRUE;
}

static lisp lisp_count_loaded_oids_cproc()
{
  return LISPFIX(fd_loaded_oids);
}

static lisp lisp_count_oids_loaded_cproc()
{
  return LISPFIX(fd_oids_loaded);
}

#if (!(FD_LIGHTWEIGHT_OIDS))
static lisp lisp_count_oids_referenced_cproc()
{
  return LISPFIX(fd_oid_table()->n_keys);
}
#endif

static lisp lisp_get_portno_cproc(lisp string)
{
  if (STRINGP(string))
    return LISPFIX(fd_get_portno(STRING_DATA(string)));
  else fd_type_error(_("not a string"),string);
}

/** Internationalization procedures **/

static lisp lisp_set_default_encoding_cproc(lisp encoding_name)
{
  if ((STRINGP(encoding_name)) || (SYMBOLP(encoding_name))) {
    char *name; 
    if (STRINGP(encoding_name)) name=STRING_DATA(encoding_name);
    else name=SYMBOL_NAME(encoding_name);
    fd_set_default_encoding(name);
    return FD_VOID;}
  else fd_type_error(_("not an encoding (string or symbol)"),
		     encoding_name);
}

static lisp lisp_set_console_encoding_cproc(lisp encoding_name)
{
  if ((STRINGP(encoding_name)) || (SYMBOLP(encoding_name))) {
    char *name; 
    if (STRINGP(encoding_name)) name=STRING_DATA(encoding_name);
    else name=SYMBOL_NAME(encoding_name);
    fd_set_file_encoding(stdin,name);
    fd_set_file_encoding(stdout,name);
    return FD_VOID;}
  else fd_type_error(_("not an encoding (string or symbol)"),
		     encoding_name);
}

static lisp lisp_setlocale_cproc(lisp locale)
{
  if (SYMBOLP(locale))
    setlocale(LC_ALL,SYMBOL_NAME(locale));
  else if (STRINGP(locale))
    setlocale(LC_ALL,STRING_DATA(locale));
  else fd_type_error(_("not a locale name (string or symbol)"),locale);
  return FD_VOID;
}

static lisp lisp_load_encoding_cproc(lisp name)
{
  fd_u8char *filename=fd_strdata(name);
  fd_u8char *encoding_name=fd_basename(filename,0);
  fd_load_encoding(encoding_name,filename);
  fd_xfree(encoding_name);
  return FD_TRUE;
}

static lisp lisp_known_encoding_cproc(lisp enc)
{
  fd_u8char *enc_name;
  struct FD_TEXT_ENCODING *e;
  if (FD_SYMBOLP(enc)) enc_name=SYMBOL_NAME(enc);
  else if (FD_STRINGP(enc)) enc_name=STRING_DATA(enc);
  else fd_type_error(_("not an encoding (string or symbol)"),enc);
  e=fd_get_encoding(enc_name);
  if (e) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_packet_to_string_cproc(lisp packet,lisp enc)
{
  uchar *packet_data; fd_u8char *string_data, *enc_name;
  struct FD_TEXT_ENCODING *e;
  if (!(FD_PACKETP(packet)))
    fd_type_error(_("not a packet"),packet);
  if (STRINGP(enc)) enc_name=(STRING_DATA(enc));
  else if (SYMBOLP(enc)) enc_name=(SYMBOL_NAME(enc));
  else fd_type_error(_("not an encoding (string or symbol)"),enc);
  e=fd_get_encoding(enc_name);
  if (e == NULL)
    fd_raise_detailed_exception(fd_UnknownEncoding,enc_name);
  packet_data=PACKET_DATA(packet);
  string_data=fd_make_utf8(packet_data,packet_data+PACKET_LENGTH(packet),e);
  return fd_init_string(string_data,-1);
}

static lisp lisp_string_to_packet_cproc(lisp string,lisp enc)
{
  uchar *enc_name;
  struct FD_TEXT_ENCODING *e;
  if (!(STRINGP(string)))
    fd_type_error(_("not a string"), string);
  else if (STRINGP(enc)) enc_name=(STRING_DATA(enc));
  else if (SYMBOLP(enc)) enc_name=(SYMBOL_NAME(enc));
  else if (FD_FALSEP(enc)) {
    int len=FD_STRING_LENGTH(string);
    uchar *packet_data=fd_malloc(len);
    strncpy(packet_data,FD_STRING_DATA(string),len);
    return fd_make_packet(len,packet_data);}
  else fd_type_error(_("not an encoding (string or symbol)"),enc);
  e=fd_get_encoding(enc_name);
  if (e) {
    int len; uchar *packet_data;
    packet_data=
      fd_convert_utf8(STRING_DATA(string),FD_STRING_LENGTH(string),e,&len);
    return fd_make_packet(len,fd_mallocize(packet_data,len));}
  else fd_raise_detailed_exception(fd_UnknownEncoding,enc_name);
}

/** Current Directory **/

/* Changes the current working directory. */
static lisp lisp_cwd_cproc(lisp arg)
{
  if (FD_STRINGP(arg))
    if (chdir(STRING_DATA(arg)) == 0) return FD_TRUE;
    else return FD_FALSE;
  else return FD_VOID;
}

/* Changes the current working directory. */
static lisp lisp_gwd_cproc()
{
  char wFD_DBUF[PATH_MAX], *wd;
  if ((wd=getcwd(wFD_DBUF,PATH_MAX)) != NULL) return fd_make_string(wd);
  else fd_raise_exception(fd_GETCWDfailed);
}

/** Accessing environment/registry **/

/* Gets an environment variable, returning #f if it isn't defined. */
static lisp lisp_getenv_cproc(lisp x)
{
  char *name;
  if (SYMBOLP(x)) name=SYMBOL_NAME(x);
  else if (STRINGP(x)) name=STRING_DATA(x);
  else fd_type_error(_("parameter is not a string or symbol"),x);
  return fd_getenv(name);
}

#ifdef WIN32
static HANDLE get_registry_root(lisp symbol)
{
  if (SYMBOLP(symbol)) {
    char *name=SYMBOL_NAME(symbol);
    if ((strcmp(name,"MACHINE") == 0)) return HKEY_LOCAL_MACHINE;
    else if ((strcmp(name,"USER") == 0)) return HKEY_CURRENT_USER;
    else if ((strcmp(name,"USERS") == 0)) return HKEY_USERS;
    else if ((strcmp(name,"CLASSES") == 0)) return HKEY_CLASSES_ROOT;
    else if ((strcmp(name,"CONFIG") == 0)) return HKEY_CURRENT_CONFIG;
    else if ((strcmp(name,"DYNAMIC") == 0)) return HKEY_DYN_DATA;
    else fd_raise_detailed_exception("Unknown registry root",name);}
  else fd_type_error(_("WIN32 registry root must be symbol"),symbol);
}

/* Gets an environment variable, returning #f if it isn't defined. */
static lisp lisp_registry_get_lexpr(lisp args)
{
  if (fd_list_length(args) == 3) {
    HANDLE root=get_registry_root(fd_get_arg(args,0,FD_VOID));
    lisp path=fd_get_arg(args,1,FD_VOID);
    lisp name=fd_get_arg(args,2,FD_VOID);
    if ((STRINGP(path)) && (STRINGP(name)))
      return fd_registry_query(root,STRING_DATA(path),STRING_DATA(name));
    else fd_raise_exception("Registry path and name must be string");}
  else {
    lisp path=fd_get_arg(args,0,FD_VOID);
    lisp name=fd_get_arg(args,1,FD_VOID);
    if ((STRINGP(path)) && (STRINGP(name)))
      return fd_registry_get(STRING_DATA(path),STRING_DATA(name));
    else fd_raise_exception("Registry path and name must be string");}
}
static lisp lisp_registry_set_lexpr(lisp args)
{
  if (fd_list_length(args) == 4) {
    HANDLE root=get_registry_root(fd_get_arg(args,0,FD_VOID));
    lisp path=fd_get_arg(args,1,FD_VOID);
    lisp name=fd_get_arg(args,2,FD_VOID);
    lisp value=fd_get_arg(args,3,FD_VOID);
    if ((STRINGP(path)) && (STRINGP(name)))
      if (fd_registry_modify
	  (root,STRING_DATA(path),STRING_DATA(name),value,1))
	return FD_TRUE;
      else return FD_FALSE;
    else fd_raise_exception("Registry path and name must be string");}
  else {
    lisp path=fd_get_arg(args,0,FD_VOID);
    lisp name=fd_get_arg(args,1,FD_VOID);
    lisp value=fd_get_arg(args,2,FD_VOID);
    if ((STRINGP(path)) && (STRINGP(name)))
      return fd_registry_set(STRING_DATA(path),STRING_DATA(name),value);
    else fd_raise_exception("Registry path and name must be string");}
}
static lisp lisp_registry_create_user_cproc(lisp path)
{
  if (fd_registry_create(HKEY_CURRENT_USER,fd_strdata(path)))
    return FD_TRUE;
  else fd_raise_detailed_exception
	 ("Can't create user registry entry",fd_strdata(path));
}
static lisp lisp_registry_create_machine_cproc(lisp path)
{
  if (fd_registry_create(HKEY_LOCAL_MACHINE,fd_strdata(path)))
    return FD_TRUE;
  else fd_raise_detailed_exception
	 ("Can't create machine registry entry",fd_strdata(path));
}
#endif

/* Gets an environment variable, returning #f if it isn't defined. */
static lisp lisp_cgetenv_cproc(lisp x)
{
  char *name, *value;
  if (SYMBOLP(x)) name=SYMBOL_NAME(x);
  else if (STRINGP(x)) name=STRING_DATA(x);
  else fd_type_error(_("parameter name is not a string or symbol"),x);
  value=getenv(name);
  if (value == NULL) return FD_FALSE;
  else return fd_make_string(value);
}

static lisp lisp_get_homedir_cproc()
{
  return fd_make_string(fd_get_homedir());
}

static lisp lisp_get_user_data_lexpr(lisp args)
{
  uid_t id;
  if (FD_EMPTY_LISTP(args)) id=fd_get_current_uid();
  else if (STRINGP(CAR(args))) id=fd_get_uid(STRING_DATA(CAR(args)));
  else if (FIXNUMP(CAR(args))) id=FIXLISP(CAR(args));
  else fd_type_error
	 ("user id is not a string or fixnum",CAR(args));
  if (id < 0) return FD_EMPTY_CHOICE;
  else return fd_get_user_data(id);
}

/** Getting uids, gids, etc. */

static lisp lisp_get_uid_lexpr(lisp args)
{
  fd_lisp arg=fd_get_arg(args,0,FD_FALSE);
  if (FD_FALSEP(arg)) {
    int id=fd_get_uid(NULL);
    if (id < 0) return FD_FALSE;
    else return FD_LISPFIX(id);}
  else if (FD_STRINGP(arg)) {
    char *uname=fd_make_os_string(FD_STRING_DATA(arg));
    int id=fd_get_uid(uname);
    fd_xfree(uname);
    if (id < 0) return FD_FALSE;
    else return FD_LISPFIX(id);}
  else fd_type_error(_("not a string"),arg);
}

static lisp lisp_get_gid_lexpr(lisp args)
{
  fd_lisp arg; fd_get_args("GET-GID",args,&arg,FD_FALSE,NULL);
  if (FD_FALSEP(arg)) {
    int id=fd_get_gid(NULL);
    if (id < 0) return FD_FALSE;
    else return FD_LISPFIX(id);}
  else if (FD_STRINGP(arg)) {
    char *uname=fd_make_os_string(FD_STRING_DATA(arg));
    int id=fd_get_gid(uname);
    fd_xfree(uname);
    if (id < 0) return FD_FALSE;
    else return FD_LISPFIX(id);}
  else fd_type_error(_("not a string"),arg);
}

static lisp lisp_set_group_cproc(lisp arg)
{
  if (FD_STRINGP(arg)) {
    char *gname=fd_make_os_string(FD_STRING_DATA(arg));
    int id=fd_get_gid(gname); 
    if (id < 0) {
      fd_xfree(gname); return FD_FALSE;}
    else if (fd_set_gid(gname)) {
      fd_xfree(gname); return FD_FALSE;}
    else {
      fd_xfree(gname); return FD_TRUE;}}
  else fd_type_error(_("not a string"),arg);
}

/** Tracking System Resources **/

static lisp lisp_footprint_size_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_maxrss > INT_MAX) return FD_FALSE;
  else return LISPFIX(((int)data.ru_maxrss));
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_data_size_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_idrss > INT_MAX) return FD_FALSE;
  else return LISPFIX(((int)data.ru_idrss));
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_stack_size_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_isrss > INT_MAX) return FD_FALSE;
  else return LISPFIX(((int)data.ru_isrss));
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_page_faults_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_majflt > INT_MAX) return FD_FALSE;
  else return LISPFIX(((int)data.ru_majflt));
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_nswaps_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_nswap > INT_MAX) return FD_FALSE;
  else return LISPFIX(((int)data.ru_nswap));
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_runtime_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  struct rusage data; fd_getrusage(&data);
  if (data.ru_stime.tv_sec+data.ru_utime.tv_sec > (INT_MAX/1000000)) {
    float num=((float)data.ru_stime.tv_sec)*1000000.0+
      ((float)data.ru_utime.tv_sec)*1000000.0+
      (float)data.ru_stime.tv_usec+
      (float)data.ru_utime.tv_usec;
    return LISPFLOAT(num);}
  else return LISPFIX(data.ru_stime.tv_sec*1000000+data.ru_stime.tv_usec+
		      data.ru_utime.tv_sec*1000000+data.ru_utime.tv_usec);
#else 
  return FD_FALSE;
#endif
}

#if defined(HAVE_SYS_RESOURCE_H)
static double get_systime(struct rusage *data)
{
  return (((double)(data->ru_stime.tv_sec))*1000000.0+
	  (double)(data->ru_stime.tv_usec))/1000000.0;
}

static double get_usertime(struct rusage *data)
{
  return (((double)(data->ru_utime.tv_sec))*1000000.0+
	  (double)(data->ru_utime.tv_usec))/1000000.0;
}
#endif

static lisp lisp_system_time_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  double num; struct rusage data; fd_getrusage(&data);
  num=get_systime(&data);
  return LISPFLOAT(num);
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_user_time_cproc()
{
#if defined(HAVE_SYS_RESOURCE_H)
  double num; struct rusage data; fd_getrusage(&data);
  num=get_usertime(&data);
  return LISPFLOAT(num);
#else 
  return FD_FALSE;
#endif
}

static lisp lisp_memusage_cproc()
{
  return LISPFIX(fd_malloc_usage()+fd_cons_usage());
}

static lisp lisp_consusage_cproc()
{
  return LISPFIX(fd_cons_usage());
}

static lisp prim_set(lisp obj,lisp slot,lisp value)
{
  fd_prim_set(obj,slot,value); decref(value);
  return FD_TRUE;
}

static void prim_set_int(lisp sm,char *slotname,long val)
{
  if (val > INT_MAX)
    prim_set(sm,fd_make_symbol(slotname),fd_make_symbol("HUGE"));
  else prim_set(sm,fd_make_symbol(slotname),LISPFIX((int)val));
}

static lisp lisp_resources_cproc()
{
  lisp sm=fd_make_slotmap(14);
  prim_set_int(sm,"CONSED-MEMORY",fd_cons_usage());
  prim_set_int(sm,"MALLOCD-MEMORY",fd_malloc_usage());
  prim_set_int(sm,"TOTAL-MEMORY",fd_malloc_usage()+fd_cons_usage());
#if (!(FD_LIGHTWEIGHT_OIDS))
  prim_set_int(sm,"REFERENCED-OIDS",fd_oid_table()->n_keys);
#endif
  prim_set_int(sm,"LOADED-OIDS",fd_loaded_oids);
  prim_set_int(sm,"NEW-OIDS",fd_new_oids);
  prim_set_int(sm,"POOLS",fd_get_pool_count());
  prim_set_int(sm,"INDICES",fd_get_index_count());
  prim_set_int(sm,"NET-CONNECTIONS",fd_get_server_count());
  prim_set(sm,fd_make_symbol("OS"),fd_make_string(FD_OSID));
  prim_set(sm,fd_make_symbol("COMPILATION-DATE"),
	   fd_make_string(__DATE__));
#if HAVE_SYS_RESOURCE_H
  {
    struct rlimit rl; struct rusage data; fd_getrusage(&data);
    prim_set_int(sm,"MAXRSS",data.ru_maxrss);
    prim_set_int(sm,"NSWAP",data.ru_nswap);
    prim_set(sm,fd_make_symbol("USER-TIME"),LISPFLOAT(get_usertime(&data)));
    prim_set(sm,fd_make_symbol("SYSTEM-TIME"),LISPFLOAT(get_systime(&data)));
    getrlimit(RLIMIT_CPU,&rl);
    prim_set_int(sm,"CURRENT-TIME-LIMIT",rl.rlim_cur);
    prim_set_int(sm,"MAX-TIME-LIMIT",rl.rlim_max);
    getrlimit(RLIMIT_DATA,&rl);
    prim_set_int(sm,"CURRENT-DATA-LIMIT",rl.rlim_cur);
    prim_set_int(sm,"MAX-DATA-LIMIT",rl.rlim_max);
#ifdef RLIMIT_NPROC
    getrlimit(RLIMIT_NPROC,&rl);
    prim_set_int(sm,"CURRENT-PROCS-LIMIT",rl.rlim_cur);
    prim_set_int(sm,"MAX-PROCS-LIMIT",rl.rlim_max);
#endif
    getrlimit(RLIMIT_NOFILE,&rl);
    prim_set_int(sm,"CURRENT-FILES-LIMIT",rl.rlim_cur);
    prim_set_int(sm,"MAX-FILES-LIMIT",rl.rlim_max);
  }
#endif
  return sm;
}

static lisp lisp_osid_cproc()
{
  return fd_make_string(FD_OSID);
}

/** Getting and setting rlimits */

static fd_exception fd_rlimitTooHigh=_("specified RLIMIT exceeds max");
static fd_lisp cputime_symbol, datasize_symbol, nprocs_symbol, nfiles_symbol;
static fd_lisp dual_symbol, stderr_symbol;

#if HAVE_SYS_RESOURCE_H
static int get_rlimit_id(fd_lisp id)
{
  if (FD_LISP_EQ(id,cputime_symbol)) return RLIMIT_CPU;
  else if (FD_LISP_EQ(id,datasize_symbol)) return RLIMIT_DATA;
#ifdef RLIMIT_NPROC
  else if (FD_LISP_EQ(id,nprocs_symbol)) return RLIMIT_NPROC;
#endif
  else if (FD_LISP_EQ(id,nfiles_symbol)) return RLIMIT_NOFILE;
  else fd_type_error(_("not a known rlimit parameter"),id);
}
#endif

static lisp lisp_get_max_rlimit_cproc(fd_lisp id)
{
#if HAVE_SYS_RESOURCE_H
  struct rlimit rl;
  getrlimit (get_rlimit_id(id),&rl);
  return FD_LISPFIX(rl.rlim_max);
#endif
  return FD_EMPTY_CHOICE;
}

static lisp lisp_get_cur_rlimit_cproc(fd_lisp id)
{
#if HAVE_SYS_RESOURCE_H
  struct rlimit rl;
  getrlimit (get_rlimit_id(id),&rl);
  return FD_LISPFIX(rl.rlim_cur);
#endif
  return FD_EMPTY_CHOICE;
}

static lisp lisp_set_rlimit_cproc(fd_lisp id,fd_lisp val)
{
#if HAVE_SYS_RESOURCE_H
  struct rlimit rl;
  int limid=get_rlimit_id(id), nval=fd_lisp2int(val);
  getrlimit (limid,&rl);
  if (nval>rl.rlim_max)
    fd_raise_lisp_exception(fd_rlimitTooHigh,FD_SYMBOL_NAME(id),val);
  else nval;
  setrlimit (get_rlimit_id(id),&rl);
  return FD_LISPFIX(rl.rlim_max);
#endif
  return FD_EMPTY_CHOICE;
}

/** System commands and URL access **/

static lisp lisp_system_handler(lisp expr,lispenv env)
{
  lisp cmd=fd_stringout(fd_get_body(expr,1),env);
  char *locally=fd_make_os_string(STRING_DATA(cmd));
  int retval=system(locally);
  fd_xfree(locally); decref(cmd);
  return LISPFIX(retval);
}

#ifdef WIN32
static lisp lisp_win32_launch_cproc(lisp path)
{
  HWND w=GetActiveWindow();
  char buf[512], *cwd=getcwd(buf,512);
  ShellExecute(w,"open",fd_strdata(path),NULL,cwd,0);
  return path;
}
#endif

/** Finding files in FDPATH **/

static lisp lisp_find_file_cproc(lisp args)
{
  lisp filename=fd_get_arg(args,0,FD_VOID);
  lisp path=fd_get_arg(args,1,FD_FALSE);
  char *found;
  if (!(STRINGP(filename)))
    fd_type_error(_("not a filestring"),filename);
  if (FD_FALSEP(path))
    found=fd_find_file(STRING_DATA(filename),fd_getenv("FDPATH"));
  else found=fd_find_file(STRING_DATA(filename),path);
  if (found) return fd_init_string(found,-1);
  else return FD_EMPTY_CHOICE;
}

/** File system access **/

static lisp lisp_file_existsp_cproc(lisp fname)
{
  if (STRINGP(fname))
    if (fd_file_existsp(STRING_DATA(fname))) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not a filestring"),fname);
}

static lisp lisp_file_writablep_cproc(lisp fname)
{
  if (STRINGP(fname))
    if (fd_file_writablep(STRING_DATA(fname))) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not a filestring"),fname);
}

static lisp lisp_directoryp_cproc(lisp path)
{
  if (STRINGP(path))
    if (fd_directoryp(STRING_DATA(path))) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_regular_filep_cproc(lisp path)
{
  if (STRINGP(path))
    if (fd_regular_filep(STRING_DATA(path))) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_symbolic_linkp_cproc(lisp path)
{
  if (STRINGP(path))
    if (fd_symbolic_linkp(STRING_DATA(path))) return FD_TRUE;
    else return FD_FALSE;
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_fullname_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *absp=fd_absolute_pathname(STRING_DATA(path));
    lisp value=fd_make_string(absp);
    fd_xfree(absp); return value;}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_basename_lexpr(lisp args)
{
  fd_lisp path, suffix;
  fd_get_args("BASENAME",args,&path,FD_VOID,&suffix,FD_FALSE,NULL);
  if (STRINGP(path)) {
    char *name=STRING_DATA(path); int has_suffix=0;
    fd_u8char *suff=((STRINGP(suffix)) ? (STRING_DATA(suffix)) : ((fd_u8char*)NULL));
    int len=strlen(name), suffix_len=((suff == NULL) ? 0 : (strlen(suff)));
    char *scan=name+len-1, *suffix_start=name+len-suffix_len;
    while (scan >= name)
      if ((*scan == '/') || (*scan == '\\')) break;
      else if (scan == suffix_start) {
	if (strcmp(scan,suff) == 0) has_suffix=1;
	scan--;}
      else scan--;
    if ((scan < name) && (!(has_suffix)))
      return incref(path);
    else if (has_suffix)
      return fd_make_substring(scan+1,suffix_start);
    else return fd_copy_string(scan+1);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_dirname_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *name=STRING_DATA(path);
    int len=strlen(name);
    char *scan=name+len-1;
    while (scan >= name)
      if ((*scan == '/') || (*scan == '\\')) break;
      else scan--;
    if (scan < name) return fd_make_string(".");
    else return fd_make_substring(name,scan);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_readlink_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *contents=fd_readlink(STRING_DATA(path));
    lisp answer=fd_make_string(contents);
    fd_xfree(contents);
    return answer;}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_filesize_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","FILESIZE",path);
    else return LISPFIX(status.st_size);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_fileowner_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","FILEOWNER",path);
    else {
      char *uname=fd_get_uname(status.st_uid);
      fd_u8char *ustring=fd_convert_os_string(uname);
      lisp answer=fd_init_string(ustring,-1); fd_xfree(uname);
      return answer;}}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_filegroup_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","FILEGROUP",path);
    else {
      char *gname=fd_get_gname(status.st_gid);
      fd_u8char *gstring=fd_convert_os_string(gname);
      lisp answer=fd_init_string(gstring,-1); fd_xfree(gname);
      return answer;}}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_fileatime_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","ACCESS-TIME",path);
    else return fd_make_timestamp(status.st_atime);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_filemtime_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","MODIFICATION-TIME",path);
    else return fd_make_timestamp(status.st_mtime);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_filectime_cproc(lisp path)
{
  if (STRINGP(path)) {
    char *filename=fd_filename(STRING_DATA(path));
    struct stat status; int stat_result=stat(filename,&status);
    fd_xfree(filename);
    if (stat_result < 0)
      fd_raise_lisp_exception("Can't get file info","CREATION-TIME",path);
    else return fd_make_timestamp(status.st_ctime);}
  else fd_type_error(_("not a filestring"),path);
}

static lisp lisp_file_olderp_cproc(lisp path1,lisp path2)
{
  if (!(STRINGP(path1)))
    fd_type_error(_("not a filestring"),path1);
  else if (!(STRINGP(path2)))
    fd_type_error(_("not a filestring"),path2);
  else {
    char *fname1=fd_filename(STRING_DATA(path1));
    char *fname2=fd_filename(STRING_DATA(path2));
    struct stat status1, status2;
    int stat_result1=stat(fname1,&status1);
    int stat_result2=stat(fname2,&status2);
    if (stat_result1 < 0)
      fd_raise_lisp_exception("Can't get file info","FILE-OLDER?",path1);
    else if (stat_result2 < 0)
      fd_raise_lisp_exception("Can't get file info","FILE-OLDER?",path2);
    if (difftime(status1.st_mtime,status2.st_mtime) < 0.0)
      return FD_TRUE;
    else return FD_FALSE;}
}

/** Walking the file system **/

#if (!(defined(WIN32)))
static lisp lisp_getfiles_cproc(lisp directory) {
  DIR *dp; struct dirent *entry; struct stat status;
  char dirname[1024], buf[1024];
  lisp answer=FD_EMPTY_CHOICE;
  CLEAR_ERR();
  if (STRINGP(directory)) {
    strcpy(dirname,STRING_DATA(directory));
    if (dirname[strlen(dirname)-1] != '/') strcat(dirname,"/");}
  else fd_raise_exception(fd_Type_Error);
  dp=opendir(dirname);
  if (dp == NULL)
    fd_raise_detailed_exception("Can't open directory",dirname);
  while ((entry=readdir(dp))) {
    strcpy(buf,dirname); strcat(buf,entry->d_name);
    stat(buf,&status);
    if (!(S_ISDIR(status.st_mode))) {
      lisp string=fd_make_string(buf);
      ADD_TO_CHOICE(answer,string);}
    CLEAR_ERR();}
  closedir(dp);
  return answer;}

static lisp lisp_getdirs_cproc(lisp directory) {
  DIR *dp; struct dirent *entry;
  struct stat status;
  char dirname[1024], buf[1024];
  lisp answer=FD_EMPTY_CHOICE;
  if (FD_STRINGP(directory)) {
    strcpy(dirname,STRING_DATA(directory));
    if (dirname[strlen(dirname)-1] != '/') strcat(dirname,"/");}
  else fd_raise_exception(fd_Type_Error);
  dp=opendir(dirname);
  if (dp == NULL)
    fd_raise_detailed_exception("Can't open directory",dirname);
  while ((entry=readdir(dp))) {
    strcpy(buf,dirname); strcat(buf,entry->d_name);
    stat(buf,&status);
    if ((S_ISDIR(status.st_mode)) &&
	(strcmp(entry->d_name,".") != 0) &&
	(strcmp(entry->d_name,"..") != 0)) {
      lisp string=fd_make_string(buf);
      ADD_TO_CHOICE(answer,string);}
    CLEAR_ERR();}
  closedir(dp);
  return answer;}
#else /* !(defined(WIN32)) */
static lisp lisp_getfiles_cproc(lisp directory) {
  WIN32_FIND_DATA data; HANDLE ptr; int keep_going=TRUE;
  DWORD file_attributes;
  char *dirname=NULL, buf[1024], *end_of_dirname;
  lisp answer=FD_EMPTY_CHOICE;
  if (STRINGP(directory))
    dirname=fd_filename(STRING_DATA(directory));
  else fd_raise_exception(fd_Type_Error);
  end_of_dirname=dirname+strlen(dirname)-1;
  if ((*dirname) && (*end_of_dirname == '\\')) *end_of_dirname='\0';
  sprintf(buf,"%s\\*.*",dirname);
  ptr=FindFirstFile(buf,&data);
  if (ptr == INVALID_HANDLE_VALUE) 
    fd_raise_exception("Can't list files");
  while (keep_going == TRUE) {
    strcpy(buf,dirname); strcat(buf,"\\");
    strcat(buf,data.cFileName);
    file_attributes=GetFileAttributes(buf);
    if ((file_attributes & FILE_ATTRIBUTE_DIRECTORY) == 0) {
      lisp string=fd_make_string(buf);
      ADD_TO_CHOICE(answer,string);}
    keep_going=FindNextFile(ptr,&data);}
  fd_xfree(dirname);
  return answer;
}

static lisp lisp_getdirs_cproc(lisp directory)
{
  WIN32_FIND_DATA data; HANDLE ptr; int keep_going=TRUE;
  DWORD file_attributes;
  char *dirname, buf[1024], *end_of_dirname;
  lisp answer=FD_EMPTY_CHOICE;
  if (STRINGP(directory))
    dirname=fd_filename(STRING_DATA(directory));
  else fd_raise_exception(fd_Type_Error);
  end_of_dirname=dirname+strlen(dirname)-1;
  if ((*dirname) && (*end_of_dirname == '\\')) *end_of_dirname='\0';
  sprintf(buf,"%s\\*.*",dirname);
  ptr=FindFirstFile(buf,&data);
  if (ptr == INVALID_HANDLE_VALUE)
    fd_raise_exception("Can't list files");
  while (keep_going == TRUE) {
    if (!((strcmp(data.cFileName,".") == 0) ||
          (strcmp(data.cFileName,"..") == 0))) {
      strcpy(buf,dirname); strcat(buf,"\\");
      strcat(buf,data.cFileName);
      file_attributes=GetFileAttributes(buf);
      if ((file_attributes & FILE_ATTRIBUTE_DIRECTORY) != 0) {
	lisp string=fd_make_string(buf);
	ADD_TO_CHOICE(answer,string);}}
    keep_going=FindNextFile(ptr,&data);}
  fd_xfree(dirname);
  return answer;
}

#endif

/** File operations **/

static lisp lisp_mkdir_cproc(lisp directory_name)
{
  if (STRINGP(directory_name)) {
    char *local=fd_filename(STRING_DATA(directory_name));
#if WIN32
    int code=mkdir(local);
#else
    int code=mkdir(local,(S_IRWXG|S_IRWXU));
#endif
   if (code < 0)
      fd_raise_lisp_exception
	("Couldn't make directory",strerror(errno),directory_name);
    else {free(local); return FD_TRUE;}}
  else fd_type_error(_("not a filestring"),directory_name);
}

static lisp lisp_remove_file_cproc(lisp filename)
{
  if (STRINGP(filename)) {
    char *local=fd_filename(STRING_DATA(filename));
    if (remove(local)) {
      if (errno) {
	fd_warn(_("Couldn't remove file %q (%s)"),filename,strerror(errno));
	CLEAR_ERR();}
      fd_xfree(local);
      return FD_FALSE;}
    else {free(local); return FD_TRUE;}}
  else fd_type_error(_("not a filestring"),filename);
}

static lisp lisp_rename_file_cproc(lisp from,lisp to)
{
  if (!(STRINGP(from)))
    fd_type_error(_("not a filestring"),from);
  else if (!(STRINGP(to)))
    fd_type_error(_("not a filestring"),to);
  else {
    char *lfrom=fd_make_os_string(STRING_DATA(from));
    char *lto=fd_make_os_string(STRING_DATA(to));
    if (rename(lfrom,lto)) {fd_xfree(lfrom); fd_xfree(lto); return FD_FALSE;}
    else {fd_xfree(lfrom); fd_xfree(lto); return FD_TRUE;}}
}

static lisp lisp_make_symbolic_link_cproc(lisp from,lisp to)
{
  if (!(STRINGP(from)))
    fd_type_error(_("not a filestring"),from);
  else if (!(STRINGP(to)))
    fd_type_error(_("not a filestring"),to);
  else {
#if WIN32
    return FD_FALSE;
#else
    char *lfrom=fd_make_os_string(STRING_DATA(from));
    char *lto=fd_make_os_string(STRING_DATA(to));
    if (symlink(lfrom,lto)) {
      fd_xfree(lfrom); fd_xfree(lto); return FD_FALSE;}
    else {fd_xfree(lfrom); fd_xfree(lto); return FD_TRUE;}
#endif
  }
}

static void copy_mode_info(char *path1,char *path2)
{
  struct stat status;
  char *p1=fd_filename(path1);
  char *p2=fd_filename(path2);
  if (stat(p1,&status) < 0) {
    perror("getting mode bits"); CLEAR_ERR();}
  if (chmod(p2,status.st_mode) < 0) {
    perror("setting mode bits"); CLEAR_ERR();}
  fd_xfree(p1); fd_xfree(p2);
}

static lisp lisp_copy_binary_file_cproc(lisp from,lisp to)
{
  if (!(STRINGP(from)))
    fd_type_error(_("not a filestring"),from);
  else if (!(STRINGP(to)))
    fd_type_error(_("not a filestring"),to);
  else {
    char *lfrom=fd_make_os_string(STRING_DATA(from));
    char *lto=fd_make_os_string(STRING_DATA(to));
    FILE *in=fd_fopen(lfrom,"rb"), *out=fd_fopen(lto,"wb");
    int bufsize=65536; char *buf=fd_xmalloc(bufsize);
    int ret_value=0, bytes=0;
    if (errno) {perror("Start of binary copy"); CLEAR_ERR();}
    if (in == NULL) 
      fd_raise_detailed_exception(fd_FileOpenFailed,lfrom);
    else if (out == NULL) 
      fd_raise_detailed_exception(fd_FileOpenWFailed,lto);
    else while ((ret_value=fread(buf,sizeof(char),bufsize,in)) || (!(feof(in)))) {
      bytes=bytes+ret_value; fwrite(buf,sizeof(char),ret_value,out);}
    fclose(out); fclose(in); copy_mode_info(lfrom,lto); 
    fd_xfree(lfrom); fd_xfree(lto); fd_xfree(buf);
    return LISPFIX(bytes);}
}

static lisp lisp_copy_text_file_cproc(lisp from,lisp to)
{
  if (!(STRINGP(from)))
    fd_type_error(_("not a filestring"),from);
  else if (!(STRINGP(to)))
    fd_type_error(_("not a filestring"),to);
  else {
    char *lfrom=fd_make_os_string(STRING_DATA(from));
    char *lto=fd_make_os_string(STRING_DATA(to));
    FILE *in=fd_fopen(lfrom,"r"), *out=fd_fopen(lto,"w");
    int bufsize=65536; char *buf=fd_xmalloc(bufsize);
    int ret_value=0, bytes=0;
    if (in == NULL) 
      fd_raise_detailed_exception(fd_FileOpenFailed,lfrom);
    else if (out == NULL) 
      fd_raise_detailed_exception(fd_FileOpenWFailed,lto);
    else while ((ret_value=fread(buf,sizeof(char),bufsize,in)) || (!(feof(in)))) {
      bytes=bytes+ret_value; fwrite(buf,sizeof(char),ret_value,out);}
    fclose(out); fclose(in); copy_mode_info(lfrom,lto); 
    fd_xfree(lfrom); fd_xfree(lto); fd_xfree(buf);
    return LISPFIX(bytes);}
}

/** Time functions **/

int need_to_init_time=1;
#if HAVE_SYS_RESOURCE_H
static long init_time;
#else
static clock_t init_time; 
#endif

static lisp months[12], days[31], dow[7], hours[24];
static lisp summer_symbol, winter_symbol, spring_symbol, autumn_symbol;
static lisp morning_symbol, afternoon_symbol, evening_symbol, night_symbol;

static int interpret_timezone(lisp zone)
{
  if ((FD_FIXNUMP(zone)) &&
      ((FD_FIXLISP(zone)) < 25) &&
      ((FD_FIXLISP(zone)) > -25))
    return FD_FIXLISP(zone)*3600;
  else if (FD_FIXNUMP(zone)) 
    return FD_FIXLISP(zone);
  else if (FD_FALSEP(zone))
    return fd_tzoff;
  else if (FD_STRINGP(zone))
    return fd_parse_tzspec(FD_STRING_DATA(zone),fd_tzoff);
  else if (FD_SYMBOLP(zone))
    return fd_parse_tzspec(FD_SYMBOL_NAME(zone),fd_tzoff);
  else return 0;
}

static int interpret_precision(lisp p)
{
  if (FD_FALSEP(p)) return 9;
  else if (FD_FIXNUMP(p))
    if ((FD_FIXLISP(p)) < 10)
     return FD_FIXLISP(p);
     else fd_type_error(_("invalid timestamp precision"),p);
  else if (FD_LISP_EQ(p,year_symbol)) return 1;
  else if (FD_LISP_EQ(p,month_symbol)) return 2;
  else if (FD_LISP_EQ(p,day_symbol)) return 3;
  else if (FD_LISP_EQ(p,hour_symbol)) return 4;
  else if (FD_LISP_EQ(p,minute_symbol)) return 5;
  else if (FD_LISP_EQ(p,second_symbol)) return 6;
  else if (FD_LISP_EQ(p,millisecond_symbol)) return 7;
  else if (FD_LISP_EQ(p,microsecond_symbol)) return 8;
  else if (FD_LISP_EQ(p,nanosecond_symbol)) return 9;
  else fd_type_error(_("invalid timestamp precision"),p);
}

static void get_opt_time_arg(lisp args,struct FD_XTIME *xtp)
{
  if (FD_EMPTY_LISTP(args)) fd_get_now(xtp);
  else if (LRECORD_TYPEP(CAR(args),timestamp_symbol))
    fd_timestamp_to_xtime(CAR(args),xtp);
  else if (FD_STRINGP(CAR(args))) 
    fd_iso8601_to_xtime(FD_STRING_DATA(CAR(args)),xtp);
  else fd_type_error(_("not a timestamp"),CAR(args));
}

static int get_opt_tzoff_arg(lisp args)
{
  if ((FD_EMPTY_LISTP(args)) || (!(FD_PAIRP(args))) ||
      (!(FD_PAIRP(FD_CDR(args)))))
    return fd_tzoff;
  else return interpret_timezone(CAR(CDR(args)));
}

static lisp lisp_getnow_lexpr(lisp args)
{
  struct FD_XTIME xtime;
  int precision=interpret_precision(fd_get_arg(args,0,FD_FALSE));
  int tzoff=interpret_timezone(fd_get_arg(args,1,FD_FALSE));
  fd_get_now(&xtime);
  if (xtime.precision < precision) precision=xtime.precision;
  return fd_make_xtimestamp(xtime.secs,xtime.nsecs,precision,tzoff);
}

static lisp lisp_timestamp_lexpr(lisp args)
{
  struct FD_XTIME xtime;
  fd_lisp result, arg, tzarg;
  fd_get_args("TIMESTAMP",args,&arg,FD_FALSE,&tzarg,FD_FALSE,NULL);
  fd_init_xtime(&xtime);
  if (!(FD_FALSEP(tzarg)))
    xtime.tzoff=interpret_timezone(tzarg);
  else xtime.tzoff=fd_tzoff;
  xtime.precision=second;
  if (FD_STRINGP(arg)) 
    fd_iso8601_to_xtime(FD_STRING_DATA(arg),&xtime);
  else if (FD_LRECORD_TYPEP(arg,timestamp_symbol))
    fd_timestamp_to_xtime(arg,&xtime);
  else if (FD_FALSEP(arg))
    fd_get_now(&xtime);
  else fd_type_error(_("invalid time spec"),arg);
  if (!(FD_FALSEP(tzarg)))
    xtime.tzoff=interpret_timezone(tzarg);
  return fd_xtime_to_timestamp(&xtime);
}
    
static lisp lisp_xtimestamp_lexpr(lisp args)
{
  struct FD_XTIME xtime; lisp result;
  int len=fd_list_length(args);
  if (len == 0) fd_get_now(&xtime);
  else if (len == 1) {
    int p=interpret_precision(CAR(args));
    fd_get_now(&xtime); 
    if (p < xtime.precision) xtime.precision=p;}
  else if (len == 2) {
    lisp targ=fd_get_arg(args,0,FD_FALSE);
    lisp parg=fd_get_arg(args,1,FD_FALSE);
    if (FD_LRECORD_TYPEP(targ,timestamp_symbol))
      fd_timestamp_to_xtime(targ,&xtime);
    else fd_type_error(_("not a timestamp"),targ);
    if (!(FD_FALSEP(parg)))
      xtime.precision=interpret_precision(parg);
    else xtime.precision=nanosecond;}
  else fd_raise_lisp_exception(fd_TooManyArgs,"XTIMESTAMP",args);
  return fd_xtime_to_timestamp(&xtime);
}
    
static lisp lisp_string2timestamp_cproc(lisp string)
{
  struct FD_XTIME xtime;
  if (FD_STRINGP(string)) {
    fd_lisp timestamp;
    if (fd_iso8601_to_xtime(STRING_DATA(string),&xtime) < 0)
      fd_raise_detailed_exception(fd_Invalid_IS08601,fd_strdata(string));
    timestamp=fd_xtime_to_timestamp(&xtime);
    return timestamp;}
  else fd_type_error(_("not a string"),string);
}

/* Gets the time of day as a string */
static lisp lisp_timestring_cproc()
{
  return fd_make_string(fd_timestring());
}

static lisp lisp_iso_timestring_lexpr(lisp args)
{
  struct FD_STRING_STREAM ss; struct FD_XTIME xtime;
  FD_INITIALIZE_STRING_STREAM(&ss,64);
  get_opt_time_arg(args,&xtime);
  xtime.tzoff=get_opt_tzoff_arg(args);
  fd_xtime_to_iso8601(&xtime,&ss);
  return fd_init_string(ss.ptr,ss.size);
}

static double timestamp_diff(lisp time1,lisp time2)
{
  if (!(LRECORD_TYPEP(time1,timestamp_symbol)))
    fd_type_error(_("not a timestamp"),time1);
  else if (!(LRECORD_TYPEP(time2,timestamp_symbol)))
    fd_type_error(_("not a timestamp"),time2);
  else {
    struct FD_XTIME t1, t2;
    double diff; fd_tmprec precision; int divisor;
    fd_timestamp_to_xtime(time1,&t1);
    fd_timestamp_to_xtime(time2,&t2);
    if (t1.precision > t2.precision) precision=t2.precision;
    else precision=t1.precision;
    if (precision == second) divisor=1000000000;
    else if (precision == millisecond) divisor=1000000;
    else if (precision == microsecond) divisor=1000;
    else if (precision == nanosecond) divisor=1;
    return ((double)difftime(t1.secs,t2.secs))+
      (((double)((t1.nsecs/divisor)-(t2.nsecs/divisor)))*((double)divisor)/1000000000.0);}
}

static lisp lisp_earlierp_cproc(lisp time1,lisp time2)
{
  if (timestamp_diff(time1,time2) < 0.0) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_laterp_cproc(lisp time1,lisp time2)
{
  if (timestamp_diff(time1,time2) < 0.0) return FD_FALSE;
  else return FD_TRUE;
}

static lisp lisp_time_difference_cproc(lisp time1,lisp time2)
{
  double diff=timestamp_diff(time1,time2);
  return LISPFLOAT(diff);
}

/* Gets the clock time in milliseconds */
static lisp lisp_clock_cproc()
{
#if HAVE_SYS_RESOURCE_H
  struct rusage data; int now;
  fd_getrusage(&data);
  now=(data.ru_utime.tv_sec)*1000000+data.ru_utime.tv_usec+
    (data.ru_stime.tv_sec)*1000000+data.ru_stime.tv_usec;
#else
  clock_t now=clock();
#endif
  if (need_to_init_time) {
    init_time=now; need_to_init_time=0; 
    return LISPFIX(0);}
  else {
#if HAVE_SYS_RESOURCE_H
    int diff=(now-init_time);
#else
    int diff=(1000000*(now-init_time))/CLOCKS_PER_SEC;
#endif
    return LISPFIX(diff);}
}

static void init_full_xtime(struct FD_XTIME *xtp,lisp args)
{
  lisp tzarg=fd_get_arg(args,1,FD_FALSE);
  int tzoff=get_opt_tzoff_arg(args);
  fd_init_xtime(xtp);
  get_opt_time_arg(args,xtp);
  if (!(FD_FALSEP(tzarg))) xtp->tzoff=tzoff;
  fd_breakup_time(&(xtp->tptr),xtp->secs,xtp->tzoff);
}

static lisp lisp_get_year_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_year);
}

static lisp lisp_get_month_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_mon);
}

static lisp lisp_get_date_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_mday);
}

static lisp lisp_get_hour_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_hour);
}

static lisp lisp_get_minute_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_min);
}

static lisp lisp_get_second_lexpr(lisp args)
{
  struct FD_XTIME xtime; init_full_xtime(&xtime,args);
  return LISPFIX(xtime.tptr.tm_sec);
}

static lisp lisp_get_day_lexpr(lisp args)
{
  struct FD_XTIME xtime;
  char namebuf[32]; fd_u8char *name; lisp result;
  init_full_xtime(&xtime,args);
  strftime(namebuf,32,"%A",&xtime.tptr); name=fd_xstring(namebuf);
  result=fd_parse_string(name); fd_xfree(name);
  return result;
}

static lisp lisp_get_monthname_lexpr(lisp args)
{
  struct FD_XTIME xtime;
  char namebuf[32]; fd_u8char *name; lisp result;
  init_full_xtime(&xtime,args);
  strftime(namebuf,32,"%B",&xtime.tptr); name=fd_xstring(namebuf);
  result=fd_parse_string(name); fd_xfree(name);
  return result;
}

static lisp lisp_get_season_lexpr(lisp args)
{
  lisp answer=(FD_EMPTY_CHOICE);
  struct FD_XTIME xtime; struct tm *now;
  init_full_xtime(&xtime,args); now=&(xtime.tptr);
  if ((now->tm_mon > 5) && (now->tm_mon < 10))
    {ADD_TO_CHOICE(answer,summer_symbol);}
  if ((now->tm_mon > 2) && (now->tm_mon < 6))
    {ADD_TO_CHOICE(answer,spring_symbol);}
  if ((now->tm_mon > 8) && (now->tm_mon < 12))
    {ADD_TO_CHOICE(answer,autumn_symbol);}
  if ((now->tm_mon < 4) || (now->tm_mon > 10))
    {ADD_TO_CHOICE(answer,winter_symbol);}
  return answer;
}

static lisp lisp_get_daytime_lexpr(lisp args)
{
  lisp answer=(FD_EMPTY_CHOICE);
  struct FD_XTIME xtime; struct tm *now;
  init_full_xtime(&xtime,args); now=&(xtime.tptr);
  if ((now->tm_hour > 5) && (now->tm_hour < 12))
    {ADD_TO_CHOICE(answer,morning_symbol);}
  if ((now->tm_hour > 12) && (now->tm_hour < 18))
    {ADD_TO_CHOICE(answer,afternoon_symbol);}
  if ((now->tm_hour > 16) && (now->tm_hour < 20))
    {ADD_TO_CHOICE(answer,evening_symbol);}
  if ((now->tm_hour < 5) || (now->tm_hour > 20))
    {ADD_TO_CHOICE(answer,night_symbol);}
  return answer;
}

static lisp lisp_timestamp_plus_cproc(lisp timestamp,lisp offset)
{
  time_t t=fd_timestamp_time(timestamp);
  time_t nt=t+(fd_lisp2int(offset));
  return fd_make_timestamp(nt);
}

static lisp lisp_get_timekeys_lexpr(lisp args)
{
  lisp answer=(FD_EMPTY_CHOICE);
  struct FD_XTIME xtime; struct tm *now;
  init_full_xtime(&xtime,args); now=&(xtime.tptr);
  /* Handle time of day */
  if (xtime.precision > day) {
    if ((now->tm_hour > 5) && (now->tm_hour < 12))
      {ADD_TO_CHOICE(answer,morning_symbol);}
    if ((now->tm_hour > 12) && (now->tm_hour < 18))
      {ADD_TO_CHOICE(answer,afternoon_symbol);}
    if ((now->tm_hour > 16) && (now->tm_hour < 20))
      {ADD_TO_CHOICE(answer,evening_symbol);}
    if ((now->tm_hour < 5) || (now->tm_hour > 20))
      {ADD_TO_CHOICE(answer,night_symbol);}}
  /* Handle season */
  if (xtime.precision > year) {
    if ((now->tm_mon >= 5) && (now->tm_mon < 9))
      {ADD_TO_CHOICE(answer,summer_symbol);}
    if ((now->tm_mon >= 2) && (now->tm_mon <= 5))
      {ADD_TO_CHOICE(answer,spring_symbol);}
    if ((now->tm_mon >= 8) && (now->tm_mon < 11))
      {ADD_TO_CHOICE(answer,autumn_symbol);}
    if ((now->tm_mon < 3) || (now->tm_mon > 10))
      {ADD_TO_CHOICE(answer,winter_symbol);}}
  /* Do the month, date, day of week, and hour */
  if (xtime.precision > year) {
    ADD_TO_CHOICE(answer,months[now->tm_mon]);}
  if (xtime.precision > month) {
    ADD_TO_CHOICE(answer,days[now->tm_mday-1]);}
  if (xtime.precision > month) {
    ADD_TO_CHOICE(answer,dow[now->tm_wday]);}
  if (xtime.precision > day) {
    ADD_TO_CHOICE(answer,hours[now->tm_hour]);}
  /* Do the week number */
  if (xtime.precision > month) {
    char buf[16]; sprintf(buf,"W%d",(now->tm_yday/7)+1);
    ADD_TO_CHOICE(answer,fd_make_symbol(buf));}
  /* Do the year */
  if (now->tm_year < 200) {
    ADD_TO_CHOICE(answer,FD_LISPFIX(now->tm_year+1900));}
  else {
    ADD_TO_CHOICE(answer,FD_LISPFIX(now->tm_year));}
  /* Do various combination forms, as strings.
     Unabashedly following American conventions here. */
  if (xtime.precision > month) {
    char buf[16]; sprintf(buf,"%d/%d",now->tm_mon+1,now->tm_mday);
    ADD_TO_CHOICE(answer,fd_make_string(buf));}
  if (xtime.precision > year) {char buf[16];
   if (now->tm_year < 200)
     sprintf(buf,"%d/%d",now->tm_mon+1,now->tm_year+1900);
   else sprintf(buf,"%d/%d",now->tm_mon+1,now->tm_year);
   ADD_TO_CHOICE(answer,fd_make_string(buf));}
  if (xtime.precision > day) {
    char buf[16]; sprintf(buf,"%d:%02d",now->tm_hour,now->tm_min);
    ADD_TO_CHOICE(answer,fd_make_string(buf));}
  return answer;
}

static lisp lisp_breakup_timestamp_lexpr(lisp args)
{
  lisp answer=fd_make_slotmap(8);
  struct FD_XTIME xtime; struct tm *now;
  init_full_xtime(&xtime,args); 
  fd_frame_add(answer,fd_make_symbol("TYPE"),fd_make_symbol("TIMESTAMP"));
  fd_frame_add(answer,year_symbol,LISPFIX(xtime.tptr.tm_year));
  fd_frame_add(answer,month_symbol,LISPFIX(xtime.tptr.tm_mon+1));
  fd_frame_add(answer,day_symbol,LISPFIX(xtime.tptr.tm_mday));
  fd_frame_add(answer,hour_symbol,LISPFIX(xtime.tptr.tm_hour));
  fd_frame_add(answer,minute_symbol,LISPFIX(xtime.tptr.tm_min));
  fd_frame_add(answer,second_symbol,LISPFIX(xtime.tptr.tm_sec));
  fd_frame_add(answer,fd_make_symbol("TZOFF"),LISPFIX(xtime.tzoff));
  return answer;
}

/** Miscellaneous: errno, sleep */

static lisp lisp_get_pid_cproc()
{
  int pid=(int)getpid();
  return FD_LISPFIX(pid);
}


static lisp lisp_get_ppid_cproc()
{
  int pid=(int)getppid();
  return FD_LISPFIX(pid);
}


static lisp lisp_check_errno_cproc(lisp place)
{
  if (errno) {perror(fd_strdata(place)); CLEAR_ERR();}
  return FD_VOID;
}

static lisp lisp_sleep(lisp secs)
{
  double time=fd_to_float(secs);
  fd_sleep(time);
  return FD_VOID;
}

/** Disabling notifications **/

static lisp lisp_disable_notifications_cproc()
{
  fd_set_notify_handler(NULL);
  return FD_VOID;
}

static FILE *notification_stream=NULL;
static fd_lisp lisp_notification_stream;

static void file_notifier(fd_u8char *message)
{
  struct tm _now, *now=&_now;
  fd_localtime(now,time(NULL));
  fprintf(notification_stream,"[%02d:%02d:%02d ",now->tm_hour,now->tm_min,now->tm_sec);
  fd_fputs_encoded(message,strlen(message),notification_stream);
  fprintf(notification_stream,"]\n"); fflush(notification_stream);
}

static lisp lisp_set_notify_cproc(lisp arg)
{
  if (FD_FALSEP(arg)) {
    if (notification_stream) {
      notification_stream=NULL;
      fd_decref(lisp_notification_stream);
      lisp_notification_stream=FD_VOID;}
    fd_set_notify_handler(NULL);}
  else if (FD_OUTPUT_FILEP(arg)) {
    lisp_notification_stream=fd_incref(arg);
    notification_stream=FD_CPTR_DATA(arg);
    fd_set_notify_handler(file_notifier);}
  else {
    if (notification_stream) {
      notification_stream=NULL;
      fd_decref(lisp_notification_stream);
      lisp_notification_stream=FD_VOID;}
    if (FD_STRINGP(arg)) {
      FILE *f=fd_fopen(fd_strdata(arg),"w");
      notification_stream=f;
      fd_set_notify_handler(file_notifier);}
    else if (FD_LISP_EQ(arg,dual_symbol))
      fd_set_notify_handler(fd_dual_notifier);
    else if (FD_LISP_EQ(arg,stderr_symbol))
      fd_set_notify_handler(fd_stderr_notifier);
    else fd_set_notify_handler(fd_stdout_notifier);}
  return FD_VOID;
}

/** Subjobs **/
static lisp subjob_symbol, remote_subjob_symbol;

#define SUBJOBP(x) \
  ((LRECORD_TYPEP(sj,subjob_symbol)) || \
   (LRECORD_TYPEP(sj,remote_subjob_symbol)))

#define ACTIVE_SUBJOBP(x) \
  ((SUBJOBP(x)) && (FIXNUMP(VECTOR_REF(LRECORD_DATA(x),0))))

#if (!(WIN32)) /* Need to fix eventually */

static lisp start_subjob(lisp args,lisp error_disposition)
{
  int commands[2], results[2], errors[2], pid;
  pipe(commands); pipe(results);
  if (FD_FALSEP(error_disposition)) pipe(errors);
  if ((pid=fork()) == 0) {
    /* This is run in the process which will get replaced by the subprocess;
       it sets stdin/stdout/stderr and constructs the argv vector. */
    lisp command_name=fd_get_arg(args,0,FD_VOID);
    int n_args=fd_list_length(args);
    char **arguments=malloc(sizeof(char *)*(n_args+1)); int i=1;
    DOLIST(arg,CDR(args)) 
      if (STRINGP(arg))
	arguments[i++]=fd_strdup(STRING_DATA(arg));
      else arguments[i++]=fd_object_to_string(arg);
    arguments[0]=fd_strdup(fd_strdata(command_name)); arguments[i]=NULL;
    /* close the end of the pipes we don't use */
    dup2(commands[0],0); dup2(results[1],1);
    close(commands[0]); close(commands[1]);
    close(results[0]); close(results[1]);
    /* If the error disposition is a string, it's the pathname for error reports. */
    if (STRINGP(error_disposition)) {
      char *path=fd_make_os_string(STRING_DATA(error_disposition));
      int fd=open(path,O_WRONLY); dup2(fd,2); close(fd);}
    /* If the error disposition is false, just use stderr, otherwise use the pipe
       created above. */
    else if (FD_FALSEP(error_disposition)) {
      dup2(errors[1],2); close(errors[0]); close(errors[1]);}
    (void) execvp(fd_strdata(command_name),arguments);
    fd_raise_detailed_exception(_("Can't exec"),fd_strdata(command_name));}
  else {
    lisp vec=fd_make_vector(5);
    /* close the end of the pipes we don't use */
    close(commands[0]); close(results[1]);
    fcntl(results[0],F_SETFL,O_NONBLOCK);
    if (FD_FALSEP(error_disposition)) 
      fcntl(errors[0],F_SETFL,O_NONBLOCK);
    FD_VECTOR_SET(vec,0,LISPFIX(pid));
    FD_VECTOR_SET(vec,1,incref(args));
    FD_VECTOR_SET(vec,2,fd_make_cptr(output_file_type,fdopen(commands[1],"w")));
    FD_VECTOR_SET(vec,3,fd_make_cptr(input_file_type,fdopen(results[0],"r")));
    if (FD_FALSEP(error_disposition)) {
      FD_VECTOR_SET(vec,4,fd_make_cptr(input_file_type,fdopen(errors[0],"r")));}
    else {FD_VECTOR_SET(vec,4,incref(error_disposition));}
    FD_CLEAR_ERR();
    return fd_make_lrecord(subjob_symbol,vec);}
}

static lisp lisp_subjob_lexpr(lisp args)
{
  return start_subjob(args,FD_FALSE);
}

static lisp lisp_start_subjob_lexpr(lisp args)
{
  lisp error_disposition=fd_get_arg(args,0,FD_VOID);
  return start_subjob(FD_CDR(args),error_disposition);
}
#endif

static void print_subjob(lisp sj,fd_string_stream s)
{
  lisp vec=LRECORD_DATA(sj);
  fd_printf(s,"[#SUBJOB %q: %q]",VECTOR_REF(vec,0),VECTOR_REF(vec,1));
}

static void print_remote_subjob(lisp sj,fd_string_stream s)
{
  lisp vec=LRECORD_DATA(sj);
  fd_printf(s,"[#REMOTE-SUBJOB %q: %q]",VECTOR_REF(vec,0),VECTOR_REF(vec,1));
}

static lisp lisp_subjob_pid_cproc(lisp sj)
{
  if (LRECORD_TYPEP(sj,subjob_symbol)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,0));}
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_subjob_socket_cproc(lisp sj)
{
  if (LRECORD_TYPEP(sj,remote_subjob_symbol)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,0));}
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_subjob_args_cproc(lisp sj)
{
  if (SUBJOBP(sj)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,1));}
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_subjob_input_cproc(lisp sj)
{
  if (ACTIVE_SUBJOBP(sj)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,2));}
  else if (SUBJOBP(sj)) fd_type_error(_("not an active subjob"),sj);
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_subjob_output_cproc(lisp sj)
{
  if (ACTIVE_SUBJOBP(sj)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,3));}
  else if (SUBJOBP(sj)) fd_type_error(_("not an active subjob"),sj);
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_subjob_errors_cproc(lisp sj)
{
  if (ACTIVE_SUBJOBP(sj)) {
    lisp vec=LRECORD_DATA(sj);
    return incref(VECTOR_REF(vec,4));}
  else if (SUBJOBP(sj)) fd_type_error(_("not an active subjob"),sj);
  else fd_type_error(_("not a subjob"),sj);
}

static lisp lisp_close_subjob_cproc(lisp sj)
{
#if (WIN32)
  if (LRECORD_TYPEP(sj,subjob_symbol)) return FD_VOID;
#else
  if (LRECORD_TYPEP(sj,subjob_symbol)) {
    lisp vec=LRECORD_DATA(sj);
    lisp pid_val=VECTOR_REF(vec,0);
    if (FIXNUMP(pid_val)) {
      (void) kill(FIXLISP(pid_val),15);
      FD_VECTOR_SET(vec,0,FD_FALSE);
      return FD_VOID;}
    else fd_type_error(_("not an active subjob"),sj);}
#endif
  else if (LRECORD_TYPEP(sj,remote_subjob_symbol)) {
    lisp vec=LRECORD_DATA(sj);
    lisp socket_val=VECTOR_REF(vec,0);
    if (FIXNUMP(socket_val)) {
      close(FIXLISP(socket_val));
      FD_VECTOR_SET(vec,0,FD_FALSE);
      return FD_VOID;}
    else fd_type_error(_("not an active subjob"),sj);}
  else fd_type_error(_("not an active subjob"),sj);
}

#if ((HAVE_SYS_WAIT_H) && (HAVE_WAIT4))
static lisp lisp_wait_for_subjob_lexpr(lisp args)
{
  fd_lisp sj, hang;
  fd_get_args("WAIT-FOR-SUBJOB",args,&sj,FD_FALSE,&hang,FD_TRUE,NULL);
#if (WIN32)
  return FD_VOID;
#else
  int pid, status; fd_lisp result;
  if (FD_FALSEP(sj)) 
    pid=wait4(-1,&status,((FD_FALSEP(hang)) ? (WNOHANG) : 0),NULL);
  else if (FD_FIXNUMP(sj)) 
    pid=wait4(FIXLISP(sj),&status,((FD_FALSEP(hang)) ? (WNOHANG) : 0),NULL);
  else if (LRECORD_TYPEP(sj,subjob_symbol)) {
    lisp vec=LRECORD_DATA(sj);
    lisp pid_val=VECTOR_REF(vec,0);
    pid=wait4(FIXLISP(pid_val),&status,((FD_FALSEP(hang)) ? (WNOHANG) : 0),NULL);}
  else fd_type_error(_("not an active subjob"),sj);
  if (pid < 0) return FD_EMPTY_CHOICE;
  else result=fd_make_vector(3);
  FD_VECTOR_SET(result,0,FD_LISPFIX(pid));
  if (WIFEXITED(status)) {}
  else if (WIFSIGNALED(status)) {
    FD_VECTOR_SET(result,1,FD_LISPFIX(WEXITSTATUS(status)));
    FD_VECTOR_SET(result,2,FD_LISPFIX(WTERMSIG(status)));}
  else {
    FD_VECTOR_SET(result,1,FD_LISPFIX(WEXITSTATUS(status)));}
  return result;
#endif
}
#endif

/** WIN32 fdopen **/

#if WIN32
static FILE *socket2file(long socket_id,char *mode)
{
  if (((socket_id = _open_osfhandle(socket_id,_O_BINARY)) < 0) ||
      ((socket_id = dup(socket_id)) < 0))
    fd_raise_exception("WIN32/OSF socket open failed");
  return fdopen(socket_id,mode);
}
#else
#define socket2file fdopen
#endif

/** Clients **/

static lisp start_remote_subjob
  (char *hostname,int port,char *port_id,lisp conn_id,int notblocking)
{
  long socket_id=
    ((port<0) ? (fd_open_file_socket(hostname,FD_STRING_DATA(conn_id),1)) :
     (fd_open_tcp_socket(hostname,port,port_id,1,NULL)));
  lisp vec=fd_make_vector(5);
#if (WIN32)
  unsigned long noblock=notblocking;
  ioctlsocket(socket_id,FIONBIO,&noblock);
#else
  if (notblocking) fcntl(socket_id,F_SETFL,O_NONBLOCK);
#endif
  
  FD_VECTOR_SET(vec,0,LISPFIX(socket_id));
  FD_VECTOR_SET(vec,1,incref(conn_id));
  FD_VECTOR_SET
    (vec,2,fd_make_cptr(output_file_type,socket2file(socket_id,"w")));
  FD_VECTOR_SET
    (vec,3,fd_make_cptr(input_file_type,socket2file(socket_id,"r")));
  FD_VECTOR_SET(vec,4,FD_FALSE);
  FD_CLEAR_ERR();
  return fd_make_lrecord(remote_subjob_symbol,vec);
}

static lisp connection_id(lisp hostname,lisp service)
{
  struct FD_STRING_STREAM ss;
  FD_INITIALIZE_STRING_STREAM(&ss,64);
  if (FD_FALSEP(service)) fd_printf(&ss,"local:");
  else {
      fd_print_lisp_to_string(service,&ss); fd_sputc(&ss,'@');}
  fd_print_lisp_to_string(hostname,&ss);
  return fd_init_string(ss.ptr,ss.size);
}

static lisp lisp_remote_subjob_cproc(lisp hostname,lisp service)
{
  if (STRINGP(hostname)) {
    fd_lisp result;
    char *host=fd_make_os_string(STRING_DATA(hostname));
    if (STRINGP(service)) {
      int portno=fd_get_portno(STRING_DATA(service));
      result=start_remote_subjob
	(host,portno,STRING_DATA(service),connection_id(hostname,service),1);}
    else if (FIXNUMP(service))
      result=start_remote_subjob
	(host,FIXLISP(service), NULL,connection_id(hostname,service),1);
    else if (FD_FALSEP(service))
      result=start_remote_subjob
	(host,-1,NULL,connection_id(hostname,service),1);
    else fd_type_error(_("service (port) is not a string or fixnum"),service);
    fd_xfree(host);
    return result;}
  else fd_type_error(_("hostname is not a string"),hostname);
  
}

/** Managing configuration files **/

enum config_file_op { set, add, reset};

static void add_to_config_file
  (fd_u8char *file,lisp var,lisp val,enum config_file_op op)
{
  /* Open the file locked */
  FILE *f=fd_fopen_locked(file,"a+",1);
  lisp entries=FD_EMPTY_LIST, entry;
  lisp chosen_entry=FD_EMPTY_CHOICE;
  struct FD_STRING_STREAM output;
  /* Raise an exception if you can't open the file for writing */
  if (f == NULL)
    fd_raise_detailed_exception(fd_FileOpenWFailed,file);
  /* Write in encoded ASCII */
  fd_set_file_encoding(f,"US-ASCII");
  /* We'll be using this to write the new config file */
  FD_INITIALIZE_STRING_STREAM(&output,1024);
  output.escape=1; output.fancy_oids=0;
  /* Read all of the entries */
  fseek(f,0,SEEK_SET);
  entry=fd_parse_lisp_from_stream(f);
  while (!(FD_EOF_OBJECTP(entry)))
    /* Do some syntax checking along the way */
    if ((!(PAIRP(entry))) || (!(SYMBOLP((CAR(entry))))))
      fd_raise_detailed_exception(fd_ConfigSyntaxError,file);
    else {
      if (LISP_EQ(CAR(entry),var)) chosen_entry=entry;
      entries=FD_MAKE_PAIR(entry,entries);
      entry=fd_parse_lisp_from_stream(f);}
  /* If there isn't an entry in there, just tack one on the end */
  if (FD_EMPTYP(chosen_entry)) {
    if (op == reset) {fd_fclose(f); return;}
    else if (op == set) 
      chosen_entry=FD_MAKE_LIST(2,var,incref(val));
    else if (op == add)
      chosen_entry=FD_MAKE_LIST(3,var,incref(val),fd_make_symbol("+"));
    fd_pprint_lisp_to_string(chosen_entry,&output,0,0,80);
    fd_sputc(&output,'\n');
    fseek(f,0,SEEK_END);
    fd_fputs_encoded(output.ptr,output.size,f); fd_fclose(f);}
  else { /* If there is one, output everything all over again, but
	    do something different on the chosen entry */
    DOLIST(entry,entries)
      if (LISP_EQ(entry,chosen_entry))
	if (op == reset) {} /* If resetting the entry, do nothing */
	else {
	  if (op == set) {
	    lisp old=CAR(CDR(entry));
	    RPLACA(CDR(entry),incref(val));
	    decref(old);}
	  else if (op == add) {
	    lisp values=CAR(CDR(entry));
	    ADD_TO_CHOICE(values,incref(val));
	    RPLACA(CDR(entry),values);}
	  fd_pprint_lisp_to_string(chosen_entry,&output,0,0,80);
	  fd_sputc(&output,'\n');}
      else {
	fd_pprint_lisp_to_string(entry,&output,0,0,80);
	fd_sputc(&output,'\n');}
#if (WIN32)
    fd_fclose(f); f=fd_fopen_locked(file,"w",1);
    if (f == NULL)
      fd_raise_detailed_exception("Can't truncate file",file);
#else
    fseek(f,0,SEEK_SET); ftruncate(fileno(f),0);
#endif
    fd_fputs_encoded(output.ptr,output.size,f); fclose(f);}
}

static lisp config_set_cproc(lisp file,lisp var,lisp val)
{
  fd_u8char *filename=fd_strdata(file);
  add_to_config_file(filename,var,val,set);
  return FD_VOID;
}

static lisp config_add_cproc(lisp file,lisp var,lisp val)
{
  fd_u8char *filename=fd_strdata(file);
  add_to_config_file(filename,var,val,add);
  return FD_VOID;
}

static lisp config_reset_cproc(lisp file,lisp var)
{
  fd_u8char *filename=fd_strdata(file);
  add_to_config_file(filename,var,FD_VOID,reset);
  return FD_VOID;
}

/** Evaluating with timeouts **/

static fd_exception time_limit_exception=_("time has run out");

void raise_timeout(int sig)
{
  fd_raise_exception(time_limit_exception);
}

#if (!(WIN32))
static lisp lisp_with_time_limit_handler(lisp expr,lispenv env)
{
  struct sigaction new_sa, old_sa; int alarm_togo=0, now;
  lisp limit=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp core=fd_get_arg(expr,2,FD_VOID), body=fd_get_body(expr,3);
  lisp result=FD_EMPTY_CHOICE;
  if (!(FD_FIXNUMP(limit))) fd_type_error(_("not a fixnum"),limit);
  else {
    WITH_HANDLING {
      new_sa.sa_handler=raise_timeout;
#ifdef SA_NODEFER
      new_sa.sa_flags=SA_NODEFER;
#else
      new_sa.sa_flags=0;
#endif
      sigemptyset(&new_sa.sa_mask);
      sigaction(SIGALRM,&new_sa,&old_sa);
      alarm_togo=alarm(FIXLISP(limit)); now=time(NULL);
      result=fd_eval_in_env(core,env);
      sigaction(SIGALRM,&old_sa,&new_sa);      
      alarm(alarm_togo);}
    ON_EXCEPTION {
      if (fd_theException() == time_limit_exception) {
	fd_clear_exception(); alarm(0);
	{DOLIST(dflt,body) {
	  fd_decref(result); result=fd_eval_in_env(dflt,env);}}
	alarm(alarm_togo);}
      else {alarm(alarm_togo); fd_reraise();}}
    END_HANDLING;}
  return result;
}
#endif

/** DNS access **/

static int parse_dotted_addr(char *string,unsigned int *addr)
{
  if ((sscanf(string,"%d.%d.%d.%d",&addr[0],&addr[1],&addr[2],&addr[3])) == 4)
    return 1;
  else return 0;
}

static fd_lisp make_addr(unsigned char *bytes)
{
  char buf[128];
  sprintf(buf,"%d.%d.%d.%d",bytes[0],bytes[1],bytes[2],bytes[3]);
  return fd_make_string(buf);
}

static fd_lisp lisp_lookup_host_cproc(fd_lisp hostname)
{
  unsigned int addr[4];
  fd_u8char *name=fd_strdata(hostname);
  char *localized=fd_make_os_string(name);
  struct hostent *entry;
  char **addrs, **names;
  fd_lisp answer;
  fd_lock_mutex(&_fd_dns_access_lock);
  if (parse_dotted_addr(localized,addr)) {
    unsigned char buf[4];
    buf[0]=addr[0]; buf[1]=addr[1]; buf[2]=addr[2]; buf[3]=addr[3];
    entry=gethostbyaddr(buf,4,AF_INET);}
  else entry=gethostbyname(localized);
  fd_xfree(localized);
  if (entry == NULL) {
    FD_CLEAR_ERR();
    fd_unlock_mutex(&_fd_dns_access_lock);
    return FD_EMPTY_CHOICE;}
  addrs=entry->h_addr_list; names=entry->h_aliases;
  answer=fd_init_string(fd_convert_os_string(entry->h_name),-1);
  FD_CLEAR_ERR();   /* gethostbyname may succeed but still set errno */
  while (*addrs) {
    fd_lisp addr=make_addr((unsigned char *)addrs[0]); addrs++;
    FD_ADD_TO_CHOICE(answer,addr);}
  while (*names) {
    fd_lisp alias=fd_make_string(*names); names++;
    FD_ADD_TO_CHOICE(answer,alias);}
  fd_unlock_mutex(&_fd_dns_access_lock);
  return answer;
}

static fd_lisp lisp_primary_hostname_cproc(fd_lisp hostname)
{
  unsigned int addr[4];
  fd_u8char *name=fd_strdata(hostname);
  char *localized=fd_make_os_string(name);
  struct hostent *entry;
  char **addrs, **names;
  fd_lisp answer;
  fd_lock_mutex(&_fd_dns_access_lock);
  if (parse_dotted_addr(localized,addr)) {
    unsigned char buf[4];
    buf[0]=addr[0]; buf[1]=addr[1]; buf[2]=addr[2]; buf[3]=addr[3];
    entry=gethostbyaddr(buf,4,AF_INET);}
  else entry=gethostbyname(localized);
  fd_xfree(localized);
  if (entry == NULL) {
    FD_CLEAR_ERR(); fd_unlock_mutex(&_fd_dns_access_lock);
    return FD_EMPTY_CHOICE;}
  else {
    fd_lisp result=
      fd_init_string(fd_convert_os_string(entry->h_name),-1);
    fd_unlock_mutex(&_fd_dns_access_lock);
    return result;}
}

/** URL access **/

static fd_lisp mime_type_slotid, text_symbol, content_slotid;
static fd_exception NonTextException=_("Not a text type");

#if FD_THREADS_ENABLED
static fd_mutex url_proto_lock;
#endif

static struct FD_URL_HANDLER {
  char *protocol;
  fd_lisp (*getter)(char *);
  struct FD_URL_HANDLER *next;} *url_handlers=NULL;

FDSCRIPT_EXPORT
/* fd_register_url_protocol:
    Arguments: a string and a pointer to an URL handler
    Returns: void
 Registers the handler for a particular protocol name.
*/
void fd_register_url_protocol(char *protocol,fd_lisp (*getter)(char *))
{  
  struct FD_URL_HANDLER *new=fd_malloc(sizeof(struct FD_URL_HANDLER));
  fd_lock_mutex(&url_proto_lock);
  new->protocol=fd_strdup(protocol); new->getter=getter;
  new->next=url_handlers; url_handlers=new;
  fd_unlock_mutex(&url_proto_lock);

}

FDSCRIPT_EXPORT
/* fd_urlget
    Arguments: a string and a pointer to an int
    Returns: a string (actually a pointer to a byte array)
  Gets the contents of a remote URL as a character string, storing the
size in the second argument (if non-NULL)
*/
fd_lisp fd_urlget(char *url)
{  
  char *colon=strchr(url,':'), protocol[256];
  struct FD_URL_HANDLER *scan=url_handlers;
  if (colon == NULL) return FD_EMPTY_CHOICE;
  else if (colon-url > 128) return FD_EMPTY_CHOICE;
  else {strncpy(protocol,url,colon-url); protocol[colon-url]=NUL;}
  while (scan) {
    if (strcmp(scan->protocol,protocol) == 0)
      return scan->getter(url);
    else scan=scan->next;}
  return FD_EMPTY_CHOICE;
}

static fd_lisp lisp_urlget_cproc(fd_lisp arg)
{
  if (STRINGP(arg)) {
    fd_lisp result=fd_urlget(FD_STRING_DATA(arg));
    if (FD_EMPTYP(result))
      if (errno) perror(FD_STRING_DATA(arg));
    FD_CLEAR_ERR();
    return result;}
  else fd_type_error("URL is not a string",arg);
}

static fd_lisp lisp_urlstring_cproc(fd_lisp arg)
{
  if (STRINGP(arg)) {
    fd_lisp entity=fd_urlget(fd_strdata(arg));
    if (FD_EMPTYP(entity))
      if (errno) perror(FD_STRING_DATA(arg));
    FD_CLEAR_ERR();
    if (STRINGP(entity)) return entity;
    else if (FD_SLOTMAPP(entity)) {
      if (fd_frame_test(entity,mime_type_slotid,text_symbol)) {
	fd_lisp content=fd_prim_get(entity,content_slotid);
	fd_decref(entity);
	return content;}
      else fd_raise_lisp_exception
	     (NonTextException,"url contents are not text",fd_frame_get(entity,mime_type_slotid));}
    else return entity;}
  else fd_type_error("URL is not a string",arg);
}

static fd_lisp set_network_timeouts_lexpr(fd_lisp args)
{
  if (FD_EMPTY_LISTP(args)) {
    fd_lisp ctimeout=fd_getenv("CONNECT_TIMEOUT");
    fd_lisp rtimeout=fd_getenv("RECEIVE_TIMEOUT");
    int ct=0, rt=0;
    if (FD_FIXNUMP(ctimeout)) ct=FD_FIXLISP(ctimeout);
    else if (FD_STRINGP(ctimeout)) 
      sscanf(FD_STRING_DATA(ctimeout),"%d",&ct);
    if (FD_FIXNUMP(rtimeout)) ct=FD_FIXLISP(rtimeout);
    else if (FD_STRINGP(rtimeout)) 
      sscanf(FD_STRING_DATA(rtimeout),"%d",&rt);
    fd_set_network_timeouts(ct,rt,0);}
  else {
    fd_lisp lct, lrt, lst;
    fd_get_args("SET-NETWORK-TIMEOUTS!",args,
		&lct,FD_LISPFIX(0),&lrt,FD_LISPFIX(0),&lst,FD_LISPFIX(0),
		NULL);
    fd_set_network_timeouts(fd_fixlisp(lct),fd_fixlisp(lrt),fd_fixlisp(lst));}
  return FD_VOID;
}

/** Initializing **/

DTYPES_EXPORT lisp fd_get_modules(void);

static void init_tables()
{
  months[0]=fd_make_symbol("JAN");  months[1]=fd_make_symbol("FEB");
  months[2]=fd_make_symbol("MAR");  months[3]=fd_make_symbol("APR");
  months[4]=fd_make_symbol("MAY");  months[5]=fd_make_symbol("JUN");
  months[6]=fd_make_symbol("JUL");  months[7]=fd_make_symbol("AUG");
  months[8]=fd_make_symbol("SEP");  months[9]=fd_make_symbol("OCT");
  months[10]=fd_make_symbol("NOV"); months[11]=fd_make_symbol("DEC");

  days[0]=fd_make_symbol("1ST");    days[1]=fd_make_symbol("2ND");
  days[2]=fd_make_symbol("3RD");    days[3]=fd_make_symbol("4TH");
  days[4]=fd_make_symbol("5TH");    days[5]=fd_make_symbol("6TH");
  days[6]=fd_make_symbol("7TH");    days[7]=fd_make_symbol("8TH");
  days[8]=fd_make_symbol("9TH");    days[9]=fd_make_symbol("10TH");
  days[10]=fd_make_symbol("11TH");  days[11]=fd_make_symbol("12TH");
  days[12]=fd_make_symbol("13TH");  days[13]=fd_make_symbol("14TH");
  days[14]=fd_make_symbol("15TH");  days[15]=fd_make_symbol("16TH");
  days[16]=fd_make_symbol("17TH");  days[17]=fd_make_symbol("18TH");
  days[18]=fd_make_symbol("19TH");  days[19]=fd_make_symbol("20TH");
  days[20]=fd_make_symbol("21ST");  days[21]=fd_make_symbol("22ND");
  days[22]=fd_make_symbol("23RD");  days[23]=fd_make_symbol("24TH");
  days[24]=fd_make_symbol("25TH");  days[25]=fd_make_symbol("26TH");
  days[26]=fd_make_symbol("27TH");  days[27]=fd_make_symbol("28TH");
  days[28]=fd_make_symbol("29TH");  days[29]=fd_make_symbol("30TH");
  days[30]=fd_make_symbol("31ST");  

  dow[0]=fd_make_symbol("SUN");     dow[1]=fd_make_symbol("MON");
  dow[2]=fd_make_symbol("TUES");    dow[3]=fd_make_symbol("WED");
  dow[4]=fd_make_symbol("THURS");   dow[5]=fd_make_symbol("FRI");
  dow[6]=fd_make_symbol("SAT");

  hours[0]=fd_make_symbol("MIDNIGHT"); hours[12]=fd_make_symbol("NOON");
  hours[1]=fd_make_symbol("1AM");      hours[13]=fd_make_symbol("1PM");
  hours[2]=fd_make_symbol("2AM");      hours[14]=fd_make_symbol("2PM");
  hours[3]=fd_make_symbol("3AM");      hours[15]=fd_make_symbol("3PM");
  hours[4]=fd_make_symbol("4AM");      hours[16]=fd_make_symbol("4PM");
  hours[5]=fd_make_symbol("5AM");      hours[17]=fd_make_symbol("5PM");
  hours[6]=fd_make_symbol("6AM");      hours[18]=fd_make_symbol("6PM");
  hours[7]=fd_make_symbol("7AM");      hours[19]=fd_make_symbol("7PM");
  hours[8]=fd_make_symbol("8AM");      hours[20]=fd_make_symbol("8PM");
  hours[9]=fd_make_symbol("9AM");      hours[21]=fd_make_symbol("9PM");
  hours[10]=fd_make_symbol("10AM");    hours[22]=fd_make_symbol("10PM");
  hours[11]=fd_make_symbol("11AM");    hours[23]=fd_make_symbol("11PM");

  morning_symbol   =fd_make_symbol("MORNING");
  afternoon_symbol =fd_make_symbol("AFTERNOON");
  evening_symbol   =fd_make_symbol("EVENING");
  night_symbol     =fd_make_symbol("NIGHT");

  spring_symbol=fd_make_symbol("SPRING");
  summer_symbol=fd_make_symbol("SUMMER");
  autumn_symbol=fd_make_symbol("AUTUMN");
  winter_symbol=fd_make_symbol("WINTER");

  mime_type_slotid=fd_make_symbol("MIME-TYPE");
  text_symbol=fd_make_symbol("TEXT");
  content_slotid=fd_make_symbol("CONTENT");
}

FDSCRIPT_EXPORT
void fd_initialize_osprims_c()
{

  fd_lispenv osenv;
  fd_osprims_env=fd_make_module();
  osenv=fd_osprims_env;

  init_tables();

  fd_register_module("OSPRIMS",osenv,FD_UNSAFE_ENV,0);

#if FD_THREADS_ENABLED
  fd_init_mutex(&(url_proto_lock));
#endif

  timestamp_symbol=fd_make_symbol("TIMESTAMP0");
  subjob_symbol=fd_make_symbol("SUBJOB");
  remote_subjob_symbol=fd_make_symbol("REMOTE-SUBJOB");

  dual_symbol=fd_make_symbol("DUAL");
  stderr_symbol=fd_make_symbol("STDERR");

  year_symbol=fd_make_symbol("YEAR");
  month_symbol=fd_make_symbol("MONTH");
  day_symbol=fd_make_symbol("DAY");
  hour_symbol=fd_make_symbol("HOUR");
  minute_symbol=fd_make_symbol("MINUTE");
  second_symbol=fd_make_symbol("SECOND");
  millisecond_symbol=fd_make_symbol("MILLISECOND");
  microsecond_symbol=fd_make_symbol("MICROSECOND");
  nanosecond_symbol=fd_make_symbol("NANOSECOND");

  cputime_symbol=fd_make_symbol("CPUTIME");
  datasize_symbol=fd_make_symbol("DATASIZE");
  nprocs_symbol=fd_make_symbol("NPROCS");
  nfiles_symbol=fd_make_symbol("NFILES");

  lisp_notification_stream=FD_VOID;

  {struct FD_TYPE_REGISTRY *r=fd_register_record(subjob_symbol);
   r->print_fcn=print_subjob;}
  {struct FD_TYPE_REGISTRY *r=fd_register_record(remote_subjob_symbol);
   r->print_fcn=print_remote_subjob;}

  fd_add_cproc(NULL,"SESSION-ID",0,lisp_get_session_id_cproc);
  fd_add_cproc(NULL,"GET-KERNEL-MODULES",0,fd_get_modules);
  fd_add_cproc(osenv,"SET-SESSION-MNEMONIC!",1,lisp_id_session_cproc);

  fd_add_cproc(NULL,"GET-PID",0,lisp_get_pid_cproc);
  fd_add_cproc(NULL,"GET-PPID",0,lisp_get_ppid_cproc);

  fd_add_cproc(osenv,"SET-LOCALE!",1,lisp_setlocale_cproc);
  fd_add_cproc(osenv,"SET-DEFAULT-ENCODING!",1,lisp_set_default_encoding_cproc);
  fd_add_cproc(osenv,"SET-CONSOLE-ENCODING!",1,lisp_set_console_encoding_cproc);
  fd_add_cproc(NULL,"PACKET->STRING",2,lisp_packet_to_string_cproc);
  fd_add_cproc(NULL,"STRING->PACKET",2,lisp_string_to_packet_cproc);
  fd_add_cproc(NULL,"LOAD-ENCODING",1,lisp_load_encoding_cproc);
  fd_add_cproc(NULL,"KNOWN-ENCODING?",1,lisp_known_encoding_cproc);

  fd_add_cproc(osenv,"DISABLE-NOTIFICATIONS!",0,lisp_disable_notifications_cproc);
  fd_add_cproc(osenv,"SET-NOTIFY!",1,lisp_set_notify_cproc);

  fd_add_cproc(osenv,"NEVER-SAVE!",0,lisp_never_save_cproc);

  fd_add_cproc(osenv,"FDGETENV",1,lisp_getenv_cproc);
  fd_add_cproc(osenv,"GETENV",1,lisp_cgetenv_cproc);
  fd_add_cproc(osenv,"CGETENV",1,lisp_cgetenv_cproc);

#if WIN32
  fd_add_cproc(osenv,"REGISTRY-CREATE-USER-KEY!",1,lisp_registry_create_user_cproc);
  fd_add_cproc(osenv,"REGISTRY-CREATE-MACHINE-KEY!",1,lisp_registry_create_machine_cproc);
  fd_add_restricted_lexpr("REGISTRY-GET",FD_NORMAL_LEXPR,lisp_registry_get_lexpr);
  fd_add_restricted_lexpr("REGISTRY-SET!",FD_NORMAL_LEXPR,lisp_registry_set_lexpr);
#endif

  fd_add_cproc(NULL,"GET-PORTNO",1,lisp_get_portno_cproc);
  fd_add_cproc(osenv,"LOOKUP-HOST",1,lisp_lookup_host_cproc);
  fd_add_cproc(osenv,"PRIMARY-HOSTNAME",1,lisp_primary_hostname_cproc);

  fd_add_lexpr(osenv,"SET-NETWORK-TIMEOUTS",
	       FD_NORMAL_LEXPR,set_network_timeouts_lexpr);

  fd_add_lexpr(NULL,"TIMESTAMP",FD_NORMAL_LEXPR,lisp_timestamp_lexpr);
  fd_add_lexpr(NULL,"XTIMESTAMP",FD_NORMAL_LEXPR,lisp_xtimestamp_lexpr);
  fd_add_cproc(NULL,"TIMESTRING",0,lisp_timestring_cproc);
  fd_add_lexpr
    (NULL,"ISO-TIMESTRING",FD_NORMAL_LEXPR,lisp_iso_timestring_lexpr);
  fd_add_lexpr
    (NULL,"GET-TIMEKEYS",FD_NORMAL_LEXPR,lisp_get_timekeys_lexpr);

  fd_add_alias(NULL,"DATESTRING","ISO-TIMESTRING");

  fd_add_cproc(NULL,"EARLIER?",2,lisp_earlierp_cproc);
  fd_add_cproc(NULL,"LATER?",2,lisp_laterp_cproc);
  fd_add_cproc(NULL,"TIMESTAMP-DIFF",2,lisp_time_difference_cproc);
  fd_add_alias(NULL,"DIFFTIME","TIMESTAMP-DIFF");

  fd_add_cproc(NULL,"TIMESTAMP-PLUS",2,lisp_timestamp_plus_cproc);

  fd_add_lexpr(NULL,"BREAKUP-TIMESTAMP",
	       FD_NORMAL_LEXPR,lisp_breakup_timestamp_lexpr);

  fd_add_cproc(NULL,"CLOCK",0,lisp_clock_cproc);
  
  fd_add_lexpr(NULL,"GET-YEAR",FD_NORMAL_LEXPR,lisp_get_year_lexpr);
  fd_add_lexpr(NULL,"GET-DATE",FD_NORMAL_LEXPR,lisp_get_date_lexpr);
  fd_add_lexpr(NULL,"GET-MONTHNUM",FD_NORMAL_LEXPR,lisp_get_month_lexpr);
  fd_add_lexpr(NULL,"GET-MONTH",FD_NORMAL_LEXPR,lisp_get_monthname_lexpr);
  fd_add_lexpr(NULL,"GET-HOUR",FD_NORMAL_LEXPR,lisp_get_hour_lexpr);
  fd_add_lexpr(NULL,"GET-MINUTE",FD_NORMAL_LEXPR,lisp_get_minute_lexpr);
  fd_add_lexpr(NULL,"GET-SECOND",FD_NORMAL_LEXPR,lisp_get_second_lexpr);
  fd_add_lexpr(NULL,"GET-DAY",FD_NORMAL_LEXPR,lisp_get_day_lexpr);

  fd_add_lexpr(NULL,"GET-SEASON",FD_NORMAL_LEXPR,lisp_get_season_lexpr);
  fd_add_lexpr(NULL,"GET-DAYTIME",FD_NORMAL_LEXPR,lisp_get_daytime_lexpr);
  /*  fd_add_lexpr(NULL,"GET-WEEKNUM",FD_NORMAL_LEXPR,lisp_get_weeknum_lexpr); */

  fd_add_cproc(NULL,"PARSE-ISO8601",1,lisp_string2timestamp_cproc);
  fd_add_lexpr(NULL,"GET-NOW",FD_NORMAL_LEXPR,lisp_getnow_lexpr);

  fd_add_cproc(NULL,"RESOURCES",0,lisp_resources_cproc);
  fd_add_cproc(NULL,"GET-OSID",0,lisp_osid_cproc);
  fd_add_cproc(NULL,"RU-FOOTPRINT",0,lisp_footprint_size_cproc);
  fd_add_cproc(NULL,"RU-STACK-SIZE",0,lisp_stack_size_cproc);
  fd_add_cproc(NULL,"RU-DATA-SIZE",0,lisp_data_size_cproc);
  fd_add_cproc(NULL,"RU-PAGE-FAULTS",0,lisp_page_faults_cproc);
  fd_add_cproc(NULL,"RU-NSWAPS",0,lisp_nswaps_cproc);
  fd_add_cproc(NULL,"RU-USER-TIME",0,lisp_user_time_cproc);
  fd_add_cproc(NULL,"RU-SYSTEM-TIME",0,lisp_system_time_cproc);
  fd_add_cproc(NULL,"RU-RUNTIME",0,lisp_runtime_cproc);

  fd_add_cproc(NULL,"GET-RLIMIT",1,lisp_get_cur_rlimit_cproc);
  fd_add_cproc(NULL,"GET-RLIMIT-MAX",1,lisp_get_max_rlimit_cproc);
  fd_add_restricted_cproc("SET-RLIMIT!",2,lisp_set_rlimit_cproc);

  fd_add_cproc(osenv,"CONFIG-SET!",3,config_set_cproc);
  fd_add_cproc(osenv,"CONFIG-ADD!",3,config_add_cproc);
  fd_add_cproc(osenv,"CONFIG-RESET!",2,config_reset_cproc);

  fd_add_cproc(NULL,"CONSUSAGE",0,lisp_consusage_cproc);
  fd_add_cproc(NULL,"MEMUSAGE",0,lisp_memusage_cproc);
  fd_add_cproc(NULL,"LOADED-OIDS",0,lisp_count_loaded_oids_cproc);
  fd_add_cproc(NULL,"OIDS-LOADED",0,lisp_count_oids_loaded_cproc);
#if (!(FD_LIGHTWEIGHT_OIDS))
  fd_add_cproc(NULL,"REFERENCED-OIDS",0,lisp_count_oids_referenced_cproc);
#endif
  fd_add_cproc(NULL,"FRAMERD-STATS",0,lisp_report_framerd_stats_cproc);
  fd_add_cproc(NULL,"MALLOC-STATS",0,lisp_report_malloc_stats_cproc);
  fd_add_cproc(NULL,"INDEX-STATS",0,lisp_report_index_stats_cproc);

#if (!(WIN32))
  fd_add_restricted_lexpr("OPEN-PROCESS",FD_NORMAL_LEXPR,lisp_subjob_lexpr);
  fd_add_alias(fd_enabled_env,"SUBJOB","OPEN-PROCESS");
  fd_add_restricted_lexpr
    ("OPEN-PROCESSX",FD_NORMAL_LEXPR,lisp_start_subjob_lexpr);
  fd_add_alias(fd_enabled_env,"START-SUBJOB","OPEN-PROCESSX");
#endif
  fd_add_cproc(osenv,"CLOSE-SUBJOB",1,lisp_close_subjob_cproc);
#if ((HAVE_SYS_WAIT_H) && (HAVE_WAIT4))
  fd_add_lexpr(osenv,"WAIT-FOR-SUBJOB",FD_NORMAL_LEXPR,lisp_wait_for_subjob_lexpr);
#endif
  fd_add_cproc(osenv,"OPEN-SOCKET",2,lisp_remote_subjob_cproc);
  fd_add_alias(osenv,"REMOTE-SUBJOB","OPEN-SOCKET");
  fd_add_cproc(osenv,"SUBJOB-PID",1,lisp_subjob_pid_cproc);
  fd_add_cproc(osenv,"SUBJOB-SOCKET",1,lisp_subjob_socket_cproc);
  fd_add_cproc(osenv,"SUBJOB-INPUT",1,lisp_subjob_input_cproc);
  fd_add_cproc(osenv,"SUBJOB-OUTPUT",1,lisp_subjob_output_cproc);
  fd_add_cproc(osenv,"SUBJOB-ERRORS",1,lisp_subjob_errors_cproc);
  fd_add_cproc(osenv,"SUBJOB-ARGS",1,lisp_subjob_args_cproc);

  fd_add_restricted_special_form("SYSTEM",lisp_system_handler);
  fd_add_cproc(osenv,"CHECK-ERRNO",1,lisp_check_errno_cproc);
#if (!(WIN32))
  fd_add_special_form(NULL,"WITH-TIME-LIMIT",lisp_with_time_limit_handler);
#endif
  fd_add_cproc(NULL,"SLEEP",1,lisp_sleep);

  fd_add_cproc(osenv,"CD",1,lisp_cwd_cproc);
  fd_add_cproc(osenv,"GET-WORKING-DIRECTORY",0,lisp_gwd_cproc);
  fd_add_alias(osenv,"PWD","GET-WORKING-DIRECTORY");

  fd_add_restricted_lexpr("FIND-FILE",FD_NORMAL_LEXPR,lisp_find_file_cproc);
  fd_add_cproc(osenv,"FILE-EXISTS?",1,lisp_file_existsp_cproc);
  fd_add_cproc(osenv,"FILE-WRITABLE?",1,lisp_file_writablep_cproc);
  fd_add_cproc(osenv,"FULLNAME",1,lisp_fullname_cproc);
  fd_add_restricted_lexpr("BASENAME",FD_NORMAL_LEXPR,lisp_basename_lexpr);
  fd_add_cproc(osenv,"DIRNAME",1,lisp_dirname_cproc);
  fd_add_cproc(osenv,"READLINK",1,lisp_readlink_cproc);
  fd_add_cproc(osenv,"DIRECTORY?",1,lisp_directoryp_cproc);
  fd_add_cproc(osenv,"SYMBOLIC-LINK?",1,lisp_symbolic_linkp_cproc);
  fd_add_cproc(osenv,"REGULAR-FILE?",1,lisp_regular_filep_cproc);
  fd_add_cproc(osenv,"FILE-SIZE",1,lisp_filesize_cproc);
  fd_add_cproc(osenv,"FILE-OWNER",1,lisp_fileowner_cproc);
  fd_add_cproc(osenv,"FILE-ACCESS-TIME",1,lisp_fileatime_cproc);
  fd_add_cproc(osenv,"FILE-MODIFICATION-TIME",1,lisp_filemtime_cproc);
  fd_add_cproc(osenv,"FILE-CREATION-TIME",1,lisp_filectime_cproc);
  fd_add_cproc(osenv,"FILE-OLDER?",2,lisp_file_olderp_cproc);

  fd_add_cproc(osenv,"GET-HOMEDIR",0,lisp_get_homedir_cproc);
  fd_add_restricted_lexpr("GET-USER-DATA",FD_NORMAL_LEXPR,lisp_get_user_data_lexpr);
  fd_add_restricted_lexpr("GET-UID",FD_NORMAL_LEXPR,lisp_get_uid_lexpr);
  fd_add_restricted_lexpr("GET-GID",FD_NORMAL_LEXPR,lisp_get_gid_lexpr);
  fd_add_restricted_cproc("SET-GROUP!",1,lisp_set_group_cproc);
  fd_add_cproc(osenv,"GETDIRS",1,lisp_getdirs_cproc);
  fd_add_cproc(osenv,"GETFILES",1,lisp_getfiles_cproc);

  fd_add_cproc(osenv,"REMOVE-FILE",1,lisp_remove_file_cproc);
  fd_add_cproc(osenv,"RENAME-FILE",2,lisp_rename_file_cproc);
  fd_add_cproc(osenv,"MAKE-SYMBOLIC-LINK",2,lisp_make_symbolic_link_cproc);
  fd_add_cproc(osenv,"COPY-BINARY-FILE",2,lisp_copy_binary_file_cproc);
  fd_add_cproc(osenv,"COPY-TEXT-FILE",2,lisp_copy_text_file_cproc);
  fd_add_cproc(osenv,"COPY-FILE",2,lisp_copy_binary_file_cproc);
  fd_add_cproc(osenv,"MKDIR",1,lisp_mkdir_cproc);

#ifdef WIN32
  fd_add_cproc(osenv,"WIN32-LAUNCH",1,lisp_win32_launch_cproc);
#endif

  fd_add_restricted_cproc("URLGET",1,lisp_urlget_cproc);
  fd_add_restricted_cproc("URLSTRING",1,lisp_urlstring_cproc);

  fd_register_source_file("osprims",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: osprims.c,v $
   Revision 1.56  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.55  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.54  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.53  2004/04/18 15:30:52  haase
   More and consistent notify handlers

   Revision 1.52  2004/03/31 17:43:14  haase
   Fixed buffer overflow in string2packet

   Revision 1.51  2004/03/07 14:41:02  haase
   Better fit of RESOURCES slot names to rusage field names

   Revision 1.50  2004/02/08 17:01:14  haase
   Renamed fd_open_local_socket to fd_open_file_socket

   Revision 1.49  2004/02/05 08:54:32  haase
   Made string->packet take its raw string data with a #f encoding

   Revision 1.48  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.47  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.46  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.45.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.45.2.2  2003/01/26 20:41:48  haase
   Misc. fixes especially some GC

   Revision 1.45.2.1  2002/09/26 02:07:17  haase
   Various fixes (Thanks, Ralph)

   Revision 1.45  2002/07/01 17:08:13  haase
   Made timekeys include a 5:00 token with minutes padded by zero.

   Revision 1.44  2002/07/01 02:52:48  haase
   Fixed off-by-one bugs in timekey extraction

   Revision 1.43  2002/06/15 14:56:30  haase
   Fixed bug in handling START-SUBJOB args

   Revision 1.42  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.41  2002/06/07 00:34:03  haase
   Made remote-subjob work with local sockets

   Revision 1.40  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.39  2002/05/13 15:21:34  haase
   Fixes to get_timekeys from Mario

   Revision 1.38  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.37  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.36  2002/04/27 02:48:11  haase
   Added mutexes protecting DNS accesses

   Revision 1.35  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.34  2002/04/16 16:14:38  haase
   Fixed some inconsistent returns

   Revision 1.33  2002/04/15 18:23:54  haase
   Added primitive for setting and resetting network timeouts

   Revision 1.32  2002/04/14 18:53:45  haase
   Made RESOURCES always include user and system time (when available)

   Revision 1.31  2002/04/10 19:02:10  haase
   Fix remote subjobs to handle new socket opening prototype

   Revision 1.30  2002/04/10 12:28:23  haase
   Fixed handling of NULL size pointer to metadata retrieval functions

   Revision 1.29  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
