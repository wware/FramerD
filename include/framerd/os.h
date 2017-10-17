/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: os.h,v 1.41 2005/01/14 16:48:44 haase Exp $

  This file is part of FramerD, a representation language and semantic
  database developed by Kenneth B. Haase and his students at the Media
  Laboratory at the Massachusetts Institute of Technology in Cambridge,
  Massachusetts.  Research at the Media Lab is supported by funds and
  equipment from a variety of corporations and government sponsors whose
  contributions are gratefully acknowledged.

    Use, modification, and redistribution of this program is permitted
    under the terms of either (at the developer's discretion) the GNU
    General Public License (GPL) Version 2, the GNU Lesser General Public
    License.

    This program is based on the FramerD library released in Fall 2001 by
    MIT under both the GPL and the LGPL licenses, both of which accompany
    this distribution.  Subsequent modifications by beingmeta, inc. are
    also released under both the GPL and LGPL licenses (at the developer's
    discretion).

*************************************************************************/

#ifndef FRAMERD_OS_H /* If defined, skip the file */
#define FRAMERD_OS_H

#include "framerd/common.h"
#include <time.h>
#if (HAVE_SYS_RESOURCE_H)
#include <sys/time.h>
#include <sys/resource.h>
#endif

/* Exceptions */

DTYPES_EXPORT fd_exception
  fd_FileWriteFailed,
  fd_HugeMalloc,
  fd_ReallocFailed,
  fd_BadUTF8,
  fd_NegativeUNGETC,
  fd_InconsistentUNGETC,
  fd_InvalidChar,
  fd_NoLocalChar,
  fd_FileLockFailed,
  fd_FileWriteFailed,
  fd_GetEnvFailed,
  fd_CantFindFile,
  fd_UnknownCmdLineArg,
  fd_InvalidUnicodeEscape,
  fd_InvalidUnicodeChar,
  fd_NonStringEnvVar,
  fd_GMTIMEfailed,
  fd_GETCWDfailed,
  fd_UnknownEncoding,
  fd_BadMultiByteChar,
  fd_BadPrintfArg,
  fd_FileOpenWFailed,
  fd_FileOpenFailed,
  fd_SignalException;

DTYPES_EXPORT fd_exception
  fd_SocketClosed,
  fd_UnknownHost,
  fd_BadServerSpec,
  fd_ConnectionFailed,
  fd_UnhandledURL;

/* Time functions */

DTYPES_EXPORT int fd_tzoff;

typedef enum fd_timestamp_precision {
  year=1, month=2, day=3, hour=4, minute=5, second=6,
  millisecond=7, microsecond=8, nanosecond=9} fd_tmprec;

struct FD_XTIME {
  struct tm tptr;
  time_t secs; int nsecs;
  fd_tmprec precision;
  int tzoff;};

DTYPES_EXPORT void fd_init_xtime(struct FD_XTIME *xtp);
DTYPES_EXPORT time_t fd_get_now(struct FD_XTIME *xtp);

DTYPES_EXPORT time_t fd_iso8601_to_xtime(char *s,struct FD_XTIME *xtp);
DTYPES_EXPORT int fd_xtime_to_iso8601(struct FD_XTIME *xtp,fd_string_stream ss);

DTYPES_EXPORT time_t fd_timestamp_to_xtime(fd_lisp timestamp,struct FD_XTIME *xtp);
DTYPES_EXPORT fd_lisp fd_xtime_to_timestamp(struct FD_XTIME *xtp);
DTYPES_EXPORT fd_lisp fd_make_xtimestamp(time_t moment,int nsecs,fd_tmprec prec,int tzoff);

DTYPES_EXPORT int fd_localtime(struct tm *tptr,time_t now);
DTYPES_EXPORT int fd_breakup_time(struct tm *tptr,time_t now,int off);
DTYPES_EXPORT char *fd_iso8601_string(time_t moment);
DTYPES_EXPORT char *fd_datestring(time_t moment);
DTYPES_EXPORT char *fd_timestring(void);
DTYPES_EXPORT time_t fd_parse_iso8601(char *string);
DTYPES_EXPORT time_t fd_mktime(struct tm *tptr,int tzoff);
DTYPES_EXPORT int fd_parse_tzspec(char *s,int dflt);

DTYPES_EXPORT void fd_sleep(double secs);

DTYPES_EXPORT fd_lisp fd_getenv(char *var);
DTYPES_EXPORT fd_u8char *fd_string_getenv(char *var);
DTYPES_EXPORT int fd_int_getenv(char *var,int dflt);
DTYPES_EXPORT int fd_bool_getenv(char *var);

DTYPES_EXPORT unsigned int fd_random(void);
DTYPES_EXPORT void fd_set_random(unsigned int x);

/* Registry functions */

#ifdef WIN32
DTYPES_EXPORT int fd_registry_create(HANDLE h,char *path);
DTYPES_EXPORT fd_lisp fd_registry_query(HANDLE h,char *path,char *name);
DTYPES_EXPORT int fd_registry_modify
  (HANDLE h,char *path,char *name,fd_lisp value, int create);
DTYPES_EXPORT fd_lisp fd_registry_get(char *path,char *name);
DTYPES_EXPORT fd_lisp fd_registry_set(char *path,char *name,fd_lisp value);
#endif


/* File Functions */

DTYPES_EXPORT char *fd_make_absolute_pathname(fd_u8char *file,char *dir);
DTYPES_EXPORT char *fd_absolute_pathname(fd_u8char *file);
DTYPES_EXPORT char *fd_filename(fd_u8char *filename);
DTYPES_EXPORT char *fd_dirname(char *filename);
DTYPES_EXPORT char *fd_basename(char *string,int with_suffix);
DTYPES_EXPORT char *fd_readlink(fd_u8char *filename);
DTYPES_EXPORT char *fd_get_real_pathname(fd_u8char *filename);

DTYPES_EXPORT FILE *fd_fopen(fd_u8char *filename,char *mode);
DTYPES_EXPORT FILE *fd_fopen_locked(fd_u8char *filename,char *mode,int wlock);
DTYPES_EXPORT void fd_fclose(FILE *);
DTYPES_EXPORT FILE *fd_fopen_tmpfile(char *namebuf,char *mode);

DTYPES_EXPORT int fd_file_existsp(fd_u8char *filename);
DTYPES_EXPORT int fd_file_writablep(fd_u8char *filename);
DTYPES_EXPORT int fd_regular_filep(fd_u8char *path);
DTYPES_EXPORT int fd_symbolic_linkp(fd_u8char *path);
DTYPES_EXPORT int fd_directoryp(fd_u8char *path);
DTYPES_EXPORT fd_lisp fd_getpath(fd_u8char *envvar);
DTYPES_EXPORT fd_u8char *fd_find_file(fd_u8char *filename,fd_lisp search_path);
DTYPES_EXPORT char *fd_get_exec_filename(char *argv0);

DTYPES_EXPORT int fd_flush_input_buffer(FILE *f);


/* File locking */

DTYPES_EXPORT char *fd_session_id(void);
DTYPES_EXPORT void fd_set_session_mnemonic(char *mnemonic);
DTYPES_EXPORT char *fd_get_session_mnemonic(void);
DTYPES_EXPORT char *fd_get_build_date(void);
DTYPES_EXPORT void fd_set_build_date(char *date);

/* User information */

DTYPES_EXPORT char *fd_get_uname(uid_t id);
DTYPES_EXPORT uid_t fd_get_uid(char *id);
DTYPES_EXPORT fd_lisp fd_get_user_data(uid_t id);
DTYPES_EXPORT uid_t fd_get_current_uid(void);
DTYPES_EXPORT int fd_set_uid(char *id);
DTYPES_EXPORT int fd_set_gid(char *id);
DTYPES_EXPORT char *fd_get_homedir(void);

DTYPES_EXPORT char *fd_get_gname(gid_t id);
DTYPES_EXPORT gid_t fd_get_gid(char *id);

/* Unicode I/O */

#define FD_ENCODING_INCLUDES_ASCII 1
#define FD_ENCODING_IS_LINEAR (FD_ENCODING_INCLUDES_ASCII<<1)

DTYPES_EXPORT unsigned int fd_chardata_warnings;

typedef int xchar;

typedef int(*mb2wc_fn)(xchar *,uchar *,size_t);
typedef int(*wc2mb_fn)(uchar *,xchar);

struct FD_MB_MAP {unsigned int from, to;};
struct FD_TEXT_ENCODING {
  char **names; int flags;
  int charset_size;
  struct FD_MB_MAP *charset;
  struct FD_MB_MAP *charset_inv;
  wc2mb_fn wc2mb; mb2wc_fn mb2wc;
  struct FD_TEXT_ENCODING *next;};

struct FD_XFILE {
  FILE *f; struct FD_TEXT_ENCODING *encoding;
  unsigned char in[16]; int in_size;
  int last_char;};
typedef struct FD_XFILE *fd_xfile;

DTYPES_EXPORT int fd_define_encoding
  (char *name,struct FD_MB_MAP *charset,int size,
   wc2mb_fn,mb2wc_fn,int flags);
DTYPES_EXPORT void fd_load_encoding(char *name,char *filename);
DTYPES_EXPORT void fd_init_xfile
   (struct FD_XFILE *,FILE *f,struct FD_TEXT_ENCODING *);
DTYPES_EXPORT struct FD_XFILE *fd_associate_xfile(FILE *f,struct FD_XFILE *);
DTYPES_EXPORT void fd_set_file_encoding(FILE *f,char *name);
DTYPES_EXPORT struct FD_TEXT_ENCODING *fd_get_file_encoding(FILE *f);
DTYPES_EXPORT void fd_set_default_encoding(char *name);
DTYPES_EXPORT struct FD_TEXT_ENCODING *fd_get_default_encoding(void);
DTYPES_EXPORT struct FD_TEXT_ENCODING *fd_get_encoding(char *);

DTYPES_EXPORT int fd_fgetc(FILE *f);
DTYPES_EXPORT void fd_xputs_encoded(unsigned char *s,int len,struct FD_XFILE *f);
DTYPES_EXPORT void fd_fputs_encoded(unsigned char *s,int len,FILE *f);
DTYPES_EXPORT void fd_xputs_raw(unsigned char *s,int len,struct FD_XFILE *f);
DTYPES_EXPORT void fd_fputs_raw(unsigned char *s,int len,FILE *f);
DTYPES_EXPORT void fd_ungetc(int c,FILE *f);
DTYPES_EXPORT void fd_fputc(xchar c,FILE *f);
DTYPES_EXPORT fd_u8char *fd_read_line(FILE *f,int *size);


DTYPES_EXPORT void fd_xputc(xchar,struct FD_XFILE *);
DTYPES_EXPORT void fd_xputc_encoded(xchar,struct FD_XFILE *);
DTYPES_EXPORT int fd_xgetc(struct FD_XFILE *);
DTYPES_EXPORT int fd_xgetc_encoded(struct FD_XFILE *);
DTYPES_EXPORT fd_u8char *fd_xgets(fd_u8char *buf,int size,struct FD_XFILE *);
DTYPES_EXPORT void fd_xungetc(int c,struct FD_XFILE *e);

DTYPES_EXPORT struct FD_XFILE *fd_get_xfile(FILE *);
DTYPES_EXPORT void fd_free_xfile(FILE *stream);

DTYPES_EXPORT int fd_utf_string_length(fd_u8char *data);
DTYPES_EXPORT int fd_utf8_strlen(fd_u8char *str,int len);
DTYPES_EXPORT char *fd_utf8_substring(fd_u8char *str,int i);
DTYPES_EXPORT int fd_utf8_string_ref(fd_u8char *str);
DTYPES_EXPORT fd_u8char *fd_make_utf8(uchar *start,uchar *end,struct FD_TEXT_ENCODING *e);
DTYPES_EXPORT unsigned char *fd_convert_utf8(fd_u8char *data,int len,struct FD_TEXT_ENCODING *e,int *);
DTYPES_EXPORT unsigned char *fd_localize_utf8(fd_u8char *data,struct FD_TEXT_ENCODING *e);
DTYPES_EXPORT int fd_write_utf8
  (struct FD_STRING_STREAM *ssp,uchar *start,uchar *end,
   struct FD_TEXT_ENCODING *e);
DTYPES_EXPORT int fd_valid_utf8p(fd_u8char *s);
DTYPES_EXPORT fd_u8char *fd_convert_os_string(char *local_string);
DTYPES_EXPORT char *fd_make_os_string(fd_u8char *local_string);
DTYPES_EXPORT fd_u8char *fd_xstring(uchar *local_string);
DTYPES_EXPORT fd_u8char *fd_interpret_unicode_escapes(fd_u8char *string);
DTYPES_EXPORT fd_u8char *fd_upcase_string(fd_u8char *string,int len);
DTYPES_EXPORT fd_u8char *fd_downcase_string(fd_u8char *string,int len);

DTYPES_EXPORT
fd_lisp fd_foreign_filestring(char *filename,struct FD_TEXT_ENCODING *e);

struct FD_CHAR_INFO {
  unsigned char type_info;
  unsigned short lower; unsigned short upper;
  short dweight;};

struct FD_COMPRESSED_CHAR_INFO {
  unsigned short lud; unsigned char type_info;};

DTYPES_EXPORT struct FD_COMPRESSED_CHAR_INFO *fd_compressed_charinfo[];

#define FD_UPPER_TYPE_CODE 0x81
#define FD_LOWER_TYPE_CODE 0x80
#define FD_NUMERIC_DIGIT_CODE 0x40

#ifndef FD_INLINE_CHARACTER_OPS
#define FD_INLINE_CHARACTER_OPS 0
#endif

#if (FD_INLINE_CHARACTER_OPS)
STATIC_INLINE struct FD_CHAR_INFO fd_get_char_data(int c)
{
  int hi=(c&0xFF00)>>8, lo=c&0xFF;
  struct FD_COMPRESSED_CHAR_INFO *codepage=fd_compressed_charinfo[hi];
  if (codepage) {
    struct FD_CHAR_INFO entry;
    struct FD_COMPRESSED_CHAR_INFO cinfo=codepage[lo];
    entry.type_info=cinfo.type_info;
    entry.upper=c; entry.lower=c; entry.dweight=-1;
    if (entry.type_info == FD_UPPER_TYPE_CODE) entry.lower=cinfo.lud;
    else if (entry.type_info == FD_LOWER_TYPE_CODE) entry.upper=cinfo.lud;
    else if (entry.type_info == FD_NUMERIC_DIGIT_CODE) entry.dweight=cinfo.lud;
    return entry;}
  else {
    struct FD_CHAR_INFO fake;
    char warnbuf[64];
    fake.type_info=0x80; fake.lower=c; fake.upper=c; fake.dweight=-1;
    if (fd_chardata_warnings) {
      sprintf(warnbuf,_("Unknown character code \\u%04x"),c);
      fd_warn("%s",warnbuf);}
    return fake;}
}
#else
DTYPES_EXPORT struct FD_CHAR_INFO _fd_get_char_data(int c);
#define fd_get_char_data(c) _fd_get_char_data(c)
#endif

#define fd_toupper(c) \
  ((c<0) ? (c) : \
   ((c<0x80) ? (toupper(c)) : (fd_get_char_data(c).upper)))
#define fd_tolower(c) \
  ((c<0) ? (c) : \
   ((c<0x80) ? (tolower(c)) : (fd_get_char_data(c).lower)))
#define fd_ctype_info(c) \
  ((c<0) ? (0) : ((fd_get_char_data(c)).type_info))
#define fd_isupper(c) \
   ((c<0x80) ? (isupper(c)) : ((fd_ctype_info(c)) == FD_UPPER_TYPE_CODE))
#define fd_islower(c) \
   ((c<0x80) ? (islower(c)) : ((fd_ctype_info(c)) == FD_LOWER_TYPE_CODE))
#define fd_isalpha(c) \
   ((c<0x80) ? (isalpha(c)) : ((fd_ctype_info(c)) & (0x80)))
#define fd_isspace(c) \
   ((c<0x80) ? (isspace(c)) : (((fd_ctype_info(c)) > (0x20)) && ((fd_ctype_info(c)) < (0x23))))
#define fd_isdigit(c) \
   ((c<0x80) ? (isdigit(c)) : ((fd_ctype_info(c)) == FD_NUMERIC_DIGIT_CODE))
#define fd_isalnum(c) \
   ((c<0x80) ? (isalnum(c)) : ((fd_isalpha(c)) || (fd_isdigit(c))))
#define fd_isnumeric(c) ((fd_ctype_info(c)) & (0x40))
#define fd_ispunct(c) \
   ((c<0x80) ? (ispunct(c)) : ((fd_ctype_info(c)) & (0x10)))
#define fd_iscntrl(c) \
   ((c<0x80) ? (iscntrl(c)) : \
    (((fd_ctype_info(c)) & (0x20)) && ((fd_ctype_info(c))>0x22)))
#define fd_isprint(c) \
   ((c<0x80) ? (isprint(c)) : (!(fd_iscntrl(c))))
#define fd_ismodifier(c) \
   ((c>0x80) && ((fd_ctype_info(c)) == (0x56)))

struct FD_UNICODE_DECOMPOSITION {
  unsigned int code; fd_u8char decomp[8];};

DTYPES_EXPORT fd_u8char *fd_decompose_char(unsigned int ch);
DTYPES_EXPORT int fd_base_char(unsigned int ch);
DTYPES_EXPORT int fd_recompose_char(fd_u8char *s);


/* Network access */

typedef struct FD_SERVER {
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif  
  int ref_count, traced; int socket; FILE *in, *out;
  fd_u8char *id; char *servername; int port; 
  void (*closefn)(struct FD_SERVER *);} *fd_server;

typedef struct FD_CLIENT {
  int socket; FILE *in, *out;
  int n_transactions, busy;
  fd_lisp current_expr;
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif
  fd_lisp (*eval_fcn)(fd_lisp,int *,struct FD_CLIENT *);
  void (*close_fcn)(struct FD_CLIENT *);
  void *data; char *id; fd_lisp label;} *fd_client;

DTYPES_EXPORT fd_lisp fd_dtype_eval(fd_lisp expr,fd_server s);
DTYPES_EXPORT fd_lisp fd_careful_dtype_eval(fd_lisp expr,fd_server s);
DTYPES_EXPORT fd_lisp fd_dtcall(fd_server s,char *fcn,...);
DTYPES_EXPORT fd_lisp fd_careful_dtcall(fd_server s,char *fcn,...);
DTYPES_EXPORT int fd_trace_dteval(int flag);

DTYPES_EXPORT void fd_init_connection
  (fd_server server,char *hostname,int port,char *id);
DTYPES_EXPORT fd_server fd_open_connection(char *,int,char *);
DTYPES_EXPORT fd_server fd_new_connection(char *,int,char *);
DTYPES_EXPORT void fd_close_connection(fd_server s);
DTYPES_EXPORT fd_server fd_connect(char *spec);
DTYPES_EXPORT fd_server fd_try_to_connect(char *spec);
DTYPES_EXPORT int fd_get_server_count(void);
DTYPES_EXPORT int fd_get_portno(char *tt_string);
DTYPES_EXPORT int fd_open_tcp_socket(char *,int,char *,int,char **);
DTYPES_EXPORT int fd_open_file_socket(char *,char *,int);
DTYPES_EXPORT char *fd_get_real_hostname(char *hostname);

DTYPES_EXPORT void fd_set_network_timeouts(int,int,int);
DTYPES_EXPORT int fd_timed_recv(int secs,int socket_id,char *data,int len,int flags);
DTYPES_EXPORT int fd_sendall(int socket,char *buf,int size,int flags);
DTYPES_EXPORT void fd_read_from_socket(struct FD_DBUF *buf,int socket);

DTYPES_EXPORT void fd_close_all_connections(void);

/* Internet functions */

DTYPES_EXPORT void fd_send_smtp_mail(char *dest,char *text,fd_lisp fields);
DTYPES_EXPORT char *fd_http_get(char *url,int *sizep);
DTYPES_EXPORT char *fd_http_head(char *url,int *sizep);
DTYPES_EXPORT char *fd_http_string(char *url);

#if FD_THREADS_ENABLED
DTYPES_EXPORT fd_mutex _fd_dns_access_lock;
#endif


/* Exceptional IO */

struct XIO_DATA {
  fd_string_stream stream;
  void (*fcn)(struct XIO_DATA *d);
  void *data;};

DTYPES_EXPORT fd_string_stream fd_get_xio(void);
DTYPES_EXPORT void fd_xio_update(void);
DTYPES_EXPORT void fd_direct_xio
   (fd_string_stream s,void (*f)(struct XIO_DATA *),void *d);


/* Output functions */

DTYPES_EXPORT void fd_disable_notifications(void);
DTYPES_EXPORT void fd_stdout_notifier(fd_u8char *message);
DTYPES_EXPORT void fd_stderr_notifier(fd_u8char *message);
DTYPES_EXPORT void fd_dual_notifier(fd_u8char *message);

DTYPES_EXPORT void fd_set_notify_handler(void (*handle)(fd_u8char *));
DTYPES_EXPORT void fd_set_warn_handler(void (*handle)(fd_u8char *));

DTYPES_EXPORT void fd_printf(fd_string_stream s,char *fstring,...);
DTYPES_EXPORT void fd_xprintf(struct FD_XFILE *f,char *fstring,...);
DTYPES_EXPORT void fd_fprintf(FILE *f,char *fstring,...);

DTYPES_EXPORT void fd_exprintf(char *fstring,...);
DTYPES_EXPORT void fd_notify(char *fstring,...);
DTYPES_EXPORT void fd_warn(char *fstring,...);

DTYPES_EXPORT void fd_add_translation(char *from,char *to);
DTYPES_EXPORT void fd_load_translations(char *file);

#define fd_default_notifier fd_dual_notifier

/* Init stuff */

DTYPES_EXPORT int fd_load_config(char *config_file);
DTYPES_EXPORT void fd_load_user_profile(void);
DTYPES_EXPORT void fd_use_profile(void);
DTYPES_EXPORT void fd_suppress_config(void);
DTYPES_EXPORT char *fd_get_config_file(void);
DTYPES_EXPORT void fd_inhibit_herald(int inhibit);
DTYPES_EXPORT void fd_inhibit_anti_warranty(int inhibit);
DTYPES_EXPORT void fd_show_startup_herald(void);

#if WIN32
#define getppid() (0)
#endif


/* Alpha to Omega */

DTYPES_EXPORT fd_u8char **fd_cmdline(int *n_params,char **argv,int argc);
DTYPES_EXPORT void fd_cmd_args(int *argcp,char ***argvp);
DTYPES_EXPORT int *fd_argcp;
DTYPES_EXPORT char ***fd_argvp;

#if HAVE_SYS_RESOURCE_H
DTYPES_EXPORT void fd_getrusage(struct rusage *);
#endif

DTYPES_EXPORT int fd_normal_exit;

DTYPES_EXPORT WIN32_NORETURN void fd_exit(int status) NORETURN;


#endif /* ndef FRAMERD_OS_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: os.h,v $
   Revision 1.41  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.40  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.39  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.38  2004/04/18 15:30:34  haase
   More and consistent notify handlers

   Revision 1.37  2004/04/16 09:39:51  haase
   Added fd_dual_notifier and made warnings always to to stderr

   Revision 1.36  2004/04/15 22:14:30  haase
   Added flag to control chardata warnings

   Revision 1.35  2004/03/30 21:47:03  haase
   Added fd_cmdline to unify command line processing

   Revision 1.34  2004/03/10 16:25:08  haase
   Added fd_make_absolute_pathname which takes a relative argument

   Revision 1.33  2004/02/16 21:54:06  haase
   Added fd_xgets with UTF8 optimization

   Revision 1.32  2004/02/08 17:01:14  haase
   Renamed fd_open_local_socket to fd_open_file_socket

   Revision 1.31  2004/02/03 16:20:38  haase
   Optimized readline for handling UTF-8, added fd_write_utf8 for transcoding buffers in memory

   Revision 1.30  2004/01/09 17:42:47  haase
   Rename client to fd_client for server code

   Revision 1.29  2003/11/26 14:03:42  haase
   Added fd_new_connection

   Revision 1.28  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.27.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.27.2.2  2003/08/02 13:49:08  haase
   XFILE changes and int/bool getenvs

   Revision 1.27.2.1  2003/01/26 20:31:33  haase
   Spelling error

   Revision 1.27  2002/05/12 13:14:06  haase
   Distinguish exceptions for invalid incoming characters from inexpressible outgoing characters

   Revision 1.26  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.25  2002/04/29 11:20:00  haase
   Removed exported fd_timed_connect --- requires all of FramerD to include sockaddr and there's a better interrace in fd_open_tcp_socket

   Revision 1.24  2002/04/28 20:37:50  haase
   Exported many network functions from libdtypes (timed connect, recv, send, etc) and removed the duplicate functionality from servers.c

   Revision 1.23  2002/04/27 02:48:09  haase
   Added mutexes protecting DNS accesses

   Revision 1.22  2002/04/17 11:46:08  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.21  2002/04/15 18:24:25  haase
   Added primitive for setting and resetting network timeouts

   Revision 1.20  2002/04/10 18:29:41  haase
   Cleaned up declarations

   Revision 1.19  2002/04/04 01:58:24  haase
   Rearranged use of stdio and raw sockets to fix some Solaris/Darwin problems

   Revision 1.18  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
