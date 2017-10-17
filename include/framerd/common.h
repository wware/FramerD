/* -*- C -*-

   common.h: common types, structs, and macros for FramerD libraries

   Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.
   Copyright (C) 1994-2001 Massachusetts Institute of Technology
   Copyright (C) 2001-2005 beingmeta, inc. (A Delaware Corporation)

   $Id: common.h,v 1.41 2005/01/14 16:48:44 haase Exp $

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
  

*************************************************************************/

#ifndef FRAMERD_COMMON_H /* If defined, skip the file */
#define FRAMERD_COMMON_H

#if WIN32
#include "win32-config.h"
#else
#include "config.h"
#endif

#ifdef WIN32
#define WIN32_NORETURN __declspec(noreturn)
#else
#define WIN32_NORETURN
#endif

#ifndef FD_LIGHTWEIGHT_OIDS
#define FD_LIGHTWEIGHT_OIDS 0
#endif

/* Do we need to be reentrant? */
#if (((FD_THREADS_ENABLED) && (HAVE_PTHREAD_H) && (HAVE_LIBPTHREAD)) || defined(WIN32))
#define _REENTRANT 1
#endif


/* Declarations for import/export */
#if ((defined(WIN32)) && (!(defined(STATICLINK))))
#define EXPORTED __declspec(dllexport)
#define IMPORTED extern __declspec(dllimport)
#ifndef DTYPES_EXPORT
#define DTYPES_EXPORT extern __declspec(dllimport)
#endif
#ifndef FRAMERD_EXPORT
#define FRAMERD_EXPORT extern __declspec(dllimport)
#endif
#ifndef FDSCRIPT_EXPORT
#define FDSCRIPT_EXPORT extern __declspec(dllimport)
#endif 
#else /* not WIN32 */
#define EXPORTED
#define IMPORTED extern 
#ifndef DTYPES_EXPORT
#define DTYPES_EXPORT extern
#endif
#ifndef FRAMERD_EXPORT
#define FRAMERD_EXPORT extern
#endif
#ifndef FDSCRIPT_EXPORT
#define FDSCRIPT_EXPORT extern
#endif
#endif

/* Definitions for static and inline declarations */
#if defined(__GNUC__)
#define STATIC_INLINE static inline
#define NORETURN __attribute__ ((noreturn))
#define UNUSED __attribute__ ((unused))
#elif defined(WIN32)
#define STATIC_INLINE static __inline
#define NORETURN
#define UNUSED
#else
#define STATIC_INLINE static inline
#define NORETURN
#define UNUSED
#endif

#if defined(DONT_INLINE_FASTOPS)
#define FASTOP static 
#else
#define FASTOP STATIC_INLINE
#endif

#ifndef DTYPES_SOURCE
#define DTYPES_SOURCE 0
#endif
#ifndef FRAMERD_SOURCE
#define FRAMERD_SOURCE 0
#endif
#ifndef FDSCRIPT_SOURCE
#define FDSCRIPT_SOURCE 0
#endif

#ifndef FD_SOURCE
#if ((DTYPES_SOURCE) || (FRAMERD_SOURCE) || (FDSCRIPT_SOURCE))
#define FD_SOURCE 1
#else
#define FD_SOURCE 0
#endif
#endif

#ifndef FD_INLINE_STRING_STREAMS
#define FD_INLINE_STRING_STREAMS FD_SOURCE
#endif

#ifndef FD_INLINE_FDMALLOC
#define FD_INLINE_FDMALLOC FD_SOURCE
#endif

#ifndef FD_LOG_MALLOC
#define FD_LOG_MALLOC 0
#endif


/* Defaults */

#ifndef FD_USE_STDIO_WITH_SOCKETS
#define FD_USE_STDIO_WITH_SOCKETS 1
#endif

#ifndef FD_DEFAULT_CHARSET
#define FD_DEFAULT_CHARSET "iso-latin1"
#endif

#ifndef FD_OSID
#define FD_OSID "unknown-unknown-unknown"
#endif

#if (!(defined(FD_BIGBUFF_DEFAULT)))
#if (defined(WIN32)) /* Assume small platform */
#define FD_BIGBUFF_DEFAULT 0
#else
#define FD_BIGBUFF_DEFAULT 512*1024
#endif
#endif

#ifndef FD_ISSUE_LOCALE_WARNING
#define FD_ISSUE_LOCALE_WARNING 0
#endif

#ifndef FD_CODDLE_MALLOC
#define FD_CODDLE_MALLOC 1
#endif

#ifndef FD_STICKY_SLOTMAPS
#define FD_STICKY_SLOTMAPS 0
#endif

#ifndef FD_SLOTMAP_DELTA
#define FD_SLOTMAP_DELTA 4
#endif

#ifndef FD_SHARED_LIB_SUFFIX
#define FD_SHARED_LIB_SUFFIX ".so"
#endif

#ifndef FD_LARGEFILES_ENABLED
#define FD_LARGEFILES_ENABLED 1
#endif

#if FD_LARGEFILES_ENABLED
#if HAVE_FSEEKO
#define _FILE_OFFSET_BITS 64
#define _LARGEFILE_SOURCE 1
#define _LARGEFILE64_SOURCE 1
#else
#undef FD_LARGEFILES_ENABLED
#define FD_LARGEFILES_ENABLED
#endif
#else
#define fseeko(f,pos,whence) fseek(f,pos,whence)
#define ftello(f) ftell(f)
#endif


/* Standard includes */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include <errno.h>
#include <time.h>
#include <assert.h>

#ifndef FD_WITH_FILE_LOCKING
#define FD_WITH_FILE_LOCKING 1
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if ((FD_WITH_DMALLOC) && (HAVE_DMALLOC_H) && (HAVE_LIBDMALLOC))
#include <dmalloc.h>
#endif

#ifndef NUL
#define NUL '\0'
#endif

/* Declarations of note */

typedef int fd_refcount; /* Tracks references */

#if (!(HAVE_WCHAR_H))
#define iswspace isspace
#define iswalnum isalnum
#define iswpunct ispunct
#define iswdigit isdigit
#define iswalpha isupper
#define iswupper islower
#define iswlower islower
#define towupper toupper
#define towlower tolower
#endif

#if (!(HAVE_UCHAR))
typedef unsigned char uchar;
#endif

typedef uchar fd_u8char;

/* Translation */

#if HAVE_LIBINTL_H
#include <libintl.h>
#define _(x) (x)
#define fd_gettext(x) gettext(x)
#else
#define _(x) (x)
#define fd_gettext(x) (x)
#define textdomain(x)
#endif

/* Source file registry */

DTYPES_EXPORT void fd_register_source_file(char *module,char *date,char *details);
DTYPES_EXPORT int fd_source_file_registeredp(char *name);


/* Handling errno */

DTYPES_EXPORT int _fd_clear_errno(void);
#define FD_CLEAR_ERR() _fd_clear_errno()

#define FD_BENIGN_ERRNOP(n) (((n) == EINTR) || ((n) == EINVAL))
#if FD_ERRNO_CHECKING_ENABLED
#define FD_TIDY_ERRNO(msg) \
   if (errno) {if (!(FD_BENIGN_ERRNOP(errno))) perror(msg); FD_CLEAR_ERR();}
#else
#define FD_TIDY_ERRNO(msg) \
   if (errno) {FD_CLEAR_ERR();}
#endif

/* Default for FD_WITH_MMAP_MALLOC */

#ifndef FD_WITH_MMAP_MALLOC
#if ((HAVE_MMAP) && (HAVE_UNISTD_H))
#define FD_WITH_MMAP_MALLOC 1
#else
#define FD_WITH_MMAP_MALLOC 0
#endif
#endif

/* Checking up on READLINE */

#if ((FD_WITH_READLINE) && (defined(HAVE_LIBREADLINE)) && \
     (HAVE_READLINE_READLINE_H))
#define USING_READLINE 1
#else
#define USING_READLINE 0
#endif

/* Thread support */

#if defined(WIN32)
#define FD_THREADS_ENABLED 1
typedef HANDLE pthread_mutex_t;
typedef HANDLE fd_mutex;
typedef HANDLE pthread_cond_t;
typedef HANDLE fd_condvar;
typedef HANDLE fd_rwlock;

#define pthread_attr_default NULL
#define pthread_mutex_lock(x)  (WaitForSingleObject((*(x)),INFINITE))
#define pthread_mutex_unlock(x) ReleaseMutex(*(x))
#define pthread_mutex_init(_mloc,attr) (*(_mloc))=CreateMutex(NULL,FALSE,NULL)
#define pthread_mutex_destroy(_mloc) CloseHandle((*(_mloc)))

#define pthread_cond_init(_cloc,attr) (*(_cloc))=CreateEvent(NULL,FALSE,FALSE,NULL)
#define pthread_cond_signal(_x)  (SetEvent((*(_x))))
#define pthread_cond_broadcast(_x)  (SetEvent((*(_x))))
#define pthread_cond_wait(_c,_m)  (ReleaseMutex(*(_m)),(WaitForSingleObject((*(_c)),INFINITE)))

#define pthread_join(_thread,_ignored) WaitForSingleObject(_thread,INFINITE)

typedef HANDLE pthread_t;

static DWORD _thread_tmp;

#define pthread_create(ptid,ignored,fcn,arg) \
   (*(ptid))=CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)fcn,arg,0,&_thread_tmp)

typedef DWORD fd_tld_key;
#define fd_new_tld_key(lvalue,ignored) (*(lvalue))=TlsAlloc();
#define fd_tld_get(key) TlsGetValue(key)
#define fd_tld_set(key,v) TlsSetValue(key,v)

#define fd_init_mutex(_mloc) \
   (*(_mloc))=CreateMutex(NULL,FALSE,NULL)
#define fd_init_rwlock(_mloc) \
   (*(_mloc))=CreateMutex(NULL,FALSE,NULL)
#define fd_destroy_mutex(x) pthread_mutex_destroy(x)
#define fd_destroy_rwlock(x) pthread_mutex_destroy(x)
#define fd_lock_mutex(x) pthread_mutex_lock((x))
#define fd_unlock_mutex(x) pthread_mutex_unlock((x))
#define fd_read_lock(x) pthread_mutex_lock((x))
#define fd_read_unlock(x) pthread_mutex_unlock((x))
#define fd_write_lock(x) pthread_mutex_lock((x))
#define fd_write_unlock(x) pthread_mutex_unlock((x))
#elif ((FD_THREADS_ENABLED) && (HAVE_PTHREAD_H) && ((HAVE_LIBPTHREAD) || (HAVE_LIBC_R)))
#define FD_THREADS_ENABLED 1
#include <pthread.h>
typedef pthread_mutex_t fd_mutex;
typedef pthread_cond_t fd_condvar;
typedef pthread_mutex_t fd_rwlock;
typedef pthread_key_t fd_tld_key;
#define pthread_attr_default NULL
#define pthread_mutexattr_default NULL
#define fd_init_mutex(x) pthread_mutex_init(x,pthread_mutexattr_default)
#define fd_destroy_mutex(x) pthread_mutex_destroy(x)
#define fd_init_rwlock(x) pthread_mutex_init(x,pthread_mutexattr_default)
#define fd_destroy_rwlock(x) pthread_mutex_destroy(x)
#define fd_new_tld_key(key_loc,del_fcn) pthread_key_create(key_loc,del_fcn)

#define fd_lock_mutex(x) pthread_mutex_lock((x))
#define fd_unlock_mutex(x) pthread_mutex_unlock((x))
#define fd_read_lock(x) pthread_mutex_lock((x))
#define fd_read_unlock(x) pthread_mutex_unlock((x))
#define fd_write_lock(x) pthread_mutex_lock((x))
#define fd_write_unlock(x) pthread_mutex_unlock((x))
#define fd_tld_get(key) pthread_getspecific(key)
#define fd_tld_set(key,v) pthread_setspecific(key,v)
#else
#define FD_THREADS_ENABLED 0
#define fd_init_mutex(x)
#define fd_init_rwlock(x)
#define fd_destroy_mutex(x)
#define fd_destroy_rwlock(x)
#define fd_new_tld_key(key_loc,del_fcn)

#define fd_lock_mutex(x) 
#define fd_unlock_mutex(x) 
#define fd_read_lock(x)
#define fd_read_unlock(x)
#define fd_write_lock(x)
#define fd_write_unlock(x)
#endif

#if (!(FD_THREADS_ENABLED))
#define FD_WITH_MUTEX_LOCKED(x)
#define FD_END_WITH_MUTEX_LOCKED(x)
#else
#define FD_WITH_MUTEX_LOCKED(x) \
   FD_UNWIND_PROTECT { \
     fd_lock_mutex(x);
#define FD_END_WITH_MUTEX_LOCKED(x) \
		  } \
  FD_ON_UNWIND {fd_unlock_mutex(x);} FD_END_UNWIND
#endif

#if (FD_SOURCE)
#define lock_mutex fd_lock_mutex
#define unlock_mutex fd_unlock_mutex
#define WITH_MUTEX_LOCKED FD_WITH_MUTEX_LOCKED 
#define END_WITH_MUTEX_LOCKED FD_END_WITH_MUTEX_LOCKED 
#endif

/* Lisp structures and macros */

#include "framerd/lisp.h"

/* Exception handling */

#include "framerd/except.h"

/* fdmalloc */

#include "framerd/fdmalloc.h"

/* OIDs */

#include "framerd/oids.h"

/* Dynamic Linking */

#if defined(WIN32)
#define FD_USING_DLLS 1
#elif ((FD_DYNAMIC_LINKING_ENABLED) && (HAVE_DLFCN_H))
#define FD_USING_DLLS 1
#else
#define FD_USING_DLLS 0
#endif

/* Memory output stream */

typedef struct FD_DBUF {unsigned char *start, *ptr, *end;} FD_DBUF;

/* String streams */

#include "framerd/strstream.h"

/* Unicode streams */

typedef unsigned short fd_unichar_t;

#if (FD_SOURCE)
#define unichar_t fd_unichar_t
#endif

/* Binary I/O functions */

#include "framerd/binio.h"

/* Kinds of providers */

enum FD_POOL_TYPE { file_pool, network_pool};
enum FD_INDEX_TYPE { file_index, network_index, compound_index };

/* Miscellaneous macros */

#define FD_DOTIMES(ivar,limit) \
  int ivar=0, _limit=limit; for (;ivar<_limit;ivar++)

/* Should probably be a typedef */ 
#define fd_exit_proc_type void (*)(void)

/* For default procedures which return lisp objects */
typedef fd_lisp (*fd_lisp_default_fn)(void);

#if (FD_SOURCE)
typedef fd_u8char u8char;
typedef fd_lisp_default_fn lisp_default_fn;
#define DOTIMES FD_DOTIMES
#define CLEAR_ERR FD_CLEAR_ERR
#define TIDY_ERRNO FD_TIDY_ERRNO
#endif

#endif /* ndef FRAMERD_COMMON_H */





/* File specific stuff */

/* The CVS log for this file
   $Log: common.h,v $
   Revision 1.41  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.40  2004/10/18 15:24:58  haase
   Added compound indices

   Revision 1.39  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.38  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.37  2004/07/19 16:57:08  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.36  2004/07/19 14:40:27  haase
   Check for off_t explicitly

   Revision 1.35  2004/07/19 07:26:36  haase
   Fix non largefiles definition of off_t to be long

   Revision 1.34  2004/07/18 14:22:19  haase
   Added option for disabling largefile support

   Revision 1.33  2004/07/16 14:34:25  haase
   Added configuration option for largefiles

   Revision 1.32  2004/07/16 13:37:21  haase
   Made file pools use off_t

   Revision 1.31  2003/12/18 03:37:55  haase
   Added defs for shared library suffix

   Revision 1.30  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.29  2003/12/03 10:59:47  haase
   Moved to using datestamp.h rather than datestamp

   Revision 1.28  2003/10/20 12:08:50  haase
   Interface renaming (mostly) for malloc logging

   Revision 1.27  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.26  2003/09/30 19:07:48  haase
   Added conditionals for custom mutex attributes

   Revision 1.25  2003/09/10 17:58:28  haase
   Made slotmaps grow linearly in size

   Revision 1.24  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.23.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.23.2.2  2002/08/13 01:52:13  haase
   Fixes for FD_ macros

   Revision 1.23.2.1  2002/08/09 16:38:59  haase
   Added FD_ prefix to configuration macros

   Revision 1.23  2002/07/03 06:04:21  haase
   Added a C-level debugging feature for the GC where some slotmaps can be
   declared "sticky" meaning that an error is signalled when the are GCd.  This
   sticky bit is set whenever a slotmap is stored under an OID and cleared
   by the procedures for swapping out OIDs.

   Revision 1.22  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.21  2002/06/10 00:29:24  haase
   Add other thread library tests, but suppress them for BSD (for now)

   Revision 1.20  2002/06/09 18:02:48  haase
   Fixes for WIN32

   Revision 1.19  2002/05/27 18:16:32  haase
   Added abstraction layer for thread-local data

   Revision 1.18  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.17  2002/04/28 20:37:50  haase
   Exported many network functions from libdtypes (timed connect, recv, send, etc) and removed the duplicate functionality from servers.c

   Revision 1.16  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.15  2002/04/27 03:39:32  haase
   More changes for furthre use of rwlock s

   Revision 1.14  2002/04/26 03:55:41  haase
   Various network code fixes for WIN32

   Revision 1.13  2002/04/19 19:30:59  haase
   Added framework for read/write locks on hashtables

   Revision 1.12  2002/04/16 13:16:45  haase
   Renamed FD_USE_STDIO_SOCKETS to clearer FD_USE_STDIO_WITH_SOCKETS

   Revision 1.11  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
