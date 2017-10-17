/* WIN32 config file, hand coded.
   Copyright (C) 1994-2001 Massachusetts Institute of Technology
   Copyright (C) 2001-2003 beingmeta, inc. (A Delaware Corporation)
   All rights reserved

    Use, modification, and redistribution of this program is permitted
    under the terms of either (at the developer's discretion) the GNU
    General Public License (GPL) Version 2, the GNU Lesser General Public
    License.

    This program is based on the FramerD library released in Fall 2001 by
    MIT under both the GPL and the LGPL licenses, both of which accompany
    this distribution.  Subsequent modifications by beingmeta, inc. are
    also released under both the GPL and LGPL licenses (at the developer's
    discretion).

   $Id: win32-config.h,v 1.11 2005/01/14 16:41:33 haase Exp $ */
/* Define to zero to compile single threaded */
#define FD_THREADS_ENABLED 1

/* Define to zero to compile without readline */
#define FD_WITH_READLINE 1

/* Define if you have the strftime function.  */
#define HAVE_STRFTIME 1

/* Define if you don't have tm_zone but do have the external array
   tzname.  */
#define HAVE_TZNAME 1

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* The number of bytes in a long.  */
#define SIZEOF_LONG 4

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

#define FD_WORDS_ARE_ALIGNED 0

/* Define if you have wide char definitions.  */
#define HAVE_WCHAR_H 1

/* Define if you have fputwc definitions.  */
#define HAVE_FPUTWC 1

/* Define if you have fgetwc definitions.  */
#define HAVE_FGETWC 1

#define HAVE_MKSTEMP 0

#define HAVE_READLINE_READLINE_H 1
#define HAVE_LIBREADLINE 1

/* Define for WIN32 platform.  */
#define FD_OSID "win32"

/* I don't think so... */
#define HAVE_SETUID 0

/* Define if you have the <sys/un.h> header file.  */
#define HAVE_SYS_UN_H 0

/* Define FD_LIGHTWEIGHT_OIDS */
#define FD_LIGHTWEIGHT_OIDS 1

/* I don't think so... */
#define HAVE_RESOURCE_H 0

#define FD_USE_STDIO_WITH_SOCKETS 1

#define FD_MAJOR_VERSION 2
#define FD_MINOR_VERSION 6
#define FD_RELEASE_VERSION 1

#define DEFAULT_FRAMERD_CONFIG "\\Program Files\\FramerD\\framerd.cfg"
#define FRAMERD_SHARE_DIR      "\\Program Files\\FramerD\\"
#define FRAMERD_MODULE_DIR     "\\Program Files\\FramerD\\modules\\"

#include <windows.h>
#include <string.h>
#define strcasecmp stricmp
#define strncasecmp strnicmp

typedef int uid_t;
typedef int gid_t;

/* Currently a noop */
#define sigsetmask(n) 

#define fseeko(f,p,w) fseek(f,p,w)
#define ftello(f) ftell(f)


/* File specific stuff */

/* The CVS log for this file
   $Log: win32-config.h,v $
   Revision 1.11  2005/01/14 16:41:33  haase
   WIN32 2.6 build and install fixes

   Revision 1.10  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.5  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8.2.4  2002/08/25 02:05:37  haase
   Version upgrade

   Revision 1.8.2.3  2002/08/14 18:07:33  haase
   Added release version

   Revision 1.8.2.2  2002/08/13 01:52:14  haase
   Fixes for FD_ macros

   Revision 1.8.2.1  2002/08/09 16:38:59  haase
   Added FD_ prefix to configuration macros

   Revision 1.8  2002/04/28 21:56:25  haase
   Made WIN32 version use lightweight OIDs

   Revision 1.7  2002/04/16 13:16:45  haase
   Renamed FD_USE_STDIO_SOCKETS to clearer FD_USE_STDIO_WITH_SOCKETS

   Revision 1.6  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
