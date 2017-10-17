/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: except.h,v 1.14 2005/01/14 16:48:44 haase Exp $

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

  This is the file except.h supporting structured exception handling
   for ANSI C.  It has been rewritten several times but the first version
   was based on an exception handling system implemented by Jonathan
   Amsterdam.  Like JBA's, it is based on setjmp and longjmp.

*************************************************************************/


#ifndef FRAMERD_EXCEPT_H /* If defined, skip the file */
#define FRAMERD_EXCEPT_H


/* Exception handling interface */

/* We use functions here because exceptions and exception contexts
   may be thread specific */
typedef char *fd_exception;
DTYPES_EXPORT fd_exception fd_theException(void);
DTYPES_EXPORT char *fd_exception_details(void);
DTYPES_EXPORT fd_lisp fd_exception_object(void);
DTYPES_EXPORT struct FD_EXCEPTION_STACK *fd_exception_stack(void);
DTYPES_EXPORT void fd_set_exception
  (fd_exception ex,fd_u8char *details,fd_lisp obj);
DTYPES_EXPORT void fd_pop_exception(void);
DTYPES_EXPORT void fd_clear_exceptions(void);

DTYPES_EXPORT void fd_set_exception_fn(void (*fn)(fd_exception,fd_u8char *,fd_lisp));

DTYPES_EXPORT struct FD_EXCEPTION_CONTEXT *fd_exception_context(int);
DTYPES_EXPORT void fd_exception_context_push(fd_lisp);

struct FD_EXCEPTION_CONTEXT {
  int size, limit; fd_lisp *stack;};

struct FD_EXCEPTION_STACK {
  fd_exception ex; fd_u8char *details;
  struct FD_EXCEPTION_CONTEXT *context;
#if FD_THREADS_ENABLED
  fd_lisp *irritant;
#else
  fd_lisp irritant;
#endif
  struct FD_EXCEPTION_STACK *next;};

typedef struct fd_setjmp_struct {
  jmp_buf jb;
  struct fd_setjmp_struct *next;
  struct fd_setjmp_struct *self;
  }  fd_setjmp_rec;
 
DTYPES_EXPORT void _fd_push_jbr(fd_setjmp_rec *jbr);
DTYPES_EXPORT void _fd_pop_jbr(void);

#if defined(WIN32)
DTYPES_EXPORT WIN32_NORETURN
void fd_raise_exception(fd_exception ex) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_raise_detailed_exception(fd_exception ex,char *details) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_raise_lisp_exception(fd_exception,char *,fd_lisp) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_throw(fd_exception ex,char *details,fd_lisp obj) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN void fd_reraise (void) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_type_error(fd_u8char *details,fd_lisp obj) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_ctype_error(char *context,fd_u8char *details,fd_lisp x) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN
void fd_record_type_error(fd_lisp x,fd_lisp tag) NORETURN ;
DTYPES_EXPORT WIN32_NORETURN void fd_pigs_fly(fd_u8char *details) NORETURN ;
#else
DTYPES_EXPORT
void fd_raise_exception(fd_exception ex) NORETURN ;
DTYPES_EXPORT
void fd_raise_detailed_exception(fd_exception ex,char *details) NORETURN ;
DTYPES_EXPORT
void fd_raise_lisp_exception(fd_exception,char *,fd_lisp) NORETURN ;
DTYPES_EXPORT
void fd_throw(fd_exception ex,char *details,fd_lisp obj) NORETURN ;
DTYPES_EXPORT void fd_reraise (void) NORETURN ;
DTYPES_EXPORT void fd_type_error(fd_u8char *details,fd_lisp x) NORETURN ;
DTYPES_EXPORT
void fd_ctype_error(char *context,fd_u8char *details,fd_lisp x) NORETURN ;
DTYPES_EXPORT void fd_record_type_error(fd_lisp x,fd_lisp tag) NORETURN ;
DTYPES_EXPORT void fd_pigs_fly(fd_u8char *details) NORETURN ;
#endif

#define FD_WITH_HANDLING \
  {fd_setjmp_rec _jbr; \
   if (setjmp(_jbr.jb) == 0) { \
     _fd_push_jbr(&_jbr);

#define FD_ON_EXCEPTION _fd_pop_jbr();} else {
#define FD_END_HANDLING }}
#define FD_UNWIND_PROTECT \
  {fd_setjmp_rec _jbr; \
   fd_exception _ex=fd_theException(); \
   if (setjmp(_jbr.jb) == 0) { \
     _fd_push_jbr(&_jbr);
#define FD_ON_UNWIND _fd_pop_jbr();}
#define FD_END_UNWIND \
   if (fd_theException() != _ex) \
     fd_reraise();}
#define fd_clear_exception() \
   fd_pop_exception()

DTYPES_EXPORT fd_exception
  fd_Out_Of_Memory,
  fd_Out_Of_Bounds,
  fd_Type_Error,
  fd_BadType,
  fd_Unexpected_EOF, 
  fd_Unexpected_EOD,
  fd_Unexpected_NULL,
  fd_NegativeUNGETC,
  fd_InconsistentUNGETC,
  fd_ReallocFailed,
  fd_PigsFly;

#define FD_CHECK_TYPE(v,predicate,msg) \
   if (!(predicate(v))) fd_type_error(msg,v)

#if (FD_SOURCE)
#define WITH_HANDLING FD_WITH_HANDLING
#define ON_EXCEPTION FD_ON_EXCEPTION
#define END_HANDLING FD_END_HANDLING
#define UNWIND_PROTECT FD_UNWIND_PROTECT
#define ON_UNWIND FD_ON_UNWIND
#define END_UNWIND FD_END_UNWIND
#endif

#endif /* FRAMERD_EXCEPT_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: except.h,v $
   Revision 1.14  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.13  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.12  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.11  2004/05/03 20:21:07  haase
   Moved dbuf i/o functions to header file

   Revision 1.10  2003/10/01 09:02:11  haase
   Added exception preface function

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8.2.1  2003/01/26 20:32:12  haase
   Introduced zstrings

   Revision 1.8  2002/04/08 16:37:18  haase
   Added fd_pigs_fly error signaller

   Revision 1.7  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
