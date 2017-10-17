/* C Mode */

/* except.c
    A primitive but general-purpose exception-handling system for C.
    Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.
    Based in part by a design presented in an article in Byte Magazine
     by Jonathan Amsterdam

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

static char vcid[] = "$Id: except.c,v 1.9 2004/07/19 16:57:14 haase Exp $";

/** Declarations **/
/** Getting exception data (threaded) **/
/** Setting exception data (threaded) **/
/** Exception contexts (threaded) **/
/** Initialization (threaded) **/
/** Declarations (unthreaded) **/
/** Setting exception data (unthreaded) **/
/** Initialization (unthreaded) **/
/** Unhandled exceptions (shared) **/
/** Raising exceptions (various forms, shared) **/
/** fd_throw: non-local exits (using exceptions) **/

#include <stddef.h>
#include "dtypes.h"

/* This exception system uses setjmp and longjmp to manage non-local exits,
   typically signalling errors and special conditions.  This file contains
   the signalling side of things; most of the handling is done by the
   macros in except.h */

/** Declarations **/

/* the type fd_exception is a string (pointer to an array of characters) */
/* unhandled_exception is called when an exception has no handlers */

/* fd_theException() returns the currently active exception */
/* fd_exception_details() returns additional information as a string */
/* _fd_push_jbr pushes a context for exception interception */
/* _fd_pop_jbr goes out one context for exception interception */
/* fd_raise_exception signals an error */
/* fd_raise_detailed_exception signals an error and binds 
     a string of supporting details */
/* fd_raise_lisp_exception signals an error and binds 
     a string of supporting details, and a lisp object */
/* fd_reraise raises the current exception again */

static void unhandled_exception(fd_exception ex,char *details,fd_lisp obj) NORETURN;

static void ((*exception_fn)(fd_exception,fd_u8char *,fd_lisp))=NULL;

DTYPES_EXPORT void fd_set_exception_fn(void (*fn)(fd_exception,fd_u8char *,fd_lisp))
{
  exception_fn=fn;
}

static struct FD_EXCEPTION_CONTEXT *make_exception_context()
{
  struct FD_EXCEPTION_CONTEXT *ec=
    fd_malloc(sizeof(struct FD_EXCEPTION_CONTEXT));
  ec->size=0; ec->limit=8;
  ec->stack=fd_malloc(sizeof(lisp)*8);
  return ec;
}

static void free_exception_context(struct FD_EXCEPTION_CONTEXT *ec)
{
  fd_free(ec->stack,sizeof(lisp)*ec->limit);
  fd_free(ec,sizeof(struct FD_EXCEPTION_CONTEXT));
}

#if FD_THREADS_ENABLED
static fd_tld_key exception_stack_key;
static fd_tld_key current_jbr_key;
fd_tld_key fd_exception_key, fd_exception_details_key;
fd_tld_key fd_exception_object_key, fd_exception_context_key;

/** Getting exception data (threaded) **/

DTYPES_EXPORT
/* fd_theException
    Arguments: none
    Returns: an exception (a string)
    Returns the current exception for the current thread. */
fd_exception fd_theException()
{
  fd_exception ex;
  ex=fd_tld_get(fd_exception_key);
  return ex;
}

DTYPES_EXPORT
/* fd_exception_details
    Arguments: none
    Returns: a string
    Returns the 'details' of the current exception for the current thread. */
char *fd_exception_details()
{
  char *d;
  d=fd_tld_get(fd_exception_details_key);
  if (d) return d; else return "";
}

DTYPES_EXPORT
/* fd_exception_object
    Arguments: none
    Returns: a lisp object
    Returns the 'irritant' lisp object of the current exception
     for the current thread. */
lisp fd_exception_object()
{
  lisp *d=fd_tld_get(fd_exception_object_key);
  if (d) return *d; else return FD_VOID;
}

DTYPES_EXPORT
/* fd_exception_context
    Arguments: none
    Returns: a pointer to a FD_EXCEPTION_CONTEXT struct
*/
struct FD_EXCEPTION_CONTEXT *fd_exception_context(int force)
{
  struct FD_EXCEPTION_CONTEXT *d=fd_tld_get(fd_exception_context_key);
  if (d) return d;
  else if (force) {
    d=make_exception_context();
    fd_tld_set(fd_exception_context_key,d);
    return d;}
  else return NULL;
}

DTYPES_EXPORT
/* fd_exception_stack
    Arguments: none
    Returns: a pointer to an exception stack structure
 Note that this does not include the current exception.
*/
struct FD_EXCEPTION_STACK *fd_exception_stack()
{
  return fd_tld_get(exception_stack_key);
}

/** Setting exception data (threaded) **/

DTYPES_EXPORT
/* fd_set_exception
    Arguments: an exception (a string), a details string, and a lisp object
    Returns: nothing
    Set's the current threads exception information. */
void fd_set_exception(fd_exception ex,fd_u8char *details,lisp object)
{
  fd_exception last_ex=fd_tld_get(fd_exception_key);
  fd_u8char *new_details;
  if (last_ex) {
    struct FD_EXCEPTION_STACK *s=fd_malloc(sizeof(struct FD_EXCEPTION_STACK));
    s->ex       =fd_tld_get(fd_exception_key);
    s->context  =fd_tld_get(fd_exception_context_key);
    s->details  =fd_tld_get(fd_exception_details_key);
    s->irritant =fd_tld_get(fd_exception_object_key);
    s->next     =fd_tld_get(exception_stack_key);
    fd_tld_set(exception_stack_key,s);}
  fd_tld_set(fd_exception_key,ex);
  if (details) {
    new_details=fd_xmalloc(strlen(details)+1);
    strcpy(new_details,details);}
  else new_details=NULL;
  fd_tld_set(fd_exception_details_key,new_details);
  fd_tld_set(fd_exception_context_key,NULL);
  if (FD_VOIDP(object)) 
    fd_tld_set(fd_exception_object_key,NULL);
  else {
    fd_lisp *object_ptr=fd_malloc(sizeof(lisp)); *object_ptr=incref(object);
    fd_tld_set(fd_exception_object_key,object_ptr);}
}

DTYPES_EXPORT
/* fd_pop_exception
    Arguments: Clears the exception arguments, restoring any stored
     exception state
    Returns: nothing
    Set's the current threads exception information. */
void fd_pop_exception()
{
  struct FD_EXCEPTION_CONTEXT *c=
    fd_tld_get(fd_exception_context_key);
  struct FD_EXCEPTION_STACK *s=fd_tld_get(exception_stack_key);
  fd_exception ex=fd_tld_get(fd_exception_key);
  fd_u8char *details   = fd_tld_get(fd_exception_details_key);
  fd_lisp *irritant = fd_tld_get(fd_exception_object_key);
  if (details) fd_xfree(details);
  if (irritant) {
    decref(*irritant); fd_free(irritant,sizeof(fd_lisp));}
  if (c) free_exception_context(c);
  if (s) {
    fd_tld_set(fd_exception_key,s->ex);
    fd_tld_set(fd_exception_context_key,s->context);
    fd_tld_set(fd_exception_details_key,s->details);
    fd_tld_set(fd_exception_object_key,s->irritant);
    fd_tld_set(exception_stack_key,s->next);
    fd_free(s,sizeof(struct FD_EXCEPTION_STACK));}
  else {
    fd_tld_set(fd_exception_key,NULL);
    fd_tld_set(fd_exception_context_key,NULL);
    fd_tld_set(fd_exception_details_key,NULL);
    fd_tld_set(fd_exception_object_key,NULL);
    fd_tld_set(exception_stack_key,NULL);}
}

/** Exception contexts (threaded) **/

/* Gets the current exception context (a jmpbuf ptr) */
FASTOP fd_setjmp_rec *get_jbr()
{
  return fd_tld_get(current_jbr_key);
}

DTYPES_EXPORT
/* _fd_push_jbr:
     Arguments: a setjmp record
     Returns: void
  Adds an entry to the exception handling stack for a particular
  setmp location */
void _fd_push_jbr(fd_setjmp_rec *jbr)
{
  jbr->next=fd_tld_get(current_jbr_key);
  jbr->self = jbr;
  fd_tld_set(current_jbr_key,(void *)jbr);
}
 
DTYPES_EXPORT
/* _fd_pop_jbr:
     Arguments: a setjmp record
     Returns: void
  Removes the most recent entry from the exception handling stack */
void _fd_pop_jbr()
{
  char *error = "Attempt to pop empty exception stack!";
  fd_setjmp_rec *cur_rec;
  cur_rec=fd_tld_get(current_jbr_key);
  if (cur_rec == NULL)
    unhandled_exception(error,NULL,FD_VOID);
  else fd_tld_set(current_jbr_key,cur_rec->next);
}
 
/** Initialization (threaded) **/

void fd_initialize_except_c()
{
  fd_new_tld_key(&fd_exception_key,NULL);
  fd_new_tld_key(&fd_exception_details_key,NULL);
  fd_new_tld_key(&fd_exception_object_key,NULL);
  fd_new_tld_key(&fd_exception_context_key,NULL);
  fd_new_tld_key(&exception_stack_key,NULL);
  fd_new_tld_key(&current_jbr_key,NULL);
}
#else
/** Getting exception data (unthreaded) **/

/* This is bound when exceptions are signaled. */
static fd_exception exception = NULL;
/* This is the current exception context */
static fd_setjmp_rec *cur_rec = NULL;
 
#define get_jbr() cur_rec

/* Extra information for more useful error messages */
static char *exception_details=NULL;

/* exception object */
static lisp exception_object;
struct FD_EXCEPTION_CONTEXT *exception_context;

/* exception stack */
static struct FD_EXCEPTION_STACK *exception_stack=NULL;

DTYPES_EXPORT fd_exception fd_theException()
{
  return exception;
}

DTYPES_EXPORT char *fd_exception_details()
{
  if (exception_details) return exception_details;
  else return "";
}

DTYPES_EXPORT lisp fd_exception_object()
{
  return exception_object;
}

DTYPES_EXPORT struct FD_EXCEPTION_CONTEXT *fd_exception_context(int force)
{
  if (exception_context) return exception_context;
  else if (force) {
    exception_context=make_exception_context();
    return exception_context;}
  else return NULL;
}

DTYPES_EXPORT struct FD_EXCEPTION_STACK *fd_exception_stack()
{
  return exception_stack;
}

/** Setting exception data (unthreaded) **/

DTYPES_EXPORT void
fd_set_exception(fd_exception ex,fd_u8char *details,lisp object)
{
  char *new_details;
  if (exception) {
    struct FD_EXCEPTION_STACK *s=fd_malloc(sizeof(struct FD_EXCEPTION_STACK));
    s->ex=exception;
    s->details=exception_details;
    s->irritant=exception_object;
    s->context=exception_context;
    s->next=exception_stack; exception_stack=s;}
  if (details) {
    new_details=fd_xmalloc(strlen(details)+1);
    strcpy(new_details,details);}
  else new_details=NULL;
  exception=ex;
  exception_context=NULL;
  exception_object=incref(object);
  exception_details=new_details;
}

DTYPES_EXPORT
/* fd_pop_exception
    Arguments: Clears the exception arguments, restoring any stored
     exception state
    Returns: nothing
    Set's the current threads exception information. */
void fd_pop_exception()
{
  struct FD_EXCEPTION_STACK *s=exception_stack;
  if (exception_details) fd_xfree(exception_details);
  if (exception_context) free_exception_context(exception_context);
  decref(exception_object);
  if (exception_stack) {
    exception=s->ex;
    exception_context=s->context;
    exception_details=s->details;
    exception_object=s->irritant;
    exception_stack=s->next;
    fd_free(s,sizeof(struct FD_EXCEPTION_STACK));}
  else {
    exception=NULL; exception_context=NULL;
    exception_details=NULL; exception_object=FD_VOID;}
}

/* Pushes an exception context */
DTYPES_EXPORT
void _fd_push_jbr(fd_setjmp_rec *jbr)
{
  jbr->next = cur_rec;
  jbr->self = jbr;
  cur_rec   = jbr;
}
 
/* Pops an exception context */
DTYPES_EXPORT
void _fd_pop_jbr()
{
  char *error = "Attempt to pop empty exception stack!";
  if (cur_rec == NULL) 
    unhandled_exception(error,NULL,FD_VOID);
  else cur_rec=cur_rec->next;
}
 
/** Initialization (unthreaded) **/

void fd_initialize_except_c()
{
  exception_object=FD_VOID;
}
#endif

/** Unhandled exceptions (shared) **/

static int aborting=0;

static void unhandled_exception(fd_exception ex,char *details,lisp irritant)
{
  if (errno) perror("System Error");
  if (details)
    fprintf(stderr,_("Unhandled exception: %s (%s)"),fd_gettext(ex),details); 
  else fprintf(stderr,_("Unhandled exception: %s"),fd_gettext(ex)); 
  fd_close_all_connections();
  if (aborting == 0) {
    aborting=1;
    if (!(FD_VOIDP(irritant))) {
      fprintf(stderr," -- ");
      fd_print_lisp(irritant,stderr);
      fprintf(stderr,"\n"); fflush(stderr);}
    else fprintf(stderr,"\n");
    fprintf(stderr,
	    "For more details, try setting the Scheme variable %%DEBUG or the shell variable DEBUG_FDSCRIPT\n");}
  exit(-1);
}

/** Raising exceptions (various forms, shared) **/

DTYPES_EXPORT
/* fd_raise_exception:
    Arguments: an exception (a string)
    Returns: no.
  Raises an exception of a particular kind without providing any
other information.
  Unhandled crises cause an exit by calling unhandled_exception. */
void fd_raise_exception(fd_exception ex)
{
  fd_setjmp_rec *jbr=get_jbr();
  if (jbr == NULL) unhandled_exception(ex,NULL,FD_VOID);
  else {
    if (exception_fn) exception_fn(ex,NULL,FD_VOID);
    fd_set_exception(ex,NULL,FD_VOID);
    if (jbr->self != jbr) {
      unhandled_exception(_("Corrupted exception stack!"),NULL,FD_VOID);}
    else {_fd_pop_jbr(); longjmp(jbr->jb,1);};
  };
}
 
DTYPES_EXPORT
/* fd_raise_detailed_exception:
    Arguments: an exception (a string) and details (another string)
    Returns: no.
  Raises an exception of a particular kind with particular
details used in generating exception reports
  Unhandled crises cause an exit by calling unhandled_exception. */
void fd_raise_detailed_exception(fd_exception ex,char *details)
{
  fd_setjmp_rec *jbr=get_jbr();
  if (jbr == NULL) unhandled_exception(ex,details,FD_VOID);
  else {
    if (exception_fn) exception_fn(ex,details,FD_VOID);
    fd_set_exception(ex,details,FD_VOID);
    if (jbr->self != jbr) {
      unhandled_exception(_("Corrupted exception stack!"),NULL,FD_VOID);}
    else {_fd_pop_jbr(); longjmp(jbr->jb,1);};
  };
}
 
DTYPES_EXPORT
/* fd_raise_lisp_exception:
    Arguments: an exception (a string), details (another string), 
               and a lisp object
    Returns: no.
  Raises an exception of a particular kind with particular
details used in generating exception reports and an associated
lisp object (the irritant)
  Unhandled crises cause an exit by calling unhandled_exception. */
void fd_raise_lisp_exception(fd_exception ex,char *details,lisp obj)
{
  fd_setjmp_rec *jbr=get_jbr();
  if (jbr == NULL) unhandled_exception(ex,details,obj);
  else {
    if (exception_fn) exception_fn(ex,details,obj);
    fd_set_exception(ex,details,obj);
    if (jbr->self != jbr) {
      unhandled_exception(_("Corrupted exception stack!"),NULL,FD_VOID);}
    else {_fd_pop_jbr(); longjmp(jbr->jb,1);};
  };
}
 
DTYPES_EXPORT
/* fd_exception_context_push
     Arguments: a lisp pointer
     Returns: void
  Adds a value to the current exception context */
void fd_exception_context_push(lisp sym)
{
  int force=1;
  struct FD_EXCEPTION_CONTEXT *ec=fd_exception_context(force);
  if (ec == NULL) return;
  if (ec->size+1 == ec->limit) {
    ec->stack=fd_realloc(ec->stack,sizeof(lisp)*ec->limit*2,
			 sizeof(lisp)*ec->limit);
    ec->limit=ec->limit*2;}
  ec->stack[ec->size]=sym; ec->size++;
}

DTYPES_EXPORT
/* fd_clear_exceptions
    Arguments: Clears the exception arguments and the exception
     stack
    Returns: nothing
  Clears the exception stack and the current exception. */
void fd_clear_exceptions()
{
  while (fd_exception_stack()) fd_pop_exception();
  fd_pop_exception();
}

DTYPES_EXPORT
/* fd_type_error:
    Arguments: details text (a string), and a lisp object
    Returns: no.
  Raises a type error with particular details and an object. */
void fd_type_error(fd_u8char *details,lisp obj)
{
  fd_raise_lisp_exception(fd_Type_Error,details,obj);
}
 
DTYPES_EXPORT
/* fd_record_type_error:
    Arguments: details text (a string), and a lisp object
    Returns: no.
  Raises a type error with particular details and an object. */
void fd_record_type_error(lisp obj,lisp tag)
{
  fd_raise_lisp_exception(fd_Type_Error,fd_object_to_string(tag),obj);
}
 
DTYPES_EXPORT
/* fd_ctype_error:
    Arguments: details text (a string), and a lisp object
    Returns: no.
  Raises a type error with particular details and an object. */
void fd_ctype_error(char *c_context,fd_u8char *details,lisp obj)
{
  fd_u8char *xlated=fd_gettext(details);
  fd_u8char *in_context=fd_malloc(strlen(xlated)+strlen(c_context)+5);
  sprintf(in_context,"%s- %s",c_context,xlated);
  fd_raise_lisp_exception(fd_Type_Error,in_context,obj);
}
 
fd_exception fd_PigsFly=_("Pigs have flown!");

DTYPES_EXPORT
/* fd_pigs_fly:
    Arguments: details text (a string)
    Returns: no.
  This is used to signal errors which should never happen. */
void fd_pigs_fly(fd_u8char *details)
{
  fd_raise_detailed_exception(fd_PigsFly,details);
}

/** fd_throw: non-local exits (using exceptions) **/

DTYPES_EXPORT
/* fd_throw:
    Arguments: an exception (a string), details (another string), 
               and a lisp object
    Returns: no.
Throws an exception, with particular details and irritant.
   This is used when the exception is expected to be caught, as
   in the use of exceptions to implement continuation.  This is
   identical to fd_raise_lisp_exception but that debuggers may often
   break on fd_raise_lisp_exception and not on fd_throw. */
void fd_throw(fd_exception ex,char *details,lisp obj)
{
  fd_setjmp_rec *jbr=get_jbr();
  if (jbr == NULL) unhandled_exception(ex,details,obj);
  else {
    fd_set_exception(ex,details,obj);
    if (jbr->self != jbr) {
      unhandled_exception(_("Corrupted exception stack!"),NULL,FD_VOID);}
    else {_fd_pop_jbr(); longjmp(jbr->jb,1);};
  };
}
 
DTYPES_EXPORT
/* fd_reraise:
    Arguments: none
    Returns: no.
Reraises the current exception.  Generally called in handlers but
not otherwise. */
void fd_reraise()
{
  fd_setjmp_rec *jbr=get_jbr();
  if (jbr == NULL)
    unhandled_exception
      (fd_theException(),fd_exception_details(),fd_exception_object());
  else if (jbr->self != jbr) {
    unhandled_exception(_("Corrupted exception stack!"),NULL,FD_VOID);}
  else {_fd_pop_jbr(); longjmp(jbr->jb,1);};
}


/* File specific stuff */

/* The CVS log for this file
   $Log: except.c,v $
   Revision 1.9  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.8  2003/10/01 09:07:41  haase
   Fixed braino in exception fn handling

   Revision 1.7  2003/10/01 09:02:18  haase
   Added exception preface function

   Revision 1.6  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/01/26 20:46:27  haase
   Don't call gettext on details

   Revision 1.5  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.4  2002/04/08 16:37:20  haase
   Added fd_pigs_fly error signaller

   Revision 1.3  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
