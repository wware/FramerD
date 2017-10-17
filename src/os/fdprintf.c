/* C Mode */

/* fdprintf.c
   Implements formatted ascii i/o and a simple internationalization model
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

static char vcid[] = "$Id: fdprintf.c,v 1.25 2005/01/14 16:48:49 haase Exp $";

/** Initial includes and declarations **/
/** Cross-language Translation **/
/** String output functions **/
/** Translated Messages (with arg numbers) **/
/** Extended printf **/
/** Exceptional I/O support **/
/** Notifications and Warnings **/
/** User level functions **/
/** Initialization **/

/** Initial includes and declarations **/

#include <time.h>
#include <stdarg.h>

#include "dtypes.h"

fd_exception fd_BadPrintfArg=_("Bad arg to fdprintf");
static char *dangling_errno_msg;

/* How big the buffer is for sprintf ing args */
#define PRINTF_CHUNK 64

/* Disables/enables notifications */
static int disable_notifications=0;

#if FD_THREADS_ENABLED
static fd_mutex notify_lock, warn_lock;
#endif

/** Cross-language Translation **/

/** String output functions **/

DTYPES_EXPORT
/* _fd_grow_string_stream:
     Arguments: a pointer to a string stream and a number of bytes
     Returns: void

  Grows the data structures for the string stream to include delta
more bytes
*/
void _fd_grow_string_stream(fd_string_stream ss,int delta)
{
  int needed_limit=ss->limit+delta, new_limit=ss->limit;
  while (new_limit < needed_limit) new_limit=new_limit*2;
  ss->ptr=fd_xrealloc(ss->ptr,new_limit);
  ss->limit=new_limit;
}

DTYPES_EXPORT
/* _fd_sputs:
    Arguments: a pointer to a string stream and a utf8 string
    Returns: void

  Internal string stream string put function, used in macro
  ssputs */
void _fd_sputs(fd_string_stream ss,fd_u8char *string)
{
  int len=strlen(string); 
  if (ss->size+len < ss->limit) {
    strcpy(ss->ptr+ss->size,string); ss->size=ss->size+len;}
  else if (ss->grows) {
    _fd_grow_string_stream(ss,len);
    strcpy(ss->ptr+ss->size,string); ss->size=ss->size+len;}
  else if (ss->size+1 < ss->limit) {
    strncpy(ss->ptr+ss->size,string,ss->limit-ss->size-1);
    ss->size=ss->limit-1;
    ss->ptr[ss->size] = '\0';}
}

DTYPES_EXPORT
/* _fd_sputn:
    Arguments: a pointer to a string stream, a utf8 string, and an int
    Returns: void

  Internal string stream string putn function, used in macro
  ssputn */
void _fd_sputn(fd_string_stream ss,fd_u8char *string,int n)
{
  if (ss->size+n < ss->limit) {
    memcpy(ss->ptr+ss->size,string,n);
    ss->size=ss->size+n; ss->ptr[ss->size]='\0';}
  else if (ss->grows) {
    _fd_grow_string_stream(ss,n);
    memcpy(ss->ptr+ss->size,string,n);
    ss->size=ss->size+n; ss->ptr[ss->size]='\0';}
  else if (ss->size+1 < ss->limit) {
    memcpy(ss->ptr+ss->size,string,ss->limit-ss->size-1);
    ss->size=ss->limit-1;
    ss->ptr[ss->size]='\0';}
}

DTYPES_EXPORT
/* _fd_sputc:
     Arguments: a pointer to a string stream and a unicode character
     Returns: void
  Writes the utf8 representation of the character to the string stream. */
void _fd_sputc(fd_string_stream ss,int ch) 
{
  uchar off[6]={0x00,0xC0,0xE0,0xF0,0xF8,0xFC};
  uchar masks[6]={0x7f,0x1F,0x0f,0x07,0x03,0x01};
  int size=0, shift=0, write;
  if (ch == 0) size=2;
  else if (ch < 0x80) size=1; 
  else if (ch < 0x800) size=2;
  else if (ch < 0x10000) size=3;
  else if (ch < 0x200000) size=4;
  else if (ch < 0x4000000) size=5;
  else if (ch < 0x80000000) size=6;
  else fd_raise_exception(_("Invalid Unicode Character"));
  shift=(size-1)*6;
  
  if (ss->size+size+1 > ss->limit) {
    if (ss->grows) _fd_grow_string_stream(ss,size);
    else return;}
  write=ss->size;
  ss->ptr[write++]=off[size-1]|(masks[size-1]&(ch>>shift));
  shift=shift-6; size--;
  while (size) {
    ss->ptr[write++]=0x80|((ch>>shift)&0x3F); shift=shift-6; size--;}
  ss->ptr[write]='\0'; ss->size=write;
}

/** Extended printf **/

/* This is the key internal function for normal printf, which checks
   for a translation of the format string, calls message_printf if there
   is one, and otherwise just processes the format string and arguments
   in order. */
static void do_printf(fd_string_stream s,char *fstring,va_list args)
{
  char *format_string=fd_gettext(fstring);
  char *scan=format_string, *end=strchr(scan,'%');
  while (end) {
    char buf[PRINTF_CHUNK], *fragment=buf;
    /* First, output everything leading up to the % sign */
    fd_sputn(s,scan,end-scan);
    /* Now dispatch on the format code */
    if (end[1] == '%') {buf[0]='%'; buf[1]='\0';}
    else if (end[1] == 'd') sprintf(buf,"%d",va_arg(args,int));
    else if (end[1] == 'u') sprintf(buf,"%u",va_arg(args,unsigned int));
    else if (end[1] == 'x') sprintf(buf,"%x",va_arg(args,int));
    else if (end[1] == 'e') sprintf(buf,"%e",va_arg(args,double));
    else if (end[1] == 'f') sprintf(buf,"%f",va_arg(args,double));
    else if (end[1] == 'g') sprintf(buf,"%.8g",va_arg(args,double));
    else if (end[1] == 's') fragment=va_arg(args,char *);
    /* Long format args */
    else if ((end[1] == 'l') && (end[2] == 'd'))
      sprintf(buf,"%ld",va_arg(args,long));
    else if ((end[1] == 'l') && (end[2] == 'u'))
      sprintf(buf,"%lu",va_arg(args,long));
    else if ((end[1] == 'l') && (end[2] == 'f'))
      sprintf(buf,"%f",va_arg(args,double));
    else if ((end[1] == 'l') && (end[2] == 'g'))
      sprintf(buf,"%.16g",va_arg(args,double));
    else if ((end[1] == 'l') && (end[2] == 'x'))
      sprintf(buf,"%lx",va_arg(args,long));
    /* the code %m indicates a string you should try to translate */
    else if (end[1] == 'm') fragment=fd_gettext(va_arg(args,char *));
    /* the code %q indicates lisp object to print */
    else if (end[1] == 'q') {
      lisp a=va_arg(args,lisp);
      fd_print_lisp_to_string(a,s);
      /* Make buf empty since pprint did its output */
      buf[0]='\0';}
    /* the code %Q indicates lisp object to pretty-print */
    else if (end[1] == 'Q') {
      lisp a=va_arg(args,lisp), width=fd_getenv("PPWIDTH");
      int indent=0; fd_u8char *scan=s->ptr+s->size;
      /* Figure out what our indent is when pretty printing */
      while ((scan > s->ptr) && (*scan != '\n')) {scan--; indent++;}
      if (FIXNUMP(width))
	fd_pprint_lisp_to_string(a,s,indent,indent,FIXLISP(width));
      else {
	fd_decref(width);
        fd_pprint_lisp_to_string(a,s,indent,indent,80);}
      /* Make buf empty since pprint did its output */
      buf[0]='\0';}
    /* the code %t indicates the current time as hh:mm:ss */
    else if (end[1] == 't') {
      time_t tick=time(NULL);
      struct tm *now=localtime(&tick);
      strftime(buf,PRINTF_CHUNK-1,"%H:%M:%S",now);}
    /* the code %lt indicates a longer time string */
    else if ((end[1] == 'l') && (end[2] == 't')) {
      time_t tick=time(NULL);
      struct tm *now=localtime(&tick);
      strftime(buf,PRINTF_CHUNK-1,"%d%b%Y@%H:%M:%S%z",now);}
    else fd_raise_exception(fd_BadPrintfArg);
    if (fragment) {
      if (FD_USE_SPACE(s,(int)strlen(fragment))) strcat(s->ptr,fragment);}
    else {
      if (FD_USE_SPACE(s,4)) strcat(s->ptr,"NULL");}
    /* Advance your parse of the format string */
    if (end[1] == 'l') scan=end+3; else scan=end+2;
    /* And find the next format directive */
    end=strchr(scan,'%');}
  fd_sputs(s,scan); /* At the end, output the tail of the format string */
}

/** Exceptional I/O support **/

/* Exception I/O lets you bind error streams on a thread-local basis. */

#if FD_THREADS_ENABLED
fd_tld_key fd_xio_key;
DTYPES_EXPORT fd_string_stream fd_get_xio() 
{
  struct XIO_DATA *xd=(struct XIO_DATA *)fd_tld_get(fd_xio_key);
  if (xd) return xd->stream; else return NULL;
}
#else
static struct XIO_DATA exceptional_io={NULL,NULL,NULL};
fd_string_stream fd_get_xio() { return exceptional_io.stream; }
#endif
#define get_xio() fd_get_xio()

DTYPES_EXPORT void fd_xio_update()
{
#if FD_THREADS_ENABLED
  struct XIO_DATA *xd=(struct XIO_DATA *)fd_tld_get(fd_xio_key);
  if (xd) {if (xd->fcn) xd->fcn(xd);}
  else fflush(stderr);
#else
  if (exceptional_io.stream) {
    if (exceptional_io.fcn) exceptional_io.fcn(&exceptional_io);}
  else fflush(stderr);
#endif
}

DTYPES_EXPORT void fd_direct_xio
  (fd_string_stream s,void (*f)(struct XIO_DATA *),void *d)
{
#if FD_THREADS_ENABLED
  struct XIO_DATA *xd=fd_tld_get(fd_xio_key);
  if (xd == NULL) {
    xd=fd_xmalloc(sizeof(struct XIO_DATA));
    fd_tld_set(fd_xio_key,(void *)xd);}
  xd->stream=s; xd->fcn=f; xd->data=d;
#else
  exceptional_io.stream=s; exceptional_io.fcn=f; exceptional_io.data=d;
#endif
}

/** Notifications and Warnings **/

static void (*warn_fcn)(fd_u8char *message)=NULL;
static void (*notify_fcn)(fd_u8char *message)=NULL;

DTYPES_EXPORT
/* fd_set_notify_handler:
    Arguments: a function taking a string arg
    Returns: void

 Sets the function which is called on notification messages */
void fd_set_notify_handler(void (*nf)(fd_u8char *msg))
{
  notify_fcn=nf;
}

DTYPES_EXPORT
/* fd_set_warn_handler:
    Arguments: a function taking a string arg
    Returns: void

 Sets the function which is called on warning messages */
void fd_set_warn_handler(void (*wf)(fd_u8char *msg))
{
  warn_fcn=wf;
}

DTYPES_EXPORT
/* fd_stdout_notifier:
    Arguments: a string
    Returns: void

 This is the default notification function, which outputs a bracketed
  time and report. */
void fd_stdout_notifier(fd_u8char *message)
{
  struct tm _now, *now=&_now;
  fd_localtime(now,time(NULL));
  fprintf(stdout,"[%02d:%02d:%02d ",now->tm_hour,now->tm_min,now->tm_sec);
  fd_fputs_encoded(message,strlen(message),stdout);
  fprintf(stdout,"]\n");
  TIDY_ERRNO(dangling_errno_msg);
}

DTYPES_EXPORT
/* fd_stderr_notifier:
    Arguments: a string
    Returns: void

 This is the default notification function, which outputs a bracketed
  time and report. */
void fd_stderr_notifier(fd_u8char *message)
{
  struct tm _now, *now=&_now;
  fd_localtime(now,time(NULL));
  fprintf(stderr,"[%02d:%02d:%02d ",now->tm_hour,now->tm_min,now->tm_sec);
  fd_fputs_encoded(message,strlen(message),stderr);
  fprintf(stderr,"]\n");
  TIDY_ERRNO(dangling_errno_msg);
}

DTYPES_EXPORT
/* fd_dual_notifier:
    Arguments: a string
    Returns: void

 This is the default notification function, which outputs a bracketed
  time and report. */
void fd_dual_notifier(fd_u8char *message)
{
  struct tm _now, *now=&_now;
  fd_localtime(now,time(NULL));
  fprintf(stderr,"[%02d:%02d:%02d ",now->tm_hour,now->tm_min,now->tm_sec);
  fprintf(stdout,"[%02d:%02d:%02d ",now->tm_hour,now->tm_min,now->tm_sec);
  fd_fputs_encoded(message,strlen(message),stderr);
  fd_fputs_encoded(message,strlen(message),stdout);
  fprintf(stderr,"]\n");
  fprintf(stdout,"]\n");
  TIDY_ERRNO(dangling_errno_msg);
}

/** User level functions **/

DTYPES_EXPORT
/* fd_printf
    Arguments: a string stream, a format string, and other args
    Returns: void

  Outputs a string to string stream generated from the format string and
using the provided arguments.  Much like printf (surprise). */
void fd_printf(fd_string_stream s,char *format_string,...)
{
  va_list args; va_start(args,format_string);
  do_printf(s,format_string,args);
}

DTYPES_EXPORT
/* fd_fprintf
    Arguments: a FILE * stream, a format string, and other args
    Returns: void

  Outputs a string to a file stream generated from the format string and
using the provided arguments.  Much like printf (surprise). */
void fd_fprintf(FILE *f,char *fstring,...)
{
  struct FD_STRING_STREAM s;
  va_list args; va_start(args,fstring);
  FD_INITIALIZE_STRING_STREAM(&s,256);
  do_printf(&s,fstring,args);
  fd_fputs_encoded(s.ptr,s.size,f);
  fd_xfree(s.ptr);
}

DTYPES_EXPORT
/* fd_xprintf
    Arguments: an XFILE stream, a format string, and other args
    Returns: void

  Outputs a string to an XFILE stream generated from the format string and
using the provided arguments.  Much like printf (surprise). */
void fd_xprintf(struct FD_XFILE *xf,char *fstring,...)
{
  struct FD_STRING_STREAM s;
  va_list args; va_start(args,fstring);
  FD_INITIALIZE_STRING_STREAM(&s,256);
  do_printf(&s,fstring,args);
  fd_xputs_encoded(s.ptr,s.size,xf);
  fd_xfree(s.ptr);
}

DTYPES_EXPORT
/* fd_exprintf
    Arguments: a format string, and other args
    Returns: void

  Outputs a string as exceptional I/O generated from the format string and
using the provided arguments.  Much like printf (surprise). */
void fd_exprintf(char *format_string,...)
{
  va_list args;
  fd_string_stream xio=fd_get_xio();
  va_start(args,format_string);
  if (xio) do_printf(xio,format_string,args);
  else {
    struct FD_STRING_STREAM s;
    FD_INITIALIZE_STRING_STREAM(&s,256);
    do_printf(&s,format_string,args);
    fd_fputs_encoded(s.ptr,s.size,stderr);
    fd_xfree(s.ptr);}
  FD_TIDY_ERRNO(dangling_errno_msg);
  fd_xio_update();
}

DTYPES_EXPORT
/* fd_notify
    Arguments: a FILE * stream, a format string, and other args
    Returns: void

  Outputs a string as a notification, which is generated from the format
string and using the provided arguments.  Much like printf (surprise). */
void fd_notify(char *format_string,...)
{
  va_list args; struct FD_STRING_STREAM s; char timebuf[32];
  if (notify_fcn == NULL) return;
  FD_INITIALIZE_STRING_STREAM(&s,1024);
  va_start(args,format_string);
  do_printf(&s,format_string,args);
  TIDY_ERRNO(dangling_errno_msg);
#if FD_THREADS_ENABLED  
  {UNWIND_PROTECT {
    lock_mutex(&notify_lock);
    notify_fcn(s.ptr);}
  ON_UNWIND
    fd_xfree(s.ptr);
    unlock_mutex(&notify_lock);
  END_UNWIND;}
#else
  notify_fcn(s.ptr);
  fd_xfree(s.ptr);
#endif
}

DTYPES_EXPORT
/* fd_warn
    Arguments: a FILE * stream, a format string, and other args
    Returns: void

  Outputs a string as a warning, which is generated from the format
  string and using the provided arguments.  Much like printf (surprise).
  Note that notifications can be turned off but warnings cannot. */
void fd_warn(char *format_string,...)
{
  va_list args; struct FD_STRING_STREAM s;
  time_t tick=time(NULL); struct tm *now;
  now=localtime(&tick);
  FD_INITIALIZE_STRING_STREAM(&s,1024);
  va_start(args,format_string);
  do_printf(&s,format_string,args);
  TIDY_ERRNO(dangling_errno_msg);
  if (notify_fcn) {
    notify_fcn(s.ptr);
    fprintf(stderr,"[%02d:%02d:%02d %s]\n",
	    now->tm_hour,now->tm_min,now->tm_sec,
	    s.ptr);}
  else fprintf(stderr,"[%02d:%02d:%02d %s]\n",
	       now->tm_hour,now->tm_min,now->tm_sec,
	       s.ptr);
  fd_xfree(s.ptr);
  fflush(stderr);
}

/** Disabling notifications **/

DTYPES_EXPORT
/* fd_disable_notifications:
    Arguments: none
    Returns: void
    Makes fd_notify into a no-op

This is different from fd_set_notify_handler(NULL) because it
works if called before initialization to keep initialization from
setting the default notify handler.
*/
void fd_disable_notifications()
{
  disable_notifications=1;
  notify_fcn=NULL;
}

/** Initialization **/

void fd_initialize_fdprintf_c()
{
#if FD_THREADS_ENABLED
  fd_new_tld_key(&fd_xio_key,NULL);
  fd_init_mutex(&notify_lock);
  fd_init_mutex(&warn_lock);
#endif
  if (disable_notifications) notify_fcn=NULL;
  else notify_fcn=fd_stdout_notifier;
  dangling_errno_msg=fd_gettext("danging errno:");
  fd_register_source_file("fdprintf",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdprintf.c,v $
   Revision 1.25  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.24  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.23  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.22  2004/07/16 16:42:09  haase
   Added %u to fd_printf handling

   Revision 1.21  2004/04/18 15:30:52  haase
   More and consistent notify handlers

   Revision 1.20  2004/04/16 09:39:53  haase
   Added fd_dual_notifier and made warnings always to to stderr

   Revision 1.19  2004/04/16 09:29:30  haase
   Made warnings and notifications go to stderr, rather than stdout

   Revision 1.18  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.17  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.16.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.16.2.2  2003/08/02 13:51:46  haase
   Renamed fd_xprintf to fd_exprintf and made fd_xprintf do a printf to an XFILE

   Revision 1.16.2.1  2003/01/26 20:46:55  haase
   Fix some off by one string errors

   Revision 1.16  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.15  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.14  2002/04/30 13:48:33  haase
   Made packaged format for homogenous vectors be bytes rather than lisp vectors, saving space in the external DType representation

   Revision 1.13  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.12  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.11  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.10  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
