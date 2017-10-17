/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: strstream.h,v 1.12 2005/01/14 16:48:44 haase Exp $

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

  This is the file strstream.h supporting output to strings.
   
*************************************************************************/


#ifndef FRAMERD_STRSTREAM_H /* If defined, skip the file */
#define FRAMERD_STRSTREAM_H

/* String output streams */

typedef struct FD_STRING_STREAM {
  int size, limit, grows; fd_u8char *ptr;
  int fancy_oids, escape;} *fd_string_stream;

DTYPES_EXPORT
void _fd_grow_string_stream(fd_string_stream ss,int delta);

DTYPES_EXPORT void _fd_sputs(fd_string_stream ss,fd_u8char *string);
DTYPES_EXPORT void _fd_sputn(fd_string_stream ss,fd_u8char *string,int n);
DTYPES_EXPORT void _fd_sputc(fd_string_stream ss,int ch);

#define FD_INITIALIZE_STRING_STREAM(s,sz) \
   if (sz == 0) fd_raise_exception(_("no zero-length string streams")); \
   (s)->limit=sz; (s)->size=0; (s)->ptr=fd_xmalloc(sz); (s)->grows=1; \
   (s)->ptr[0]='\0'; (s)->fancy_oids=1; (s)->escape=1
#define FD_INITIALIZE_FIXED_STRING_STREAM(s,sz,buf) \
   if (sz == 0) fd_raise_exception(_("no zero-length string streams")); \
   (s)->limit=sz; (s)->size=0; (s)->ptr=buf; (s)->grows=0; \
   (s)->ptr[0]='\0'; (s)->fancy_oids=1; (s)->escape=1

#define FD_USE_SPACE(ss,delta) \
  ((ss->size+delta < ss->limit) ? (ss->size=ss->size+delta,1) : \
   ((ss->grows) ? (_fd_grow_string_stream(ss,delta),ss->size=ss->size+delta,1) \
    : (ss->size=ss->limit,0)))

DTYPES_EXPORT int  _fd_sgetc(fd_u8char **ss);
DTYPES_EXPORT void _fd_sputc(fd_string_stream ss,int ch);
DTYPES_EXPORT void _fd_sputs(fd_string_stream ss,fd_u8char *string);
DTYPES_EXPORT void _fd_sputn(fd_string_stream ss,fd_u8char *string,int n);

#if (FD_INLINE_STRING_STREAMS)
FASTOP void fd_sputs(fd_string_stream ss,fd_u8char *string) UNUSED;
FASTOP void fd_sputs(fd_string_stream ss,fd_u8char *string)
{
  int len=strlen((const char *)string); 
  if (ss->size+len < ss->limit) {
    strcpy((fd_u8char *)ss->ptr+ss->size,(const fd_u8char *)string);
    ss->size=ss->size+len;}
  else _fd_sputs(ss,string);
}

FASTOP void fd_sputn(fd_string_stream ss,fd_u8char *string,int n) UNUSED;
FASTOP void fd_sputn(fd_string_stream ss,fd_u8char *string,int n)
{
  if (ss->size+n < ss->limit) {
    strncpy((fd_u8char *)ss->ptr+ss->size,(const fd_u8char *)string,n);
    ss->size=ss->size+n; ss->ptr[ss->size]=NUL;}
  else _fd_sputn(ss,string,n);
}

FASTOP void fd_sputc(fd_string_stream ss,int ch) UNUSED;
FASTOP void fd_sputc(fd_string_stream ss,int ch)
{
  if ((ch > 0) && (ch < 0x80) && (ss->size+1 < ss->limit)) {
    ss->ptr[ss->size++]=ch; ss->ptr[ss->size]='\0';}
  else _fd_sputc(ss,ch);
}
FASTOP int fd_sgetc(fd_u8char **s) UNUSED;
FASTOP int fd_sgetc(fd_u8char **s)
{
  if (**s == 0) return -1;
  else if (**s < 0x80) return *((*s)++);
  else return _fd_sgetc(s);
}
#else /* else FD_INLINE_STRING_STREAMS */
#define fd_sgetc _fd_sgetc
#define fd_sputs _fd_sputs
#define fd_sputn _fd_sputn
#define fd_sputc _fd_sputc
#endif /* else FD_INLINE_STRING_STREAMS */
#endif /* FRAMERD_STRSTREAM_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: strstream.h,v $
   Revision 1.12  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.11  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.10  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.9.2.1  2002/09/26 02:10:49  haase
   Move declaration before use

   Revision 1.9  2002/04/19 13:19:51  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.8  2002/04/17 11:46:08  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.7  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
