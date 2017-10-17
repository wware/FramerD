/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: binio.h,v 1.11 2005/01/14 16:48:44 haase Exp $

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

  This is the file binio.h supporting binary/io to standard IO stream
for FramerD, particularly regularizing byte order.

*************************************************************************/

#ifndef FRAMERD_BINIO_H /* If defined, skip the file */
#define FRAMERD_BINIO_H 1

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/* Binary I/O */

STATIC_INLINE unsigned int fd_flip_word(unsigned int _w)
{ return ((((_w) << 24) & 0xff000000) | (((_w) << 8) & 0x00ff0000) | 
          (((_w) >> 8) & 0x0000ff00) | ((_w) >>24) );}

STATIC_INLINE unsigned int fd_flip_ushort(unsigned short _w)
{ return ((((_w) >> 8) & 0x0000ff) | (((_w) << 8) & 0x0000ff00) );}

#if ((defined(OS2)) || (defined(linux)))
int getw (FILE * stream);
int putw (int w, FILE * stream);
#endif /* OS2  or linux (since pgcc does not define)*/

#if (defined(linux))
#define trunc(x) ((double) ((long) (x)))
#endif

#ifdef WORDS_BIGENDIAN
#define fd_net_order(x) (x)
#define fd_host_order(x) (x)
#define fd_ushort_net_order(x) (x)
#define fd_ushort_host_order(x) (x)
#else
#define fd_net_order(x) fd_flip_word(x)
#define fd_host_order(x) fd_flip_word(x)
#define fd_ushort_host_order(x) fd_flip_ushort(x)
#define fd_ushort_net_order(x) fd_flip_ushort(x)
#endif

/* Binary I/O to STDOUT */

#define fd_fwrite_byte(x,stream) \
   {int return_code=putc(((char)x),stream); \
    if (return_code == EOF) fd_raise_exception(fd_FileWriteFailed);}
#define fd_fwrite_4bytes(x,stream) \
   {int return_code=putw(fd_net_order(x),stream); \
    if ((return_code == EOF) && (ferror(stream))) \
      fd_raise_exception(fd_FileWriteFailed);}
#define fd_fwrite_bytes(x,n,stream) \
   {if (fwrite(x,sizeof(unsigned char),n,stream) != (unsigned int)n) \
     fd_raise_exception(fd_FileWriteFailed);}

#define fd_funread_byte(code, stream) ungetc(code, stream)

static void _fread_all_bytes(char *data,unsigned int size,FILE *stream) UNUSED;
static void _fread_all_bytes(char *data,unsigned int size,FILE *stream)
{
  fd_set readfds;
  unsigned int residue=size;
  int fd=fileno(stream);

  while (residue > 0) {
    int result=0;
    FD_ZERO(&readfds); FD_SET(fd,&readfds);
    switch(select(fd + 1, &readfds, NULL, NULL, NULL)) {
    case 1:
      clearerr(stream);
      result=fread(data,sizeof(char),residue,stream);
      if (result == 0)
	fd_raise_exception(fd_Unexpected_EOF);
      clearerr(stream);
      data=data+result;
      residue=residue-result;
      FD_CLEAR_ERR();
    case 0:
      break;
    default:
      if (errno != EINTR)
	fd_raise_exception(fd_Unexpected_EOF);
      break;
    }
  }
}      

STATIC_INLINE void fd_fread_bytes(char *data,unsigned int size,FILE *stream) UNUSED;
STATIC_INLINE void fd_fread_bytes(char *data,unsigned int size,FILE *stream)
{
  unsigned int result;
  result=fread(data,sizeof(char),size,stream);
  if (result != size) 
    _fread_all_bytes(data+result,size-result,stream);
}

STATIC_INLINE int fd_fread_byte(FILE *stream) UNUSED;
STATIC_INLINE int fd_fread_byte(FILE *stream)
{
  return getc(stream);
}

STATIC_INLINE unsigned int fd_fread_4bytes(FILE *stream) UNUSED;
STATIC_INLINE unsigned int fd_fread_4bytes(FILE *stream)
{
  int input;
  fd_fread_bytes((char *) &input, sizeof(input), stream);
  return ((unsigned int) fd_host_order(input));
}

/* Binary I/O to FD_DBUFs */

#define FD_DBUF_DELTA 16834

#if FD_INLINE_DBUF
static void grow_dbuf(struct FD_DBUF *b,int size)
{
  unsigned int current_size=b->ptr-b->start, need_size=current_size+size;
  unsigned int new_size=((need_size/FD_DBUF_DELTA)+1)*FD_DBUF_DELTA;
  unsigned char *new=fd_xrealloc(b->start,new_size);
  if (new == NULL) fd_raise_exception(fd_ReallocFailed);
  b->ptr=new+(b->ptr-b->start); 
  b->start=new; b->end=b->start+new_size;
  
}
FASTOP void fd_dbuf_needs(struct FD_DBUF *b,int size)
{
  if (b->ptr+size > b->end) grow_dbuf(b,size);
}
#else
DTYPES_EXPORT void fd_dbuf_needs(struct FD_DBUF *b,int size);
#define fd_dbuf_needs _fd_dbuf_needs
#endif

static void fd_dwrite_byte(unsigned char byte,struct FD_DBUF *b)
{
  fd_dbuf_needs(b,1); *(b->ptr++)=byte; 
}

static void fd_dwrite_4bytes(unsigned int word,struct FD_DBUF *b)
{
  fd_dbuf_needs(b,4); 
#if FD_WORDS_ARE_ALIGNED
  *(b->ptr++)=((word>>24)&0xff);
  *(b->ptr++)=((word>>16)&0xff);
  *(b->ptr++)=((word>>8)&0xff);
  *(b->ptr++)=((word)&0xff);
#else
  *((unsigned int *) b->ptr)=fd_net_order(word);
  b->ptr=b->ptr+4;
#endif
}

static void fd_dwrite_bytes(char *data,int size,struct FD_DBUF *b)
{
  fd_dbuf_needs(b,size);
  memcpy(b->ptr,data,size); b->ptr=b->ptr+size;    
}

static int fd_dread_byte(struct FD_DBUF *b)
{
  if (b->ptr >= b->end) fd_raise_exception(fd_Unexpected_EOD);
  else return *(b->ptr++);
  return 0; /* Never reached */
}

static void fd_dunread_byte(unsigned char byte,struct FD_DBUF *b)
{
  if (b->ptr == b->start) fd_raise_exception(fd_NegativeUNGETC);
  else if (*(b->ptr-1) != byte) fd_raise_exception(fd_InconsistentUNGETC);
  else b->ptr--;
}

#ifdef FD_WITHOUT_POINTER_KLUDGES
static unsigned int fd_dread_4bytes(struct FD_DBUF *b)
{
  unsigned int num;
  if (b->ptr+4 > b->end) fd_raise_exception(fd_Unexpected_EOD);
  num=(b->ptr[0]<<24)+(b->ptr[1]<<16)+(b->ptr[2]<<8)+b->ptr[3];
  b->ptr=b->ptr+4;
  return num;
}
#else
static unsigned int fd_dread_4bytes(struct FD_DBUF *b)
{
  unsigned int num;
  if (b->ptr+4 > b->end) fd_raise_exception(fd_Unexpected_EOD);
  num=fd_host_order(*((unsigned int *)(b->ptr))); b->ptr=b->ptr+4;
  return num;
}
#endif

static void fd_dread_bytes(char *data,unsigned int size,struct FD_DBUF *b)
{
  if (b->ptr+size > b->end) fd_raise_exception(fd_Unexpected_EOD);
  memcpy(data,b->ptr,size); b->ptr=b->ptr+size;
}

#endif

#if (FD_SOURCE)
#define flip_word fd_flip_word
#define flip_short fd_flip_ushort
#define net_order fd_net_order
#define host_order fd_host_order

#define fwrite_byte fd_fwrite_byte
#define fwrite_4bytes fd_fwrite_4bytes
#define fwrite_bytes fd_fwrite_bytes
#define funread_byte fd_funread_byte

#define fread_bytes fd_fread_bytes
#define fread_byte  fd_fread_byte
#define fread_4bytes fd_fread_4bytes
#endif



/* File specific stuff */

/* The CVS log for this file
   $Log: binio.h,v $
   Revision 1.11  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.10  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.9  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.8  2004/05/03 20:21:07  haase
   Moved dbuf i/o functions to header file

   Revision 1.7  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.6.2.2  2003/08/15 13:29:35  haase
   Various extensions to file pools and indices to handle custom extensions

   Revision 1.6.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.6  2002/04/30 13:48:33  haase
   Made packaged format for homogenous vectors be bytes rather than lisp vectors, saving space in the external DType representation

   Revision 1.5  2002/04/03 01:33:09  haase
   Moved indextools out of FD_SOURCE core

   Revision 1.4  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
