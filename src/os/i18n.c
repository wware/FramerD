/* C Mode */

/* i18n.c
   Implements internationalization support for FramerD, particularly
    reading different kinds of character encodings.
   Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.

   Copyright (C) 1999 Massachusetts Institute of Technology

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

static char vcid[] = "$Id: i18n.c,v 1.36 2004/07/20 09:16:14 haase Exp $";

/** Headers **/
/** FD_XFILE functions **/
/** Text encoding functions **/
/** Support functions (all FASTOPs) **/
/** Basic PUTC and GETC functions **/
/** External functions **/
/** Converting strings **/
/** some standard encodings **/
/** filestring with text encoding **/
/** Adding more character information **/
/** Loading Encodings **/
/** Initialization **/

/** Headers **/

#define FD_INLINE_CHARACTER_OPS 1

#include <string.h>

#include "dtypes.h"
#include "charsets.h"
#include "chardata.h"

#if HAVE_FCNTL_H
#include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif

unsigned int fd_chardata_warnings=0;

fd_exception
  fd_InvalidUnicodeEscape=_("Invalid Unicode escape sequence"),
  fd_InvalidUnicodeChar=_("Bad Unicode character"),
  fd_BadUTF8=_("Bad UTF-8 string"),
  fd_UnknownEncoding=_("Unknown text encoding"),
  fd_NoLocalChar=_("Can't render character in local character set"),
  fd_InvalidChar=_("Character code is not defined in local character set"),
  fd_NegativeUNGETC=_("Can't UNGETC past beginning of string"),
  fd_InconsistentUNGETC=_("Inconsistent use of UNGETC"),
  fd_BadMultiByteChar=_("Bad multi-byte character in external encoding"),
  fd_XFileConflict=_("XFile Conflict: file pointer already has associated XFILE");

static struct FD_TEXT_ENCODING
  *encodings=NULL, *default_encoding=NULL, *utf8_encoding=NULL,
  *ascii_encoding, *latin1_encoding, *system_encoding;

struct FD_COMPRESSED_CHAR_INFO *fd_compressed_charinfo[256]={
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,

  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,

  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,

  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

static char *default_encoding_name="US-ASCII";

static struct FD_XFILE *encodings_by_file=NULL;

static int compare_encoding_names(char *name1,char *name2);
static int compute_flags(struct FD_MB_MAP *chset,int size);
static struct FD_TEXT_ENCODING *try_to_load_encoding(char *name);

static int fread_noblock(FILE *f,unsigned char *buf,int n)
{
  return fread(buf,1,n,f);
}

#if WIN32 /* Now do I do this under WIN32? */
static void set_noblock(FILE *f)
{
}
#else
static void set_noblock(FILE *f)
{
  fcntl(fileno(f),F_SETFL,O_NONBLOCK);  
}
#endif

#define MB_MAX 16

#if FD_THREADS_ENABLED
static fd_mutex xfile_mutex;
#endif

static struct FD_TEXT_ENCODING *lookup_encoding_name(char *name)
{
  struct FD_TEXT_ENCODING *scan=encodings;
  if (name == NULL) return NULL;
  else while (scan) {
    char **names=scan->names;
    while (*names)
      if (compare_encoding_names(*names,name)) return scan;
      else names++;
    scan=scan->next;}
  return NULL;
}

/** fd_get_chardata for non inliners */

DTYPES_EXPORT struct FD_CHAR_INFO _fd_get_char_data(int c)
{
  return fd_get_char_data(c);
}

/** Canonical Decomposition functions **/

static int n_decompositions=0;
static struct FD_UNICODE_DECOMPOSITION **decomp_table;

static struct FD_UNICODE_DECOMPOSITION *lookup_char(unsigned int ch)
{
  int bot=0, top=n_decompositions-1;
  while (top >= bot) {
    int mid=bot+(top-bot)/2;
    if (decompositions[mid].code == ch)
      return &(decompositions[mid]);
    else if (ch < decompositions[mid].code) top=mid-1;
    else bot=mid+1;}
  return NULL;
}

static int compare_decomps(const void *vx,const void *vy)
{
  struct FD_UNICODE_DECOMPOSITION **x=(struct FD_UNICODE_DECOMPOSITION **)vx;
  struct FD_UNICODE_DECOMPOSITION **y=(struct FD_UNICODE_DECOMPOSITION **)vy;
  return strcmp((*x)->decomp,(*y)->decomp);
}

static void setup_decomposition_tables()
{
  int i=0;
  while (decompositions[n_decompositions].code)
    n_decompositions++;
  decomp_table=
    fd_malloc(sizeof(struct FD_UNICODE_DECOMPOSITION *)*
	      n_decompositions);
  while (i < n_decompositions) {
    decomp_table[i]=&(decompositions[i]); i++;}
  qsort(decomp_table,n_decompositions,
	sizeof(struct FD_UNICODE_DECOMPOSITION *),
	compare_decomps);
}

static struct FD_UNICODE_DECOMPOSITION *lookup_decomp(char *ch)
{
  int bot=0, top=n_decompositions-1;
  while (top >= bot) {
    int mid=bot+(top-bot)/2;
    int cmp=strcmp(decompositions[mid].decomp,ch);
    if (cmp == 0)
      return &(decompositions[mid]);
    else if (cmp < 0) top=mid-1;
    else bot=mid+1;}
  return NULL;
}

DTYPES_EXPORT fd_u8char *fd_decompose_char(unsigned int ch)
{
  struct FD_UNICODE_DECOMPOSITION *ud=lookup_char(ch);
  if (ud) return ud->decomp;
  else return NULL;
}

DTYPES_EXPORT int fd_base_char(unsigned int ch)
{
  struct FD_UNICODE_DECOMPOSITION *ud=lookup_char(ch);
  if (ud) return ud->decomp[0];
  else return -1;
}

DTYPES_EXPORT int fd_recompose_char(fd_u8char *s)
{
  struct FD_UNICODE_DECOMPOSITION *ud=lookup_decomp(s);
  if (ud) return ud->code;
  else return -1;
}

/** FD_XFILE functions **/
/*  (XFILEs deal with alien characters) */

static struct XFILE_MAP { FILE *f; struct FD_XFILE *xf; int mallocd; } *xfile_map=NULL;
static int n_xfile_maps=0, max_xfile_maps=0;

FASTOP struct FD_XFILE *get_xfile(FILE *f)
{
  int i=0, lim; struct FD_XFILE *result=NULL;
  lock_mutex(&xfile_mutex);
  lim=n_xfile_maps; while (i < lim)
    if (f == xfile_map[i].f) break;
    else i++;
  if (i < lim) result=xfile_map[i].xf;
  unlock_mutex(&xfile_mutex);
  return result;
}

DTYPES_EXPORT
/* fd_get_xfile:
    Arguments: a FILE pointer
    Returns: an XFILE pointer (or NULL)

 Gets the XFILE struct associated with a particular file pointer.
*/
struct FD_XFILE *fd_get_xfile(FILE *f)
{
  if (f == NULL)
    fd_raise_exception(fd_FileOpenFailed);
  return get_xfile(f);
}

DTYPES_EXPORT
/* fd_associate_xfile:
    Arguments: a FILE pointer, an XFILE pointer
    Returns: an XFILE pointer

 Associates an XFILE struct with a particular file pointer, creating one if the second argument is NULL.
*/
struct FD_XFILE *fd_associate_xfile(FILE *f,struct FD_XFILE *xf)
{
  int i=0, lim, mallocd=(xf == NULL); 
  lock_mutex(&xfile_mutex);
  lim=n_xfile_maps; while (i < lim)
    if (f == xfile_map[i].f) break;
    else i++;
  if (i < lim) {
    if ((xf != NULL) && (xfile_map[i].xf != xf)) fd_raise_exception(fd_XFileConflict);
    xf=xfile_map[i].xf;}
  else {
    if (xf == NULL) {
      xf=fd_malloc(sizeof(struct FD_XFILE)); fd_init_xfile(xf,f,fd_get_default_encoding());}
    if (n_xfile_maps >= max_xfile_maps) {
      if (xfile_map == NULL) {
	xfile_map=fd_malloc(4*sizeof(struct XFILE_MAP)); max_xfile_maps=4;}
      else {
	xfile_map=fd_realloc
	  (xfile_map,(16+max_xfile_maps)*sizeof(struct XFILE_MAP),max_xfile_maps*sizeof(struct XFILE_MAP));
	max_xfile_maps=max_xfile_maps+16;}}
    xfile_map[n_xfile_maps].f=f; xfile_map[n_xfile_maps].xf=xf;
    xfile_map[n_xfile_maps].mallocd=mallocd;
    n_xfile_maps++;}
  unlock_mutex(&xfile_mutex);
  return xf;
}

DTYPES_EXPORT
/* fd_free_xfile:
    Arguments: an FILE pointer
    Returns: nothing

 Frees the XFILE entry from the linked list of FILE association pointers.
*/
void fd_free_xfile(FILE *f)
{
  int i=0, lim; 
  lock_mutex(&xfile_mutex);
  lim=n_xfile_maps; while (i < lim)
    if (f == xfile_map[i].f) break;
    else i++;
  if (i < lim) {
    xfile_map[i].f=NULL;
    if (xfile_map[i].mallocd) fd_free(xfile_map[i].xf,sizeof(struct FD_XFILE));
    if (i+1 < n_xfile_maps)
      memmove(&(xfile_map[i]),&(xfile_map[i+1]),sizeof(struct XFILE_MAP)*(n_xfile_maps-i));
    n_xfile_maps--;}
  unlock_mutex(&xfile_mutex);
}

/** Text encoding functions **/

DTYPES_EXPORT
/* fd_get_encoding:
    Arguments: an ASCII string
    Returns: a pointer to an FD_TEXT_ENCODING struct

  This gets the structure describing a particular encoding given
its ASCII name.
*/
struct FD_TEXT_ENCODING *fd_get_encoding(char *name)
{
  if (name == NULL) return NULL;
  else {
    struct FD_TEXT_ENCODING *encoding=lookup_encoding_name(name);
    if (encoding) return encoding;
    else return try_to_load_encoding(name);}
}

void fd_init_xfile(struct FD_XFILE *xf,FILE *f,struct FD_TEXT_ENCODING *e)
{
  xf->f=f; xf->encoding=e; xf->last_char=-1; xf->in_size=0;
}

DTYPES_EXPORT
/* fd_set_file_encoding:
    Arguments: a FILE pointer and an ASCII string naming an encoding
    Returns: void

  Sets the encoding for a particular FILE pointer to the encoding
with the specified name.
*/
void fd_set_file_encoding(FILE *f,char *name)
{
  struct FD_TEXT_ENCODING *scan;
  if (name == NULL) scan=default_encoding;
  else scan=fd_get_encoding(name);
  if (scan) {
    struct FD_XFILE *xf=fd_associate_xfile(f,NULL);
    xf->encoding=scan;
    /* When reading character by character, f needs to be non-blocking 
       so that it can return a valid EOF. */
    if (scan->mb2wc) set_noblock(f);}
  else fd_raise_detailed_exception(fd_UnknownEncoding,name);
}

DTYPES_EXPORT
/* fd_get_file_encoding:
    Arguments: a FILE pointer
    Returns: a pointer to an encoding struct or NULL

  Returns the encoding associated with a particular file stream.
*/
struct FD_TEXT_ENCODING *fd_get_file_encoding(FILE *f)
{
  struct FD_XFILE *xf=get_xfile(f);
  return ((xf) ? (xf->encoding) : (NULL));
}

DTYPES_EXPORT
/* fd_set_default_encoding:
    Arguments: an encoding name
    Returns: void
  Sets the default encoding used for XFILES
*/
void fd_set_default_encoding(char *name)
{
  if (default_encoding) {
    struct FD_TEXT_ENCODING *new=fd_get_encoding(name);
    if (new) default_encoding=new;
    else fd_warn(_("Unknown text encoding %s"),name);}
  else default_encoding_name=fd_strdup(name);
}
DTYPES_EXPORT
/* fd_get_default_encoding:
    Arguments: 
    Returns: a pointer to a FD_TEXT_ENCODING struct
  Returns the default encoding used for XFILES
*/
struct FD_TEXT_ENCODING *fd_get_default_encoding()
{
  return default_encoding;
}

DTYPES_EXPORT
/* fd_set_system_encoding:
    Arguments: an encoding name
    Returns: void
  Sets the encoding used by the operating system
   (e.g. for system calls, filenames, etc)
*/
void fd_set_system_encoding(char *name)
{
  struct FD_TEXT_ENCODING *new=fd_get_encoding(name);
  if (new) system_encoding=new;
  else fd_warn(_("Unknown text encoding %s"),name);
}
DTYPES_EXPORT
/* fd_get_system_encoding:
    Arguments: 
    Returns: a pointer to a FD_TEXT_ENCODING struct
  Returns the encoding used by the operating system
  (e.g. by system calls, filenames, etc)
*/
struct FD_TEXT_ENCODING *fd_get_system_encoding()
{
  if (system_encoding)
    return system_encoding;
  else return fd_get_default_encoding();
}

static int compare_encoding_names(char *name1,char *name2)
{
  char *scan1=name1, *scan2=name2;
  while ((*scan1) && (*scan2))
    if (*scan1 == *scan2) {scan1++; scan2++;}
    else if (tolower(*scan1) == tolower(*scan2)) {scan1++; scan2++;}
    else if ((*scan1 == '-') || (*scan1 == '_') || (*scan1 == '/') || (*scan1 == ' '))
      scan1++;
    else if ((*scan2 == '-') || (*scan2 == '_') || (*scan2 == '/') || (*scan2 == ' '))
      scan2++;
    else return 0;
  while ((*scan1) && (isspace(*scan1))) scan1++;
  while ((*scan2) && (isspace(*scan2))) scan2++;
  if (*scan1 == *scan2) return 1;
  else return 0;
}

/** Charset primitives **/

static int mb_lookup_code(int code,struct FD_MB_MAP *map,int size)
{
  int bot=0, top=size-1;
  while (top >= bot) {
    int split=bot+(top-bot)/2;
    if (code == map[split].from) return map[split].to;
    else if (code < map[split].from) top=split-1;
    else bot=split+1;}
  return -1;
}

FASTOP int table_mb2wc
  (xchar *o,uchar *s,size_t n,struct FD_TEXT_ENCODING *e)
{
  if ((e->flags)&(FD_ENCODING_IS_LINEAR)) {
    *o=(e->charset)[*s].to; return 1;}
  else {
    int i=0, size=0, code=0, try, n_bytes=((n > 4) ? 4 : n);
    while (i < n_bytes) {
      code=(code<<8)|(*s); s++; size++;
      try=mb_lookup_code(code,e->charset,e->charset_size);
      if (try >= 0) {*o=try; return size;}
      else i++;}
    return -1;}
}
  
FASTOP int table_wc2mb
  (uchar *o,xchar ch,struct FD_TEXT_ENCODING *e)
{
  int code=mb_lookup_code(ch,e->charset_inv,e->charset_size);
  if (code < 0) return -1;
  else if (code < 0x100) {*o=code; return 1;}
  else if (code < 0x10000) {
    o[0]=(code&0xFF00)>>8; o[1]=(code&0xFF); return 2;}
  else if (code < 0x1000000) {
    o[0]=(code&0xFF0000)>>16; o[1]=(code&0xFF00)>>8; o[2]=(code&0xFF);
    return 3;}
  else {
    o[0]=(code&0xFF000000)>>24; o[1]=(code&0xFF0000)>>16;
    o[2]=(code&0xFF00)>>8; o[3]=(code&0xFF);
    return 4;}
}

static int charset_order(const void *vx,const void *vy)
{
  struct FD_MB_MAP *x=(struct FD_MB_MAP *)vx;
  struct FD_MB_MAP *y=(struct FD_MB_MAP *)vy;
  int ix=x->from, iy=y->from;
  if (ix == iy) return 0;
  else if (ix < iy) return -1;
  else return 1;
}

static void sort_charset(struct FD_MB_MAP *chset,int size)
{
  qsort(chset,size,sizeof(struct FD_MB_MAP),charset_order);
}

static struct FD_MB_MAP *invert_charset(struct FD_MB_MAP *input,int size)
{
  struct FD_MB_MAP *inv=fd_malloc(sizeof(struct FD_MB_MAP)*size);
  int i=0; while (i < size) {
    inv[i].to=input[i].from; inv[i].from=input[i].to; i++;}
  sort_charset(inv,size);
  return inv;
}

/** Defining new encoding types **/

DTYPES_EXPORT
/* fd_define_encoding:
     Arguments: a name, a pointer to a charset, 
        a wide-char to multi-byte conversion function,
        a multi-byte to wide-char conversion function,
        and a set of flags.
     Returns: 1 if the map was used, zero if it wasn't (mapping was already defined)

Defines an encoding with a name and the associated properties.  If an
encoding with the give properties already exists, the name is added to
that encoding structure. */
int fd_define_encoding
  (char *name,struct FD_MB_MAP *charset,int size,
   wc2mb_fn wc2mb,mb2wc_fn mb2wc,int flags)
{
  struct FD_TEXT_ENCODING *scan=encodings;
  while (scan) 
    if ((scan->charset == charset) && (scan->flags == flags) &&
	(scan->wc2mb == wc2mb) && (scan->mb2wc == mb2wc)) {
      char **names=scan->names; int j=0;
      while (*names)
	if (compare_encoding_names(*names,name)) return 0;
	else {names++; j++;}
      *names=fd_strdup(name);
      scan->names=fd_xrealloc(scan->names,sizeof(char *)*(j+2));
      scan->names[j+1]=NULL;
      return 0;}
    else scan=scan->next;
  scan=lookup_encoding_name(name);
  if (scan) {
    fd_warn(_("Text encoding `%s' is already declared"),name);
    return 0;}
  scan=fd_malloc(sizeof(struct FD_TEXT_ENCODING));
  scan->names=fd_xmalloc(sizeof(char *)*2);
  scan->names[0]=fd_strdup(name); scan->names[1]=NULL;
  if (size) {
    scan->charset=charset; scan->charset_size=size;
    sort_charset(charset,size);
    scan->charset_inv=invert_charset(charset,size);}
  else {scan->charset=NULL; scan->charset_inv=NULL;}
  scan->wc2mb=wc2mb; scan->mb2wc=mb2wc;
  scan->flags=flags; scan->next=encodings; encodings=scan;
  return 1;
}


/* MB Interpret */

/** UTF-8 string i/o **/

static get_utf8_size(fd_u8char s1)
{
  if (s1 < 0x80) return 1;
  else if (s1 < 0xC0) return -1;
  else if (s1 < 0xE0) return 2;
  else if (s1 < 0xF0) return 3;
  else if (s1 < 0xF8) return 4;
  else if (s1 < 0xFC) return 5;
  else if (s1 < 0xFE) return 6;
  else return -1;
}

STATIC_INLINE int check_utf8_ptr(fd_u8char *s,int size)
{
  int i=1;
  if (size == 1) return size;
  /* Now check that the string is valid */
  while (i < size)
    if (s[i] < 0x80) return -i;
    else if (s[i] > 0xc0) return -i;
    else i++;
  return size;
}

STATIC_INLINE int valid_utf8p(fd_u8char *s)
{
  int sz=check_utf8_ptr(s,get_utf8_size(*s));
  while (sz > 0)
    if (*s == NUL) return 1;
    else {
      s=s+sz; sz=check_utf8_ptr(s,get_utf8_size(*s));}
  return 0;
}

static write_hex_form(uchar *in,char *out,int in_size,int out_size)
{
  char *ptr=out; uchar *scan=in;
  sprintf(ptr,"0x"); ptr=ptr+2;
  while (((ptr-out)+2 < out_size-1) && ((scan-in) < in_size)) {
    sprintf(ptr,"%02x",*scan); ptr=ptr+2; scan++;}
}

static int _uni_sgetc(fd_u8char **ss)
{
  fd_u8char *start=*ss;
  int i, ch;
  int size=get_utf8_size(**ss), check=check_utf8_ptr(*ss,size);
  if (size < 0) {
    char buf[16]; sprintf(buf,"%02x",**ss);
    fd_warn(_("Unexpected UTF-8 continuation [0x%s]"),buf); (*ss)++;
    return 0xFFFD;}
  else if (check < 0) {
    char buf[128];
    write_hex_form(start,buf,size+1,128); *ss=*ss+(-check);
    fd_warn(_("%d-byte UTF-8 sequence ends after %d bytes [%s]"),
	    size,-check,buf);
    return 0xFFFD;}
  else if (size == 1) ch=**ss;
  else if (size == 2) ch=**ss&0x1F;
  else if (size == 3) ch=**ss&0x0F;
  else if (size == 4) ch=**ss&0x07;
  else if (size == 5) ch=**ss&0x3;
  else {assert(size == 6); ch=**ss&0x1;}
  i=size-1; (*ss)++;
  while (i) {
    ch=(ch<<6)|(**ss&0x3F); (*ss)++; i--;}
  return ch;
}

FASTOP int uni_sgetc(fd_u8char **ss)
{
  if (**ss == '\0') return -1;
  else if (**ss < 0x80) return *((*ss)++);
  else return _uni_sgetc(ss);
}

DTYPES_EXPORT
/* fd_valid_utf8p:
    Arguments: a possible utf8 string
    Returns: 1 if the string is valid, 0 otherwise.
*/
int fd_valid_utf8p(fd_u8char *s)
{
  return valid_utf8p(s);
}

/** Multi-byte I/0 **/

FASTOP int uni_fgetc(FILE *f,mb2wc_fn mb2wc,int flags,struct FD_XFILE *xf)
{
  /* Handle the simple, ascii case without calling fread (if you can). */
  if ((xf->in_size > 0) && (xf->in[0] < 0x80) &&
      (flags&FD_ENCODING_INCLUDES_ASCII)) {
    int c=xf->in[0];
    memmove(xf->in,xf->in+1,xf->in_size-1); xf->in_size--;
    return c;}
  /* If it's not a simple case, (try to) fill up the buffer
     with 8 bytes of data which you'll use to get a single byte . */
  else if (xf->in_size < 8) {
    int cur=xf->in_size, delta;
    delta=fread_noblock(f,xf->in+cur,8-cur);
    if (delta) xf->in_size=cur+delta;
    else if (xf->in_size) {
      xchar ch; int sz=mb2wc(&ch,xf->in,xf->in_size);
      if (sz < 0) return -1;
      else {
	memmove(xf->in,xf->in+sz,xf->in_size-sz);
	xf->in_size=xf->in_size-sz;
	return ch;}}
    else return -1;}
  /* Return EOF if you can't get anything */
  if (xf->in_size < 1) return -1; 
  else {
    xchar result;
    int sz=mb2wc(&result,xf->in,xf->in_size);
    if (sz > 0) {
      memmove(xf->in,xf->in+sz,xf->in_size-sz);
      xf->in_size=xf->in_size-sz;
      return result;}
    else {
      int skip;
      if (sz == 0)  skip=1; else skip=-sz;
      memmove(xf->in,xf->in+skip,xf->in_size-skip);
      xf->in_size=xf->in_size-skip;
      return 0xFFFD;}}
}

/* Returns number of bytes written or -1 if the character can't
   be represented. */
FASTOP int uni_fputc(xchar c,FILE *f,wc2mb_fn wc2mb,int flags)
{
  if ((c < 0x80) && (flags&FD_ENCODING_INCLUDES_ASCII)) {
    putc(c,f); return 1;}
  else {
    char buf[16]; int n_bytes=wc2mb(buf,c);
    if (n_bytes > 0) {
      fwrite(buf,sizeof(char),n_bytes,f); return n_bytes;}
    else return -1;}
}
  
/** XFILE putc and getc **/

/* This gets the code in charset for the character c */
FASTOP int lookup_unicode(int c,unichar_t *charset)
{
  if (charset == NULL) return c;
  else if ((c < 0x80) && (charset[256] == 0)) return c;
  else {
    int i=0; while (i < 0x100)
      if (c == charset[i]) return i; else i++;
    return -1;}
}

static void cant_write(int c,struct FD_TEXT_ENCODING *e)
{
  lisp ch=CODE_CHAR(c);
  if (e)
    fd_raise_lisp_exception(fd_NoLocalChar,e->names[0],ch);
  else fd_raise_lisp_exception(fd_NoLocalChar,"unencoded",ch);
    fd_raise_lisp_exception
      ("Can't write wide character",e->names[0],ch);
}

/* This puts a character out to a file using the encoding on its
    XFILE struct.  If raw is zero, it will output unicode escapes. */
FASTOP void xfputc(int c,FILE *f,struct FD_XFILE *ei,int raw) 
{
  struct FD_TEXT_ENCODING *e;
  if (ei == NULL) e=default_encoding;
  else e=ei->encoding;
  if (e == NULL)
    if (c == 0)
      if (raw) fputc(c,f); else fprintf(f,"\\u0000");
    else if (c<0x80) fputc(c,f);
    else if (raw) cant_write(c,e);
    else if (c < 0x10000) fprintf(f,"\\u%04x",c);
    else fprintf(f,"\\U%08x",c);
  else if (e->charset)
    if ((c == 0) && (!(raw))) fprintf(f,"\\u0000");
    else if ((c<0x80) && (e->flags&FD_ENCODING_INCLUDES_ASCII))
      fputc(c,f);
    else {
      char buf[8]; int size=table_wc2mb(buf,c,e);
      if (size < 0)
	if (raw) cant_write(c,e);
	else if (c < 0x10000) fprintf(f,"\\u%04x",c);
	else fprintf(f,"\\U%08x",c);
      else {buf[size]='\0'; fputs(buf,f);}}
  else if (e->wc2mb) {
    if (uni_fputc(c,f,e->wc2mb,0) < 0) {
      if (raw) cant_write(c,e);
      else if (c < 0x10000) fprintf(f,"\\u%04x",c);
      else fprintf(f,"\\U%08x",c);}}
  else if ((c == 0) && (!(raw))) fprintf(f,"\\u0000");
  else if (c < 0x80) fputc(c,f);
  else if (raw) cant_write(c,e);
  else if (c < 0x10000) fprintf(f,"\\u%04x",c);
  else fprintf(f,"\\U%08x",c);
}

static int xfgetc_raw(FILE *f,struct FD_XFILE *ei) 
{
  struct FD_TEXT_ENCODING *e;
  if (ei == NULL) return getc(f);
  else if (ei->last_char >= 0) {
    int c=ei->last_char; ei->last_char=-1; return c;}
  else if  (ei->encoding == NULL) return getc(f);
  else e=ei->encoding;
  if (e->charset) {
    int c=getc(f);
    if (c < 0) return c;
    else if ((c<0x80) && (e->flags&FD_ENCODING_INCLUDES_ASCII)) return c;
    else if (c>=0x100) fd_raise_exception(_("GETC returned non-byte"));
    else if ((e->flags)&(FD_ENCODING_IS_LINEAR)) return e->charset[c].to;
    else {
      char buf[8]; int i=0; xchar result;
      buf[i++]=c; buf[i]='\0';
      while (table_mb2wc(&result,buf,i,e) < 0) {
	c=getc(f); if (c < 0) return c;
	else if (i == 4) {
	  char errbuf[128];
	  sprintf(errbuf,"%s: ",e->names[0]);
	  write_hex_form(buf,errbuf,4,128-strlen(e->names[0])-3);
	  fd_raise_detailed_exception(fd_BadMultiByteChar,errbuf);}
	else {buf[i++]=c; buf[i]='\0';}}
      return result;}}
  else if (e->mb2wc) 
    return uni_fgetc(f,e->mb2wc,e->flags,ei);
  else return fgetc(f);
}

FASTOP int xfgetc_encoded(FILE *f,struct FD_XFILE *ei) 
{
  int c1;
  if (ei == NULL) c1=getc(f);
  else if (ei->last_char >= 0) {
    /* if there is an e->last_char, it has already gone through
       encoding, so we take it directly. */
    int c=ei->last_char; ei->last_char=-1; return c;}
  else c1=xfgetc_raw(f,ei);
  if (c1 == '\\') {
    int c2=xfgetc_raw(f,ei); char buf[16];
    if ((c2 == 'u') || (c2 == 'U')) {
      int code, r; char buf[16];
      int i=0, lim=((c2 == 'U') ? 8 : 4);
      CLEAR_ERR();
      while (i < lim) {
	int c=xfgetc_raw(f,ei);
	if ((c<0x80) && (isxdigit(c))) buf[i++]=c;
	else {
	  memmove(buf+1,buf,i); buf[0]='"';
	  buf[i+1]=c; buf[i+2]='"'; buf[i+3]=NUL;
	  fd_raise_detailed_exception(fd_InvalidUnicodeEscape,buf);}}
      buf[lim]=NUL;
      code=strtol(buf,NULL,16);
      if (errno == 0) return code;
      else fd_raise_exception(fd_InvalidUnicodeEscape);}
    else if (c2 == 'U') {
      int code, r; r=fscanf(f,"%8x",&code);
      if (r == 1) return code;
      else fd_raise_exception(fd_InvalidUnicodeEscape);}
     else {
      if (ei) ei->last_char=c2; else ungetc(c2,f);
      return c1;}}
  else if (c1 == '\r') {
    int c2=xfgetc_raw(f,ei);
    if (c2 == '\n') return c2;
    else {fd_xungetc(c2,ei); return c1;}}
  else return c1;
}

DTYPES_EXPORT
fd_u8char *fd_read_line(FILE *f,int *size)
{
  struct FD_STRING_STREAM ss;
  struct FD_XFILE *ei=get_xfile(f);
  int c=xfgetc_raw(f,ei);
  FD_INITIALIZE_STRING_STREAM(&ss,256);
  while ((c>=0) && (c != '\n') && (c != '\r')) {
    fd_sputc(&ss,c); c=xfgetc_raw(f,ei);}
  *size=ss.size; return ss.ptr;
}

/** External functions **/

DTYPES_EXPORT
/* fd_fputc:
     Arguments: a wide char and a FILE pointer
     Returns: void
Writes the character to the FILE stream
*/
void fd_fputc(xchar c,FILE *f) 
{
  xfputc(c,f,get_xfile(f),1);
}

DTYPES_EXPORT
/* fd_xputc:
     Arguments: a wide char and an XFILE pointer
     Returns: void
Writes the character to XFILE stream, signalling
an error if the stream cannot handle the character.
*/
void fd_xputc(xchar c,struct FD_XFILE *f) 
{
  xfputc(c,f->f,f,1);
}

DTYPES_EXPORT
/* fd_xputc_encoded:
     Arguments: a wide char and an XFILE pointer
     Returns: void
Writes the character to XFILE stream, using unicode
escapes if the stream cannot handle the character.
*/
void fd_xputc_encoded(xchar c,struct FD_XFILE *f) 
{
  xfputc(c,f->f,f,0);
}

DTYPES_EXPORT
/* fd_fgetc:
     Arguments: a FILE pointer
     Returns: a wide character

Returns a wide character from a stream according to its encoding.
*/
int fd_fgetc(FILE *f) 
{
  return xfgetc_raw(f,get_xfile(f));
}

DTYPES_EXPORT
/* fd_xgetc:
     Arguments: an XFILE pointer
     Returns: a wide character

Returns a wide character from a stream according to its encoding.
This will *not* interpret \u and \U escapes in the file.
*/
int fd_xgetc(struct FD_XFILE *f) 
{
  int c=xfgetc_raw(f->f,f);
  if (c == '\r') {
    int nc=xfgetc_raw(f->f,f);
    if (nc == '\n') return nc;
    else {
      fd_xungetc(nc,f); return c;}}
  else return c;
}

DTYPES_EXPORT
/* fd_xgetc_encoded:
     Arguments: an XFILE pointer
     Returns: a wide character

Returns a wide character from a stream according to its encoding.
This will interpret \u and \U escapes in the file.
*/
int fd_xgetc_encoded(struct FD_XFILE *f)
{
  return xfgetc_encoded(f->f,f);
}

DTYPES_EXPORT
/* fd_xgets:
     Arguments: an XFILE pointer
     Returns: a wide character

Returns a wide character from a stream according to its encoding.
This will interpret \u and \U escapes in the file.
*/
fd_u8char *fd_xgets(fd_u8char *b,int size,struct FD_XFILE *xf)
{
  FILE *f=xf->f;
  struct FD_TEXT_ENCODING *enc=
    ((xf) ? (xf->encoding) : (fd_get_default_encoding()));
  struct FD_STRING_STREAM out; char buf[2048]; int bytes_read;
  if (buf) {
    FD_INITIALIZE_FIXED_STRING_STREAM(&out,size,b);}
  else {FD_INITIALIZE_STRING_STREAM(&out,size);}
  while (fgets(buf,2048,f)) {
    int len=strlen(buf); 
    if (buf[len-1]=='\n') {
      fd_write_utf8(&out,buf,buf+len-1,enc); break;}
    else fd_write_utf8(&out,buf,buf+len-1,enc);}
  return out.ptr;
}

DTYPES_EXPORT
/* fd_xputs_raw:
     Arguments: a utf8 string, a length, and an XFILE pointer
     Returns: nothing

Writes the contents of the string to the XFILE, signalling
an error if the stream does not accept any of the characters
in the XFILE.
*/
void fd_xputs_raw(fd_u8char *s,int len,struct FD_XFILE *e)
{
  if ((e) ? (e->encoding == utf8_encoding) :
      (default_encoding == utf8_encoding)) {
    int to_go=len; fd_u8char *scan=s;
    while (to_go) {
      int n_bytes=fwrite(scan,sizeof(char),to_go,e->f);
      to_go=to_go-n_bytes; scan=scan+n_bytes;}}
  else {
    fd_u8char *limit=s+len; FILE *f=e->f;
    while (s < limit)
      if (*s) {int c=uni_sgetc(&s); xfputc((xchar)c,f,e,1);}
      else xfputc((xchar)*s++,f,e,0);}
}

DTYPES_EXPORT
/* fd_fputs_raw:
     Arguments: a utf8 string, a length, and FILE pointer
     Returns: nothing

Writes the contents of the string to the FILE, signalling
an error if the stream does not accept any of the characters
in the XFILE.
*/
void fd_fputs_raw(fd_u8char *s,int len,FILE *f)
{
  struct FD_XFILE *e=get_xfile(f);
  if ((e) ? (e->encoding == utf8_encoding) :
      (default_encoding == utf8_encoding)) {
    int to_go=len; fd_u8char *scan=s;
    while (to_go) {
      int n_bytes=fwrite(scan,sizeof(char),to_go,f);
      to_go=to_go-n_bytes; scan=scan+n_bytes;}}
  else {
    fd_u8char *limit=s+len;
    while (s < limit)
      if (*s) {int c=uni_sgetc(&s); xfputc((xchar)c,f,e,1);}
      else xfputc((xchar)*s++,f,e,0);}
}

DTYPES_EXPORT
/* fd_xputs_encoded:
     Arguments: a utf8 string, a length, and an XFILE pointer
     Returns: nothing

Writes the contents of the string to the XFILE, writing
unhandled characters with unicode (\u and \U escapes).
*/
void fd_xputs_encoded(fd_u8char *s,int len,struct FD_XFILE *e)
{
  if ((e) ? (e->encoding == utf8_encoding) :
      (default_encoding == utf8_encoding)) {
    int to_go=len; fd_u8char *scan=s;
    while (to_go) {
      int n_bytes=fwrite(scan,sizeof(char),to_go,e->f);
      to_go=to_go-n_bytes; scan=scan+n_bytes;}}
  else {
    fd_u8char *limit=s+len; FILE *f=e->f;
    while (s < limit) {
      int c=uni_sgetc(&s);
      if (*s == 0)
	if (s < limit)
	  fd_raise_detailed_exception(fd_BadUTF8,_("unexpected NULL"));
      xfputc((xchar)c,f,e,0);}}
}

DTYPES_EXPORT
/* fd_fputs_encoded:
     Arguments: a utf8 string, a length, and FILE pointer
     Returns: nothing

Writes the contents of the string to the FILE, writing
unhandled characters with unicode (\u and \U escapes).
*/
void fd_fputs_encoded(fd_u8char *s,int len,FILE *f)
{
  struct FD_XFILE *e=get_xfile(f);
  if ((e) ? (e->encoding == utf8_encoding) :
      (default_encoding == utf8_encoding)) {
    int to_go=len; fd_u8char *scan=s;
    while (to_go>0) {
      int n_bytes=fwrite(scan,sizeof(char),to_go,f);
      to_go=to_go-n_bytes; scan=scan+n_bytes;}}
  else {
    fd_u8char *limit=s+len;
    while (s < limit) {
      int c=uni_sgetc(&s);
      if (*s == 0)
	if (s < limit)
	  fd_raise_detailed_exception(fd_BadUTF8,_("unexpected NULL"));
      xfputc((xchar)c,f,e,0);}}
}

DTYPES_EXPORT
/* fd_ungetc:
     Arguments: a wide character and a FILE pointer
     Returns: nothing

Ungets the character on the stream, using its XFILE
structure if possible.
*/
void fd_ungetc(int c,FILE *f)
{
  struct FD_XFILE *e=get_xfile(f);
  if (e == NULL) ungetc(c,f);
  else if (e->last_char >= 0)
    fd_raise_exception(_("Can only ungetc once"));
  else e->last_char=c;
}

DTYPES_EXPORT
/* fd_xungetc:
     Arguments: a wide character and an XFILE pointer
     Returns: nothing

Ungets the character on an XFILE stream.
*/
void fd_xungetc(int c,struct FD_XFILE *e)
{
  if (e->last_char >= 0) fd_raise_exception(_("Can only ungetc once"));
  else if (c < 0) fd_raise_exception(fd_InvalidUnicodeChar);
  else e->last_char=c;
}

/** Converting strings **/

DTYPES_EXPORT
/* fd_make_utf8:
     Arguments: start and end pointers to an 8BIT string representation
       and a text encoding
     Returns: a utf8 encoded string
If the end pointer is NULL, it is set to the end of the string.
*/
fd_u8char *fd_make_utf8(uchar *start,uchar *end,struct FD_TEXT_ENCODING *e)
{
  if (e) {
    uchar *string=start; struct FD_MB_MAP *charset=e->charset;
    int includes_ascii=(e->flags&FD_ENCODING_INCLUDES_ASCII);
    struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,64);
    if (end == NULL) end=start+strlen(start);
    if ((charset)&&(e->flags&(FD_ENCODING_IS_LINEAR)))
      while (string<end) {
	if ((includes_ascii) && (*string < 0x80)) {
	  fd_sputc(&ss,*string); string++;}
	else {fd_sputc(&ss,charset[*string].to); string++;}}
    else if (charset) {
      uchar *scan=string; xchar c, l;
      while (scan<end) {
	l=table_mb2wc(&c,scan,16,e);
	if (l < 0) {
	  fd_warn(_("%s string ended early"),e->names[0]); break;}
	else {fd_sputc(&ss,c); scan=scan+l;}}}
    else if (e->mb2wc) {
      uchar *scan=string; xchar c; int l;
      mb2wc_fn mb2wc=e->mb2wc;
      while (scan<end) {
	l=mb2wc(&c,scan,16);
	if (l < 0) {
	  fd_warn(_("%s string ended early"),e->names[0]); break;}
	else {fd_sputc(&ss,c); scan=scan+l;}}}
    else {
      unsigned char *scan=start; 
      while (scan<end)
	if (*scan < 0x80) fd_sputc(&ss,*scan++);
	else fd_raise_detailed_exception(fd_InvalidChar,"ascii");}
    return ss.ptr;}
  else {
    int len=((end==NULL) ? (strlen(start)) : (end-start));
    fd_u8char *copy=fd_xmalloc(len+1);
    strncpy(copy,start,len); copy[len]=NUL;
    if (valid_utf8p(copy)) return copy;
    else {
      fd_xfree(copy);
      return fd_make_utf8(start,end,latin1_encoding);}}
}

DTYPES_EXPORT
/* fd_write_utf8:
     Arguments: a string stream, start and end pointers to an 8BIT text string,
and a pointer to the text encoding for the string
     Returns: a utf8 encoded string
*/
int fd_write_utf8
  (struct FD_STRING_STREAM *ssp,uchar *start,uchar *end,
   struct FD_TEXT_ENCODING *e)
{
  uchar *string=start; struct FD_MB_MAP *charset=e->charset;
  int includes_ascii=(e->flags&FD_ENCODING_INCLUDES_ASCII);
  int chars_read=0;
  if (e == utf8_encoding) {
    fd_u8char *scan=start, *limit=end;
    while (scan < limit) {
      if (*scan < 0x80) scan++;
      else if (*scan < 0xc0) {
	fd_warn(_("Unexpected continuation character 0x%x"),
		*scan);
	fd_sputn(ssp,start,scan-start);
	fd_sputc(ssp,0xFFFD); fd_sputc(ssp,*scan);
	scan++; start=scan;}
      else if (*scan < 0xE0) scan=scan+2;
      else if (*scan < 0xF0) scan=scan+3;
      else if (*scan < 0xF8) scan=scan+4;
      else if (*scan < 0xFC) scan=scan+5;     
      else if (*scan < 0xFE) scan=scan+6;
      else scan++;
      chars_read++;}
    fd_sputn(ssp,start,end-start);}
  else if ((charset)&&(e->flags&(FD_ENCODING_IS_LINEAR)))
    while (string<end) {
      if ((includes_ascii) && (*string < 0x80)) {
	fd_sputc(ssp,*string); string++; chars_read++;}
      else {fd_sputc(ssp,charset[*string].to); string++; chars_read++;}}
  else if (charset) {
    uchar *scan=string; xchar c, l;
    while (scan<end) {
      l=table_mb2wc(&c,scan,16,e);
      if (l < 0) {
	fd_warn(_("%s string ended early"),e->names[0]); break;}
      else {fd_sputc(ssp,c); scan=scan+l; chars_read++;}}}
  else if (e->mb2wc) {
    uchar *scan=string; xchar c; int l;
    mb2wc_fn mb2wc=e->mb2wc;
    while (scan<end) {
      l=mb2wc(&c,scan,16);
      if (l < 0) {
	fd_warn(_("%s string ended early"),e->names[0]); break;}
      else {fd_sputc(ssp,c); scan=scan+l; chars_read++;}}}
  else {
    unsigned char *scan=start; 
    while (scan<end)
      if (*scan < 0x80) {fd_sputc(ssp,*scan++); chars_read++;}
      else fd_raise_detailed_exception(fd_InvalidChar,"ascii");}
  return chars_read;
}

DTYPES_EXPORT
fd_u8char *fd_xstring(uchar *string)
{
  if (default_encoding == ascii_encoding)
    return fd_make_utf8(string,NULL,latin1_encoding);
  else return fd_make_utf8(string,NULL,default_encoding);
}

DTYPES_EXPORT
char *fd_make_os_string(fd_u8char *string)
{
  if (system_encoding)
    return fd_localize_utf8(string,system_encoding);
  else return fd_strdup(string);
}

DTYPES_EXPORT
fd_u8char *fd_convert_os_string(char *string)
{
  if (system_encoding)
    return fd_make_utf8(string,NULL,system_encoding);
  /* if you don't know the system encoding, we'll assume
     utf8, but only if the string is valid; */
  else if (valid_utf8p(string))
    return fd_strdup(string);
  /* otherwise, we'll go with latin1 */
  else return fd_make_utf8(string,NULL,latin1_encoding);
}

DTYPES_EXPORT
/* fd_interpret_unicode_escapes:
     Arguments: a utf8 string with (potentially) embedded unicode escapes
     Returns: a utf8 string where those escapes have been expanded
*/
fd_u8char *fd_interpret_unicode_escapes(fd_u8char *string)
{
  struct FD_STRING_STREAM ss; int c;
  FD_INITIALIZE_STRING_STREAM(&ss,128);
  while ((c=fd_sgetc(&string))>0)
    if (c == '\\') {
      int nc=fd_sgetc(&string);
      if (nc == 'u') {
	int code;
	if (sscanf(string,"%4x",&code) != 1)
	  fd_raise_detailed_exception(fd_InvalidUnicodeEscape,string);
	fd_sputc(&ss,code);
	string=string+4;}
      else if (nc == 'U') {
	int code;
	if (sscanf(string,"%8x",&code) != 1)
	  fd_raise_detailed_exception(fd_InvalidUnicodeEscape,string);
	fd_sputc(&ss,code);
	string=string+8;}
      else {fd_sputc(&ss,'\\'); fd_sputc(&ss,nc);}}
    else fd_sputc(&ss,c);
  return ss.ptr;
}

DTYPES_EXPORT
/* fd_convert_utf8:
     Arguments: a utf8 encoded string and a text encoding
     Returns: a regular string

  Returns an 8BIT string encoded using the text encoding.
*/
unsigned char *fd_convert_utf8(fd_u8char *string,int slen,struct FD_TEXT_ENCODING *e,int *size_loc)
{
  if (e) {
    struct FD_MB_MAP *inv=e->charset_inv; int c;
    int len=((slen<0) ? (strlen(string)) : slen);
    char *new=fd_xmalloc(len*6), *write=new; fd_u8char *limit=string+len;
    while (string<limit) {
      c=uni_sgetc(&string);
      if (c < 0) c=0;
      if ((c<0x80)&&(e->flags&(FD_ENCODING_INCLUDES_ASCII))) *write++=c;
      else if (inv) {
	int size=table_wc2mb(write,(xchar)c,e);
	if (size < 0) cant_write(c,e);
	else write=write+size
;}
      else {
	wc2mb_fn wc2mb=e->wc2mb; int l;
	if (wc2mb == NULL) wc2mb=(wc2mb_fn)wctomb;
	l=wc2mb(write,(xchar)c); write=write+l;}}
    if (size_loc) *size_loc=write-new;
    *write++='\0'; /* Null terminate it */
    return new;}
  else if (size_loc) {
    int len=((slen<0) ? (strlen(string)) : slen);
    fd_u8char *copy=fd_xmalloc(len+1);
    strncpy(copy,string,len); copy[len]=NUL;
    *size_loc=len; return copy;}
  else {
    int len=((slen<0) ? (strlen(string)) : slen);
    fd_u8char *copy=fd_xmalloc(len+1);
    strncpy(copy,string,len); copy[len]=NUL;
    return copy;}
}

DTYPES_EXPORT
/* fd_localize_utf8:
     Arguments: a utf8 encoded string and a text encoding
     Returns: a regular string

  Returns an 8BIT string encoded using the text encoding.
*/
unsigned char *fd_localize_utf8(fd_u8char *string,struct FD_TEXT_ENCODING *e)
{
  return fd_convert_utf8(string,strlen(string),e,NULL);
}

/** some standard encodings **/

/* UTF-8 encoding */
static int utf8towc(xchar *o,uchar *s,size_t n)
{
  uchar *start=s;
  int size=get_utf8_size(*s);
  if (size == 1) {*o=*s; return 1;}
  else {
    *o=_uni_sgetc(&s);
    return s-start;}
}

static int wctoutf8(fd_u8char *o,xchar ch)
{
  uchar off[6]={0x00,0xC0,0xE0,0xF0,0xF8,0xFC};
  int i, size=0; uchar buf[6];
  if (ch < 0x80) {size=1; buf[0]=(unsigned char)ch;}
  else {
    buf[size++]=(ch&0x3F)|0x80; ch=ch>>6;
    while (ch) {
      buf[size++]=(ch&0x3F)|0x80; ch=ch>>6;}
    buf[size-1]=off[size-1]|buf[size-1];}
  i=size-1; while (i>=0) *o++=buf[i--];
  return size;
}

/* UTF-16 encoding */
static int utf16towc(xchar *o,uchar *i,size_t n)
{
  *o=(i[0]<<8)|(i[1]);
  return 2;
}

static int wctoutf16(uchar *buf,xchar ch)
{
  buf[0]=(ch>>8); buf[1]=ch&0xff; return 2;
}

/** filestring with text encoding **/

DTYPES_EXPORT
/* fd_foreign_filestring:
     Arguments: a filename (a localized string) and a text encoding
     Returns: a utf-8 string

 Interprets the contents of the file according to the encoding and
 returns a UTF-8 encoded unicode string. */
lisp fd_foreign_filestring(char *filename,struct FD_TEXT_ENCODING *e)
{
  struct FD_XFILE xfs;
  struct FD_STRING_STREAM ss; FILE *f; int ch;
  FD_INITIALIZE_STRING_STREAM(&ss,1024);
  f=fd_fopen(filename,"r"); 
  if (f == NULL) 
    fd_raise_detailed_exception(fd_FileOpenFailed,filename);
  else {
    fd_init_xfile(&xfs,f,e);
    while ((ch=xfgetc_raw(f,&xfs)) >= 0) fd_sputc(&ss,ch);}
  fd_fclose(f);
  return fd_init_string(ss.ptr,ss.size);
}

static int compute_flags(struct FD_MB_MAP *chset,int size)
{
  int includes_ascii=1, linear=1, i=1, j=1;
  sort_charset(chset,size);
  if (size >= 256) {
    while (i < 128) {
      if ((chset[i].to != i)||(chset[i].from != i)) {
	includes_ascii=0; break;}
      else i++;}}
  while (j < size)
    if (chset[j].from != j) {
      linear=0; break;}
    else j++;
  if ((includes_ascii) && (linear))
    return ((FD_ENCODING_INCLUDES_ASCII) |
	    (FD_ENCODING_IS_LINEAR));
  else if (includes_ascii) return (FD_ENCODING_INCLUDES_ASCII);
  else if (linear) return (FD_ENCODING_IS_LINEAR);
  else return 0;
}

static int declare_charset(char *name,struct FD_MB_MAP *chset)
{
  int size=chset[0].from;
  sort_charset(chset+1,size);
  return fd_define_encoding
    (name,chset+1,size,NULL,NULL,compute_flags(chset+1,size));
}

/** Loading Encodings **/

void load_unicode_consortium_encoding(char *name,FILE *f)
{
  char buf[512];
  struct FD_MB_MAP *map=fd_malloc(sizeof(struct FD_MB_MAP)*256);
  int size=0, limit=256;
  while (fgets(buf,512,f) != NULL) {
    int from, to;
    if (sscanf(buf,"0x%x\t0x%x",&from,&to) == 2) {
      if (size >=limit) {
	map=fd_realloc(map,sizeof(struct FD_MB_MAP)*(limit+256),
		       sizeof(struct FD_MB_MAP)*(limit));
	limit=limit+256;}
      map[size].from=from; map[size].to=to; size++;}}
  fd_fclose(f);
  if (fd_define_encoding(name,map,size,NULL,NULL,compute_flags(map,size)) == 0)
    /* If the coding was alredy defined, free the map you just made. */
    fd_free(map,sizeof(struct FD_MB_MAP)*limit);
}

/* This reads the byte sequence for a charmap file */
static unsigned int parse_seq(char *start,char *end)
{
  if (end-start == 4) {
    int one; sscanf(start,"/x%2x",&one); return one;}
  else if (end-start == 8) {
    int one, two;
    sscanf(start,"/x%2x/x%2x",&one,&two);
    return (one<<8)+two;}
  else if (end-start == 12) {
    int one, two, three;
    sscanf(start,"/x%2x/x%2x/x%2x",&one,&two,&three);
    return (one<<16)+(two<<8)+three;}
  else if (end-start == 16) {
    int one, two, three, four;
    sscanf(start,"/x%2x/x%2x/x%2x/x%2x",&one,&two,&three,&four);
    return (one<<24)+(two<<16)+(three<<8)+four;}
  else fd_raise_exception("Two many bytes in char");
}

void load_charmap_encoding(char *name,FILE *f)
{
  /* We don't actually parse the headers */
  char buf[512]; char *aliases[64];
  struct FD_MB_MAP *map=fd_malloc(sizeof(struct FD_MB_MAP)*256);
  int size=0, limit=256, n_aliases=0;
  /* Find where the charmap starts */
  while (fgets(buf,512,f) != NULL)
    if (strcmp(buf,"CHARMAP\n") == 0) break;
    else if (strncmp(buf,"<code_set_name> ",16) == 0) {
      if ((strcmp(name,buf+16)) == 0) {
	if (n_aliases >= 64) fd_raise_exception(_("Too many charmap aliases"));
	else {
	  char *copy=fd_strdup(buf+16);
	  int len=strlen(copy);
	  if (copy[len] == '\n') copy[len]=0;
	  aliases[n_aliases++]=copy;}}}
    else if (strncmp(buf,"% alias ",8) == 0) {
      if (n_aliases >= 64) fd_raise_exception(_("Too many charmap aliases"));
      else {
	char *copy=fd_strdup(buf+8);
	int len=strlen(copy);
	if (copy[len] == '\n') copy[len]=0;
	aliases[n_aliases++]=copy;}}
    else continue;
  /* Read the entries */
  while (fgets(buf,512,f) != NULL) {
    char *seq_start=strstr(buf,"/x"), *seq_end=NULL, *code_start=NULL;
    unsigned int from, to;
    if (seq_start) {
      char *ns=strchr(seq_start,' '), *nt=strchr(seq_start,'\t');;
      if (ns == NULL) seq_end=nt;
      else if (nt == NULL) seq_end=ns;
      else if (ns < nt) seq_end=ns; else seq_end=nt;}
    if (seq_end) code_start=strstr(buf,"<U");
    if (code_start == NULL) {
      if (strncmp(buf,"END CHARMAP",11) == 0) break;
      else continue;}
    from=parse_seq(seq_start,seq_end);
    sscanf(code_start+2,"%x>",&to);
    if (size >=limit) {
      map=fd_realloc(map,sizeof(struct FD_MB_MAP)*(limit+256),
		     sizeof(struct FD_MB_MAP)*(limit));
      limit=limit+256;}
    map[size].from=from; map[size].to=to; size++;}
  fd_fclose(f);
  {
    int i=0, flags=compute_flags(map,size);
    int map_used=fd_define_encoding(name,map,size,NULL,NULL,flags);
    while (i < n_aliases) {
      if (fd_define_encoding(aliases[i],map,size,NULL,NULL,flags))
	map_used=1;
      fd_xfree(aliases[i]); i++;}
    if (map_used == 0)
      fd_free(map,sizeof(struct FD_MB_MAP)*limit);}
}

fd_u8char *upcase_string(fd_u8char *string,int len)
{
  struct FD_STRING_STREAM out; int c;
  FD_INITIALIZE_STRING_STREAM(&out,256);
  if (len < 0) 
    while ((c=uni_sgetc(&string))>=0) {
      int upper=fd_toupper(c); fd_sputc(&out,upper);}
  else {
    fd_u8char *scan=string, *limit=scan+len;
    while (scan < limit) {
      int ch=uni_sgetc(&scan);
      fd_sputc(&out,fd_toupper(ch));}}
  return out.ptr;
}

fd_u8char *downcase_string(fd_u8char *string,int len)
{
  struct FD_STRING_STREAM out; int c;
  FD_INITIALIZE_STRING_STREAM(&out,256);
  if (len < 0) 
    while ((c=uni_sgetc(&string))>=0) {
      int lower=fd_tolower(c); fd_sputc(&out,lower);}
  else {
    fd_u8char *scan=string, *limit=scan+len;
    while (scan < limit) {
      int ch=uni_sgetc(&scan);
      fd_sputc(&out,fd_tolower(ch));}}
  return out.ptr;
}

DTYPES_EXPORT
/* fd_upcase_string:
     Arguments: a utf8 string
     Returns: a copy of the argument converted to upper case
*/
fd_u8char *fd_upcase_string(fd_u8char *string,int len)
{
  return upcase_string(string,len);
}

DTYPES_EXPORT
/* fd_downcase_string:
     Arguments: a utf8 string
     Returns: a copy of the argument converted to lower case
*/
fd_u8char *fd_downcase_string(fd_u8char *string,int len)
{
  return downcase_string(string,len);
}

/* This assumes that the name does not contain any internal NULs */
fd_u8char *standardize_encoding_name(fd_u8char *string)
{
  struct FD_STRING_STREAM out; int c;
  FD_INITIALIZE_STRING_STREAM(&out,256);
  while ((c=uni_sgetc(&string))>0)
    if (fd_isalnum(c)) {
      int upper=fd_toupper(c); fd_sputc(&out,upper);}
  return out.ptr;
}

static struct FD_TEXT_ENCODING *try_to_load_encoding(char *name)
{
  lisp encodings_path=fd_getenv("ENCODINGS_PATH");
  if (!(FD_VOIDP(encodings_path)))  {
    char *found=fd_find_file(name,encodings_path);
    if (found == NULL) {
      char *std=standardize_encoding_name(name);
      found=fd_find_file(std,encodings_path);
      if (found == NULL) {
	/* One last try */
	int trylen=strlen(FRAMERD_SHARE_DIR)+strlen(std)+20;
	char *try=fd_malloc(trylen);
	sprintf(try,"%s/encodings/%s",FRAMERD_SHARE_DIR,std);
	if (fd_file_existsp(try)) found=try;
	else fd_free(try,trylen);}
      fd_xfree(std);}
    if (found) {
      fd_load_encoding(name,found); fd_xfree(found);
      return fd_get_encoding(name);}
    else return NULL;}
}

DTYPES_EXPORT
/* fd_load_encoding:
     Arguments: a name and a filename
     Returns: void

Defines a text encoding based on a text file of byte sequence to
unicode mappings.  This interprets the standard mappings files provided
by the Unicode consortium at ftp://ftp.unicode.org/Public/MAPPINGS/.
*/
void fd_load_encoding(char *name,char *file)
{
  FILE *f=fd_fopen(file,"r"); char buf[512];
  if (f == NULL)
    fd_raise_detailed_exception(fd_FileOpenFailed,file);
  else fd_notify(_("Loading text encoding %s from %s"),name,file);
  fgets(buf,512,f); fseek(f,0,SEEK_SET);
  if (strncmp(buf,"<code_set_name>",strlen("<code_set_name")) == 0)
    load_charmap_encoding(name,f);
  else load_unicode_consortium_encoding(name,f);
}

/** Initialization **/

void fd_initialize_i18n_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&xfile_mutex);
#endif
  init_chardata();

  fd_define_encoding
    ("ASCII",NULL,0,NULL,NULL,FD_ENCODING_INCLUDES_ASCII);
  fd_define_encoding
    ("US-ASCII",NULL,0,NULL,NULL,FD_ENCODING_INCLUDES_ASCII);
  declare_charset("latin1",iso_8859_1_map);
  declare_charset("iso-latin1",iso_8859_1_map);
  declare_charset("iso-8859/1",iso_8859_1_map);
  declare_charset("latin2",iso_8859_2_map);
  declare_charset("iso-latin2",iso_8859_2_map);
  declare_charset("iso-8859/2",iso_8859_2_map);
  declare_charset("latin3",iso_8859_3_map);
  declare_charset("iso-latin3",iso_8859_3_map);
  declare_charset("iso-8859/3",iso_8859_3_map);
  declare_charset("latin4",iso_8859_4_map);
  declare_charset("iso-latin4",iso_8859_4_map);
  declare_charset("iso-8859/4",iso_8859_4_map);
  declare_charset("cyrillic",iso_8859_5_map);
  declare_charset("iso-8859/5",iso_8859_5_map);
  declare_charset("arabic",iso_8859_6_map);
  declare_charset("iso-8859/6",iso_8859_6_map);
  declare_charset("greek",iso_8859_7_map);
  declare_charset("iso-8859/7",iso_8859_7_map);
  declare_charset("hebrew",iso_8859_8_map);
  declare_charset("iso-8859/8",iso_8859_8_map);
  declare_charset("iso-8859/8I",iso_8859_8_map);
  declare_charset("latin5",iso_8859_9_map);
  declare_charset("iso-latin5",iso_8859_9_map);
  declare_charset("iso-8859/9",iso_8859_9_map);
  declare_charset("latin6",iso_8859_10_map);
  declare_charset("iso-latin6",iso_8859_10_map);
  declare_charset("iso-8859/10",iso_8859_10_map);
  declare_charset("latin7",iso_8859_13_map);
  declare_charset("iso-latin7",iso_8859_13_map);
  declare_charset("iso-8859/13",iso_8859_13_map);
  declare_charset("latin8",iso_8859_14_map);
  declare_charset("iso-latin8",iso_8859_14_map);
  declare_charset("iso-8859/14",iso_8859_14_map);
  declare_charset("iso-latin0",iso_8859_15_map);
  declare_charset("latin9",iso_8859_15_map);
  declare_charset("iso-latin9",iso_8859_15_map);
  declare_charset("iso-8859/15",iso_8859_15_map);
  declare_charset("latin0",iso_8859_15_map);

  declare_charset("koi-8",koi_8_map);

  fd_define_encoding("UTF-8",NULL,0,wctoutf8,utf8towc,
		     FD_ENCODING_INCLUDES_ASCII);
  fd_define_encoding("UTF/8",NULL,0,wctoutf8,utf8towc,
		     FD_ENCODING_INCLUDES_ASCII);
  fd_define_encoding("UTF/16",NULL,0,wctoutf16,utf16towc,0);
  fd_define_encoding("UTF-16",NULL,0,wctoutf16,utf16towc,0);
  fd_define_encoding("UCS-2",NULL,0,wctoutf16,utf16towc,0);
  fd_define_encoding("UCS/2",NULL,0,wctoutf16,utf16towc,0);
  
  ascii_encoding=fd_get_encoding("ASCII");
  utf8_encoding=fd_get_encoding("UTF-8");
  latin1_encoding=fd_get_encoding("LATIN-1");
  /* Assuming anything else can cause problems */
  default_encoding=fd_get_encoding(default_encoding_name);

  setup_decomposition_tables();
}
  

/* File specific stuff */

/* The CVS log for this file
   $Log: i18n.c,v $
   Revision 1.36  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.35  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.34  2004/04/15 22:14:38  haase
   Added flag to control chardata warnings

   Revision 1.33  2004/04/02 10:09:40  haase
   Made string conversion start with a smaller pre-allocated string

   Revision 1.32  2004/03/08 12:57:47  haase
   Raw returns in strings are not translated into \r

   Revision 1.31  2004/02/17 02:51:13  haase
   Indentation changes

   Revision 1.30  2004/02/16 21:54:09  haase
   Added fd_xgets with UTF8 optimization

   Revision 1.29  2004/02/04 21:38:16  haase
   Added primitives for validating and patching invalid UTF-8 strings

   Revision 1.28  2004/02/03 16:20:42  haase
   Optimized readline for handling UTF-8, added fd_write_utf8 for transcoding buffers in memory

   Revision 1.27  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.26  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.25.2.3  2003/08/02 13:45:55  haase
   Reimplemented XFILE lookup added more fd_x functions for using XFILE

   Revision 1.25.2.2  2003/01/26 20:56:05  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.25.2.1  2002/08/09 16:58:58  haase
   Fix fencepost error

   Revision 1.25  2002/05/12 13:14:06  haase
   Distinguish exceptions for invalid incoming characters from inexpressible outgoing characters

   Revision 1.24  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.23  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.22  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.21  2002/04/17 13:06:00  haase
   Fix error in downcasing ASCII strings

   Revision 1.20  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.19  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
