/* C Mode */

/* io.c
   Implements binary and ascii i/o of DType objects from/to
     both files and memory.
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

static char vcid[] = "$Id: io.c,v 1.27 2005/01/14 16:48:44 haase Exp $";

/** Initial declarations **/
/** Binary I/O Support for different OSs **/
/** Output to memory (Utilities) **/
/** Packaged types **/
/** Dtypes from/to memory **/
/** Dtypes from/to files **/
/** Dtypes size counting **/
/** Validate DTYPE **/
/** String stream support **/
/** Printing lisp objects to strings **/
/** Printout OIDs to strings **/
/** Top level output functions **/
/** Pretty printing **/
/** Parsing OID references **/
/** Parsing atoms **/
/** Parsing argument strings **/
/** Parsing LISP objects from strings **/
/** Parsing LISP objects from streams **/
/** Top level input functions **/
/** Getting at files **/
/** Loading CONFIG files **/
/** Initializing the IO package **/

#define FD_INLINE_CHARDATA 1
#define FD_INLINE_DBUF 1

#include "dtypes.h"

#include <time.h>
#include <stdarg.h>
#include <limits.h>
#ifdef OS2
#include <types.h>
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif

/** Initial declarations **/

fd_exception
  fd_Unexpected_EOF=_("Unexpected end of file while reading DTYPE"),
  fd_Unexpected_EOD=_("Unexpected End of DType data"),
  fd_Unexpected_NULL=_("Unexpected NULL in text stream"),
  fd_Unknown_Record_Type=_("Unknown (and unprintable) record type"),
  fd_NoDTypeRep=_("Object has no portable DType representation"),
  fd_InvalidDType=_("Reading an invalid DType representation"),
  fd_UnknownDTCode=_("Unknown DTYPE DTCode"),
  fd_ParseError=_("Error parsing LISP data");

static fd_exception CantLookupOIDName=_("Can't look up OID name");

static void ((*print_oid)(lisp x,fd_string_stream ss));
static lisp ((*parse_oid)(fd_u8char *string));
static fd_lisp ((*lookup_oid)(fd_lisp x))=NULL;

static lisp quote_symbol, backquote_symbol, unquote_symbol,
     unquote_splice_symbol, error_symbol, exception_symbol;

static unsigned int aliased_super_pool=0, super_pool_alias=0;

#define isodigit(x) ((isdigit(x)) && (x < '8'))
#define issymbreak(c) \
   ((fd_isspace(c)) || (c == '"') || (c == '(') || (c == '#') || \
    (c == '{') || (c == ')') || (c == '}') || (c == ']') || (c == ']'))
#define can_start_oid_tag(c) \
   ((c == '"') || (c == '(') || (c == '{'))

/** Binary I/O Support for different OSs **/

#if (defined(OS2) || defined(BEOS))
int getw (FILE * stream)
{
  int i;
  fread (&i, sizeof (int), 1, stream);
  return i;
}
int putw (int w, FILE * stream)
{
  fwrite (&w, sizeof (int), 1, stream);
}
#endif /* OS2 */

/* funny_type_code: (static)
    Arguments: a type code (for external representation)
    Returns: (shouldn't)
  If the code is the EOF marker, it probably means that 
   there was an unexpected end of file and that error is
   signalled; otherwise, a "funny type code" error is signalled.
*/
static lisp funny_type_code(int code)
{
  char buf[128];
  if (code == EOF)
    fd_raise_exception(fd_Unexpected_EOF);
  else {sprintf(buf,"%x",code); 
	fd_raise_detailed_exception(fd_UnknownDTCode,buf);}
  return (FD_EMPTY_CHOICE); /* Never reached */
}

FASTOP int asciip(unsigned char *string)
{
  while (*string)
    if (*string >= 0x80) return 0; else string++;
  return 1;
}

/* Unicode string I/O */

static int _uni_sgetc(fd_u8char **ss)
{
  int i, size=1, ch; fd_u8char *start=*ss;
  if (**ss == '\0') return -1;
  else if (**ss < 0x80) ch=**ss;
  else if (**ss < 0xc0) {
    char buf[16]; sprintf(buf,"0x%02x",**ss);
    fd_warn(_("Unexpected continuation character %s"),buf);
    (*ss)++; return 0xFFFD;}
  else if (**ss < 0xE0) {size=2; ch=**ss&0x1F;}
  else if (**ss < 0xF0) {size=3; ch=**ss&0x0F;}
  else if (**ss < 0xF8) {size=4; ch=**ss&0x07;}
  else if (**ss < 0xFC) {size=5; ch=**ss&0x3;}     
  else if (**ss < 0xFE) {size=6; ch=**ss&0x1;}
  else {(*ss)++; return 0xFFFD;}
  i=size-1; (*ss)++;
  while (i) {
    if ((**ss<0x80) || (**ss>=0xC0))
      fd_raise_detailed_exception(fd_BadUTF8,start);
    else {ch=(ch<<6)|(**ss&0x3F); (*ss)++; i--;}}
  return ch;
}

FASTOP int uni_sgetc(fd_u8char **ss)
{
  if (**ss == 0) return -1;
  else if (**ss < 0x80) return *((*ss)++);
  else return _uni_sgetc(ss);
}

static void uni_sungetc(int c,fd_u8char **ss)
{
  if (c < 0) {}
  else if (c < 0x80) *ss=*ss-1;
  else if (c < 0x800) *ss=*ss-2;
  else if (c < 0x10000) *ss=*ss-3;
  else if (c < 0x200000) *ss=*ss-4;
  else if (c < 0x4000000) *ss=*ss-5;
  else if (c < 0x80000000) *ss=*ss-6;
  else {}
}

DTYPES_EXPORT
/* _fd_sgetc:
     Arguments: a pointer to a pointer to a UTF-8 string
     Returns: an int (representing a unicode character)

Reads a single unicode character from a utf-8 string, advancing the
string past the character.
*/
int _fd_sgetc(fd_u8char **ss) { return uni_sgetc(ss); }

/** Output to memory (Utilities) **/

DTYPES_EXPORT void _fd_dbuf_needs(struct FD_DBUF *b,int size)
{
  fd_dbuf_needs(b,size);
}

/** Packaged types **/

static lisp make_packaged_packet
   (dt_type_code package,dt_subcode code,int size,char *data)
{
  struct FD_TYPE_REGISTRY *r=
	  fd_lookup_package_code
	  ((unsigned char)package,(unsigned char)code);
  if (r) return r->package_restore_fcn(size,data);
  else {
    fd_mystery m=fd_malloc(sizeof(struct FD_MYSTERY)); 
    m->package=package; m->code=code;
    m->length=size; m->data.bytes=data;
    return fd_make_cptr((mystery_type),m);}
}

static lisp make_packaged_vector
   (dt_type_code package,dt_subcode code,int size,lisp *data)
{
  struct FD_TYPE_REGISTRY *r=
    fd_lookup_package_code
	((unsigned char)package,(unsigned char)code);
  if (r) return r->package_restore_fcn(size,data);
  else {
    fd_mystery m=fd_malloc(sizeof(struct FD_MYSTERY)); 
    m->package=package; m->code=code;
    m->length=size; m->data.dtypes=data;
    return fd_make_cptr((mystery_type),m);}
}

/** Dtypes from/to memory **/

#define INCLUDE_READING 1
#define INCLUDE_WRITING 1 

#define read_byte(stream)        fd_dread_byte(stream)        
#define read_4bytes(stream)      fd_dread_4bytes(stream)        
#define unread_byte(code,stream) fd_dunread_byte(code, stream)
#define read_bytes(x,n,stream)   fd_dread_bytes(x,n,stream)            
#define read_float(x,stream)     fd_dread_float(x,stream)            

#define write_byte(x,stream)       fd_dwrite_byte(x,stream)              
#define write_4bytes(x,stream)     fd_dwrite_4bytes(x,stream)              
#define write_bytes(x,n,stream)    fd_dwrite_bytes(x,n,stream)          
#define write_float(x,stream)      fd_dwrite_float(x,stream)            

#define STREAM FD_DBUF

#define write_dtype           fd_dwrite_dtype_x
#define write_character_dtype dwrite_character_dtype
#define write_mystery_dtype   dwrite_mystery_dtype
#define write_extended_dtype  dwrite_extended_dtype
#define write_utf8_as_utf16   dwrite_utf8_as_utf16

#define read_dtype          fd_dread_dtype
#define read_packaged_dtype dread_packaged_dtype

#define TESTEOF(c,s) (c == EOF)

#include "dtio.c"

int fd_dwrite_dtype(fd_lisp x,FD_DBUF *d)
{
  return fd_dwrite_dtype_x(x,d,0);
}

/* Undefine what you just did to avoid compiler warnings */

#undef read_byte
#undef read_4bytes
#undef unread_byte
#undef read_bytes
#undef read_float

#undef write_byte
#undef write_4bytes
#undef write_bytes
#undef write_float

#undef STREAM

#undef write_dtype
#undef write_character_dtype
#undef write_mystery_dtype
#undef write_extended_dtype
#undef write_utf8_as_utf16

#undef read_dtype
#undef read_packaged_dtype

#undef TESTEOF

/** Dtypes from/to files **/

#define read_byte(stream)        fread_byte(stream)        
#define read_4bytes(stream)      fread_4bytes(stream)            
#define unread_byte(code,stream) funread_byte(code, stream)
#define read_bytes(x,n,stream)   fread_bytes(x,n,stream)            
#define read_float(x,stream)     fread_float(x,stream)            

#define write_byte(x,stream)     fwrite_byte(x,stream)              
#define write_4bytes(x,stream)   fwrite_4bytes(x,stream);
#define write_bytes(x,n,stream)  fwrite_bytes(x,n,stream)          
#define write_float(x,stream)    fwrite_float(x,stream)            

#define STREAM FILE

#define write_dtype           fd_fwrite_dtype_x
#define write_character_dtype fwrite_character_dtype
#define write_mystery_dtype   fwrite_mystery_dtype
#define write_extended_dtype    fwrite_extended_dtype
#define write_utf8_as_utf16   fwrite_utf8_as_utf16

#define read_dtype fd_fread_dtype
#define read_packaged_dtype  fread_packaged_dtype

#define TESTEOF(c,s) ((c == EOF) && (feof(s)))

#include "dtio.c"

int fd_fwrite_dtype(fd_lisp x,FILE *f)
{
  return fd_fwrite_dtype_x(x,f,0);
}

#undef write_byte
#undef write_4bytes
#undef write_bytes
#undef write_float

#undef STREAM

#undef write_dtype
#undef write_character_dtype
#undef write_mystery_dtype
#undef write_extended_dtype
#undef write_utf8_as_utf16

/** Dtypes size counting **/

#undef INCLUDE_READING
#define INCLUDE_READING 0
#define write_byte(x,stream)       (*stream)++
#define write_4bytes(x,stream)     (*stream)++
#define write_bytes(x,n,stream)    (*stream)++
#define write_float(x,stream)      (*stream)++

#define STREAM unsigned int

#define write_dtype           fd_fake_dtype_x
#define write_character_dtype size_character_dtype
#define write_mystery_dtype   size_mystery_dtype
#define write_extended_dtype    size_extended_dtype
#define write_utf8_as_utf16   size_utf8_as_utf16

#include "dtio.c"

int fd_fake_dtype(fd_lisp x,unsigned int *op_ptr)
{
  return fd_fake_dtype_x(x,op_ptr,0);
}

DTYPES_EXPORT
/* fd_dtype_size:
     Arguments: a lis pointer
     Returns: an integer
 Returns the number of bytes which will be used by the DType representation
 of its argument. */
unsigned int fd_dtype_size(lisp x)
{
  unsigned int n_ops=0;
  return fd_fake_dtype(x,&n_ops);
}

/** Validate DTYPE **/

/* Internal function for validating a dtype in memory */
static unsigned char *validate_dtype(unsigned char *ptr,unsigned char *end)
{
  if (ptr == NULL) return NULL;
  else if (ptr >= end) return NULL;
  else switch (*ptr) {
  case dt_null: case dt_void: return ptr+1;
  case dt_bool: return ptr+2;
  case dt_fixnum: case dt_float: return ptr+5;
  case dt_oid: return ptr+9;
  case dt_packet: case dt_string: case dt_symbol:
    if (ptr+5 >= end) return NULL;
    else {unsigned int size=((ptr[1]<<24)|(ptr[2]<<16)|(ptr[3]<<8)|ptr[4]);
	  return ptr+5+size;}
  case dt_error: case dt_exception:
    return validate_dtype(ptr+1,end);
  case dt_pair: case dt_compound:
    return validate_dtype(validate_dtype(ptr+1,end),end);
  case dt_vector:
    if (ptr+5 >= end) return NULL;
    else {unsigned int size=((ptr[1]<<24)|(ptr[2]<<16)|(ptr[3]<<8)|ptr[4]);
	  unsigned int i=0; ptr=ptr+5; while (i < size)
	    {ptr=validate_dtype(ptr,end); i++;}
	  return ptr;}
  default:
    if ((*ptr)&0x40)
      if (ptr+1 >= end) return NULL;
      else if (ptr[1]&0x80) {
	unsigned int i=0, size;
	if (ptr[1]&0x40)
	  if (ptr+6 >= end) return NULL;
	  else {size=((ptr[2]<<24)|(ptr[3]<<16)|(ptr[4]<<8)|ptr[5]);
		ptr=ptr+6;}
	else if (ptr+2 >= end) return NULL;
	else {size=ptr[2]; ptr=ptr+3;}
	while (i < size) {ptr=validate_dtype(ptr,end); i++;}
	return ptr;}
      else {
	unsigned int size;
	if (ptr[1]&0x40)
	  if (ptr+6 >= end) return NULL;
	  else {size=((ptr[2]<<24)|(ptr[3]<<16)|(ptr[4]<<8)|ptr[5]);
	  ptr=ptr+6;}
	else if (ptr+2 >= end) return NULL;
	else {size=ptr[2]; ptr=ptr+3;}
	return ptr+size;}
    else fd_raise_exception(fd_InvalidDType);
  }
}

DTYPES_EXPORT
/* fd_validate_dtype:
     Arguments: two pointers into an array of bytes
     Returns: 1 if the range contains a valid dtype representation
*/
int fd_validate_dtype(unsigned char *buf,unsigned char *end)
{
  unsigned char *answer=validate_dtype(buf,end);
  if (answer && (answer == end)) 
	return 1; else return 0;
}

/** Printing lisp objects to strings **/

static void print_pair_to_string(lisp x,fd_string_stream s);    
static void print_vector(lisp vec,fd_string_stream ss);
static void print_slotmap(lisp sm,fd_string_stream ss);

DTYPES_EXPORT
/* fd_print_lisp_to_string:
     Arguments: a lisp object and a pointer to a "string stream"
     Returns: nothing

Outputs an ASCII representation of the object to the string stream.
*/
void fd_print_lisp_to_string(lisp x,fd_string_stream s)
{
  switch (PTR_TYPE (x)) {
  case immediate_type:
    switch (PTR_DATA(x,fixnum)) {
    case FD_EMPTY_LIST_CODE: fd_sputs(s,"()"); return;
    case FD_FALSE_CODE: fd_sputs(s,"#f"); return;
    case FD_TRUE_CODE: fd_sputs(s,"#t"); return;
    case FD_VOID_CODE: fd_sputs(s,"#?"); return;
    case FD_EMPTY_CHOICE_CODE: fd_sputs(s,"{}"); return;
    case FD_QUOTED_EMPTY_CHOICE_CODE: fd_sputs(s,"#{}"); return;
    case FD_LISP_EOF_CODE: fd_sputs(s,"#EOF"); return;
    default: {
      char buf[32]; 
      sprintf(buf,"#0X%x",PTR_DATA(x,fixnum));
      fd_sputs(s,buf); return;}}
  case character_type: {
    unsigned int c=PTR_DATA(x,fixnum); char buf[32];
    if (s->escape == 0) {
      unsigned char buf[2]; 
      buf[0]=(unsigned char)c; buf[1]='\0';
      fd_sputs(s,buf); break;}
    if (c == ' ') sprintf(buf,"#\\Space");
    else if ((c < 128) && (isprint(c))) sprintf(buf,"#\\%c",c);
    else if (c == '\n') sprintf(buf,"#\\Newline");
    else if (c == '\t') sprintf(buf,"#\\Tab");
    else if (c == '\r') sprintf(buf,"#\\Return");
    else if (c < 65536) sprintf(buf,"#\\u%04x",c);
    else sprintf(buf,"#\\U%08x",c);
    fd_sputs(s,buf); return;}
  case fixnum_type: {fd_printf(s,"%d",PTR_DATA(x,fixnum)); return;}
  case flonum_type: {fd_printf(s,"%f",FD_FLOATLISP(x)); return;}
  case string_type: case qstring_type: case zstring_type: {
    unsigned char *start=STRING_DATA(x);
    unsigned char *scan=start, *end=scan+STRING_LENGTH(x);
    if (PTR_TYPE(x) == zstring_type) fd_sputs(s,"#Z");
    if (s->escape) {fd_sputs(s,"\"");}
    while (scan < end)
      if ((s->escape) &&
	  ((*scan == '"') || (*scan == '\\') || (iscntrl(*scan)))) {
	fd_sputn(s,start,scan-start);
	if (*scan == '"') fd_sputs(s,"\\\"");
	else if (*scan == '\\') fd_sputs(s,"\\\\");
	else if (*scan == '\r') fd_sputs(s,"\\r");
	else if (*scan == '\n') fd_sputs(s,"\\n");
	else fd_sputc(s,*scan);
	scan++; start=scan;}
      else scan++;
    fd_sputn(s,start,end-start);
    if (s->escape) {fd_sputs(s,"\"");} 
    return;}
  case pair_type:
    print_pair_to_string(x,s); return;
  case quoted_choice_type: fd_sputs(s,"#");
  case choice_type: case proper_choice_type: {
    fd_choice ch=PTR_DATA(x,choice); int set_size=ch->size;
    fd_sputs(s,"{");
    if (ch->elt_type) {
      union FD_DATA *scan=ch->elements.data, *limit=scan+set_size;
      lisp tmp_ptr; tmp_ptr.type=ch->elt_type;
      while (scan < limit) {
	tmp_ptr.data=*scan++; fd_print_lisp_to_string(tmp_ptr,s);
	if (scan != limit) {fd_sputs(s," ");}}}
    else {
      lisp *scan=ch->elements.lisp, *limit=scan+set_size;
      while (scan < limit) {
	fd_print_lisp_to_string(*scan++,s);
	if (scan != limit) {fd_sputs(s," ");}}}
    fd_sputs(s,"}");
    return;}
  case symbol_type: {
    int need_escape=0, c;
    fd_u8char *start=SYMBOL_NAME(x);
    while ((c=uni_sgetc(&start))>=0) {
      if ((fd_islower(c)) || (issymbreak(c))) {need_escape=1; break;}}
    if (need_escape) {fd_sputc(s,'|');}
    start=SYMBOL_NAME(x);
    while ((c=uni_sgetc(&start))>=0) {
      if (c == '|')  {fd_sputc(s,'\\'); fd_sputc(s,'|');}
      else fd_sputc(s,c);}
    if (need_escape) {fd_sputc(s,'|');}
    return;}
  case object_type: {
    if (s->fancy_oids) {print_oid(x,s); return;}
    else {
      fd_printf(s,"@%x/%x",FD_OID_ADDR_HIGH(x),FD_OID_ADDR_LOW(x));
      return;}}
  case slotmap_type: { print_slotmap(x,s); return;}
  case vector_type: case tail_call_type: case multiple_value_type: {
    print_vector(x,s); return;}
  case record_type: case lrecord_type: {
    lisp tag=RECORD_TAG(x);
    struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
    if (r && r->print_fcn) {r->print_fcn(x,s); return;}
    else if (RECORDP(x)) {
      fd_sputs(s,"#?("); fd_print_lisp_to_string(tag,s); fd_sputs(s,")");}
    else {
      fd_sputs(s,"#R("); fd_print_lisp_to_string(tag,s);
      fd_sputs(s," "); fd_print_lisp_to_string(LRECORD_DATA(x),s);
      fd_sputs(s,")");}
    return;
  }
  case rational_type:
    fd_printf(s,"%q/%q",FD_NUMERATOR(x),FD_DENOMINATOR(x)); return;
  case complex_type:
    fd_printf(s,"%q+%qi",FD_REALPART(x),FD_IMAGPART(x)); return;
  case bad_type: fd_raise_exception(fd_BadType);
  default: {
    struct FD_TYPE_REGISTRY *r=fd_lookup_typecode(PTR_TYPE(x));
    if ((r) && (r->print_fcn)) {r->print_fcn(x,s); return;}
    else fd_printf(s,_("#<unprintable>"));}
  }  /*switch (PTR_TYPE(x)) */
}

static void print_pair_to_string(lisp x,fd_string_stream s)
{
  /* more efficient than just print the car; print the cdr*/
  lisp car = CAR (x), cdr = CDR (x);
  if (PRIM_TYPEP(cdr,pair_type)) {
    if (LISP_EQ(car,quote_symbol))
      {fd_printf(s,"'%q",CAR(cdr)); return;}
    else if (LISP_EQ(car,unquote_symbol))
      {fd_printf(s,",%q",CAR(cdr)); return;}
    else if (LISP_EQ(car,unquote_splice_symbol))
      {fd_printf(s,",@%q",CAR(cdr)); return;}
    else if (LISP_EQ(car,backquote_symbol)) 
      {fd_printf(s,"`%q",CAR(cdr)); return;}}
  fd_sputs(s,"(");
  fd_print_lisp_to_string(car,s);
  while (PAIRP(cdr)) {
    car = CAR (cdr); cdr = CDR (cdr); fd_sputs(s," ");
    fd_print_lisp_to_string(car,s);}
  if (FD_EMPTY_LISTP(cdr)) {fd_sputs(s,")");}
  else fd_printf(s," . %q)",cdr);
}

/* Prints a vector. */
static void print_vector(lisp vec,fd_string_stream ss)
{
  lisp_vector v=PTR_DATA(vec,vector);
  lisp *elts=v->elements, *limit=elts+v->length;;
  if (PRIM_TYPEP(vec,vector_type)) fd_sputs(ss,"#(");
  else if (PRIM_TYPEP(vec,tail_call_type)) fd_sputs(ss,"#<CALL ");
  else if (PRIM_TYPEP(vec,multiple_value_type)) fd_sputs(ss,"#<VALUES ");
  else fd_raise_exception(_("unprintable vector"));
  while (elts < limit) {
    fd_print_lisp_to_string(*elts,ss); elts++;
    if (elts != limit) fd_sputs(ss," ");}
  if (PRIM_TYPEP(vec,vector_type)) fd_sputs(ss,")");
  else fd_sputs(ss,">");
}

static void print_slotmap(lisp x,fd_string_stream s)
{
  int first_entry=1;
  fd_sputs(s,"#[");
  {FD_DO_SLOTMAP(slotid,slotval,x) {
    if (first_entry) first_entry=0; else fd_sputs(s," ");
    fd_print_lisp_to_string(slotid,s); fd_sputs(s," ");
    fd_print_lisp_to_string(slotval,s);}}
  fd_sputs(s,"]");
}

/** Printout OIDs to strings **/

/** Top level output functions **/

DTYPES_EXPORT
/* fd_print_lisp:
     Arguments: a lisp object and a standard output stream
     Returns: nothing

  Outputs an ascii representation of the object to the output stream
*/
void fd_print_lisp (lisp x, FILE * stream)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_print_lisp_to_string(x,&ss);
  fd_fputs_encoded(ss.ptr,ss.size,stream); fflush(stream); fd_xfree(ss.ptr);
}

DTYPES_EXPORT
/* fd_print_lisp_to_stdout:
     Arguments: a lisp object
     Returns: nothing

  Outputs an ascii representation of the object to the standard output
*/
void fd_print_lisp_to_stdout(lisp dtype)
{
  fd_print_lisp(dtype,stdout);
  printf("\n"); fflush(stdout);
}

DTYPES_EXPORT
/* fd_object_to_string:
     Arguments: a lisp object
     Returns: a string

  Returns a string containing a UTF-8 representation of the object.
*/
fd_u8char *fd_object_to_string(lisp object)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_print_lisp_to_string(object,&ss);
  return ss.ptr;
}

DTYPES_EXPORT
/* fd_write_dtype_to_file:
     Arguments: a lisp object, a filename (a string)
     Returns: nothing

  Outputs a dtype representation of the object to the
   specified file.
*/
void fd_write_dtype_to_file(lisp v,char *filename)
{
   FILE *f=fd_fopen(filename,"wb");
   fd_fwrite_dtype(v,f); fd_fclose(f);
}

DTYPES_EXPORT
/* fd_add_dtype_to_file:
     Arguments: a lisp object, a filename (a string)
     Returns: nothing

  Outputs a dtype representation of the object to the
   end of the specified file (creating it if neccessary).
*/
void fd_add_dtype_to_file(lisp v,char *filename)
{
   FILE *f=fd_fopen_locked(filename,"ab",1);
   fd_fwrite_dtype(v,f); fd_fclose(f);
}

/** Pretty printing **/

static int print_elt(lisp x,fd_string_stream s,int pos,int indent,int limit)
{
  int original=s->size, offset;
  if ((CHOICEP(x)) || (PAIRP(x)) || (VECTORP(x))) {
    fd_pprint_lisp_to_string(x,s,pos,indent,limit);
    offset=s->size-original;}
  else {fd_print_lisp_to_string(x,s); offset=s->size-original;}
  return pos+offset;
}

static int pprint_elt(lisp x,fd_string_stream s,int pos,int indent,int limit)
{
  int original=s->size, offset;
  fd_print_lisp_to_string(x,s); 
  offset=s->size-original;
  if (pos+offset < limit) return pos+offset;
  else {
    s->size=original; s->ptr[original]='\0';
    if (FD_USE_SPACE(s,indent+1)) {
      unsigned char *scan=s->ptr+original, *end=scan+indent+1;
      *scan++='\n'; while (scan < end) *scan++=' ';
      *scan++='\0'; pos=indent;
      return print_elt(x,s,pos,indent,limit);}
    else return pos;}
}

/* fd_pprint_lisp_to_string:
     Arguments: a lisp pointer, a string stream pointer,
                a current position (relative to the left margin),
		an indentation, and a limit
     Returns: the current position after the object
              (relative to the left margin)
  Outputs a pretty (multi-line, indented) version of the lisp object
   to the string stream.
*/
unsigned int fd_pprint_lisp_to_string
    (lisp x,fd_string_stream s,
	 unsigned int pos,unsigned int indent,unsigned int limit)
{
  if (CHOICEP(x)) {
    unsigned int original=s->size, n_elts=CHOICE_SIZE(x), n_printed=0;
    fd_print_lisp_to_string(x,s);
    if (pos+s->size-original < limit) return  pos+s->size-original;
    s->size=original; s->ptr[s->size]='\0';
    fd_sputs(s,"{"); pos=pos+1;
    {DO_CHOICES(r,x)
       if (n_printed) {
	 pos=pprint_elt(r,s,pos,indent+1,limit); n_printed++;
	 if (n_printed < n_elts) {fd_sputs(s," ");}}
       else {
	 pos=print_elt(r,s,pos,indent+1,limit); n_printed++;
	 if (n_printed < n_elts) {fd_sputs(s," ");}}
    END_DO_CHOICES;}
    fd_sputs(s,"}");
    return pos+1;}
  else if (PAIRP(x)) {
    unsigned int original=s->size, first_done=0, ind=1;
    lisp ptr=x;
    fd_print_lisp_to_string(x,s);
    if (pos+s->size-original < limit) return pos+s->size-original;
    s->size=original; s->ptr[s->size]='\0';
    fd_sputs(s,"("); pos=pos+1;
    if (SYMBOLP(CAR(ptr))) ind=3;
    else if (OIDP(CAR(ptr))) ind=1;
    while (PAIRP(ptr)) {
      lisp e=CAR(ptr);
      if (first_done) pos=pprint_elt(e,s,pos,indent+ind,limit);
      else {first_done=1; pos=print_elt(e,s,pos,indent+ind,limit);}
      ptr=CDR(ptr);
      if (!(FD_EMPTY_LISTP(ptr))) {fd_sputs(s," "); pos++;}}
    if (FD_EMPTY_LISTP(ptr)) {fd_sputs(s,")"); return pos+1;}
    else {
      pos=pos+3;
      fd_sputs(s," . "); pos=print_elt(ptr,s,pos,indent+1,limit);
      fd_sputs(s,")");
      return pos+1;}}
  else if (VECTORP(x)) {
    unsigned int original=s->size, first_done=0; fd_print_lisp_to_string(x,s);
    if (pos+s->size-original < limit) return  pos+s->size-original;
    s->size=original; s->ptr[s->size]='\0';
    fd_sputs(s,"#("); pos=pos+2;
    {int size=VECTOR_LENGTH(x), i=0; while (i < size) {
      lisp elt=VECTOR_REF(x,i);
      if (first_done) pos=pprint_elt(elt,s,pos,indent+2,limit);
      else {first_done=1; pos=print_elt(elt,s,pos,indent+2,limit);}
      i++; if (i < size) {fd_sputs(s," "); pos++;}}}
    fd_sputs(s,")");
    return pos+1;}
  else if (SLOTMAPP(x)) {
    unsigned int original=s->size;
    fd_print_lisp_to_string(x,s);
    if (pos+s->size-original < limit) return  pos+s->size-original;
    else {
      lisp *data, *scan, *slimit; int size;
      size=_fd_slotmap_data(x,(void **)&data); scan=data; slimit=scan+size;
      s->size=original; s->ptr[s->size]='\0';
      fd_sputs(s,"#["); pos=pos+2;
      while (scan < slimit) {
	pos=pos+pprint_elt(*scan++,s,pos,indent+2,limit);
	fd_sputs(s," "); pos=pos+1;
	pos=pos+pprint_elt(*scan++,s,pos,indent+4,limit);
	if (scan == slimit) {
	  fd_sputs(s,"]"); _fd_done_with_slotmap_data(data,size);
	  return pos+1;}
	else {
	  unsigned int j=0;
	  fd_sputs(s,"\n"); while (j < indent+2) {fd_sputs(s," "); j++;}
	  pos=indent+2;}}
      return pos;}}
  else return print_elt(x,s,pos,indent,limit);
}

/* fd_pprint_lisp:
     Arguments: a lisp pointer, a file stream, a width
     Returns: nothing
  Outputs a pretty (multi-line, indented) version of the lisp object
   to a file stream with a particular right margin
*/
void fd_pprint_lisp(lisp x,FILE *stream,int width)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_pprint_lisp_to_string(x,&ss,0,0,width);
  fd_fputs_encoded(ss.ptr,ss.size,stream); fd_xfree(ss.ptr); fflush(stream);
}

/* fd_pprint_lisp:
     Arguments: a lisp pointer, a file stream, a width, an offset
     Returns: nothing
  Outputs a pretty (multi-line, indented) version of the lisp object
   to a file stream with a particular right margin and a particular
   left margin (offset)
*/
void fd_pprint_lisp_with_offset(lisp x,FILE *stream,int width,int offset)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_pprint_lisp_to_string(x,&ss,offset,offset,width);
  fd_fputs_encoded(ss.ptr,ss.size,stream); fd_xfree(ss.ptr); fflush(stream);
}

/* fd_ppstring:
     Arguments: a lisp pointer, a width
     Returns: a multi-line indented string
  Returns a multi-line indented string containing a pretty version of the
   LISP object.
*/
fd_u8char *fd_ppstring(lisp x,int width)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
  fd_pprint_lisp_to_string(x,&ss,0,0,width);
  return ss.ptr;
}

/** OID print and parse **/

DTYPES_EXPORT
/* fd_default_print_oid:
     Arguments: a pointer to a LISP OID pointer
                and a pointer to a "string stream"
     Returns: nothing

  Outputs the most primitive ASCII representation of the object
  to the string stream.
*/
void fd_default_print_oid(lisp obj,fd_string_stream ss)
{
  FD_OID id=FD_OID_ADDR(obj);
  fd_printf(ss,"@%x/%x",FD_OID_HIGH(id),FD_OID_LOW(id));
}

DTYPES_EXPORT
/* fd_default_pares_oid:
     Arguments: a pointer to a UTF-8 string
     Returns: a lisp pointer to an FD_OID

  Outputs the most primitive ASCII representation of the object
  to the string stream.
*/
lisp fd_default_parse_oid(fd_u8char *string)
{
  unsigned int hi, lo; FD_OID id;
  if (sscanf(string,"%x/%x",&hi,&lo) == 2) {
    FD_SET_OID_HIGH(id,hi); FD_SET_OID_LOW(id,lo);
    return fd_make_oid(id);}
  else fd_raise_detailed_exception(fd_ParseError,string);
}

static void ((*print_oid)(lisp x,fd_string_stream ss))=fd_default_print_oid;
static lisp ((*parse_oid)(fd_u8char *string))=fd_default_parse_oid;

DTYPES_EXPORT
/* fd_configure_oid_io:
     Arguments: a pointer to two C functions; the first outputs OIDs to
       string streams and the second parses utf8 strings into OIDs
     Returns: nothing

   Changes the default printer and parser for OIDs.
*/
void fd_configure_oid_io
  (void ((*print_fcn)(lisp x,fd_string_stream ss)),
   lisp ((*parse_fcn)(fd_u8char *string)),
   lisp ((*lookup_fcn)(fd_lisp name)))
{
  print_oid=print_fcn;
  parse_oid=parse_fcn;
  lookup_oid=lookup_fcn;
}

/** Parsing atoms **/

static lisp ((*bignum_parser)(char *digits,int base))=NULL;
     
DTYPES_EXPORT
/* fd_set_bignum_parser
    Arguments: a function for parsing bignums from strings and returning
     lisp objects
    Returns: void

Defines the function used for parsing large integers into lisp objects.
The function takes a string and an integral radix and returns a 
lisp object representing the number described by the string. */
void fd_set_bignum_parser(lisp (*fcn)(char *,int))
{
  bignum_parser=fcn;
}
     
DTYPES_EXPORT
/* fd_parse_number:
    Arguments: a utf8 string and an integral base
    Returns: a lisp object
 Parses the number assuming the specified base.
*/
lisp fd_parse_number(fd_u8char *string,int base)
{
  long i;
  fd_u8char *dot=strchr(string,'.'); 
  fd_u8char *slash=strchr(string,'/'); 
  fd_u8char *last=string+strlen(string)-1;
  errno=0; /* Get clean slate */
  if ((string[1] == 0) && ((*string == '+') || (*string == '-')))
    return FD_FALSE;
  if (((*last == 'i') || (*last == 'I'))) {
    lisp real, imag;
    fd_u8char *plus=strchr(string+1,'+');
    fd_u8char *minus=strchr(string+1,'-');
    fd_u8char *split=plus;
    char ichar, schar;
    if (split == NULL) split=minus;
    if (split == NULL) return FD_FALSE;
    ichar=*last; *last='\0'; schar=*split; *split='\0';
    real=fd_parse_number(string,base);
    if (schar == '+') imag=fd_parse_number(split+1,base);
    else {*split=schar; imag=fd_parse_number(split,base);}
    *last=ichar; *split=schar;
    return fd_make_complex(real,imag);}
  else if (slash) {
    lisp num, denom;
    *slash='\0';
    num=fd_parse_number(string,base); denom=fd_parse_number(slash+1,base);
    *slash='/';
    return fd_make_rational(num,denom);}
  else if (dot) {
    char *cfloat, *tmp, buf[64]; double d;
    /* Append a zero if it needs it. */
    if (dot == string)
      if (string[1]) {
	sprintf(buf,"0%s",string);
	cfloat=buf;}
      else return FD_FALSE;
    else cfloat=string;
    d=atof(cfloat);
    if (errno) {
      fd_warn(_("Invalid floating point number %s, taking as symbol"),string);
      CLEAR_ERR(); return fd_make_symbol(string);}
    return LISPFLOAT(d);}
  else if (base < 0) { /* Figure out the base */
    fd_u8char *scan=string+1;
    if ((*string == '0') && ((string[1] == 'x') || (string[1] == 'X'))) {
      base=16; scan++; while (*scan)
	if (isxdigit(*scan)) scan++;
	else {
	  fd_warn(_("Invalid hex number %s, taking as symbol"),string);
	  return fd_make_symbol(string);}}
    else {
      base=10; while (*scan)
	if (isdigit(*scan)) scan++;
	else return fd_make_symbol(string);}}
  else if (base == 10) {
    fd_u8char *scan=string;
    if ((*scan == '+') || (*scan == '-')) scan++;
    while (*scan)
      if (isdigit(*scan)) scan++;
      else return fd_make_symbol(string);}
  if (base > 0) {
    i=strtol(string,NULL,base);
    if ((i > INT_MAX) || (i < INT_MIN))
      if (bignum_parser != NULL) {
	CLEAR_ERR(); return bignum_parser(string,base);}
      else {
	fd_warn(_("Number %s too big"),string); CLEAR_ERR();
	return fd_copy_string(string);}
    else if (errno == 0) return LISPFIX(i);
    else if (errno == ERANGE)
      if (bignum_parser != NULL) {
	CLEAR_ERR(); return bignum_parser(string,base);}
      else {fd_warn(_("Number %s too big,string")); CLEAR_ERR();
      return fd_copy_string(string);}
    else {
      fd_warn(_("Invalid number %s, taking as symbol"),string);
      return fd_make_symbol(string);}}
  else {
    fd_warn(_("Really weird number %s, taking as symbol"),string);
    return fd_make_symbol(string);}
}

static lisp parse_atom(fd_u8char *string,int assume_symbol)
{
  if (*string == '\0')
    return fd_make_string("");
  else if ((*string == '-') && (*(string+1) == '\0'))
    return fd_make_symbol("-");
  else if ((*string == '+') && (*(string+1) == '\0'))
    return fd_make_symbol("+");
  else if ((isdigit(*string)) ||
	   (((*string == '-') || (*string == '+') || (*string == '.')) &&
	    (isdigit(string[1]))))
    return fd_parse_number(string,-1);
  else if (*string == '@')
    return parse_oid(string+1);
  else if (assume_symbol) 
    return fd_make_symbol(string);
  else return fd_copy_string(string);
}

static lisp interpret_character_name(fd_u8char *name)
{
  int code=0;
  if (*name == '0')
    if ((name[1] == 'x') || (name[1] == 'X'))
      sscanf(name+2,"%x",&code);
    else sscanf(name,"%o",&code);
  else if (*name == '\\')
    if ((name[1] == 'u') || (name[1] == 'U'))
      if (sscanf(name+2,"%x",&code) == 1) {}
      else fd_raise_detailed_exception
	     (fd_ParseError,_("Invalid character constant"));
    else fd_raise_detailed_exception
	    (fd_ParseError,_("Invalid character constant"));
  else if (*name == 'U') sscanf(name+1,"%x",&code);
  else if (strcasecmp(name,"newline") == 0) code='\n';
  else if (strcasecmp(name,"tab") == 0) code='\t';
  else if (strcasecmp(name,"space") == 0) code=' ';
  else if (strcasecmp(name,"return") == 0) code='\r';
  else if (strcasecmp(name,"cr") == 0) code='\r';
  else if (strcasecmp(name,"null") == 0) code=0;
  else fd_raise_detailed_exception
    (fd_ParseError,"Invalid character constant");
  return fd_make_character(code);
}

/** Parsing LISP objects from strings **/

typedef fd_u8char **string_input_stream;

#define lisp_parser fd_parse_lisp_from_string
#define uni_getc uni_sgetc
#define uni_getc_raw uni_sgetc
#define uni_ungetc uni_sungetc
#define read_atom sread_atom
#define parse_vector sparse_vector
#define parse_choice sparse_choice
#define parse_list sparse_list
#define parse_slotmap sparse_slotmap
#define TXSTREAM string_input_stream

#include "txio.c"

/** Parsing LISP objects from streams **/

FASTOP int probe_ascii_char(struct FD_XFILE *xf) 
{
  if (xf->last_char >= 0) return xf->last_char;
  else if ((xf->encoding) && (xf->in_size == 0) &&
	   (xf->encoding->flags&FD_ENCODING_INCLUDES_ASCII)) {
    int ch=getc(xf->f); ungetc(ch,xf->f);
    if (ch < 0x80) return ch; else return -1;}
  else if ((xf->encoding) && (xf->in_size>0) &&
	   (xf->encoding->flags&FD_ENCODING_INCLUDES_ASCII))
    if (xf->in[0]<0x80) return xf->in[0];
    else return -1;
  else return -1;
    
}
FASTOP int get_ascii_char(struct FD_XFILE *xf) 
{
  if (xf->last_char >= 0) {
    int ch=xf->last_char; xf->last_char=-1;
    return ch;}
  else if ((xf->encoding) && (xf->in_size == 0) &&
	   (xf->encoding->flags&FD_ENCODING_INCLUDES_ASCII)) {
    int ch=getc(xf->f); 
    if (ch < 0x80) return ch;
    else {ungetc(ch,xf->f); return -1;}}
  else if ((xf->encoding) && (xf->in_size>0) &&
	   (xf->encoding->flags&FD_ENCODING_INCLUDES_ASCII) &&
	   (xf->in[0] < 0x80)) {
    int ch=xf->in[0];
    memmove(xf->in,xf->in+1,xf->in_size-1); xf->in_size--;
    return ch;}
  else return -1;
}

FASTOP int fast_xgetc(struct FD_XFILE *xf) 
{
  int ch=probe_ascii_char(xf);
  if (ch < 0) return fd_xgetc_encoded(xf);
  else if (ch == '\\') return fd_xgetc_encoded(xf);
  else if (ch == 13) {
    int nc; get_ascii_char(xf); nc=fd_xgetc(xf);
    if (nc == 10) return '\n';
    else {fd_xungetc(nc,xf); return ch;}}
  else return get_ascii_char(xf);
}
FASTOP int fast_xgetc_raw(struct FD_XFILE *xf) 
{
  int ch=get_ascii_char(xf);
  if (ch >= 0) return ch;
  else return fd_xgetc(xf);
}

FASTOP void fast_xungetc(int c,struct FD_XFILE *e)
{
  if (e->last_char >= 0) fd_raise_exception(_("Can only ungetc once"));
  else if (c < 0) fd_raise_exception(fd_InvalidUnicodeChar);
  else e->last_char=c;
}

typedef FILE *FILEP;

#undef lisp_parser
#define lisp_parser fd_parse_lisp_from_xfile
#undef uni_getc
#define uni_getc fast_xgetc
#undef uni_getc_raw
#define uni_getc_raw fast_xgetc_raw
#undef uni_ungetc
#define uni_ungetc fast_xungetc
#undef read_atom
#define read_atom fread_atom
#undef parse_vector
#define parse_vector fparse_vector
#undef parse_choice
#define parse_choice fparse_choice
#undef parse_list
#define parse_list fparse_list
#undef parse_slotmap
#define parse_slotmap fparse_slotmap
#undef TXSTREAM
#define TXSTREAM fd_xfile

#include "txio.c"

/** Parsing argument strings **/

DTYPES_EXPORT
/* fd_parse_string:
     Arguments: a utf8 string
     Returns: a lisp object
  Returns the lisp object described by the printed representation
in its argument. */
lisp fd_parse_string(fd_u8char *string)
{
  return fd_parse_lisp_from_string(&string);
}

DTYPES_EXPORT
/* fd_parse_arg:
     Arguments: a localized string
     Returns: a lisp object
  Returns the lisp object described by the printed representation
in its argument. */
lisp fd_parse_arg(char *xstring)
{
  fd_u8char *string=fd_convert_os_string(xstring);
  lisp parsed;
  if ((*string == '(') || (*string == '#') ||
      (*string == '[') || (*string == '{') ||
      (*string == '@') || (*string == '|')) 
    parsed=fd_parse_string(string);
  else if (strchr(string,' ')) return fd_init_string(string,-1);
  else if (*string == ':')
    parsed=parse_atom(string+1,1);
  else parsed=parse_atom(string,0);
  fd_xfree(string);
  return parsed;
}

/** Top level input functions **/

DTYPES_EXPORT
/* fd_parse_lisp_from_stream:
     Arguments: a FILE pointer
     Returns: a lisp object
  Parses the printed representation of an object from
  a stdio file stream */
lisp fd_parse_lisp_from_stream(FILE *f)
{
  fd_xfile xf=fd_get_xfile(f);
  if (xf) return fd_parse_lisp_from_xfile(xf);
  else {
    struct FD_XFILE xfs;
    fd_init_xfile(&xfs,f,fd_get_default_encoding());
    return fd_parse_lisp_from_xfile(&xfs);}
}


DTYPES_EXPORT
/* fd_read_dtype_from_file:
    Arguments: a localized string (a filename)
    Returns: a lisp object
 Returns the lisp object described by the first DTYPE
 in filename. */
lisp fd_read_dtype_from_file(char *filename)
{
   FILE *f=fd_fopen(filename,"rb");
   if (f) return fd_fread_dtype(f);
   else fd_raise_detailed_exception(fd_FileOpenFailed,filename);
}

DTYPES_EXPORT
/* fd_read_dtypes_from_file:
    Arguments: a localized string (a filename)
    Returns: a lisp object
 Returns the lisp objects described by the DTYPES
 in filename. */
lisp fd_read_dtypes_from_file(char *filename)
{
   FILE *f=fd_fopen(filename,"rb");
   if (f) {
     lisp results=FD_EMPTY_CHOICE; int c;
     while ((c=fgetc(f)) != EOF) {
       lisp v; ungetc(c,f); v=fd_fread_dtype(f);
       ADD_TO_CHOICE(results,v);}
     fd_fclose(f);
     return results;}
   else return FD_EMPTY_CHOICE;
}

/** Getting at files **/

DTYPES_EXPORT char *fd_filestring(char *filename)
/* fd_filestring:
     Arguments: a filename (a localized string)
     Returns: another string (its contents)
 Returns the contents of a file as a string. */
{
  char *buf=fd_xmalloc(4096); int size=0, limit=4096, delta;
  FILE *f=fd_fopen(filename,"r");
  if (f == NULL) fd_raise_detailed_exception(fd_FileOpenFailed,filename);
  while ((delta=fread(buf+size,sizeof(char),limit-size,f)) == limit-size) {
    buf=fd_xrealloc(buf,limit+limit/2); limit=limit+limit/2;
    size=size+delta; buf[size]='\0';}
  size=size+delta; buf[size]='\0'; fclose(f);
  return buf;
}

DTYPES_EXPORT
/* fd_set_super_pool_aliasing:
     Arguments: two OIDs
     Returns: nothing

  Sets up the DType reader to translate OIDs in the super pool of
*from* into OIDs in the super pool for *to*. */
void fd_set_super_pool_aliasing(FD_OID from,FD_OID to)
{
  aliased_super_pool=FD_OID_HIGH(from);
  super_pool_alias=FD_OID_HIGH(to);
}

/** Initializing the IO package **/

void fd_initialize_io_c()
{
  quote_symbol=fd_make_symbol("QUOTE");
  backquote_symbol=fd_make_symbol("QUASIQUOTE");
  unquote_symbol=fd_make_symbol("UNQUOTE");
  unquote_splice_symbol=fd_make_symbol("UNQUOTE-SPLICING");
  error_symbol=fd_make_symbol("ERROR");
  exception_symbol=fd_make_symbol("EXCEPTION");
  fd_register_source_file("io",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   Log: io.c,v $
   Revision 1.17  2004/03/08 12:57:45  haase
   Raw returns in strings are not translated into \r

   Revision 1.16  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.15  2003/08/31 16:56:12  haase
   Fixed display of quoted empty choice

   Revision 1.14  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.13.2.4  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.13.2.3  2003/06/29 19:34:29  haase
   Added FD_DO_SLOTMAP and used it in slotmap printing

   Revision 1.13.2.2  2003/01/26 20:34:35  haase
   Misc. fixes, introduced zstrings

   Revision 1.13.2.1  2002/08/13 01:52:14  haase
   Fixes for FD_ macros

   Revision 1.13  2002/06/15 16:56:00  haase
   Restored super pool aliasing

   Revision 1.12  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.11  2002/04/19 13:19:51  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.10  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.9  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
