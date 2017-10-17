/* dtio.c
   This file implements reading and writing of DTYPEs for FramerD.
   It is actually included several times in the file io.c, each
   time with different macro definitions to define different
   functions.  Admittedly, a kludge which would be easy with C++,
   but I'm trying to keep the cognitive overload in other places.

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

#if INCLUDE_WRITING

unsigned int write_mystery_dtype(lisp x,STREAM *stream,int capability)
{
  fd_mystery m=(fd_mystery) RECORD_DATA(x);
  unsigned int n_elts=m->length, size;
  write_byte(m->package,stream);
  if (n_elts < 256) {
    write_byte(((unsigned char)((m->code)&~0x40)),stream);
    write_byte((unsigned char)n_elts,stream); size=3;}
  else {
    write_byte(((unsigned char)(m->code|0x40)),stream);
    write_4bytes(n_elts,stream); size=6;}
  if (m->code & 0x80) {
    lisp *data=m->data.dtypes, *limit=data+n_elts;
    while (data < limit) {
      size=size+write_dtype(*data,stream,capability); data++;}
    return size;}
  else {
    write_bytes(m->data.bytes,n_elts,stream);
    return size+n_elts;}
}

unsigned int write_extended_dtype
  (lisp x,struct FD_TYPE_REGISTRY *r,STREAM *stream,int capability)
{
  if (r == NULL)
    fd_raise_exception(fd_Unknown_Record_Type);    
  else if (r->package_code)
    if (r->subcode & 0x80) {
      lisp *data; int n_elts, size=2;
      n_elts=r->package_data_fcn(x,(void **)&data);
      write_byte(r->package_code,stream);
      if (n_elts < 256) {
	write_byte(r->subcode,stream);
	write_byte((unsigned char)n_elts,stream); size=size+1;}
      else {
	write_byte((unsigned char)(r->subcode|0x40),stream);
	write_4bytes(n_elts,stream); size=size+4;}
      {
	lisp *scan=data, *limit=data+n_elts;
	while (scan < limit) {
	  size=size+write_dtype(*scan,stream,capability); scan++;}
	/* In case we locked something */
	if (r->package_data_done_fcn)
	  r->package_data_done_fcn(x,n_elts,(void **)&data);
	return size;}}
    else {
      unsigned char *data; unsigned int n_elts, size=2;
      n_elts=r->package_data_fcn(x,(void **)&data);
      write_byte(r->package_code,stream);
      if (n_elts < 256) {
	write_byte(r->subcode,stream);
	write_byte((unsigned char)n_elts,stream); size=size+1;}
      else {
	write_byte((unsigned char)(r->subcode|0x40),stream);
	write_4bytes(n_elts,stream); size=size+4;}
      write_bytes(data,n_elts,stream);
      if (r->package_data_done_fcn)
	r->package_data_done_fcn(x,n_elts,(void **)&data);
      return size+n_elts;}
  else if (!(FD_VOIDP(r->compound_tag))) {
    lisp external_form=r->compound_dump_fcn(x);
    int size=1;
    write_byte(dt_compound,stream);
    size=size+write_dtype(r->compound_tag,stream,capability);
    size=size+write_dtype(external_form,stream,capability);
    decref(external_form);
    return size;}
  else if ((RECORDP(x)) || (LRECORDP(x))) {
    lisp tag=RECORD_TAG(x);
    if (PRIM_TYPEP(tag,immediate_type)) {
      struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
      fd_exprintf(_("Can't write DTYPE %q\n"),x);
      fd_printf(&ss,_("Can't write DTYPE %q"),x);
      write_byte(dt_exception,stream);
      write_byte(dt_string,stream); write_4bytes(ss.size,stream);
      write_bytes(ss.ptr,ss.size,stream); fd_xfree(ss.ptr);
      return 1+1+4+ss.size;}
    else {
      int size=1; write_byte(dt_compound,stream);
      size=size+write_dtype(tag,stream,capability);
      size=size+write_dtype(LRECORD_DATA(x),stream,capability);
      return size;}}
  else {
    struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,1024);
    fd_exprintf(_("Can't write DTYPE %q\n"),x);
    fd_printf(&ss,_("Can't write DTYPE %q"),x);
    write_byte(dt_exception,stream);
    write_byte(dt_string,stream); write_4bytes(ss.size,stream);
    write_bytes(ss.ptr,ss.size,stream); fd_xfree(ss.ptr);
    return 1+1+4+ss.size;}
  return 0; /* Never reached */
}

static int write_utf8_as_utf16
    (unsigned char scode,fd_u8char *string,STREAM *stream)
{
  int i=0; fd_u8char *str=string; int hdr=0;
  while (*str)
    if (*str < 0x80) {i++; str++;}
    else if (*str < 0xe0) {i++; str=str+2;}
    else {i++; str=str+3;}
  if (i < 128) {
      hdr=2; 
	  write_byte((fd_u8char)(scode&(~(0x40))),stream); 
	  write_byte((fd_u8char)(2*i),stream);}
  else {
      hdr=5; write_byte(scode,stream); write_4bytes(2*i,stream);}
  str=string; while (*str) {
      int c=str[0]; int ch;
      if (c < 0x80) {ch=c; str++;}
      else if (c < 0xe0) {
	  int c1=str[1]; ch=((c&0x1f)<<6)|(c1&0x3f); str=str+2;}
      else {
	  int c1=str[1], c2=str[2]; str=str+3;
	  ch=((c&0x0f)<<12)|((c1&0x3f)<<6)|(c2&0x3f);}
      write_byte((fd_u8char)((ch&0xFF00)>>8),stream);
      write_byte((fd_u8char)((ch&0xFF)),stream);}
  return hdr+i*2;
}

/* write_dtype:
     Arguments: a lisp object and a stream
     Returns: an integer 
    Writes the dtype representation of an object into the current
     position of a file stream, returning the number of bytes taken
     by the representation.
*/
DTYPES_EXPORT
int write_dtype (lisp x,STREAM *stream,int capability)
{
  switch (PTR_TYPE(x)) {
  case immediate_type:
    switch (PTR_DATA(x,fixnum)) {
    case FD_EMPTY_LIST_CODE: write_byte (dt_null, stream); return 1;
    case FD_FALSE_CODE:
      write_byte(dt_bool,stream); write_byte(0,stream);return 2;
    case FD_TRUE_CODE:
      write_byte(dt_bool,stream); write_byte(1,stream);return 2;
    case FD_VOID_CODE: write_byte (dt_void, stream); return 1;
    case FD_EMPTY_CHOICE_CODE:
      write_byte(dt_framerd,stream);
      write_byte(dt_small_set,stream);
      write_byte(0,stream);
      return 3;
    default:
      fd_warn("No external DTYPE representation for %q, writing VOID",x);
      write_byte (dt_void, stream); return 1;}
  case character_type: {
    int c=PTR_DATA(x,fixnum);
    if (c <= 255) {
      write_byte(dt_extended_character,stream);
      write_byte(dt_ascii_char, stream);
      write_byte(1, stream);	/* Size */
      write_byte((char)c, stream);
      return 4;}
    else {
      write_byte(dt_extended_character,stream);
      write_byte(dt_unicode_char,stream);
      write_byte(2,stream);	/* Size */
      write_byte((char)((c>>8)&0xFF),stream);
      write_byte((char)(c&0xFF),stream);
      return 5;}}
  case fixnum_type: {
    write_byte (dt_fixnum, stream);
    write_4bytes (FIXLISP (x), stream);
    return 5;}
  case rational_type: {
    write_byte (dt_extended_numeric, stream);
    write_byte (dt_rational, stream);
    write_byte (2, stream);
    return 3+write_dtype(FD_NUMERATOR(x),stream,capability)+
      write_dtype(FD_DENOMINATOR(x),stream,capability);}
  case complex_type: {
    write_byte (dt_extended_numeric, stream);
    write_byte (dt_complex, stream);
    write_byte (2, stream);
    return 3+write_dtype(FD_REALPART(x),stream,capability)+
      write_dtype(FD_IMAGPART(x),stream,capability);}
  case choice_type: case proper_choice_type: case quoted_choice_type: {
    fd_choice ch=PTR_DATA(x,choice); int size=6;
    int set_size=ch->size;
    if (set_size == 1)
      if (ch->elt_type) {
	lisp ptr; ptr.type=ch->elt_type; ptr.data=ch->elements.data[0];
	return write_dtype(ptr,stream,capability);}
      else return write_dtype(ch->elements.lisp[0],stream,capability);
    else {
      write_byte(dt_framerd,stream);
      if (set_size < 256) {
	size=3; write_byte(dt_small_set,stream);
	write_byte((unsigned char) set_size,stream);}
      else {write_byte(dt_set,stream); write_4bytes(set_size,stream);}
      if (ch->elt_type) {
	union FD_DATA *scan=ch->elements.data, *limit=scan+set_size;
	lisp tmp_ptr; tmp_ptr.type=ch->elt_type;
	while (scan < limit) {
	  tmp_ptr.data=*scan++; size=size+write_dtype(tmp_ptr,stream,capability);}
	return size;}
      else {
	lisp *scan=ch->elements.lisp, *limit=scan+set_size;
	while (scan < limit) {
	  size=size+write_dtype(*scan,stream,capability); scan++;}
	return size;}}}
  case string_type: case qstring_type: case zstring_type:
    if (capability&FD_UTF8_DTYPES) {
      write_byte(FD_ZSTRINGP(x)?dt_zstring:dt_string,stream);
      write_4bytes(STRING_LENGTH(x),stream);
      if (STRING_LENGTH(x)) {
	write_bytes(STRING_DATA(x),((unsigned int)(STRING_LENGTH(x))),stream);}
      return 5+STRING_LENGTH(x);}
    else {
      fd_lisp_string s=PTR_DATA(x,string);
      if (s->utf8) {
	write_byte(dt_extended_character,stream);
	return 1+write_utf8_as_utf16
	  ((FD_ZSTRINGP(x)?dt_unicode_zstring:dt_unicode_string),s->data,stream);}
      else {
	write_byte(FD_ZSTRINGP(x)?dt_zstring:dt_string,stream);
	write_4bytes(STRING_LENGTH(x),stream);
	if (STRING_LENGTH(x)) {
	  write_bytes(STRING_DATA(x),((unsigned int)(STRING_LENGTH(x))),stream);}
	return 5+STRING_LENGTH(x);}}
  case symbol_type: {
    char *s = SYMBOL_NAME (x);
    if ((asciip(s)) || (capability&FD_UTF8_DTYPES)) {
      unsigned int l=strlen(s);
      write_byte(dt_symbol,stream);
      write_4bytes(l,stream);
      if (l) {write_bytes(s,l,stream);}
      return 5+l;}
    else {
	write_byte(dt_extended_character,stream);
	return 1+write_utf8_as_utf16(dt_unicode_symbol,s,stream);}}
  case object_type: {
    FD_OID name=FD_OID_ADDR(x);
    write_byte(dt_oid,stream);
    if (FD_OID_HIGH(name) == aliased_super_pool) {
      write_4bytes(super_pool_alias,stream);}
    else {write_4bytes(FD_OID_HIGH(name),stream);}
    write_4bytes(FD_OID_LOW(name),stream);
    return 9;}
  case pair_type: {
    lisp ptr=x; int bytes=0;
    while (PRIM_TYPEP(ptr,pair_type)) {
      write_byte (dt_pair, stream);
      bytes=bytes+1+write_dtype (CAR (ptr),stream,capability);
      ptr=CDR(ptr);}
    bytes=bytes+write_dtype(ptr,stream,capability);
    return bytes;}
  case vector_type: {
    lisp_vector v=PTR_DATA(x,vector);
    lisp *scan=v->elements, *limit=scan+v->length;
    int size=5;
    write_byte(dt_vector,stream); write_4bytes(v->length,stream);
    while (scan < limit) {size=size+write_dtype(*scan,stream,capability); scan++;}
    return size;}
  case slotmap_type: {
      int size=1; fd_slotmap sm=PTR_DATA(x,slotmap);
      WITH_MUTEX_LOCKED(&(sm->lock)) {
	fd_lisp *sscan=sm->schema, *slimit=sscan+sm->size;
	fd_lisp *vscan=sm->values;
	write_byte(dt_framerd,stream);
	if (sm->size > 127) {
	  write_byte(dt_slotmap,stream);
	  write_4bytes(sm->size*2,stream); size=size+5;}
	else {
	  write_byte(dt_small_slotmap,stream);
	  write_byte(((unsigned char)(sm->size*2)),stream);
	  size=size+2;}
	while (sscan < slimit) {
	  size=size+write_dtype(*sscan,stream,capability);
	  size=size+write_dtype(*vscan,stream,capability);
	  sscan++; vscan++;}}
      END_WITH_MUTEX_LOCKED((&(sm->lock)));
      return size;}
  case packet_type: {
    fd_lisp_string  p=PTR_DATA(x,string);
    write_byte(dt_packet,stream);
    write_4bytes(p->length,stream);
    write_bytes(p->data,((unsigned int)p->length),stream);
    return p->length+5;}
  case record_type: {
    lisp tag = LRECORD_TAG(x);
    return write_extended_dtype(x,fd_lookup_record(tag),stream,capability);}
  case lrecord_type: {
    lisp tag = LRECORD_TAG(x); struct FD_TYPE_REGISTRY *r;
    if (LISP_EQ(tag,error_symbol)) {
      write_byte(dt_error,stream);
      return 1+write_dtype(LRECORD_DATA(x),stream,capability);}
    else if (LISP_EQ(tag,exception_symbol)) {
      write_byte(dt_exception,stream);
      return 1+write_dtype(LRECORD_DATA(x),stream,capability);}
    else if (r=fd_lookup_compound(tag))
      return write_extended_dtype(x,r,stream,capability);
    else {
      int bytes=1;
      write_byte(dt_compound,stream);
      bytes=bytes+write_dtype(tag,stream,capability);
      bytes=bytes+write_dtype(LRECORD_DATA(x),stream,capability);
      return bytes;}}
  case mystery_type: {
    struct FD_MYSTERY *myst=FD_CPTR_DATA(x); int bytes=2;
    write_byte(myst->package,stream); write_byte(myst->code,stream);
    if (myst->code&0x40) {
      write_4bytes(myst->length,stream); bytes=bytes+4;}
    else {write_byte(myst->length,stream); bytes++;}
    if (myst->code&0x80) {
      fd_lisp *data=myst->data.dtypes; int i=0, len=myst->length;
      while (i < len) {
	bytes=bytes+write_dtype(data[i],stream,capability); i++;}}
    else {
      unsigned char *data=myst->data.bytes; int i=0, len=myst->length;
      while (i < len) {write_byte(data[i],stream); i++;}
      bytes=bytes+len;}
    return bytes;}
  case bad_type: fd_raise_exception(fd_BadType); return 0;
  default: {
    return write_extended_dtype(x,fd_lookup_typecode(PTR_TYPE(x)),stream,capability);}
  } /*switch (PTR_TYPE(x))*/
} 

#endif


/* Reading Dtypes */

#if INCLUDE_READING

/* read_packaged_dtype()
    Arguments: a package type code, a subtype code, and a stream
    Returns: A lisp object either generated from the registry or a
"mystery object" which can be rewritten.
*/
static lisp read_packaged_dtype
  (dt_type_code package,dt_subcode code,STREAM *stream)
{
  int size;
  if (code & 0x40) size=read_4bytes(stream);
  else {
    size=read_byte(stream);
    while ((size == EOF) && (!(TESTEOF(size,stream))))
      size=read_byte(stream);}
  if (size == EOF) fd_raise_exception(fd_Unexpected_EOF);
  else if (size < 0) fd_raise_exception("Negative size");
  else if (code & 0x80)
    if (size == 0)
      return make_packaged_vector(package,code,0,NULL);
    else {
      lisp *vec=fd_malloc(sizeof(lisp)*size), *scan=vec, *limit=scan+size; 
      while (scan < limit) {
	*scan=read_dtype(stream);
	scan++;}
      return make_packaged_vector(package,code,size,vec);}
  else if (size == 0)
    return make_packaged_packet(package,code,0,NULL);
  else {
    char *data=fd_malloc(size); read_bytes(data,size,stream);
    return make_packaged_packet(package,code,size,data);}
  return (FD_FALSE); /* Never reached */
}

/* read_dtype:
     Arguments: a pointer to a FILE structure
     Returns: a lisp object
    Parses and returns a single lisp object from the file
     stream.
*/
DTYPES_EXPORT
lisp read_dtype (STREAM *stream)
{
  int code = read_byte (stream);
  while ((code == EOF) && (!(TESTEOF(code,stream)))) code=read_byte(stream);
  switch (code) {
  case dt_null: return (FD_EMPTY_LIST);
  case dt_void: return (FD_VOID);
  case dt_bool: 
    code=read_byte (stream);
    if (code) return (FD_TRUE);
    else return (FD_FALSE);
  case dt_fixnum: {
    int num = read_4bytes(stream);
    return LISPFIX (num);}
  case dt_string: case dt_zstring: {
    int size = read_4bytes (stream);
    fd_u8char *data=fd_xmalloc(size+1), *scan=data, *limit=scan+size;
    fd_lisp_string r=fd_malloca(struct FD_STRING); 
    fd_lisp str; FD_SET_PRIM_TYPE(str,string_type); str.data.string=r;
    fd_malloc_adjust(size+1);
    r->utf8=0; r->length = size; r->data = data;
    read_bytes (data, size, stream); 
    /* Null terminate the string */
    data[size] = '\0';
    /* Check that it is ASCII */
    while (scan < limit)
      if ((*scan == 0) || (*scan >= 0x80)) {
	r->utf8=1; break;}
      else scan++;
    if (code == dt_zstring) {
      fd_lisp v = fd_zify_string(r); fd_decref(str); return v;}
    else return str;}
  case dt_packet: {
    int size = read_4bytes (stream); char *data; 
    data = (char *) fd_malloc(sizeof (char) * size);
    read_bytes(data,size,stream);
    return fd_make_packet(size,data);}
  case dt_symbol: {
    int size = read_4bytes (stream); char *name, buf[256]; lisp result;
    if (size < 256) name = buf;
    else name = fd_malloc(sizeof(char)*(size+1));
    read_bytes (name, size, stream); name[size] = 0;
    result = fd_make_symbol (name);
    if (buf != name) fd_free(name,sizeof(char)*(size+1));
    return result;}
  case dt_oid: {
    FD_OID id;
    FD_SET_OID_HIGH(id,read_4bytes(stream));
    FD_SET_OID_LOW(id,read_4bytes(stream));
    return fd_make_oid(id);}
  case dt_float: {
    union { 
      volatile unsigned int as_read;
      volatile float as_float;
    } u;
    u.as_read=read_4bytes(stream);
    return LISPFLOAT((double)u.as_float);}
  case dt_pair: {
    lisp p = FD_MAKE_PAIR (read_dtype(stream), (FD_FALSE)), head = p;
    int code = read_byte (stream);
    while (code == dt_pair) {
      ((PTR_DATA (p, pair)->cdr)) = 
	FD_MAKE_PAIR (read_dtype(stream),(FD_FALSE));
      p = PTR_DATA (p, pair)->cdr;
      code = read_byte (stream);}
    unread_byte (((char)code), stream);
    ((PTR_DATA (p, pair)->cdr)) = read_dtype(stream);
    return head;}
  case dt_error: return fd_make_error (read_dtype (stream));
  case dt_exception: return fd_make_exception (read_dtype (stream));
  case dt_vector: {
    int i = 0, size = read_4bytes (stream);
    lisp *elts, *scan;
    if (size == 0) return fd_init_vector(size,0);
    scan=elts=fd_malloc(sizeof(lisp)*size);
    while (i < size) {
      *scan = read_dtype(stream); scan++; i++;}
    return fd_init_vector(size,elts);}
  case dt_compound: {
    lisp tag=read_dtype(stream);
    lisp data=read_dtype(stream);
    struct FD_TYPE_REGISTRY *r=fd_lookup_compound(tag);
    if (r) {fd_decref(tag); return r->compound_restore_fcn(data);}
    else return fd_make_lrecord(tag,data);}
  default:
    if (code == EOF) {
      funny_type_code(code); return FD_VOID;}
    else if (code & 0x40) {
      int subcode=read_byte(stream);
      while ((subcode == EOF) && (!(TESTEOF(subcode,stream))))
	subcode=read_byte(stream);
      if (TESTEOF(subcode,stream)) funny_type_code(subcode);
      return read_packaged_dtype(code,(unsigned char)subcode,stream);}
    else {
      funny_type_code(code); return FD_VOID;}} /*switch (code)*/
}
#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: dtio.c,v $
   Revision 1.16  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.15  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.14  2004/05/10 15:52:11  haase
   Added writing of mystery dtypes

   Revision 1.13  2004/03/31 11:19:56  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.12  2004/03/31 03:13:11  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.11  2004/03/30 10:55:26  haase
   Defined _x versions of DTYPE writing to handle extended capabilities

   Revision 1.10  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8.2.2  2003/08/02 13:51:47  haase
   Renamed fd_xprintf to fd_exprintf and made fd_xprintf do a printf to an XFILE

   Revision 1.8.2.1  2003/01/26 20:34:35  haase
   Misc. fixes, introduced zstrings

   Revision 1.8  2002/05/13 06:51:29  haase
   Made undeclared lrecords dump with dt_compound

   Revision 1.7  2002/04/11 03:06:22  haase
   Fixed bug in writing of extended dtypes (particularly short vectors)

   Revision 1.6  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.5  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
