/* Mode: C */

/* xdata.c
   This file implements some extended Lisp datatypes from C and
    creates entries for them in the record registry.  Included here
    are vectors, floating point numers, slotmaps, errors, exceptions,
    and packets.  In addition, big (> 29 bit) fixnums are handled here
    as are the reading of ND sets.  This file also provides some basic
    functions for user types to define copying and garbage collection
    strategies. 

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

static char vcid[] = "$Id: xdata.c,v 1.16 2005/01/14 16:48:44 haase Exp $";

/** Initializations **/
/** Quoting expressions **/
/** Making choices **/
/** Rationals and complex numbers **/
/** Errors and exceptions **/
/** Packets (Binary byte vectors) **/
/** Standard methods for new user types **/
/** Slotmaps **/
/** Hashtables and Hashsets for LISP **/
/** Character types **/
/** Unicode strings **/
/** Initialization **/

#include "dtypes.h"
#include <stdarg.h>
#include <limits.h>
fd_exception
  fd_Invalid_Cons_Format=_("Invalid format for fd_cons");

/** Initializations **/

/* This makes a number fit into 24 bits */
#define MAGIC_MODULUS 16777213

#define hash_lisp(x) (((PTR_TYPE(x)) < 4) ? (x) : fd_hash_lisp(x))

/** Quoting expressions **/

static lisp quote_symbol, timestamp_symbol;
fd_lisp fd_error_tag, fd_exception_tag;

DTYPES_EXPORT
/* fd_quote_lisp:
    Arguments: a lisp object
    Returns: another object, which when evaluated, returns the first
     This also copies any structure copied to it.
*/
lisp fd_quote_lisp(lisp x)
{
  switch (PTR_TYPE(x)) {
  case fixnum_type: case immediate_type: case object_type:
  case character_type: case zstring_type:
    return x;
  case qstring_type: case string_type: case record_type: case lrecord_type: case flonum_type:
    return incref(x);
  case symbol_type:
    return FD_MAKE_PAIR(quote_symbol,FD_MAKE_LIST1(x));
  case pair_type: case slotmap_type: case vector_type:
  case choice_type: case proper_choice_type: case quoted_choice_type: 
    return FD_MAKE_PAIR(quote_symbol,FD_MAKE_LIST1(incref(x)));
  case bad_type: fd_raise_exception(fd_BadType);
  default: return incref(x);
  }
}


/** Rationals and complex numbers **/

static lisp init_rational(int size,void *vdata)
{
  lisp *data=(lisp *)vdata;
  if (size != 2) fd_raise_detailed_exception(fd_InvalidDType,"rational");
  else {
    lisp r=fd_make_rational(data[0],data[1]);
    fd_free(data,sizeof(lisp)*2);
    return r;}
}

static lisp init_complex(int size,void *vdata)
{
  lisp *data=(lisp *)vdata;
  if (size != 2) fd_raise_detailed_exception(fd_InvalidDType,"complex");
  else {
    lisp r=fd_make_complex(data[0],data[1]);
    fd_free(data,sizeof(lisp)*2);
    return r;}
}


/** Errors and exceptions **/

DTYPES_EXPORT
/* fd_make_error: 
     Arguments: another lisp object describing some error
     Returns: an error object whose *details* are the given arguments
   Note: the details are not copied
*/
lisp fd_make_error (lisp data)
{
  return fd_make_lrecord((fd_error_tag),data);
}

static void print_error(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#ERROR %q]",LRECORD_DATA(x));
}

DTYPES_EXPORT
/* fd_make_exception: 
     Arguments: another lisp object describing some exception
     Returns: an exception object whose *details* are the given arguments
   Note: the details are not copied
*/
lisp fd_make_exception (lisp data)
{
  return fd_make_lrecord((fd_exception_tag),data);
}

static void print_exception(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#EXCEPTION %q]",LRECORD_DATA(x));
}


/** Packets (Binary byte vectors) **/

DTYPES_EXPORT
/* fd_make_packet: 
     Arguments: an integer and a pointer to an array of bytes
     Returns: a "packet object" containing the array of bytes
   Note: the details are not copied
*/
lisp fd_make_packet (int len,unsigned char *data)
{
  fd_lisp_string  p=fd_malloca(struct FD_STRING);
  p->length = len;
  p->data = (void *) data;
  {RETURN_LISP(packet_type,string,p);}
}

static void free_packet(lisp x)
{
  fd_lisp_string  p=PTR_DATA(x,string);
  fd_free(p->data,p->length);
  fd_qfree(p,sizeof(struct FD_STRING));
}

static lisp copy_packet(lisp x)
{
  fd_lisp_string  p=PTR_DATA(x,string); 
  char *copy=fd_malloc(p->length); memcpy(copy,p->data,p->length);
  return fd_make_packet(p->length,copy);
}

static void print_packet(lisp x,fd_string_stream ss)
{
  fd_lisp_string s=PTR_DATA(x,string);
  unsigned char *scan=s->data, *limit=scan+s->length;
  fd_printf(ss,"[#PACKET %d 0x",s->length);
  if (ss->escape == 0)
    if (s->length > 100) limit=scan+100;
  while (scan < limit) {
    int j=*scan++; char buf[3]; sprintf(buf,"%02x",j); fd_sputs(ss,buf);}
  if (ss->escape) fd_printf(ss,"]");
  else if (s->length > 100) fd_printf(ss,"...]");
  else fd_printf(ss,"]");
}

static unsigned int compare_packets(lisp x,lisp y)
{
  fd_lisp_string  px=PTR_DATA(x,string), py=PTR_DATA(y,string); 
  if ((STRING_LENGTH(x)) != (STRING_LENGTH(y))) return 0;
  else if (memcmp(px->data,py->data,STRING_LENGTH(x)) == 0) return 1;
  else return 0;
}

static unsigned int hash_packet(lisp x,fd_hashfn h)
{
  int size=PACKET_LENGTH(x);
  unsigned char *data=PACKET_DATA(x);
  unsigned char *ptr=data, *limit=ptr+size;
  unsigned int sum=0;
  while (ptr < limit) sum=((sum<<4)+(*ptr++))%MAGIC_MODULUS;
  return sum;
}

DTYPES_EXPORT
/* fd_parse_packet:
    Arguments: a utf8 string
    Returns: a lisp pointer to a packet structure
Takes a long hex string and turns it into a packet */
lisp fd_parse_packet(fd_u8char *string)
{
  int len=strlen(string), plen;
  fd_u8char *data, *write, *read=string, *limit=read+len;
  if (len%2) plen=(len+1)/2; else plen=len/2;
  data=fd_malloc(plen); write=data; 
  if (len%2) {
    int code=0; sscanf(read,"%1X",&code);
    *write++=(fd_u8char) code; read++;}
  while (read < limit) {
    int code; sscanf(read,"%2X",&code); *write++=(fd_u8char) code;
    if (errno == ERANGE)
      fd_raise_detailed_exception(fd_ParseError,"packet");
    read=read+2;}
  return fd_make_packet(plen,data);
}


/** Standard methods for new user types **/

DTYPES_EXPORT
/* fd_copy_cptr:
    Arguments: a cptr object
    Returns: another cptr object
  Makes a new reference counting CONS for a wrapped cptr.
*/
lisp fd_copy_cptr(lisp x)
{
  return fd_make_cptr(PTR_TYPE(x),CPTR_DATA(x));
}

DTYPES_EXPORT
/* fd_compare_cptrs:
    Arguments: two cptr objects
    Returns: an unsigned int
  Compares the pointers underlying two cptrs.
Returns 1 if they are the same.
*/
unsigned int fd_compare_cptrs(lisp x,lisp y)
{
  if (PTR_TYPE(x) == PTR_TYPE(y))
    return ((CPTR_DATA(x)) == (CPTR_DATA(y)));
  else return 0;
}

DTYPES_EXPORT void fd_free_cptr(lisp x)
/* fd_free_cptr:
    Arguments: a cptr
    Returns: nothing
  Frees the cons used by a cptr.
*/
{
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}


/** Hashtables and Hashsets for LISP **/

DTYPES_EXPORT
/* fd_make_hashtable_for_lisp:
     Arguments: an integer
     Returns: a lisp pointer to a hashtable
*/
lisp fd_make_hashtable_for_lisp(int size)
{
  return fd_make_cptr(hashtable_type,fd_make_hashtable(size));
}

DTYPES_EXPORT
/* fd_make_hashset_for_lisp:
     Arguments: an integer
     Returns: a lisp pointer to a hashtable
*/
lisp fd_make_hashset_for_lisp(int size)
{
  return fd_make_cptr(hashset_type,fd_make_hashset(size));
}

static void print_hashtable(lisp ht,fd_string_stream s)
{
  fd_hashtable h= (fd_hashtable)CPTR_DATA(ht);
  fd_printf(s,"[#HASHTABLE %d/%d]",h->n_keys,h->n_slots);
}

static void free_hashtable(lisp x)
{
  fd_hashtable h=(fd_hashtable)(CPTR_DATA(x));
  fd_free_hashtable(h); fd_free(h,sizeof(struct FD_HASHTABLE));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

static void print_hashset(lisp ht,fd_string_stream s)
{
  fd_hashset h= (fd_hashset) CPTR_DATA(ht);
  fd_printf(s,"[#HASHSET %d/%d]",h->n_keys,h->n_slots);
}

static void free_hashset(lisp x)
{
  fd_hashset h= (fd_hashset) (CPTR_DATA(x));
  fd_free_hashset(h); fd_free(h,sizeof(struct FD_HASHSET));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

DTYPES_EXPORT
/* fd_hashtable_to_alist:
    Arguments: a lisp pointer to a hashtable
    Returns: a lisp association list

 Converts a hashtable to an association list
*/
lisp fd_hashtable_to_alist(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    fd_pair *scan=h->table, *limit=scan+h->n_slots;
    lisp answer=FD_EMPTY_LIST;
    while (scan < limit)
      if ((*scan) && (!(FD_EMPTYP((*scan)->cdr)))) {
	answer=FD_MAKE_PAIR(FD_MAKE_PAIR(incref((*scan)->car),
					 incref((*scan)->cdr)),
			    answer);
	scan++;}
      else scan++;
    return answer;}
  else fd_ctype_error("fd_hashtable_to_alist",_("not a hashtable"),table);
}

DTYPES_EXPORT
/* fd_alist_to_hashtable:
    Arguments: a lisp association list
    Returns: a lisp pointer to a hashtable

 Converts an association list to a hashtable
*/
lisp fd_alist_to_hashtable(lisp alist)
{
  if (PAIRP(alist)) {
    int size=0; lisp table; fd_hashtable h;
    DOLIST(elt,alist)
      if (!(PAIRP(elt)))
	fd_ctype_error("fd_alist_to_hashtable",_("improper alist"),elt);
      else size++;
    table=fd_make_hashtable_for_lisp(size*2);
    h=(fd_hashtable)CPTR_DATA(table);
    {DOLIST(elt,alist) fd_hashtable_set(h,CAR(elt),CDR(elt));}
    return table;}
  else if (FD_EMPTY_LISTP(alist)) return fd_make_hashtable_for_lisp(5);
  else fd_ctype_error("fd_alist_to_hashtable",_("alist elt not a list"),alist);
}

DTYPES_EXPORT
/* fd_lisp_hashset_elts:
    Arguments: a lisp pointer to a hashset
    Returns: a lisp pointer
Returns all the elements of a hashset as a choice. */
lisp fd_lisp_hashset_elts(lisp table)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return fd_hashset_elts(h);}
  else fd_type_error(_("not a hashset"),table);
}

DTYPES_EXPORT lisp fd_values_to_hashset(lisp elts)
/* fd_lisp_hashset_elts:
    Arguments: a lisp pointer
    Returns: a lisp pointer to a hashset
Returns a hashset containing all the elements of the choice
given as an argument. */
{
  lisp set=fd_make_hashset_for_lisp(CHOICE_SIZE(elts)*2);
  fd_hashset h=(fd_hashset)CPTR_DATA(set);
  DO_CHOICES(elt,elts) fd_hashset_add(h,elt); END_DO_CHOICES;
  return set;
}

static lisp init_hashtable(int isize,void *vdata)
{
  int size=isize/2, i=0, j=0;
  lisp *data=(lisp *) vdata;
  fd_hashtable h=fd_make_hashtable(isize);
  while (i < isize) {
    fd_lisp key=data[i], value=data[i+1];
    fd_hashtable_set(h,key,value);
    fd_decref(key); fd_decref(value);
    i=i+2;}
  fd_free(vdata,sizeof(lisp)*isize);
  return fd_make_cptr(hashtable_type,h);
}

static int hashtable_data(lisp x,void **velts)
{
  fd_hashtable h=(fd_hashtable)CPTR_DATA(x);
  fd_lisp *kvals=fd_malloc(sizeof(fd_lisp)*(h->n_keys)*2);
  fd_lisp *kvwrite=kvals;
  fd_pair *scan=h->table, *limit=scan+h->n_slots;
  while (scan < limit)
    if ((*scan) && (!(FD_EMPTYP((*scan)->cdr)))) {
      *kvwrite++=incref((*scan)->car);
      *kvwrite++=incref((*scan)->cdr);      
      scan++;}
    else scan++;
  *velts=(void *)kvals;
  return h->n_keys*2;
}

static void done_with_hashtable_data(fd_lisp x,int size,void **d)
{
  fd_lisp *elts=(fd_lisp *)*d; int i=1;
  while (i < size) {fd_decref(elts[i]); i=i+2;}
  fd_free(elts,sizeof(lisp)*size);
}


/** Character types **/

lisp init_ascii_character(int size,void *data)
{
  unsigned char *v=data; int cd=v[0]; fd_free(data,size);
  if (size != 1) fd_raise_detailed_exception(fd_InvalidDType,_("ascii char")); 
  else return fd_make_character(cd);
}

lisp init_unicode_character(int size,void *data)
{
  fd_u8char *v=data; int cd=(v[0]<<8)|(v[1]); fd_free(data,size);
  if (size != 2)
    fd_raise_detailed_exception(fd_InvalidDType,_("unicode char")); 
  else return fd_make_character(cd);
}


/** Unicode strings **/

static lisp init_unicode_string(int size,void *data)
{
  if (size == 0) return fd_init_string(fd_strdup(""),0);
  else {
    lisp answer;
    unsigned char *cdata=(unsigned char *)data;
    struct FD_STRING_STREAM ss; int i=0;
    FD_INITIALIZE_STRING_STREAM(&ss,size);
    while (i < size) {
      int c1=cdata[i], c2=cdata[i+1], ch=(c1<<8)|c2;
      fd_sputc(&ss,ch); i=i+2;}
    answer=fd_stream_string(&ss); fd_free(data,size);
    return answer;}
}

static lisp init_unicode_zstring(int size,void *data)
{
  if (size == 0) return fd_make_zstring("",0);
  else {
    fd_lisp answer, v;
    unsigned char *cdata=(unsigned char *)data;
    struct FD_STRING_STREAM ss; int i=0;
    FD_INITIALIZE_STRING_STREAM(&ss,size);
    while (i < size) {
      int c1=cdata[i], c2=cdata[i+1], ch=(c1<<8)|c2;
      fd_sputc(&ss,ch); i=i+2;}
    v=fd_stream_string(&ss); fd_free(data,size);
    answer=fd_qify_string(FD_PTR_DATA(v,string));
    fd_decref(v);
    return answer;}
}

static lisp init_unicode_symbol(int size,void *data)
{
  lisp symbol;
  unsigned char *cdata=(unsigned char *)data;
  struct FD_STRING_STREAM ss; int i=0;
  FD_INITIALIZE_STRING_STREAM(&ss,size);
  while (i < size) {
    int c1=cdata[i], c2=cdata[i+1], ch=(c1<<8)|c2;
    fd_sputc(&ss,ch); i=i+2;}
  symbol=fd_make_symbol(ss.ptr); fd_xfree(ss.ptr);
  fd_free(data,size);
  return symbol;
}


/** Homogenous int vectors **/

DTYPES_EXPORT
/* fd_make_int_vector:
    Arguments: an int length and a pointer to an array of ints
    Returns: a homongenous lisp vector of ints */
lisp fd_make_int_vector(int len,int *data)
{
  struct FD_INT_VECTOR *iv=fd_malloca(struct FD_INT_VECTOR);
  iv->n_refs=1; iv->length=len; iv->elements=data;
  {RETURN_LISP(int_vector_type,ivector,iv);}
}

static void print_int_vector(lisp x,fd_string_stream ss)
{
  struct FD_INT_VECTOR *iv=FD_PTR_DATA(x,ivector);
  int *scan=iv->elements, *limit=scan+iv->length;
  fd_printf(ss,"[#IVECTOR/%d:",iv->length);
  if (ss->escape == 0)
    if (iv->length > 100) limit=scan+100;
  while (scan < limit) {
    fd_printf(ss," %d",*scan); scan++;}
  if (ss->escape) fd_printf(ss,"]");
  else if (iv->length > 100) fd_printf(ss,"...]");
  else fd_printf(ss,"]");
}

static lisp copy_int_vector(lisp x)
{
  struct FD_INT_VECTOR *iv=FD_PTR_DATA(x,ivector);
  struct FD_INT_VECTOR *copy=fd_malloca(struct FD_INT_VECTOR);
  int *data_copy=fd_malloc(sizeof(int)*iv->length);
  memcpy(data_copy,iv->elements,sizeof(int)*iv->length);
  copy->n_refs=1; copy->length=iv->length; copy->elements=data_copy;
  {RETURN_LISP(int_vector_type,ivector,iv);}
}

static unsigned int compare_int_vectors(lisp key1,lisp key2)
{
  struct FD_INT_VECTOR *iv1=FD_PTR_DATA(key1,ivector);
  struct FD_INT_VECTOR *iv2=FD_PTR_DATA(key2,ivector);
  if (iv1->length != iv2->length) return 0;
  else {
    int *data1=iv1->elements, *data2=iv2->elements;
    int i=0, len=iv1->length; while (i < len)
      if (data1[i] != data2[i]) return 0; else i++;
    return 1;}
}

static unsigned int hash_int_vector(lisp x,fd_hashfn h)
{
  struct FD_INT_VECTOR *iv=FD_PTR_DATA(x,ivector);
  int i=0, len=iv->length, *data=iv->elements, hash=0;
  while (i < len) hash=hash^data[i++];
  return hash;
}

static void free_int_vector(lisp x)
{
  struct FD_INT_VECTOR *iv=FD_PTR_DATA(x,ivector);
  fd_free(iv->elements,sizeof(int)*iv->length);
  fd_qfree(PTR_DATA(x,ivector),sizeof(struct FD_INT_VECTOR));
}

static lisp init_int_vector(int size,void *vdata)
{
  lisp result;
  int *idata=(int *)vdata, *scan=idata, *limit=idata+size/4;
  /* Normalize byte order */
  if (fd_net_order(3) != 3)
    while (scan < limit) {
      *scan=fd_host_order(*scan); scan++;}
  return fd_make_int_vector(size/4,idata);
}

/* Returns a copy of the data from a short vector in order to
   generate a packaged DTYPE from it.  Note that we could probably
   be clever, on BIG_ENDIAN machines, and just incref(x) and return
   the data currently being used.   But that's for later.  */
static int int_vector_data(lisp x,void **velts)
{
  struct FD_INT_VECTOR *iv=FD_PTR_DATA(x,ivector);
  int i=0, len=iv->length; int *ielts=iv->elements;
  int *v=(int *)fd_memdup((char *)ielts,len*4);
  if (fd_net_order(3) != 3) {
    int *scan=v, *limit=v+len;
    while (scan < limit) {
      *scan=fd_host_order(*scan); scan++;}}
  *velts=(void *) v;
  return len*4;
}

/* Frees the data generated above */
static void done_with_int_vector_data(lisp x,int n,void **velts)
{
  fd_free(*velts,n);
}


/** Homogenous short vectors **/

DTYPES_EXPORT
/* fd_make_short_vector:
    Arguments: a float
    Returns: a LISP floating point number */
lisp fd_make_short_vector(int len,short *data)
{
  struct FD_SHORT_VECTOR *iv=fd_malloca(struct FD_SHORT_VECTOR);
  iv->n_refs=1; iv->length=len; iv->elements=data;
  {RETURN_LISP(short_vector_type,svector,iv);}
}

static void print_short_vector(lisp x,fd_string_stream ss)
{
  struct FD_SHORT_VECTOR *iv=FD_PTR_DATA(x,svector);
  short *scan=iv->elements, *limit=scan+iv->length;
  fd_printf(ss,"[#SVECTOR/%d:",iv->length);
  if (ss->escape == 0)
    if (iv->length > 100) limit=scan+100;
  while (scan < limit) {
    fd_printf(ss," %d",*scan); scan++;}
  if (ss->escape) fd_printf(ss,"]");
  else if (iv->length > 100) fd_printf(ss,"...]");
  else fd_printf(ss,"]");
}

static lisp copy_short_vector(lisp x)
{
  struct FD_SHORT_VECTOR *iv=FD_PTR_DATA(x,svector);
  struct FD_SHORT_VECTOR *copy=fd_malloca(struct FD_SHORT_VECTOR);
  short *data_copy=fd_malloc(sizeof(short)*iv->length);
  memcpy(data_copy,iv->elements,sizeof(short)*iv->length);
  copy->n_refs=1; copy->length=iv->length; copy->elements=data_copy;
  {RETURN_LISP(short_vector_type,svector,iv);}
}

static unsigned int compare_short_vectors(lisp key1,lisp key2)
{
  struct FD_SHORT_VECTOR *iv1=FD_PTR_DATA(key1,svector);
  struct FD_SHORT_VECTOR *iv2=FD_PTR_DATA(key2,svector);
  if (iv1->length != iv2->length) return 0;
  else {
    short *data1=iv1->elements, *data2=iv2->elements;
    int i=0, len=iv1->length; while (i < len)
      if (data1[i] != data2[i]) return 0; else i++;
    return 1;}
}

static unsigned int hash_short_vector(lisp x,fd_hashfn h)
{
  struct FD_SHORT_VECTOR *iv=FD_PTR_DATA(x,svector);
  int i=0, len=iv->length, hash=0;
  short *data=iv->elements;
  while (i < len) hash=hash^data[i++];
  return hash;
}

static void free_short_vector(lisp x)
{
  struct FD_SHORT_VECTOR *iv=FD_PTR_DATA(x,svector);
  fd_free(iv->elements,sizeof(short)*iv->length);
  fd_qfree(PTR_DATA(x,svector),sizeof(struct FD_SHORT_VECTOR));
}

static lisp init_short_vector(int size,void *vdata)
{
  lisp result;
  short *idata=(short *)vdata, *scan=idata, *limit=idata+size/2;
  /* Normalize byte order */
  if (fd_net_order(3) != 3)
    while (scan < limit) {
      *scan=fd_ushort_host_order(*scan); scan++;}
  return fd_make_short_vector(size/2,idata);
}

/* Returns a copy of the data from a short vector in order to
   generate a packaged DTYPE from it.  Note that we could probably
   be clever, on BIG_ENDIAN machines, and just incref(x) and return
   the data currently being used.   But that's for later.  */
static int short_vector_data(lisp x,void **velts)
{
  struct FD_SHORT_VECTOR *iv=FD_PTR_DATA(x,svector);
  int i=0, len=iv->length; short *ielts=iv->elements;
  short *v=(short *)fd_memdup((char *)ielts,len*2);
  if (fd_net_order(3) != 3) {
    short *scan=v, *limit=v+len;
    while (scan < limit) {
      *scan=fd_ushort_net_order(*scan); scan++;}}
  *velts=(char *) v;
  return len*2;
}

/* Frees the data generated above */
static void done_with_short_vector_data(lisp x,int n,void **velts)
{
  fd_free(*velts,n);
}


/** Homogenous float vectors **/

DTYPES_EXPORT
/* fd_make_float_vector:
    Arguments: a int length and a pointer to an array of floats
    Returns: a homongenous floating point vector */
lisp fd_make_float_vector(int len,float *data)
{
  struct FD_FLOAT_VECTOR *iv=fd_malloca(struct FD_FLOAT_VECTOR);
  iv->n_refs=1; iv->length=len; iv->elements=data;
  {RETURN_LISP(float_vector_type,fvector,iv);}
}

static void print_float_vector(lisp x,fd_string_stream ss)
{
  struct FD_FLOAT_VECTOR *iv=FD_PTR_DATA(x,fvector);
  float *scan=iv->elements, *limit=scan+iv->length;
  fd_printf(ss,"[#FVECTOR/%d:",iv->length);
  if (ss->escape == 0)
    if (iv->length > 100) limit=scan+100;
  while (scan < limit) {
    fd_printf(ss," %g",(double)(*scan)); scan++;}
  if (ss->escape) fd_printf(ss,"]");
  else if (iv->length > 100) fd_printf(ss,"...]");
  else fd_printf(ss,"]");
}

static lisp copy_float_vector(lisp x)
{
  struct FD_FLOAT_VECTOR *iv=FD_PTR_DATA(x,fvector);
  struct FD_FLOAT_VECTOR *copy=fd_malloca(struct FD_FLOAT_VECTOR);
  float *data_copy=fd_malloc(sizeof(float)*iv->length);
  memcpy(data_copy,iv->elements,sizeof(float)*iv->length);
  copy->n_refs=1; copy->length=iv->length; copy->elements=data_copy;
  {RETURN_LISP(float_vector_type,fvector,iv);}
}

static unsigned int compare_float_vectors(lisp key1,lisp key2)
{
  struct FD_FLOAT_VECTOR *iv1=FD_PTR_DATA(key1,fvector);
  struct FD_FLOAT_VECTOR *iv2=FD_PTR_DATA(key2,fvector);
  if (iv1->length != iv2->length) return 0;
  else {
    float *data1=iv1->elements, *data2=iv2->elements;
    int i=0, len=iv1->length; while (i < len)
      if (data1[i] != data2[i]) return 0; else i++;
    return 1;}
}

static unsigned int hash_float_vector(lisp x,fd_hashfn h)
{
  struct FD_FLOAT_VECTOR *iv=FD_PTR_DATA(x,fvector);
  int i=0, len=iv->length, hash=0;
  float *data=iv->elements;
  while (i < len) {
    float f=data[i++]; int *ip=(int *)&f;
    hash=hash^*ip;}
  return hash;
}

static void free_float_vector(lisp x)
{
  struct FD_FLOAT_VECTOR *iv=FD_PTR_DATA(x,fvector);
  fd_free(iv->elements,sizeof(float)*iv->length);
  fd_qfree(PTR_DATA(x,fvector),sizeof(struct FD_FLOAT_VECTOR));
}


static lisp init_float_vector(int size,void *vdata)
{
  lisp result;
  int *idata=(int *)vdata, *scan=idata, *limit=idata+size/4;
  assert(sizeof(float) == 4);
  /* Normalize byte order */
  if (fd_net_order(3) != 3)
    while (scan < limit) {
      *scan=fd_host_order(*scan); scan++;}
  return fd_make_float_vector(size/4,(float *)idata);
}

/* Returns a copy of the data from a float vector in order to
   generate a packaged DTYPE from it.  Note that we could probably
   be clever, on BIG_ENDIAN machines, and just incref(x) and return
   the data currently being used.   But that's for later.  */
static int float_vector_data(lisp x,void **velts)
{
  struct FD_FLOAT_VECTOR *iv=FD_PTR_DATA(x,fvector);
  int i=0, len=iv->length; int *ielts=(int *)iv->elements;
  int *v=(int *)fd_memdup((char *)ielts,len*4);
  if (fd_net_order(3) != 3) {
    int *scan=v, *limit=v+len;
    while (scan < limit) {
      *scan=fd_host_order(*scan); scan++;}}
  *velts=(void *) v;
  return len*4;
}

/* Frees the data generated above */
static void done_with_float_vector_data(lisp x,int n,void **velts)
{
  fd_free(*velts,n);
}


/** Homogenous double vectors **/

DTYPES_EXPORT
/* fd_make_double_vector:
    Arguments: a int length and a pointer to an array of doubles
    Returns: a homongenous double floating point vector */
lisp fd_make_double_vector(int len,double *data)
{
  struct FD_DOUBLE_VECTOR *iv=fd_malloca(struct FD_DOUBLE_VECTOR);
  iv->n_refs=1; iv->length=len; iv->elements=data;
  {RETURN_LISP(double_vector_type,dvector,iv);}
}

static void print_double_vector(lisp x,fd_string_stream ss)
{
  struct FD_DOUBLE_VECTOR *iv=FD_PTR_DATA(x,dvector);
  double *scan=iv->elements, *limit=scan+iv->length;
  fd_printf(ss,"[#DVECTOR/%d:",iv->length);
  if (ss->escape == 0)
    if (iv->length > 100) limit=scan+100;
  while (scan < limit) {
    fd_printf(ss," %lg",*scan); scan++;}
  if (ss->escape) fd_printf(ss,"]");
  else if (iv->length > 100) fd_printf(ss,"...]");
  else fd_printf(ss,"]");
}

static lisp copy_double_vector(lisp x)
{
  struct FD_DOUBLE_VECTOR *iv=FD_PTR_DATA(x,dvector);
  struct FD_DOUBLE_VECTOR *copy=fd_malloca(struct FD_DOUBLE_VECTOR);
  double *data_copy=fd_malloc(sizeof(double)*iv->length);
  memcpy(data_copy,iv->elements,sizeof(double)*iv->length);
  copy->n_refs=1; copy->length=iv->length; copy->elements=data_copy;
  {RETURN_LISP(double_vector_type,dvector,iv);}
}

static unsigned int compare_double_vectors(lisp key1,lisp key2)
{
  struct FD_DOUBLE_VECTOR *iv1=FD_PTR_DATA(key1,dvector);
  struct FD_DOUBLE_VECTOR *iv2=FD_PTR_DATA(key2,dvector);
  if (iv1->length != iv2->length) return 0;
  else {
    double *data1=iv1->elements, *data2=iv2->elements;
    int i=0, len=iv1->length; while (i < len)
      if (data1[i] != data2[i]) return 0; else i++;
    return 1;}
}

static unsigned int hash_double_vector(lisp x,fd_hashfn h)
{
  struct FD_DOUBLE_VECTOR *iv=FD_PTR_DATA(x,dvector);
  int i=0, len=iv->length, hash=0;
  double *data=iv->elements;
  while (i < len) {
    float f=(float)data[i++]; int *ip=(int *)&f;
    hash=hash^*ip;}
  return hash;
}

static void free_double_vector(lisp x)
{
  struct FD_DOUBLE_VECTOR *iv=FD_PTR_DATA(x,dvector);
  fd_free(iv->elements,sizeof(double)*iv->length);
  fd_qfree(PTR_DATA(x,dvector),sizeof(struct FD_DOUBLE_VECTOR));
}

static lisp init_double_vector(int size,void *vdata)
{
  char *data=(char *)vdata;
  if (3 != net_order(3)) {
    char *scan=data, *limit=data+size;
    while (scan < limit) {
      char bytes[8]; int i;
      i=0; while (i < 8) {bytes[i]=scan[7-i]; i++;}
      i=0; while (i < 8) {scan[i]=bytes[i]; i++;}
      scan=scan+8;}}
  return fd_make_double_vector(size/8,(double *)data);
}

/* Returns a copy of the data from a double vector in order to
   generate a packaged DTYPE from it.  Note that we could probably
   be clever, on BIG_ENDIAN machines, and just incref(x) and return
   the data currently being used.   But that's for later.  */
static int double_vector_data(lisp x,void **velts)
{
  struct FD_DOUBLE_VECTOR *iv=FD_PTR_DATA(x,dvector);
  uchar *data=(uchar *)fd_memdup((uchar *)iv->elements,iv->length*8);
  if (3 != net_order(3)) {
    uchar *scan=data, *limit=scan+iv->length*8;
    while (scan < limit) {
      char bytes[8]; int i;
      i=0; while (i < 8) {bytes[i]=scan[7-i]; i++;}
      i=0; while (i < 8) {scan[i]=bytes[i]; i++;}
      scan=scan+8;}}
  *velts=(void *) data;
  return iv->length*8;
}

/* Frees the data generated above */
static void done_with_double_vector_data(lisp x,int n,void **velts)
{
  fd_free(*velts,n);
}


/** Special numeric types **/

DTYPES_EXPORT
/* fd_make_flonum:
    Arguments: a float
    Returns: a LISP floating point number */
lisp fd_make_flonum(double f)
{
  struct FD_DOUBLE *dp=fd_malloca(struct FD_DOUBLE);
  dp->n_refs=1; dp->d=f;
  {RETURN_LISP(flonum_type,fdouble,dp);}
}

static lisp init_double(int size,void *vdata)
{
  char *data=(char *)vdata;
  if (size != 8) fd_raise_detailed_exception(fd_InvalidDType,"double");
  else {
    char bytes[8]; double *d=(double *)&bytes;
    if (3 != net_order(3)) {
      int i=0; while (i < 8) {bytes[i]=data[7-i]; i++;}}
    else memcpy(bytes,data,8);
    fd_free(data,8);
    return fd_make_flonum(*d);}
}

/* Returns a byte vector from a double in order to make a
   packaged DTYPE from it. */
static int double_data(lisp x,void **velts)
{
  double d=FLOATLISP(x); char byte[8], *v=fd_malloc(8);
  double *faked= (double *) &byte[0];
  *faked=d; *velts=(void *)v;
  if (3 != net_order(3)) { /* Endianness */
    int i=0; while (i < 8) {v[i]=byte[7-i]; i++;}}
  else memcpy(v,byte,8); 
  return 8;
}

/* Frees the data generated above */
static void done_with_double_data(lisp x,int n,void **velts)
{
  fd_free(*velts,8);
}

DTYPES_EXPORT
/* fd_make_rational:
    Arguments: two lisp numbers
    Returns: a lisp rational */
lisp fd_make_rational(lisp num,lisp denom)
{
  struct FD_RATIONAL_NUMBER *c=fd_malloca(struct FD_RATIONAL_NUMBER);
  c->numerator = num; c->denominator = denom;
  {RETURN_LISP(rational_type,rational_number,c);}
}

static lisp copy_rational(lisp x)
{
  return fd_make_rational
    (copy_lisp(FD_NUMERATOR(x)),copy_lisp(FD_DENOMINATOR(x)));
}

static unsigned int compare_rationals(lisp key0,lisp key1)
{
  return ((LISP_EQUAL(FD_NUMERATOR(key0),FD_NUMERATOR(key1))) &&
	  (LISP_EQUAL(FD_DENOMINATOR(key0),FD_DENOMINATOR(key1))));
}

static unsigned int hash_rational(lisp x,fd_hashfn h)
{
  int h1=(h(FD_NUMERATOR(x))), h2=h(FD_DENOMINATOR(x));
  return ((flip_word(h2)^(h1))%(MAGIC_MODULUS));
}

static void free_rational(lisp x)
{
  decref(FD_NUMERATOR(x)); decref(FD_DENOMINATOR(x));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_RATIONAL_NUMBER));
}

/* Returns the data of the rational to use in producing a packaged
   DTYPE from it. */
static int rational_data(lisp x,void **velts)
{
  lisp *v=fd_malloc(sizeof(lisp)*2);
  v[0]=FD_NUMERATOR(x); v[1]=FD_DENOMINATOR(x);
  *velts=(void *) v;
  return 2;
}

/* Frees the data generated above */
static void done_with_rational_data(lisp x,int n,void **velts)
{
  fd_free(*velts,sizeof(lisp)*2);
}

DTYPES_EXPORT
/* fd_make_complex:
    Arguments: two numbers
    Returns: a lisp complex */
lisp fd_make_complex(lisp real,lisp imag)
{
  struct FD_COMPLEX_NUMBER *c=fd_malloca(struct FD_COMPLEX_NUMBER);
  c->real = real; c->imag = imag;
  {RETURN_LISP(complex_type,complex_number,c);}
}

static lisp copy_complex(lisp x)
{
  return fd_make_complex
    (copy_lisp(FD_REALPART(x)),copy_lisp(FD_IMAGPART(x)));
}

static unsigned int compare_complex(lisp key0,lisp key1)
{
  return ((LISP_EQUAL(FD_REALPART(key0),FD_REALPART(key1))) &&
	  (LISP_EQUAL(FD_IMAGPART(key0),FD_IMAGPART(key1))));
}

static unsigned int hash_complex(lisp x,fd_hashfn h)
{
  return (((flip_word((h(FD_REALPART(x)))%(MAGIC_MODULUS)))%MAGIC_MODULUS)^
	  ((h(FD_IMAGPART(x)))%(MAGIC_MODULUS)));
}

static void free_complex(lisp x)
{
  decref(FD_REALPART(x)); decref(FD_IMAGPART(x));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_COMPLEX_NUMBER));
}

/* Returns the data of the rational to use in producing a packaged
   DTYPE from it. */
static int complex_data(lisp x,void **velts)
{
  lisp *v=fd_malloc(sizeof(lisp)*2);
  v[0]=FD_REALPART(x); v[1]=FD_IMAGPART(x);
  *velts=(void *) v;
  return 2;
}

/* Frees the data generated above */
static void done_with_complex_data(lisp x,int n,void **velts)
{
  fd_free(*velts,sizeof(lisp)*2);
}


/** Timestamps **/

DTYPES_EXPORT
/* fd_make_timestamp:
     Arguments: a time_t value
     Returns: a lisp record whose tag is the symbol timestamp
*/
lisp fd_make_timestamp(time_t moment)
{
  return fd_make_lrecord(timestamp_symbol,LISPFIX(moment));
}

DTYPES_EXPORT
/* fd_make_xtimestamp:
     Arguments: a time_t value, a nanoseconds value, a precision, and a timezone string
     Returns: a lisp record whose tag is the symbol timestamp
*/
lisp fd_make_xtimestamp(time_t moment,int nsecs,fd_tmprec prec,int tzoff)
{
  lisp vec=fd_make_vector(4);
  FD_VECTOR_SET(vec,0,FD_LISPFIX(moment));
  FD_VECTOR_SET(vec,1,FD_LISPFIX(nsecs));
  FD_VECTOR_SET(vec,2,FD_LISPFIX((int)prec));
  FD_VECTOR_SET(vec,3,FD_LISPFIX(tzoff));
  return fd_make_lrecord(timestamp_symbol,vec);
}

static void print_timestamp(lisp timestamp,fd_string_stream ss)
{
  struct FD_XTIME xtime;
  if (fd_timestamp_to_xtime(timestamp,&xtime) >= 0) {
    fd_printf(ss,"#<");
    fd_xtime_to_iso8601(&xtime,ss);
    fd_printf(ss,">");}
  else fd_printf(ss,_("#<Malformed timestamp>")); 
}


/** fd_cons **/

static lisp do_cons(char **fmt,va_list args)
{
  if (**fmt == 0) return FD_VOID;
  else {
    char code=*((*fmt)++);
    if (code == 'q') {
      lisp arg=va_arg(args,lisp);
      return incref(arg);}
    else if (code == 'Q') {
      lisp arg=va_arg(args,lisp);
      return arg;}
    else if (code == 'i')
      return LISPFIX(va_arg(args,int));
    else if (code == 'f')
      return LISPFLOAT(va_arg(args,double));
    else if (code == 'S') { /* make a symbol */
      fd_u8char *s=va_arg(args,char *);
      lisp result=fd_make_symbol(s);
      return result;}
    else if (code == 's') /* locally encoded */
      return fd_make_string(va_arg(args,char *));
    else if (code == 'u') /* UTF-8 encoded */
      return fd_copy_string(va_arg(args,char *));
    else if (code == '(') {
      lisp result=FD_EMPTY_LIST, *tail=&result; char *start=*fmt-1;
      while (**fmt != ')')
	if (**fmt == '.') {
	  lisp cdr; (*fmt)++; cdr=do_cons(fmt,args);
	  *tail=cdr;
	  if (**fmt != ')') 
	    fd_raise_detailed_exception(fd_Invalid_Cons_Format,start);}
	else if (**fmt) {
	  lisp v=do_cons(fmt,args);
	  *tail=FD_MAKE_PAIR(v,FD_EMPTY_LIST);
	  tail=&(CDR(*tail));}
	else fd_raise_detailed_exception(fd_Invalid_Cons_Format,start);
      (*fmt)++;
      return result;}
    else if (code == '{') {
      lisp result=FD_EMPTY_CHOICE; char *start=*fmt-1;
      while (**fmt != '}')
	if (**fmt) {
	  lisp v=do_cons(fmt,args); ADD_TO_CHOICE(result,v);}
	else fd_raise_detailed_exception(fd_Invalid_Cons_Format,start);
      (*fmt)++; return result;}
    else if (code == '\'')
      return FD_MAKE_LIST(2,quote_symbol,do_cons(fmt,args));
    else if (code == '#') {
      if (*((*fmt)++) == '(') {
	lisp lst=FD_EMPTY_LIST, *tail=&lst, result, scan;
	int i=0, length=0; char *start=*fmt-2;
	while (**fmt != ')')
	  if (**fmt) {
	    lisp v=do_cons(fmt,args); length++;
	    *tail=FD_MAKE_PAIR(v,FD_EMPTY_LIST);
	    tail=&(CDR(*tail));}
	  else fd_raise_detailed_exception(fd_Invalid_Cons_Format,start);
	(*fmt)++; scan=lst;
	result=fd_make_vector(length);
	while (i < length) {
	  FD_VECTOR_SET(result,i,incref(CAR(scan)));
	  i++; scan=CDR(scan);}
	decref(lst);
	return result;}
      else fd_raise_detailed_exception(fd_Invalid_Cons_Format,*fmt-1);}
    else fd_raise_detailed_exception(fd_Invalid_Cons_Format,*fmt-1);}
}

DTYPES_EXPORT
/* fd_cons:
     Arguments: a format string and a number of args
     Returns: a lisp object
  Geneates a lisp object based on the format string.  Codes in
the format string are interpreted as follows:
   i  integer
   f  long
   q  lisp pointer (will be incref'd)
   Q  lisp pointer (won't be incref'd)
   s  locally encoded string
   S  locally encoded symbol
   u  utf8 encoded string
   U  utf8 encoded symbol
   (xxx)  list with elements
   {xxx}  choice with elements
   #(xxx) vector with elements
*/
fd_lisp fd_cons(char *format,...)
{
  va_list args; va_start(args,format);
  return do_cons(&format,args);
}


/** Initialization **/

void fd_initialize_xdata_c()
{
  timestamp_symbol=fd_make_symbol("TIMESTAMP0");
  quote_symbol=fd_make_symbol("QUOTE");
  fd_exception_tag=fd_make_symbol("EXCEPTION");
  fd_register_record(fd_exception_tag)->print_fcn=print_exception;
  fd_error_tag=fd_make_symbol("ERROR");
  fd_register_record(fd_error_tag)->print_fcn=print_error;

  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(packet_type);
    r->copy_fcn=copy_packet; r->gc_fcn=free_packet;
    r->compare_fcn=compare_packets; r->hash_fcn=hash_packet;
    r->print_fcn=print_packet;}

  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(int_vector_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_short_int_vector;
    r->print_fcn=print_int_vector; r->copy_fcn=copy_int_vector;
    r->gc_fcn=free_int_vector;
    r->package_data_fcn=int_vector_data;
    r->package_data_done_fcn=done_with_int_vector_data;
    r->compare_fcn=compare_int_vectors;
    r->hash_fcn=hash_int_vector;
    r->package_restore_fcn=init_int_vector;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(short_vector_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_short_short_vector;
    r->print_fcn=print_short_vector; r->copy_fcn=copy_short_vector;
    r->gc_fcn=free_short_vector;
    r->package_data_fcn=short_vector_data;
    r->package_data_done_fcn=done_with_short_vector_data;
    r->compare_fcn=compare_short_vectors;
    r->hash_fcn=hash_short_vector;
    r->package_restore_fcn=init_short_vector;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(float_vector_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_short_float_vector;
    r->print_fcn=print_float_vector; r->copy_fcn=copy_float_vector;
    r->gc_fcn=free_float_vector;
    r->package_data_fcn=float_vector_data;
    r->package_data_done_fcn=done_with_float_vector_data;
    r->compare_fcn=compare_float_vectors;
    r->hash_fcn=hash_float_vector;
    r->package_restore_fcn=init_float_vector;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(double_vector_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_short_double_vector;
    r->print_fcn=print_double_vector; r->copy_fcn=copy_double_vector;
    r->gc_fcn=free_double_vector;
    r->package_data_fcn=double_vector_data;
    r->package_data_done_fcn=done_with_double_vector_data;
    r->compare_fcn=compare_double_vectors;
    r->hash_fcn=hash_double_vector;
    r->package_restore_fcn=init_double_vector;}

  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(rational_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_rational;
    r->copy_fcn=copy_rational; r->gc_fcn=free_rational;
    r->package_data_fcn=rational_data;
    r->package_data_done_fcn=done_with_rational_data;
    r->compare_fcn=compare_rationals;
    r->hash_fcn=hash_rational;
    r->package_restore_fcn=init_rational;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(complex_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_complex;
    r->copy_fcn=copy_complex; r->gc_fcn=free_complex;
    r->compare_fcn=compare_complex;
    r->hash_fcn=hash_complex;
    r->package_data_fcn=complex_data;
    r->package_data_done_fcn=done_with_complex_data;
    r->package_restore_fcn=init_complex;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(flonum_type);
    r->package_code=dt_extended_numeric; r->subcode=dt_double;
    r->package_data_fcn=double_data;
    r->package_data_done_fcn=done_with_double_data;
    r->package_restore_fcn=init_double;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(hashtable_type);
    r->package_code=dt_framerd; r->subcode=dt_small_hashtable;
    r->gc_fcn=free_hashtable;
    r->print_fcn=print_hashtable;
    r->compound_tag=fd_make_symbol("HASHTABLE");
    r->compound_dump_fcn=fd_hashtable_to_alist;
    r->compound_restore_fcn=fd_alist_to_hashtable;
    r->package_data_fcn=hashtable_data; 
    r->package_data_done_fcn=done_with_hashtable_data;
    r->package_restore_fcn=init_hashtable;}
  { 
    struct FD_TYPE_REGISTRY *r=fd_register_typecode(hashset_type);
    r->gc_fcn=free_hashset;
    r->print_fcn=print_hashset;
    r->compound_tag=fd_make_symbol("HASHSET");
    r->compound_dump_fcn=fd_lisp_hashset_elts;
    r->compound_restore_fcn=fd_values_to_hashset;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_record((FD_EMPTY_CHOICE));
    r->package_code=dt_extended_character; r->subcode=dt_ascii_char;
    r->package_restore_fcn=init_ascii_character;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_record((FD_EMPTY_CHOICE));
    r->package_code=dt_extended_character; r->subcode=dt_unicode_char;
    r->package_restore_fcn=init_unicode_character;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_record((FD_EMPTY_CHOICE));
    r->package_code=dt_extended_character;
    r->subcode=dt_unicode_short_string;
    r->package_restore_fcn=init_unicode_string;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_record((FD_EMPTY_CHOICE));
    r->package_code=dt_extended_character;
    r->subcode=dt_unicode_short_zstring;
    r->package_restore_fcn=init_unicode_zstring;}
  {
    struct FD_TYPE_REGISTRY *r=fd_register_record((FD_EMPTY_CHOICE));
    r->package_code=dt_extended_character;
    r->subcode=dt_unicode_short_symbol;
    r->package_restore_fcn=init_unicode_symbol;}
  fd_register_source_file("xdata",__DATE__,vcid);

  {
    struct FD_TYPE_REGISTRY *r=fd_register_record(timestamp_symbol);
    r->print_fcn=print_timestamp;}

}


/* File specific stuff */

/* The CVS log for this file
   $Log: xdata.c,v $
   Revision 1.16  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.15  2004/08/24 15:24:30  haase
   Added a dt_hashtable type in the dt_framerd package

   Revision 1.14  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.13  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.12  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11.2.1  2003/01/26 20:36:13  haase
   Introduced zstrings

   Revision 1.11  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.10  2002/05/13 06:50:19  haase
   Fixed erroneous call to fd_free (vs. fd_xfree) in freeing the data from writing dtypes from homogeous arrays

   Revision 1.9  2002/04/30 13:48:33  haase
   Made packaged format for homogenous vectors be bytes rather than lisp vectors, saving space in the external DType representation

   Revision 1.8  2002/04/11 03:05:09  haase
   Fixed some typos in type handlers for homogenous vectors (especially copying)

   Revision 1.7  2002/04/04 18:48:53  haase
   Added homongenous vectors of ints, shorts, floats, and doubles.
   Also changed the field "size" in some structs to "length" to indicate
   that the underlying data structure is ordered.

   Revision 1.6  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
