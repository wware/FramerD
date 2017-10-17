/* C Mode */

/* portahash.c
   Implements a portable hash function for DType objects.
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

static char vcid[] = "$Id: portahash.c,v 1.23 2006/06/27 11:41:06 haase Exp $";

#define FD_INLINE_STRING_STREAMS 1

#include "framerd.h"

static unsigned int hash_mystery(struct FD_MYSTERY *m);
static unsigned int hash_mystery2(struct FD_MYSTERY *m);

/* Used to generate hash codes */
#define MAGIC_MODULUS 16777213 /* 256000001 */
#define MIDDLIN_MODULUS 573786077 /* 256000001 */
#define MYSTERIOUS_MODULUS 2000239099 /* 256000001 */

/** Hashing lisp objects **/

/* hash_string_dtype: (static)
     Arguments: a lisp pointer (to a string)
     Returns: a hash value (an unsigned int)

  Computes an iterative hash over the characters in the string.
*/
FASTOP unsigned int hash_string_dtype (lisp x)
{
  char *ptr=STRING_DATA(x), *limit=ptr+STRING_LENGTH(x);
  unsigned int sum=0;
  while (ptr < limit) {
    sum=(sum<<8)+(*ptr++); 
    sum=sum%(MAGIC_MODULUS);}
  return sum;
}

/* hash_unicode_string_dtype: (static)
     Arguments: a lisp pointer (to a string)
     Returns: a hash value (an unsigned int)

  Computes an iterative hash over the characters in the string.
*/
static unsigned int hash_unicode_string_dtype (lisp x)
{
  unsigned char *ptr=STRING_DATA(x), *limit=ptr+STRING_LENGTH(x);
  unsigned int sum=0;
  while (ptr < limit) {
    int c=fd_sgetc(&ptr);
    sum=(sum<<8)+(c);
    sum=sum%(MAGIC_MODULUS);}
  return sum;
}

/* hash_symbol_dtype: (static)
     Arguments: a lisp pointer (to a symbol)
     Returns: a hash value (an unsigned int)

  Computes an iterative hash over the characters in the symbol's name.
*/
FASTOP unsigned int hash_symbol_dtype(lisp x)
{
  unsigned char *s = SYMBOL_NAME (x);
  unsigned int sum=0;
  while (*s != '\0') {
    unichar_t c;
    if (*s < 0x80) c=*s++;
    else if (*s < 0xe0) {
      c=(unichar_t)((*s++)&(0x3f)<<6);
      c=c|((unichar_t)((*s++)&(0x3f)));}
    else {
      c=(unichar_t)((*s++)&(0x1f)<<12);
      c=c|(unichar_t)(((*s++)&(0x3f))<<6);
      c=c|(unichar_t)((*s++)&(0x3f));}
    sum=(sum<<8)+(c); sum=sum%(MAGIC_MODULUS);}
  return sum;
}

static unsigned int hash_pair_dtype(lisp string);
static unsigned int hash_record_dtype(lisp string);

/*
   hash_dtype: (static inline)
     Arguments: a "lisp" pointer
     Returns: an unsigned int computed from the pointer

     Basic strategy:
      integers become themselves, immediates get special codes,
       strings and symbols get characters scanned, objects use their
       low order bytes, vectors and pairs get their elements combined
       with an ordering (to keep it asymmetric).

     Notes: we assume 24 bit hash values so that any LISP implementation
      can compute hashes using just fixnum arithmetic.
*/
FASTOP unsigned int hash_dtype(lisp x)
{
  if (FD_IMMEDIATEP(x))
    if ((FD_EMPTY_LISTP(x)) || (FD_FALSEP(x))) return 37;
    else if (FD_TRUEP(x)) return 17;
    else if (FD_EMPTYP(x)) return 13;
    else {
      fd_fprintf(stderr,_("Strange immediate: %q"),x);
      return 19;}
  else if (FIXNUMP(x))
    return (FIXLISP(x))%(MAGIC_MODULUS);
  else if (ASCII_STRINGP(x))
    return hash_string_dtype(x);
  else if (UNICODE_STRINGP(x))
    return hash_unicode_string_dtype(x);
  else if (PAIRP(x))
    return hash_pair_dtype(x);
  else if (SYMBOLP(x))
    return hash_symbol_dtype(x);
  else if (OIDP(x)) {
    FD_OID id=OID_ADDR(x);
#if FD_OIDS_ARE_SCALARS
    return id%(MAGIC_MODULUS);
#else
    unsigned int hi=FD_OID_HIGH(id), lo=FD_OID_LOW(id);
    int i=0; while (i++ < 4)
      {hi=((hi<<8)|(lo>>24))%(MAGIC_MODULUS); lo=lo<<8;}
    return hi;
#endif
      }
  else if (CHOICEP(x)) {
    unsigned int sum=0;
    DO_CHOICES(elt,x)
      sum=((sum+hash_dtype(elt))%MAGIC_MODULUS);
    END_DO_CHOICES;
    return sum;}
  else if (CHARACTERP(x)) return (CHAR_CODE(x))%(MAGIC_MODULUS);
  else if (FLONUMP(x)) {
    unsigned int as_int; float *f=(float *)(&as_int);
    *f=FLOATLISP(x);
    return (as_int)%(MAGIC_MODULUS);}
  else if (VECTORP(x)) {
    int size=VECTOR_LENGTH(x); unsigned int sum=0;
    DOTIMES(i,size)
      sum=(((sum<<4)+(hash_dtype(VECTOR_REF(x,i))))%MAGIC_MODULUS);
    return sum;}
  else if (SLOTMAPP(x)) {
    unsigned int sum=0;
    DO_SLOTS(k,v,x) {
      sum=(sum+hash_dtype(k))%MAGIC_MODULUS;
      sum=(sum+(flip_word(hash_dtype(v))%MAGIC_MODULUS))%MAGIC_MODULUS;}
    return sum;}
  else if (FD_RATIONALP(x)) {
    unsigned int sum=hash_dtype(FD_NUMERATOR(x))%MAGIC_MODULUS;
    sum=(sum<<4)+hash_dtype(FD_DENOMINATOR(x))%MAGIC_MODULUS;
    return sum%MAGIC_MODULUS;}
  else if (FD_COMPLEXP(x)) {
    unsigned int sum=hash_dtype(FD_REALPART(x))%MAGIC_MODULUS;
    sum=(sum<<4)+hash_dtype(FD_IMAGPART(x))%MAGIC_MODULUS;
    return sum%MAGIC_MODULUS;}
  else if (LRECORDP(x)) {
    unsigned int sum=hash_dtype(LRECORD_TAG(x))%MAGIC_MODULUS;
    sum=(sum<<4)+hash_dtype(LRECORD_DATA(x))%MAGIC_MODULUS;
    return sum%MAGIC_MODULUS;}
  else if (RECORDP(x)) return hash_record_dtype(x);
  else if (FD_PRIM_TYPEP(x,mystery_type))
    return hash_mystery(((struct FD_MYSTERY *)FD_CPTR_DATA(x)));
  else {
    struct FD_TYPE_REGISTRY *r=fd_lookup_typecode(PTR_TYPE(x));
    if (r == NULL) fd_raise_exception(fd_NoHashMethod);
    else if (r->hash_fcn) return r->hash_fcn(x,hash_dtype);
    else if (r->package_data_fcn) {
      unsigned char *data; int n_bytes; unsigned int sum=0;
      unsigned char *ptr, *limit;
      n_bytes=r->package_data_fcn(x,(void *)&data);
      ptr=data; limit=ptr+n_bytes;
      while (ptr < limit) {
	sum=(sum<<8)+(*ptr++); sum=sum%(MAGIC_MODULUS);}
      if (r->package_data_done_fcn)
	r->package_data_done_fcn(x,n_bytes,(void **)&data);
      return sum%(MAGIC_MODULUS);}
    else fd_raise_exception(fd_NoHashMethod);}
}

static unsigned int hash_mystery(struct FD_MYSTERY *m)
{
  unsigned char *data; int n_bytes; unsigned int sum=0;
  unsigned char *ptr=m->data.bytes, *limit=ptr+m->length;
  while (ptr < limit) {
    sum=(sum<<8)+(*ptr++); sum=sum%(MAGIC_MODULUS);}
  return sum;
}

/* hash_pair_dtype: (static)
     Arguments: a lisp pointer (to a pair)
     Returns: a hash value (an unsigned int)

     Adds the hash of the list's elements together, shifting each
     value by 4 bits and ending with the final CDR.  The shift is
     done to have the hash function be sensitive to element permuations.
*/
static unsigned int hash_pair_dtype(lisp x)
{
  lisp ptr=x; unsigned int sum=0;
  /* The shift here is introduced to make the hash function asymmetric */
  while (PAIRP(ptr)) {
    sum=((sum*2)+hash_dtype(CAR(ptr)))%(MAGIC_MODULUS);
    ptr=CDR(ptr);}
  if (!(FD_EMPTY_LISTP(ptr))) {
    unsigned int cdr_hash=hash_dtype(ptr);
    sum=(sum+(flip_word(cdr_hash)>>8))%(MAGIC_MODULUS);}
  return sum;
}

/* hash_record_dtype: (static)
     Arguments: a lisp pointer (to a record)
     Returns: a hash value (an unsigned int)

   Uses a variety of methods to compute a hash function
*/
static unsigned int hash_record_dtype(lisp x)
{
  struct FD_TYPE_REGISTRY *entry=fd_lookup_record(RECORD_TAG(x));
  if (entry) {
    if (entry->compound_dump_fcn) {
      lisp dump=entry->compound_dump_fcn(x);
      unsigned int hash=hash_dtype(dump);
      decref(dump);
      return hash;}
    else if ((entry->package_data_fcn) && ((entry->subcode) & 0x80)) {
      lisp *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      lisp *ptr=data, *limit=ptr+size;
      unsigned int sum=0;
      while (ptr < limit) {
	sum=((sum<<4)+(hash_dtype(*ptr)))%MAGIC_MODULUS; ptr++;}
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return sum;}
    else {
      unsigned char *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      unsigned char *ptr=data, *limit=ptr+size;
      unsigned int sum=0;
      while (ptr < limit) sum=((sum<<4)+(*ptr++))%MAGIC_MODULUS;
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return sum;}}
  else fd_raise_lisp_exception
	 ("Can't store in index file","unknown type",x);
}

FRAMERD_EXPORT
/* fd_hash_dtype:
     Arguments: a list pointer
     Returns: an unsigned int
  This returns an integer corresponding to its argument which will
   be the same on any platform.
*/
unsigned int fd_hash_dtype(lisp x)
{
  return hash_dtype(x);
}

/* Multiplier heavy hashing */

static unsigned int hash_pair_dtype2(lisp x);
static unsigned int hash_record_dtype2(lisp x);

typedef unsigned long long ull;

static unsigned int hash_mult(unsigned int x,unsigned int y)
{
  unsigned long long prod=((ull)x)*((ull)y);
  return (prod*2100000523)%(MYSTERIOUS_MODULUS);
}

static unsigned int non_hash_mult(unsigned int x,unsigned int y)
{
  if (x == 1) return y;
  else if (y == 1) return x;
  if ((x == 0) || (y == 0)) return 0;
  else {
    unsigned int a=(x>>16), b=(x&0xFFFF); 
    unsigned int c=(y>>16), d=(y&0xFFFF); 
    unsigned int bd=b*d, ad=a*d, bc=b*c, ac=a*c;
    unsigned int hi=ac, lo=(bd&0xFFFF), tmp, carry, i;
    tmp=(bd>>16)+(ad&0xFFFF)+(bc&0xFFFF);
    lo=lo+((tmp&0xFFFF)<<16); carry=(tmp>>16);
    hi=hi+carry+(ad>>16)+(bc>>16);
    i=0; while (i++ < 4) {
      hi=((hi<<8)|(lo>>24))%(MYSTERIOUS_MODULUS); lo=lo<<8;}
    return hi;}
}

static unsigned int hash_combine(unsigned int x,unsigned int y)
{
  if ((x == 0) && (y == 0)) return MAGIC_MODULUS+2;
  else if ((x == 0) || (y == 0))
    return x+y;
  else return hash_mult(x,y);
}

FASTOP unsigned int mult_hash_string(unsigned char *start,int len)
{
  unsigned int prod=1, asint;
  unsigned char *ptr=start, *limit=ptr+len;
  /* Compute a starting place */
  while (ptr < limit) prod=prod+*ptr++;
  /* Now do a multiplication */
  ptr=start; limit=ptr+((len%4) ? (4*(len/4)) : (len));
  while (ptr < limit) {
    asint=(ptr[0]<<24)|(ptr[1]<<16)|(ptr[2]<<8)|(ptr[3]);
    prod=hash_combine(prod,asint); ptr=ptr+4;}
  switch (len%4) {
  case 0: asint=1; break;
  case 1: asint=ptr[0]; break;
  case 2: asint=ptr[0]|(ptr[1]<<8); break;
  case 3: asint=ptr[0]|(ptr[1]<<8)|(ptr[2]<<16); break;}
  return hash_combine(prod,asint);
}

/* hash_string_dtype2: (static)
     Arguments: a lisp pointer (to a string)
     Returns: a hash value (an unsigned int)

  Computes an iterative hash over the characters in the string.
*/
FASTOP unsigned int hash_string_dtype2(lisp x)
{
  int len=STRING_LENGTH(x);
  if (len == 0) return MAGIC_MODULUS+2;
  else if (len == 1)
    if (*(FD_STRING_DATA(x))) return (*(FD_STRING_DATA(x)));
    else return MAGIC_MODULUS-2;
  else return mult_hash_string(FD_STRING_DATA(x),len);
}

FASTOP unsigned int hash_dtype2(lisp x)
{
  if (FD_IMMEDIATEP(x))
    if ((FD_EMPTY_LISTP(x)) || (FD_FALSEP(x))) return 37;
    else if (FD_TRUEP(x)) return 17;
    else if (FD_EMPTYP(x)) return 13;
    else {
      fd_fprintf(stderr,_("Strange immediate: %q"),x);
      return 19;}
  else if (FIXNUMP(x))
    return (FIXLISP(x))%(MAGIC_MODULUS);
  else if (STRINGP(x))
    return hash_string_dtype2(x);
  else if (PAIRP(x))
    return hash_pair_dtype2(x);
  else if (SYMBOLP(x))
    return hash_symbol_dtype(x);
  else if (OIDP(x)) {
    FD_OID id=OID_ADDR(x);
#if FD_OIDS_ARE_SCALARS
    return id%(MAGIC_MODULUS);
#else
    unsigned int hi=FD_OID_HIGH(id), lo=FD_OID_LOW(id);
    int i=0; while (i++ < 4) {
      hi=((hi<<8)|(lo>>24))%(MAGIC_MODULUS); lo=lo<<8;}
    return hi;
#endif
  }
  else if (CHOICEP(x)) {
    unsigned int sum=0;
    DO_CHOICES(elt,x)
      sum=(sum+(hash_dtype2(elt))%MAGIC_MODULUS);
    END_DO_CHOICES;
    return sum;}
  else if (CHARACTERP(x)) return (CHAR_CODE(x))%(MAGIC_MODULUS);
  else if (FLONUMP(x)) {
    unsigned int as_int; float *f=(float *)(&as_int);
    *f=FLOATLISP(x);
    return (as_int)%(MAGIC_MODULUS);}
  else if (VECTORP(x)) {
    int size=VECTOR_LENGTH(x); unsigned int prod=1;
    DOTIMES(i,size)
      prod=hash_combine(prod,hash_dtype2(VECTOR_REF(x,i)));
    return prod;}
  else if (SLOTMAPP(x)) {
    unsigned int sum=0;
    DO_SLOTS(k,v,x) {
      unsigned int prod=hash_combine(hash_dtype2(k),hash_dtype2(v));
      sum=(sum+prod)%(MYSTERIOUS_MODULUS);}
    return sum;}
  else if (FD_RATIONALP(x)) 
    return hash_combine(hash_dtype2(FD_NUMERATOR(x)),
		     hash_dtype2(FD_DENOMINATOR(x)));
  else if (FD_COMPLEXP(x))
    return hash_combine(hash_dtype2(FD_NUMERATOR(x))%MAGIC_MODULUS,
		     hash_dtype2(FD_DENOMINATOR(x)));
  else if (LRECORDP(x)) {
    unsigned int taghash=hash_dtype2(LRECORD_TAG(x));
    unsigned int datahash=hash_dtype2(LRECORD_DATA(x));
    return hash_combine(datahash,((taghash^datahash)&0x7FFFFFFF));}
  else if (RECORDP(x)) return hash_record_dtype2(x);
  else if (FD_PRIM_TYPEP(x,mystery_type))
    return hash_mystery2(((struct FD_MYSTERY *)FD_CPTR_DATA(x)));
  else {
    struct FD_TYPE_REGISTRY *r=fd_lookup_typecode(PTR_TYPE(x));
    if (r == NULL) fd_raise_exception(fd_NoHashMethod);
    else if (r->hash_fcn) return r->hash_fcn(x,hash_dtype2);
    else if (r->package_data_fcn) {
      unsigned char *data; int n_bytes; unsigned int hash=0;
      n_bytes=r->package_data_fcn(x,(void *)&data);
      hash=mult_hash_string(data,n_bytes);
      if (r->package_data_done_fcn)
	r->package_data_done_fcn(x,n_bytes,(void **)&data);
      return hash%(MYSTERIOUS_MODULUS);}
    else fd_raise_exception(fd_NoHashMethod);}
}

FRAMERD_EXPORT
/* fd_hash_dtype2:
     Arguments: a list pointer
     Returns: an unsigned int
  This is a better hashing algorithm than the legacy used for years.
*/
unsigned int fd_hash_dtype2(lisp x)
{
  return hash_dtype2(x);
}

/* hash_pair_dtype2: (static)
     Arguments: a lisp pointer (to a pair)
     Returns: a hash value (an unsigned int)
*/
static unsigned int hash_pair_dtype2(lisp x)
{
  lisp ptr=x; unsigned int prod=1;
  while (PAIRP(ptr)) {
    prod=hash_combine(prod,hash_dtype2(CAR(ptr)));
    ptr=CDR(ptr);}
  if (!(FD_EMPTY_LISTP(ptr))) {
    unsigned int cdr_hash=hash_dtype2(ptr);
    prod=hash_combine(prod,cdr_hash);}
  return prod;
}

static unsigned int hash_mystery2(struct FD_MYSTERY *m)
{
  
  return mult_hash_string(m->data.bytes,m->length);
}

/* hash_record_dtype2: (static)
     Arguments: a lisp pointer (to a record)
     Returns: a hash value (an unsigned int)

   Uses a variety of methods to compute a hash function
*/
static unsigned int hash_record_dtype2(lisp x)
{
  fd_lisp tag=FD_RECORD_TAG(x);
  struct FD_TYPE_REGISTRY *entry=fd_lookup_record(tag);
  if (entry) {
    if (entry->compound_dump_fcn) {
      lisp dump=entry->compound_dump_fcn(x);
      unsigned int hash=hash_dtype2(dump);
      decref(dump);
      return hash;}
    else if ((entry->package_data_fcn) && ((entry->subcode) & 0x80)) {
      lisp *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      lisp *ptr=data, *limit=ptr+size;
      unsigned int prod=hash_dtype2(tag);
      while (ptr < limit) {
	prod=hash_combine(prod,hash_dtype2(*ptr)); ptr++;}
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return prod;}
    else {
      unsigned char *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      unsigned int hash=mult_hash_string(data,size);
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return hash_combine(hash_dtype2(tag),hash);}}
  else fd_raise_lisp_exception
	 ("Can't store in index file","unknown type",x);
}

/* hash3 */

static unsigned int hash_pair_dtype3(lisp x);
static unsigned int hash_mystery3(struct FD_MYSTERY *m);
static unsigned int hash_record_dtype3(lisp x);

/* hash_string_dtype3: (static)
     Arguments: a lisp pointer (to a string)
     Returns: a hash value (an unsigned int)

  Computes an iterative hash over the characters in the string.
*/
FASTOP unsigned int hash_string_dtype3(lisp x)
{
  int len=STRING_LENGTH(x);
  if (len == 0) return MAGIC_MODULUS+2;
  else if (len == 1)
    if (*(FD_STRING_DATA(x))) return (*(FD_STRING_DATA(x)));
    else return MAGIC_MODULUS-2;
  else return mult_hash_string(FD_STRING_DATA(x),len);
}

FASTOP unsigned int hash_dtype3(lisp x)
{
  if (FD_IMMEDIATEP(x))
    if ((FD_EMPTY_LISTP(x)) || (FD_FALSEP(x))) return 37;
    else if (FD_TRUEP(x)) return 17;
    else if (FD_EMPTYP(x)) return 13;
    else {
      fd_fprintf(stderr,_("Strange immediate: %q"),x);
      return 19;}
  else if (FIXNUMP(x))
    return (FIXLISP(x))%(MAGIC_MODULUS);
  else if (STRINGP(x))
    return hash_string_dtype3(x);
  else if (PAIRP(x))
    return hash_pair_dtype3(x);
  else if (SYMBOLP(x))
    return hash_symbol_dtype(x);
  else if (OIDP(x)) {
    FD_OID id=OID_ADDR(x);
#if FD_OIDS_ARE_SCALARS
    return id%(MYSTERIOUS_MODULUS);
#else
    unsigned int hi=FD_OID_HIGH(id), lo=FD_OID_LOW(id);
    int i=0; while (i++ < 4) {
      hi=((hi<<8)|(lo>>24))%(MYSTERIOUS_MODULUS); lo=lo<<8;}
    return hi;
#endif
  }
  else if (CHOICEP(x)) {
    unsigned int sum=0;
    DO_CHOICES(elt,x)
      sum=(sum+(hash_dtype3(elt))%MIDDLIN_MODULUS);
    END_DO_CHOICES;
    return sum;}
  else if (CHARACTERP(x)) return (CHAR_CODE(x))%(MAGIC_MODULUS);
  else if (FLONUMP(x)) {
    unsigned int as_int; float *f=(float *)(&as_int);
    *f=FLOATLISP(x);
    return (as_int)%(MAGIC_MODULUS);}
  else if (VECTORP(x)) {
    int size=VECTOR_LENGTH(x); unsigned int prod=1;
    DOTIMES(i,size)
      prod=hash_combine(prod,hash_dtype3(VECTOR_REF(x,i)));
    return prod;}
  else if (SLOTMAPP(x)) {
    unsigned int sum=0;
    DO_SLOTS(k,v,x) {
      unsigned int prod=hash_combine(hash_dtype3(k),hash_dtype3(v));
      sum=(sum+prod)%(MYSTERIOUS_MODULUS);}
    return sum;}
  else if (FD_RATIONALP(x)) 
    return hash_combine(hash_dtype3(FD_NUMERATOR(x)),
		     hash_dtype3(FD_DENOMINATOR(x)));
  else if (FD_COMPLEXP(x))
    return hash_combine(hash_dtype3(FD_NUMERATOR(x))%MAGIC_MODULUS,
		     hash_dtype3(FD_DENOMINATOR(x)));
  else if (LRECORDP(x)) {
    unsigned int taghash=hash_dtype3(LRECORD_TAG(x));
    unsigned int datahash=hash_dtype3(LRECORD_DATA(x));
    return hash_combine(datahash,((taghash^datahash)&0x7FFFFFFF));}
  else if (RECORDP(x)) return hash_record_dtype3(x);
  else if (FD_PRIM_TYPEP(x,mystery_type))
    return hash_mystery3(((struct FD_MYSTERY *)FD_CPTR_DATA(x)));
  else {
    struct FD_TYPE_REGISTRY *r=fd_lookup_typecode(PTR_TYPE(x));
    if (r == NULL) fd_raise_exception(fd_NoHashMethod);
    else if (r->hash_fcn) return r->hash_fcn(x,hash_dtype3);
    else if (r->package_data_fcn) {
      unsigned char *data; int n_bytes; unsigned int hash=0;
      n_bytes=r->package_data_fcn(x,(void *)&data);
      hash=mult_hash_string(data,n_bytes);
      if (r->package_data_done_fcn)
	r->package_data_done_fcn(x,n_bytes,(void **)&data);
      return hash%(MYSTERIOUS_MODULUS);}
    else fd_raise_exception(fd_NoHashMethod);}
}

FRAMERD_EXPORT
/* fd_hash_dtype3:
     Arguments: a list pointer
     Returns: an unsigned int
  This is a better hashing algorithm than the legacy used for years.
*/
unsigned int fd_hash_dtype3(lisp x)
{
  return hash_dtype3(x);
}

/* hash_pair_dtype3: (static)
     Arguments: a lisp pointer (to a pair)
     Returns: a hash value (an unsigned int)
*/
static unsigned int hash_pair_dtype3(lisp x)
{
  lisp ptr=x; unsigned int prod=1;
  while (PAIRP(ptr)) {
    prod=hash_combine(prod,hash_dtype3(CAR(ptr)));
    ptr=CDR(ptr);}
  if (!(FD_EMPTY_LISTP(ptr))) {
    unsigned int cdr_hash=hash_dtype3(ptr);
    prod=hash_combine(prod,cdr_hash);}
  return prod;
}

static unsigned int hash_mystery3(struct FD_MYSTERY *m)
{
  
  return mult_hash_string(m->data.bytes,m->length);
}

/* hash_record_dtype3: (static)
     Arguments: a lisp pointer (to a record)
     Returns: a hash value (an unsigned int)

   Uses a variety of methods to compute a hash function
*/
static unsigned int hash_record_dtype3(lisp x)
{
  fd_lisp tag=FD_RECORD_TAG(x);
  struct FD_TYPE_REGISTRY *entry=fd_lookup_record(tag);
  if (entry) {
    if (entry->compound_dump_fcn) {
      lisp dump=entry->compound_dump_fcn(x);
      unsigned int hash=hash_dtype3(dump);
      decref(dump);
      return hash;}
    else if ((entry->package_data_fcn) && ((entry->subcode) & 0x80)) {
      lisp *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      lisp *ptr=data, *limit=ptr+size;
      unsigned int prod=hash_dtype3(tag);
      while (ptr < limit) {
	prod=hash_combine(prod,hash_dtype3(*ptr)); ptr++;}
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return prod;}
    else {
      unsigned char *data;
      unsigned int size=entry->package_data_fcn(x,(void **)&data);
      unsigned int hash=mult_hash_string(data,size);
      if (entry->package_data_done_fcn)
	entry->package_data_done_fcn(x,size,(void **)&data);
      return hash_combine(hash_dtype3(tag),hash);}}
  else fd_raise_lisp_exception
	 ("Can't store in index file","unknown type",x);
}




/* File specific stuff */

/* The CVS log for this file
   $Log: portahash.c,v $
   Revision 1.23  2006/06/27 11:41:06  haase
   Added experimental v3 hash function

   Revision 1.22  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.21  2004/09/15 17:11:40  haase
   Fixed bug in possible mystery (bignum) hashing

   Revision 1.20  2004/09/15 12:38:50  haase
   Fixed hashing default for OIDs

   Revision 1.19  2004/09/08 18:34:12  haase
   Made hashing work for mystery dtypes

   Revision 1.18  2004/07/20 17:57:48  haase
   New hash algorith constants

   Revision 1.17  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.16  2004/07/16 17:51:58  haase
   Fix doc header for hash functions

   Revision 1.15  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.14  2004/05/03 22:49:22  haase
   New portahash function

   Revision 1.3  2004/04/26 11:15:50  haase
   Patches to index hash patches

   Revision 1.2  2004/04/25 23:10:29  haase
   Fixed fixed point in the new hashing algorithm

   Revision 1.1.1.1  2004/04/06 11:15:24  haase
   Initial import of proprietary FramerD into beingmeta CVS

   Revision 1.13  2004/03/31 17:44:46  haase
   Removed left over irreleveant comment

   Revision 1.12  2004/03/30 11:32:15  haase
   Renamed mult_hash functions

   Revision 1.11  2004/03/14 00:29:58  haase
   Fixed degenerate case of new hash algorithm

   Revision 1.10  2004/03/13 00:08:56  haase
   New improved hashing algorithm for a new kind of index better in general and especially better suited to very large indices

   Revision 1.9  2004/03/12 20:30:26  haase
   Added new kind of index with multiplicative hash function more appropriate to very large indices

   Revision 1.8  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.7.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.7  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.6  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
