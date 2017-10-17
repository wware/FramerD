/* C Mode */

/* seq.c
   Sequence primitives for FDScript
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

static char vcid[] = "$Id: seq.c,v 1.22 2005/01/14 16:48:46 haase Exp $";

/** Declarations **/
/** Operations on vectors **/
/** Generic sequence functions **/
/** LENGTH **/
/** FIND **/
/** COUNT **/
/** POSITION **/
/** SUBSEQ **/
/** REMOVE **/
/** REVERSE **/
/** MISMATCH **/
/** SEARCH **/
/** DOSEQ **/
/** EVERY **/
/** SOME **/
/** Initialization **/

#include "fdscript.h"

/** Declarations **/

#include <time.h>
#include <limits.h>

extern void eval_exprs_noreturn(lisp exprs,fd_lispenv env);

/** Operations on vectors **/

static lisp vector_handler(lisp elts)
{
  lisp vec; int i=0, size=0;
  {DOLIST(elt,elts) size++;}
  vec=fd_make_vector(size);
  {DOLIST(elt,elts) {
    FD_VECTOR_SET(vec,i,incref(elt)); i++;}}
  return vec;
}

static lisp lisp_vector_length_cproc(lisp x)
{
  if (VECTORP(x)) return LISPFIX(VECTOR_LENGTH(x));
  else fd_type_error(_("not a vector"),x);
}

static WIN32_NORETURN void bounds_error(fd_lisp x,int i) NORETURN;

static lisp vector_subst(lisp x,lisp index,lisp value)
{
  if ((VECTORP(x)) && (PRIM_TYPEP(index,fixnum_type))) {
     int i=FD_FIXLISP(index);
     if ((i < 0) || (i >= VECTOR_LENGTH(x))) bounds_error(x,i);
     fd_decref(VECTOR_REF(x,i));
     FD_VECTOR_SET(x,i,fd_incref(value));
     return fd_incref(x);}
  else return FD_EMPTY_CHOICE;
}

static void bounds_error(fd_lisp x,int i)
{
  char buf[32]; sprintf(buf,"%d",i);
  fd_raise_lisp_exception(fd_Out_Of_Bounds,buf,x);
}

static lisp vector_set_lexpr(lisp args)
{
  fd_lisp x_arg, index_arg, value;
  fd_get_args("VECTOR-SET!",args,&x_arg,FD_VOID,&index_arg,FD_VOID,&value,FD_VOID,NULL);
  {DO_CHOICES(x,x_arg) {
    DO_CHOICES(index,index_arg) {
      if (!(VECTORP(x))) fd_type_error(_("not a vector"),x);
      else if (!(PRIM_TYPEP(index,fixnum_type)))
	fd_type_error(_("not a fixnum offset"),index);
      else {
	int off=FIXLISP(index);
	if (off < 0) bounds_error(x,off);
	else if (off >= FD_VECTOR_LENGTH(x)) bounds_error(x,off);
	else {
	  lisp oelt=VECTOR_REF(x,off);
	  decref(oelt);
	  FD_VECTOR_SET(x,off,incref(value));}}}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  return FD_VOID;
}

static lisp lisp_vector_ref_cproc(lisp x,lisp index)
{
  if (!(VECTORP(x)))
    fd_type_error(_("not a vector"),x);
  else if (!(FIXNUMP(index)))
    fd_type_error(_("not a fixnum offset"),index);
  else {
    int i=FIXLISP(index), limit=VECTOR_LENGTH(x);
    if (i < 0) bounds_error(x,i);
    else if (i >= limit) bounds_error(x,i);
    else return incref(VECTOR_REF(x,i));}
}

static lisp vector_fill(lisp x,lisp value)
{
  if (!(VECTORP(x))) fd_type_error(_("not a vector"),x);
  else {
    int i=0, limit=VECTOR_LENGTH(x);
    while (i < limit) {
      lisp copy=copy_lisp(value), old=VECTOR_REF(x,i);
      FD_VECTOR_SET(x,i,copy); i++; decref(old);}
    return FD_VOID;}
}

static lisp make_vector_lexpr(lisp args)
{
  fd_lisp size, init, vec; int i=0, sz;
  fd_get_args("MAKE-VECTOR",args,&size,FD_VOID,&init,FD_FALSE,NULL);
  FD_CHECK_TYPE(size,FD_FIXNUMP,"an integer");
  sz=FD_FIXLISP(size); vec=fd_make_vector(sz);
  while (i < sz) {VECTOR_REF(vec,i)=incref(init); i++;}
  return vec;
}


/* Packet Operations */

static lisp vector2packet(lisp vector)
{
  int size=fd_vector_length(vector);
  unsigned char *packet_data=fd_malloc(size);
  DOTIMES(i,size) {
    lisp elt=VECTOR_REF(vector,i);
    if (FIXNUMP(elt)) {
      int j=FIXLISP(elt);
      if ((j >= 0) && (j < 256)) packet_data[i]= (unsigned char) j;
      else fd_type_error(_("element can't fit in packet"),elt);}
    else  fd_type_error(_("element can't fit in packet"),elt);}
  return fd_make_packet(size,packet_data);
}

static lisp packet2vector(lisp packet)
{
  if (FD_PACKETP(packet)) {
    int size=PACKET_LENGTH(packet);
    unsigned char *packet_data=PACKET_DATA(packet);
    lisp vector=fd_make_vector(size);
    DOTIMES(i,size) {
      unsigned char elt=packet_data[i];
      FD_VECTOR_SET(vector,i,LISPFIX(elt));}
    return vector;}
  else fd_type_error(_("not a packet"),packet);
}

static lisp lisp_packet_set_cproc(lisp packet,lisp index,lisp value)
{
  if (!(PACKETP(packet)))
    fd_type_error(_("not a packet"),packet);
  else if (!(FIXNUMP(index)))
    fd_type_error(_("not a fixnum offset"),index);
  else if ((!(FIXNUMP(value))) ||
	   (FIXLISP(value) < 0) ||
	   (FIXLISP(value) > 255))
    fd_type_error(_("value is not a byte"),value);
  else {
    int i=FIXLISP(index), limit=FD_PACKET_LENGTH(packet);
    if (i < 0) bounds_error(packet,i);
    else if (i >= limit) bounds_error(packet,i);
    else {
      FD_PACKET_DATA(packet)[i]=FIXLISP(value);}}
  return FD_VOID;
}

static lisp lisp_packet_ref_cproc(lisp x,lisp index)
{
  if (!(PACKETP(x))) fd_type_error(_("not a packet"),x);
  else if (!(FIXNUMP(index))) fd_type_error(_("not a fixnum offset"),index);
  else {
    int i=FIXLISP(index), limit=FD_PACKET_LENGTH(x);
    if (i < 0) bounds_error(x,i);
    else if (i >= limit) bounds_error(x,i);
    else return FD_LISPFIX(FD_PACKET_DATA(x)[i]);}
}

static lisp lisp_packet_to_uint_cproc(lisp x)
{
  if (!(PACKETP(x))) fd_type_error(_("not a packet"),x);
  else if (PACKET_LENGTH(x) > 4)
    fd_raise_exception(_("Packet to bignum not done"));
  else {
    unsigned char *data=FD_PACKET_DATA(x);
    unsigned int answer=0, i=0, len=FD_PACKET_LENGTH(x);
    while (i < len) answer=(answer<<8)|data[i++];
    return FD_LISPFIX(answer);}
}

static lisp lisp_uint_to_packet_cproc(lisp x)
{
  if (!(FD_FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else if ((FD_FIXLISP(x))<0)
    fd_type_error(_("not an unsigned int"),x);
  else {
    int ix=FD_FIXLISP(x);
    int len; unsigned char *copy, data[4];
    if (ix == 0) len=0;
    else if (ix < 256) len=1;
    else if (ix < 65536) len=2;
    else if (ix < 256*65536) len=3;
    else len=4;
    data[0]=(ix>>24); data[1]=(ix>>16)&0xFF;
    data[2]=(ix>>8)&0xFF; data[3]=ix&0xFF;
    copy=fd_malloc(len);
    memcpy(copy,data+(4-len),len);
    return fd_make_packet(len,copy);}
}

/** Homogenous vector functions **/

static fd_lisp lisp_make_int_vector_lexpr(fd_lisp args)
{
  fd_lisp length, init; int *vec, i=0, len, init_val;
  fd_get_args("MAKE-INT-VECTOR",args,&length,FD_VOID,&init,FD_LISPFIX(0),NULL);
  len=fd_fixlisp(length); vec=fd_malloc(sizeof(int)*len); init_val=fd_fixlisp(init);
  while (i < len) vec[i++]=init_val;
  return fd_make_int_vector(len,vec);
}

static lisp lisp_int_vector_lexpr(fd_lisp args)
{
  fd_lisp scan=args;
  int i=0, len=fd_list_length(args), *vec=fd_malloc(sizeof(int)*len); 
  while (i < len) {
    vec[i++]=fd_fixlisp(FD_CAR(scan)); scan=FD_CDR(scan);}
  return fd_make_int_vector(len,vec);
}

static fd_lisp lisp_make_short_vector_lexpr(fd_lisp args)
{
  fd_lisp length, init; int i=0, len; short *vec, init_val;
  fd_get_args("MAKE-SHORT-VECTOR",args,&length,FD_VOID,&init,FD_LISPFIX(0),NULL);
  len=fd_fixlisp(length); vec=fd_malloc(sizeof(short)*len);
  if ((fd_fixlisp(init) < SHRT_MAX) && (fd_fixlisp(init) > SHRT_MIN))
    init_val=(short)fd_fixlisp(init);
  else fd_type_error(_("not a short"),init);
  while (i < len) vec[i++]=init_val;
  return fd_make_short_vector(len,vec);
}

static lisp lisp_short_vector_lexpr(fd_lisp args)
{
  fd_lisp scan=args;
  int i=0, len=fd_list_length(args);
  short *vec=fd_malloc(sizeof(short)*len); 
  while (i < len) {
    int iv=fd_fixlisp(FD_CAR(scan));
    if (!((iv < SHRT_MAX) && (iv > SHRT_MIN)))
      fd_type_error(_("not a short"),FD_CAR(scan));
    vec[i++]=(short)iv; scan=FD_CDR(scan);}
  return fd_make_short_vector(len,vec);
}

static fd_lisp lisp_make_float_vector_lexpr(fd_lisp args)
{
  fd_lisp length, init; int i=0, len; float *vec, init_val;
  fd_get_args("MAKE-FLOAT-VECTOR",args,&length,FD_VOID,&init,FD_LISPFIX(0),NULL);
  len=fd_fixlisp(length); vec=fd_malloc(sizeof(float)*len);
  init_val=(float)fd_lisp2float(init);
  while (i < len) vec[i++]=init_val;
  return fd_make_float_vector(len,vec);
}

static lisp lisp_float_vector_lexpr(fd_lisp args)
{
  fd_lisp scan=args;
  int i=0, len=fd_list_length(args);
  float *vec=fd_malloc(sizeof(float)*len); 
  while (i < len) {
    float fv=fd_lisp2float(FD_CAR(scan));
    vec[i++]=(float)fv; scan=FD_CDR(scan);}
  return fd_make_float_vector(len,vec);
}

static fd_lisp lisp_make_double_vector_lexpr(fd_lisp args)
{
  fd_lisp length, init; int i=0, len; double *vec, init_val;
  fd_get_args("MAKE-DOUBLE-VECTOR",args,&length,FD_VOID,&init,FD_LISPFIX(0),NULL);
  len=fd_fixlisp(length); vec=fd_malloc(sizeof(double)*len);
  init_val=(double)fd_lisp2float(init);
  while (i < len) vec[i++]=init_val;
  return fd_make_double_vector(len,vec);
}

static lisp lisp_double_vector_lexpr(fd_lisp args)
{
  fd_lisp scan=args;
  int i=0, len=fd_list_length(args);
  double *vec=fd_malloc(sizeof(double)*len); 
  while (i < len) {
    double fv=fd_lisp2float(FD_CAR(scan));
    vec[i++]=(double)fv; scan=FD_CDR(scan);}
  return fd_make_double_vector(len,vec);
}

/** Generic sequence functions **/

static lisp seq_elt(lisp seq,int i)
{
  char ibuf[32];
  if (i < 0) {
    sprintf(ibuf,"%d",i);
    fd_raise_lisp_exception(fd_Out_Of_Bounds,ibuf,seq);}
  if (PAIRP(seq)) {
    DOLIST(elt,seq) if (--i < 0) return incref(elt);}
  else if (ASCII_STRINGP(seq)) {
    int size=STRING_LENGTH(seq);
    if (i < size) return CODE_CHAR(STRING_DATA(seq)[i]);}
  else if (UNICODE_STRINGP(seq)) {
    char *ptr=fd_utf8_substring(STRING_DATA(seq),i);
    if (ptr) return CODE_CHAR(fd_utf8_string_ref(ptr));}
  else if (VECTORP(seq)) {
    int j=VECTOR_LENGTH(seq);
    lisp *elements=FD_VECTOR_ELEMENTS(seq);
    if (i < j) return incref(elements[i]);}
  else if (FD_HVECTORP(seq)) {
    int j=FD_HVECTOR_LENGTH(seq);
    if (i >= j) {}
    else switch (FD_PTR_TYPE(seq)) {
    case int_vector_type:
      return FD_LISPFIX(FD_HVECTOR_REF(seq,ivector,i));
    case short_vector_type:
      return FD_LISPFIX(FD_HVECTOR_REF(seq,svector,i));
    case float_vector_type:
      return FD_LISPFLOAT(FD_HVECTOR_REF(seq,fvector,i));
    case double_vector_type:
      return FD_LISPFLOAT(FD_HVECTOR_REF(seq,dvector,i));
    default:
      fd_raise_exception(_("SEQ-ELT: this should never happen"));}}
  else if (FD_PACKETP(seq)) {
    unsigned char *packet_data=PACKET_DATA(seq);
    int size=PACKET_LENGTH(seq);
    if (i < size) return LISPFIX(packet_data[i]);}
  else fd_type_error(_("not a sequence"),seq);
  sprintf(ibuf,"%d",i);
  fd_raise_lisp_exception(fd_Out_Of_Bounds,ibuf,seq);
}

static lisp set_seq_elt(lisp seq,int i,lisp v)
{
  if (FD_STRINGP(seq)) {
    if (CHARACTERP(v))
      fd_string_set(seq,i,CHAR_CODE(v));
    else fd_type_error(_("Not a character"),v);}
  else if (FD_VECTORP(seq)) {
    if ((i>=0) && (i<FD_VECTOR_LENGTH(seq))) {
      lisp old=FD_VECTOR_REF(seq,i);
      FD_VECTOR_SET(seq,i,incref(v));
      decref(old);}
    else bounds_error(seq,i);}
  else if (FD_HVECTORP(seq)) {
    int j=FD_HVECTOR_LENGTH(seq);
    if (i >= j) {}
    else switch (FD_PTR_TYPE(seq)) {
    case int_vector_type: 
      FD_HVECTOR_SET(seq,ivector,i,fd_fixlisp(v)); break;
    case short_vector_type: {
      int iv=fd_fixlisp(v);
      if ((iv > SHRT_MAX) || (iv < SHRT_MIN))
	fd_type_error(_("not a short"),v);
      else {
	FD_HVECTOR_SET(seq,svector,i,(short)iv);}
      break;}
    case float_vector_type: {
      FD_HVECTOR_SET(seq,fvector,i,(float)fd_lisp2float(v));
      break;}
    case double_vector_type: {
      FD_HVECTOR_SET(seq,dvector,i,fd_lisp2float(v));
      break;}
    default:
      fd_raise_exception(_("SET-SEQ-ELT: this should never happen"));}}
  else if (FD_PACKETP(seq)) {
    if (!(FIXNUMP(v)))
      fd_type_error(_("packet data not an int"),v);
    else if ((FIXLISP(v)<0) || (FIXLISP(v)>255))
      fd_type_error(_("packet data in [0,255]"),v);
    else {
      unsigned char *data=FD_PACKET_DATA(seq);
      data[i]=(unsigned char)(FIXLISP(v));}}
  else if (FD_PAIRP(seq)) {
    int j=i; fd_lisp scan=seq;
    while ((j>0) && (FD_PAIRP(scan))) {
      scan=FD_CDR(scan); j--;}
    if (j) bounds_error(seq,i);
    else {fd_decref(FD_CAR(scan)); FD_RPLACA(scan,incref(v));}}
  else if (FD_EMPTY_LISTP(seq))
    bounds_error(seq,i);
  else fd_type_error(_("Not a sequence"),seq);
  return FD_VOID;
}

static lisp lisp_set_seq_elt_lexpr(lisp args)
{
  fd_lisp seqs, indices, v;
  fd_get_args("SET-ELT!",args,&seqs,FD_VOID,&indices,FD_VOID,&v,FD_VOID,NULL);
  {DO_CHOICES(seq,seqs) {
    DO_CHOICES(index,indices) {
      if (!(FD_FIXNUMP(index)))
	fd_type_error(_("not a fixnum offset"),index);
      else set_seq_elt(seq,FIXLISP(index),v);}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  return FD_VOID;
}

static lisp elt(lisp seq,lisp index)
{
  if (FIXNUMP(index))
    return seq_elt(seq,FIXLISP(index));
  else fd_type_error(_("not a fixnum offset"),index);
}

FDSCRIPT_EXPORT
/* fd_seq_elt:
    Arguments: a lisp pointer to a sequence and an integer index.
    Returns: a lisp pointer
  Returns the indexth element of the sequence argument.
 */
fd_lisp fd_seq_elt(lisp seq,int index)
{
  return seq_elt(seq,index);
}

static lisp elts_to_choice(lisp seq)
{
  lisp answer=FD_EMPTY_CHOICE;
  if (FD_EMPTY_LISTP(seq)) return answer;
  else if (PAIRP(seq)) {
    DOLIST(elt,seq) {ADD_TO_CHOICE(answer,incref(elt));}}
  else if (ASCII_STRINGP(seq)) {
    char *string=STRING_DATA(seq); int i=0, size=STRING_LENGTH(seq);
     while (i <size) {
       ADD_TO_CHOICE(answer,fd_make_character(string[i])); i++;}}
  else if (UNICODE_STRINGP(seq)) {
    fd_u8char *data=STRING_DATA(seq); int c=fd_sgetc(&data);
    while (c >= 0) {
      ADD_TO_CHOICE(answer,fd_make_character(c));
      c=fd_sgetc(&data);}}
  else if (VECTORP(seq)) {
    int i=0, j=VECTOR_LENGTH(seq);
    while (i < j) {
      ADD_TO_CHOICE(answer,incref(VECTOR_REF(seq,i))); i++;}}
  else if (FD_HVECTORP(seq)) {
    int i=0, j=VECTOR_LENGTH(seq);
    while (i < j) {
      ADD_TO_CHOICE(answer,seq_elt(seq,i)); i++;}}
  else if (FD_PACKETP(seq)) {
    int i=0, j=PACKET_LENGTH(seq);
    unsigned char *packet_data=PACKET_DATA(seq);
    while (i < j) {
      ADD_TO_CHOICE(answer,LISPFIX(packet_data[i])); i++;}}
  else fd_type_error(_("not a sequence"),seq);
  return fd_return_proper_choice(answer);
}

/** LENGTH **/

static int seq_length(lisp seq)
{
  if (FD_EMPTY_LISTP(seq)) return 0;
  else if (PRIM_TYPEP(seq,pair_type)) {
    int i=0; DOLIST(elt,seq) i++; return i;}
  else if (ASCII_STRINGP(seq)) return STRING_LENGTH(seq);
  else if (UNICODE_STRINGP(seq))
    return fd_utf8_strlen(STRING_DATA(seq),STRING_LENGTH(seq));
  else if (VECTORP(seq)) return VECTOR_LENGTH(seq);
  else if (FD_HVECTORP(seq)) return FD_HVECTOR_LENGTH(seq);
  else if (FD_PACKETP(seq)) return PACKET_LENGTH(seq);
  else fd_type_error(_("not a sequence"),seq);
}

static lisp lisp_seq_length_cproc_cproc(lisp seq)
{
  int len=seq_length(seq);
  return LISPFIX(len);
}

FDSCRIPT_EXPORT
/* fd_seq_length:
    Arguments: a lisp pointer to a sequence
    Returns: an int
  Returns the length of a sequence. */
int fd_seq_length(fd_lisp seq)
{
  return seq_length(seq);
}

/** FIND **/

enum seq_op { find_op, pos_op, count_op } ;

static int hvector_helper(lisp item,lisp seq,enum seq_op op)
{
  int n_items=0, len=FD_HVECTOR_LENGTH(seq);
  if (FD_FIXNUMP(item)) {
    int iv=FD_FIXLISP(item);
    if (FD_PRIM_TYPEP(seq,int_vector_type)) {
      int i=0; while (i < len)
	if (iv == FD_HVECTOR_REF(seq,ivector,i))
	  switch (op) {
	  case find_op: return 1;
	  case count_op: n_items++;
	  case pos_op: return i;}
	else i++;
      if (op == count_op) return n_items;
      else return -1;}
    else if ((FD_PRIM_TYPEP(seq,short_vector_type)) &&
	     (iv < SHRT_MAX) && (iv > SHRT_MIN)) {
      int i=0; while (i < len)
	if (iv == FD_HVECTOR_REF(seq,svector,i))
	  switch (op) {
	  case find_op: return 1;
	  case count_op: n_items++;
	  case pos_op: return i;}
	else i++;
      if (op == count_op) return n_items;
      else return -1;}
    else if (op == count_op) return 0;
    else return -1;}
  else if (FD_FLONUMP(item)) {
    double dv=FD_FLOATLISP(item);
    if (FD_PRIM_TYPEP(seq,float_vector_type)) {
      float fv=(float)dv;
      int i=0; while (i < len)
	if (fv == FD_HVECTOR_REF(seq,fvector,i))
	  switch (op) {
	  case find_op: return 1;
	  case count_op: n_items++;
	  case pos_op: return i;}
	else i++;
      if (op == count_op) return n_items;
      else return -1;}
    else if (FD_PRIM_TYPEP(seq,double_vector_type)) {
      int i=0; while (i < len)
	if (dv == FD_HVECTOR_REF(seq,dvector,i))
	  switch (op) {
	  case find_op: return 1;
	  case count_op: n_items++;
	  case pos_op: return i;}
	else i++;
      if (op == count_op) return n_items;
      else return -1;}
    else if (op == count_op) return 0;
    else return -1;}
  else if (op == count_op) return 0;
  else return -1;
}

static lisp lisp_seq_find_cproc(lisp item,lisp seq)
{
  if (PAIRP(seq)) {
    DOLIST(elt,seq)
      if (LISP_EQUAL(item,elt)) return incref(elt);
    return FD_FALSE;}
  else if (VECTORP(seq)) {
    int i=0, limit=VECTOR_LENGTH(seq);
    while (i < limit) 
      if (LISP_EQUAL(item,VECTOR_REF(seq,i)))
	return incref(VECTOR_REF(seq,i));
      else i++;
    return FD_FALSE;}
  else if (FD_HVECTORP(seq))
    if (hvector_helper(item,seq,find_op) == 1)
      return FD_TRUE;
    else return FD_FALSE;
  else if (ASCII_STRINGP(seq))
    if (CHARACTERP(item)) {
      char *scan=STRING_DATA(seq), code=CHAR_CODE(item);
      char *limit=scan+STRING_LENGTH(seq);
      while (scan<limit)
	if (*scan == code) return FD_TRUE; else scan++;
      return FD_FALSE;}
    else return FD_FALSE;
  else if (UNICODE_STRINGP(seq))     
    if (CHARACTERP(item)) {
      fd_u8char *scan=STRING_DATA(seq);
      fd_u8char *limit=scan+STRING_LENGTH(seq);
      int ccode=CHAR_CODE(item);
      while (scan < limit) 
	if (fd_sgetc(&scan) == ccode) return FD_TRUE;
      return FD_FALSE;}
    else return FD_FALSE;
  else if (FD_PACKETP(seq))
    if ((FIXNUMP(item)) && ((FIXLISP(item)) > 0) &&
	((FIXLISP(item)) < 256)) {
      unsigned char *packet_data=PACKET_DATA(seq);
      unsigned char key=(unsigned char) FIXLISP(item);
      int i=0, limit=PACKET_LENGTH(seq);
      while (i < limit)
	if (key == packet_data[i]) return FD_TRUE; else i++;}
    else return FD_FALSE;
  else if (FD_EMPTY_LISTP(seq)) return FD_FALSE;
  else fd_type_error(_("not a sequence"),seq);
}

/** COUNT **/

static fd_lisp lisp_seq_count_cproc(lisp item,lisp seq)
{
  int count=0;
  if (PAIRP(seq)) {
    DOLIST(elt,seq) if (LISP_EQUAL(item,elt)) count++;}
  else if (VECTORP(seq)) {
    int i=0, limit=VECTOR_LENGTH(seq);
    while (i < limit) 
      if (LISP_EQUAL(item,VECTOR_REF(seq,i))) {count++; i++;}
      else i++;}
  else if (FD_HVECTORP(seq))
    return FD_LISPFIX(hvector_helper(item,seq,count_op));
  else if (ASCII_STRINGP(seq))
    if (CHARACTERP(item)) {
      char *scan=STRING_DATA(seq), code=CHAR_CODE(item);
      char *limit=scan+STRING_LENGTH(seq);
      while (scan<limit) if (*scan++ == code) count++;}
    else {}
  else if (UNICODE_STRINGP(seq))     
    if (CHARACTERP(item)) {
      fd_u8char *scan=STRING_DATA(seq);
      fd_u8char *limit=scan+STRING_LENGTH(seq);
      int ccode=CHAR_CODE(item);
      while (scan < limit) 
	if (fd_sgetc(&scan) == ccode) count++;}
    else count=0;
  else if (FD_PACKETP(seq))
    if ((FIXNUMP(item)) && ((FIXLISP(item)) > 0) &&
	((FIXLISP(item)) < 256)) {
      unsigned char *packet_data=PACKET_DATA(seq);
      unsigned char key=(unsigned char) FIXLISP(item);
      int i=0, limit=PACKET_LENGTH(seq);
      while (i < limit)
	if (key == packet_data[i]) {count++; i++;} else i++;}
    else count=0;
  else if (FD_EMPTY_LISTP(seq)) return LISPFIX(0);
  else fd_type_error(_("not a sequence"),seq);
  return LISPFIX(count);
}

/** POSITION **/

static int seq_position(lisp item,lisp seq,int start)
{
  if (PAIRP(seq)) {
    int i=0; DOLIST(elt,seq)
      if (i < start) i++;
      else if (LISP_EQUAL(item,elt)) return i;
      else i++;
    return -1;}
  else if (VECTORP(seq)) {
    int i=start, limit=VECTOR_LENGTH(seq);
    while (i < limit) 
      if (LISP_EQUAL(item,VECTOR_REF(seq,i)))
	return i;
      else i++;
    return -1;}
  else if (FD_HVECTORP(seq))
    return hvector_helper(item,seq,pos_op);
  else if (ASCII_STRINGP(seq))
    if (CHARACTERP(item)) {
      fd_u8char *base=STRING_DATA(seq)+start; int code=CHAR_CODE(item);
      fd_u8char *scan=base, *limit=STRING_DATA(seq)+STRING_LENGTH(seq);
      int pos=-1;
      if (code >= 0x80) return -1;
      while (scan<limit)
	if (*scan == code) return scan-STRING_DATA(seq);
	else scan++;
      return -1;}
    else return -1;
  else if (UNICODE_STRINGP(seq))     
    if (CHARACTERP(item)) {
      fd_u8char *scan=STRING_DATA(seq);
      fd_u8char *limit=scan+STRING_LENGTH(seq);
      int ccode=CHAR_CODE(item), pos=-1, count=0;
      while (scan < limit)
	if (count <= start) {
	  fd_sgetc(&scan); count++;}
	else if (pos>=0) fd_sgetc(&scan);
	else if (fd_sgetc(&scan) == ccode) pos=count;
	else count++;
      return pos;}
    else return -1;
  else if (FD_PACKETP(seq))
    if ((FIXNUMP(item)) &&
	(start < PACKET_LENGTH(seq)) &&
	((FIXLISP(item)) > 0) &&
	((FIXLISP(item)) < 256)) {
      unsigned char *packet_data=PACKET_DATA(seq);
      unsigned char key=(unsigned char) FIXLISP(item);
      int i=start, limit=PACKET_LENGTH(seq);
      while (i < limit)
	if (key == packet_data[i]) return i; else i++;
      return -1;}
    else return -1;
  else if (FD_EMPTY_LISTP(seq)) return -1;
  else fd_type_error(_("not a sequence"),seq);
}

static lisp lisp_seq_position_lexpr(lisp args)
{
  lisp answers=FD_EMPTY_CHOICE;
  fd_lisp items, seqs, starts;
  fd_get_args("POSITION",args,&items,FD_VOID,&seqs,FD_VOID,
	      &starts,FD_LISPFIX(0),NULL);
  {DO_CHOICES(item,items) {
    DO_CHOICES(seq,seqs) {
      DO_CHOICES(start,starts) {
	int pos;
	if (FIXNUMP(start)) pos=seq_position(item,seq,FIXLISP(start));
	else fd_type_error(_("not a fixnum offset"),start);
	if (pos >= 0) ADD_TO_CHOICE(answers,LISPFIX(pos));}
      END_DO_CHOICES;}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  if (FD_EMPTYP(answers)) return FD_FALSE;
  else return answers;
}

/** SUBSEQ **/

static lisp new_hvector(lisp seq,int new_size)
{
  switch (FD_PTR_TYPE(seq)) {
  case int_vector_type: 
    return fd_make_int_vector(new_size,fd_malloc(sizeof(int)*new_size));
  case short_vector_type: 
    return fd_make_short_vector(new_size,fd_malloc(sizeof(short)*new_size));
  case float_vector_type: 
    return fd_make_float_vector(new_size,fd_malloc(sizeof(float)*new_size));
  case double_vector_type: 
    return fd_make_double_vector(new_size,fd_malloc(sizeof(double)*new_size));
  default:
    fd_raise_exception("NEW-HVECTOR: you should never see this");}
}

static lisp hvector_subseq(lisp seq,int start,int end)
{
  fd_lisp sub=new_hvector(seq,end-start);
  int read=start, write=0;
  switch (FD_PTR_TYPE(seq)) {
  case int_vector_type: {
    int *source=(FD_PTR_DATA(seq,ivector)->elements);
    int *dest=(FD_PTR_DATA(sub,ivector)->elements);
    while (read < end) dest[write++]=source[read++];
    return sub;}
  case short_vector_type: {
    short *source=(FD_PTR_DATA(seq,svector)->elements);
    short *dest=(FD_PTR_DATA(sub,svector)->elements);
    while (read < end) dest[write++]=source[read++];
    return sub;}
  case float_vector_type: {
    float *source=(FD_PTR_DATA(seq,fvector)->elements);
    float *dest=(FD_PTR_DATA(sub,fvector)->elements);
    while (read < end) dest[write++]=source[read++];
    return sub;}
  case double_vector_type: {
    double *source=(FD_PTR_DATA(seq,dvector)->elements);
    double *dest=(FD_PTR_DATA(sub,dvector)->elements);
    while (read < end) dest[write++]=source[read++];
    return sub;}
  default: fd_raise_exception("HVECTOR-SUBSEQ: you should never see this");}
}

static lisp seq_subseq(lisp seq,int start,int end)
{
  int length=seq_length(seq); 
  if (start > length)
   fd_raise_lisp_exception(fd_Out_Of_Bounds,"SUBSEQ start",LISPFIX(start));
  if (end < 0) end=length;
  if ((start == end) && ((start == 0) || (end < length)))
    if ((PAIRP(seq)) || (FD_EMPTY_LISTP(seq))) return FD_EMPTY_LIST;
    else if (VECTORP(seq)) return fd_make_vector(0);
    else if (STRINGP(seq)) return fd_make_string("");
    else if (FD_PACKETP(seq)) return fd_make_packet(0,NULL);
    else fd_type_error(_("not a sequence"),seq);
  else if (end > length)
    fd_raise_lisp_exception(fd_Out_Of_Bounds,"SUBSEQ",LISPFIX(end));
  else if (end < start)
    fd_raise_lisp_exception(fd_Out_Of_Bounds,"SUBSEQ backwards",seq);
  else if (PAIRP(seq)) {
    lisp answer=FD_EMPTY_LIST, *tail=&answer; int i=0;
    DOLIST(elt,seq) {
      if (i >= start) {
	lisp new=FD_MAKE_LIST1(incref(elt));
	*tail=new; tail=&(CDR(new)); i++;}
      else i++;
      if (i >= end) return answer;}
    return answer;}
  else if (VECTORP(seq)) {
    lisp new=fd_make_vector(end-start);
    int i=end-1, j=(end-start)-1;
    while (i >= start) {
      FD_VECTOR_SET(new,j,incref(VECTOR_REF(seq,i)));
      i--; j--;}
    return new;}
  else if (ASCII_STRINGP(seq)) {
    char *copy=fd_xmalloc((end-start)+1);
    strncpy(copy,STRING_DATA(seq)+start,end-start);
    copy[end-start]='\0'; 
    return fd_init_string(copy,end-start);}
  else if (UNICODE_STRINGP(seq)) {
    fd_u8char *data=STRING_DATA(seq);
    fd_u8char *s_start=fd_utf8_substring(data,start);
    fd_u8char *s_end=fd_utf8_substring(data,end);
    if (s_end == NULL)
      return fd_copy_string(s_start);
    else {
      char *copy=fd_xmalloc((s_end-s_start)+1);
      strncpy(copy,s_start,s_end-s_start); copy[s_end-s_start]='\0';
      return fd_init_string(copy,s_end-s_start); }}
  else if (FD_PACKETP(seq)) {
    unsigned char *copy=fd_malloc((end-start));
    unsigned char *data=PACKET_DATA(seq);
    memcpy(copy,data+start,end-start);
    return fd_make_packet(end-start,copy);}
  else if (FD_EMPTY_LISTP(seq)) return FD_EMPTY_LIST;
  else if (FD_HVECTORP(seq))
    return hvector_subseq(seq,start,end);
  else fd_type_error(_("not a sequence"),seq);
}

static lisp lisp_subseq_lexpr(lisp args)
{
  fd_lisp answer=FD_EMPTY_CHOICE, seqs, starts, ends;
  fd_get_args("SUBSEQ",args,&seqs,FD_VOID,&starts,FD_VOID,&ends,FD_FALSE,NULL);
  {DO_CHOICES(seq,seqs) {
    DO_CHOICES(start,starts) {
      DO_CHOICES(end,ends) {
	lisp value;
	if (FD_FALSEP(end))
	  value=seq_subseq(seq,fd_lisp2int(start),-1);
	else value=seq_subseq(seq,fd_lisp2int(start),fd_lisp2int(end));
	ADD_TO_CHOICE(answer,value);}
      END_DO_CHOICES;}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  return answer;
}

/** REMOVE **/

static lisp hvector_remove(lisp item,lisp seq)
{
  int len=FD_HVECTOR_LENGTH(seq);
  int count=hvector_helper(item,seq,count_op);
  if (count == 0) return fd_incref(seq);
  else {
    fd_lisp nvec=new_hvector(seq,len-count);
    int read=0, write=0;
    switch (FD_PTR_TYPE(seq)) {
    case int_vector_type: {
      int *source=(FD_PTR_DATA(seq,ivector)->elements);
      int *dest=(FD_PTR_DATA(nvec,ivector)->elements);
      int v=fd_fixlisp(item);
      while (read < len)
	if (v == source[read]) read++;
	else dest[write++]=source[read++];
      return nvec;}
    case short_vector_type: {
      short *source=(FD_PTR_DATA(seq,svector)->elements);
      short *dest=(FD_PTR_DATA(nvec,svector)->elements);
      int v=fd_fixlisp(item);
      while (read < len)
	if (v == source[read]) read++;
	else dest[write++]=source[read++];
      return nvec;}
    case float_vector_type: {
      float *source=(FD_PTR_DATA(seq,fvector)->elements);
      float *dest=(FD_PTR_DATA(nvec,fvector)->elements);
      double v=fd_lisp2float(item);
      while (read < len)
	if (v == source[read]) read++;
	else dest[write++]=source[read++];
      return nvec;}
    case double_vector_type: {
      double *source=(FD_PTR_DATA(seq,dvector)->elements);
      double *dest=(FD_PTR_DATA(nvec,dvector)->elements);
      double v=fd_lisp2float(item);
      while (read < len)
	if (v == source[read]) read++;
	else dest[write++]=source[read++];
      return nvec;}
    default: fd_raise_exception("HVECTOR-SUBSEQ: you should never see this");}}
}

static lisp lisp_seq_remove_cproc(lisp item,lisp seq)
{
  int len=seq_length(seq);
  if (len == 0) return fd_incref(seq);
  else if ((PAIRP(seq)) || (VECTORP(seq))) {
    lisp new_elts=FD_EMPTY_LIST; int size=0;
    if (PAIRP(seq)) {
      DOLIST(elt,seq)
	if (!(LISP_EQUAL(elt,item))) {
	  new_elts=FD_MAKE_PAIR(incref(elt),new_elts); size++;}}
    else {
      int i=0, limit=VECTOR_LENGTH(seq);
      while (i < limit) {
	lisp elt=VECTOR_REF(seq,i);
	if (!(LISP_EQUAL(elt,item))) {
	  new_elts=FD_MAKE_PAIR(incref(elt),new_elts); size++;}
	i++;}}
    if (PAIRP(seq)) {
      lisp answer=FD_EMPTY_LIST;
      DOLIST(elt,new_elts)
	answer=FD_MAKE_PAIR(incref(elt),answer);
      decref(new_elts); return answer;}
    else {
      lisp new=fd_make_vector(size); int i=size-1;
      DOLIST(elt,new_elts) {
	FD_VECTOR_SET(new,i,incref(elt)); i--;}
      decref(new_elts); return new;}}
  else if (STRINGP(seq)) {
    struct FD_STRING_STREAM ss;
    fd_u8char *scan=STRING_DATA(seq);
    fd_u8char *limit=scan+STRING_LENGTH(seq);
    int c, citem=CHAR_CODE(item);
    FD_INITIALIZE_STRING_STREAM(&ss,STRING_LENGTH(seq)*2);
    while (scan < limit) {
      int c=fd_sgetc(&scan);
      if (c == citem) {} else fd_sputc(&ss,c);}
    return fd_stream_string(&ss);}
  else if (FD_PACKETP(seq)) {
    if ((FD_FIXNUMP(item)) && (FD_FIXLISP(item)>=0) &&
	(FD_FIXLISP(item)<=255)) {
      int len=FD_PACKET_LENGTH(seq), int_item=FD_FIXLISP(item);
      unsigned char *working=fd_malloc(len), *write=working, *copy;
      unsigned char *data=FD_PACKET_DATA(seq), *scan=data, *limit=scan+len;
      while (scan < limit) {
	if (*scan == int_item) scan++; else *write++=*scan++;}
      copy=fd_malloc(write-working);
      memcpy(copy,working,write-working);
      fd_free(working,len);
      return fd_make_packet(write-working,copy);}
    else return incref(seq);}
  else if (FD_EMPTY_LISTP(seq)) return seq;
  else if (FD_HVECTORP(seq))
    return hvector_remove(item,seq);
  else fd_type_error(_("not a sequence"),seq);
}

/** REVERSE **/

static lisp hvector_reverse(lisp seq,int len)
{
  fd_lisp sub=new_hvector(seq,len);
  int read=0, write=len-1, end=len;
  switch (FD_PTR_TYPE(seq)) {
  case int_vector_type: {
    int *source=(FD_PTR_DATA(seq,ivector)->elements);
    int *dest=(FD_PTR_DATA(sub,ivector)->elements);
    while (read < end) dest[write--]=source[read++];
    return sub;}
  case short_vector_type: {
    short *source=(FD_PTR_DATA(seq,svector)->elements);
    short *dest=(FD_PTR_DATA(sub,svector)->elements);
    while (read < end) dest[write--]=source[read++];
    return sub;}
  case float_vector_type: {
    float *source=(FD_PTR_DATA(seq,fvector)->elements);
    float *dest=(FD_PTR_DATA(sub,fvector)->elements);
    while (read < end) dest[write--]=source[read++];
    return sub;}
  case double_vector_type: {
    double *source=(FD_PTR_DATA(seq,dvector)->elements);
    double *dest=(FD_PTR_DATA(sub,dvector)->elements);
    while (read < end) dest[write--]=source[read++];
    return sub;}
  default: fd_raise_exception("HVECTOR-SUBSEQ: you should never see this");}
}

static lisp lisp_seq_reverse_cproc(lisp seq)
{
  if (FD_EMPTY_LISTP(seq)) return seq;
  else if (PAIRP(seq)) {
    lisp result=FD_EMPTY_LIST;
    DOLIST(elt,seq)
      result=FD_MAKE_PAIR(incref(elt),result);
    return result;}
  else if (VECTORP(seq)) {
    lisp new=fd_make_vector(VECTOR_LENGTH(seq));
    int i=0, size=VECTOR_LENGTH(seq);
    while (i < size) {
      FD_VECTOR_SET(new,i,incref(VECTOR_REF(seq,size-i-1)));
      i++;}
    return new;}
  else if (ASCII_STRINGP(seq)) {
    char *new=fd_xmalloc(STRING_LENGTH(seq)+1), *old=STRING_DATA(seq);
    char *write=new, *read=old+STRING_LENGTH(seq)-1;
    while (read >= old) *write++=*read--; *write='\0';
    return fd_init_string(new,-1);}
  else if (UNICODE_STRINGP(seq)) {
    unichar_t *buf=fd_malloc(sizeof(unichar_t)*STRING_LENGTH(seq));
    fd_u8char *scan=STRING_DATA(seq);
    fd_u8char *limit=scan+STRING_LENGTH(seq);
    struct FD_STRING_STREAM ss;
    int c, i=0;
    while (scan < limit) buf[i++]=fd_sgetc(&scan); i=i-1;
    FD_INITIALIZE_STRING_STREAM(&ss,STRING_LENGTH(seq));
    while (i >= 0) {fd_sputc(&ss,buf[i]); i--;}
    fd_free(buf,sizeof(unichar_t)*STRING_LENGTH(seq));
    return fd_stream_string(&ss);}
  else if (FD_PACKETP(seq)) {
    int len=FD_PACKET_LENGTH(seq);
    unsigned char *working=fd_malloc(len), *write=working;
    unsigned char *data=FD_PACKET_DATA(seq), *scan=data+len-1, *limit=data;
    while (scan >= limit) {*write++=*scan--;}
    return fd_make_packet(len,working);}
  else if (FD_HVECTORP(seq))
    return hvector_reverse(seq,FD_HVECTOR_LENGTH(seq));
  else  fd_type_error(_("not a sequence"),seq);
}

/** MISMATCH **/

static int seq_mismatch(lisp seq1,lisp seq2,int i1,int i2)
{
  int l1=seq_length(seq1), l2=seq_length(seq2);
  while ((i1 < l1) && (i2 < l2)) {
    lisp e1=seq_elt(seq1,i1), e2=seq_elt(seq2,i2);
    if (LISP_EQUAL(e1,e2)) {
      decref(e1); decref(e2); i1++; i2++;}
    else {decref(e1); decref(e2); return i1;}}
  return -1;
}

static lisp lisp_seq_mismatch_lexpr(lisp args)
{
  fd_lisp answer=FD_EMPTY_CHOICE, seqs1, seqs2, starts1, starts2;
  fd_get_args("MISMATCH",args,&seqs1,FD_VOID,&seqs2,FD_VOID,
	      &starts1,FD_LISPFIX(0),&starts2,FD_LISPFIX(0),NULL);
  {DO_CHOICES(seq1,seqs1) {
    DO_CHOICES(seq2,seqs2) {
      DO_CHOICES(start1,starts1) {
	DO_CHOICES(start2,starts2) {
	  int mmatch=seq_mismatch
	    (seq1,seq2,fd_lisp2int(start1),fd_lisp2int(start2));
	  if (mmatch >= 0) {ADD_TO_CHOICE(answer,LISPFIX(mmatch));}}
	END_DO_CHOICES;}
      END_DO_CHOICES;}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  if (FD_EMPTYP(answer)) return FD_FALSE;
  else return answer;
}

/** SEARCH **/

static int seq_search(lisp seq1,lisp seq2,int start)
{
  lisp first=seq_elt(seq1,0);
  int pos=seq_position(first,seq2,start);
  int len1=seq_length(seq1), len2=seq_length(seq2), lim=len2-len1;
  fd_decref(first);
  while ((pos >= 0) && (pos <= lim))
    if ((seq_mismatch(seq1,seq2,0,pos)) < 0)
      return pos;
    else pos=seq_position(first,seq2,pos+1);
  return -1;
}

static lisp lisp_seq_search_lexpr(lisp args)
{
  fd_lisp answer=FD_EMPTY_CHOICE, seq1, seq2, start;
  fd_get_args("SEARCH",args,&seq1,FD_VOID,&seq2,FD_VOID,
	      &start,FD_LISPFIX(0),NULL);
  {DO_CHOICES(s1,seq1) {
    DO_CHOICES(s2,seq2) {
      DO_CHOICES(st,start) {
	int off=((FD_FALSEP(st)) ? 0 : (fd_lisp2int(st)));
	int pos=seq_search(s1,s2,off);
	if (pos < 0) {
	  ADD_TO_CHOICE(answer,FD_FALSE);}
	else {ADD_TO_CHOICE(answer,LISPFIX(pos));}}
      END_DO_CHOICES;}
    END_DO_CHOICES;}
  END_DO_CHOICES;}
  return answer;
}

static lisp seq_first(lisp seq) { return seq_elt(seq,0); }
static lisp seq_second(lisp seq) { return seq_elt(seq,1); }
static lisp seq_third(lisp seq) { return seq_elt(seq,2); }
static lisp seq_fourth(lisp seq) { return seq_elt(seq,3); }
static lisp seq_fifth(lisp seq) { return seq_elt(seq,4); }

/** DOSEQ **/

static lisp lisp_doseq_handler(lisp expr,lispenv env)
{
  lisp spec=fd_get_arg(expr,1,FD_VOID);
  lisp elt_var=fd_get_arg(spec,0,FD_VOID);
  lisp seq_expr=fd_get_arg(spec,1,FD_VOID);
  lisp ivar=fd_get_arg(spec,2,FD_FALSE);
  lisp body=fd_get_body(expr,2);
  lisp sequence=fd_eval_in_env(seq_expr,env);
  int i=0, len=seq_length(sequence); int bind_i=1;
  FD_WITH_LEXICAL_ENV(doseq_env,env,2) {
    if (FD_FALSEP(ivar)) bind_i=0;
    fd_bind_value(elt_var,FD_VOID,doseq_env);
    if (bind_i)
      fd_bind_value(ivar,FD_VOID,doseq_env);
    while (i < len) {
      lisp selt=seq_elt(sequence,i);
      fd_set_value(elt_var,selt,doseq_env);
      decref(selt);
      if (bind_i)
	fd_set_value(ivar,LISPFIX(i),doseq_env);
      eval_exprs_noreturn(body,doseq_env);
      i++;}}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
  decref(sequence);
  return FD_VOID;
}

/** EVERY **/

static lisp lisp_every_lexpr(lisp args)
{
  fd_lisp test, seq; int i=0, len;
  fd_get_args("EVERY?",args,&test,FD_VOID,&seq,FD_VOID,NULL);
  len=seq_length(seq); 
  while (i < len) {
    lisp elt=seq_elt(seq,i);
    lisp argl=FD_MAKE_LIST1(elt);
    lisp result=fd_apply(test,argl);
    int false=(FD_FALSEP(result));
    decref(argl); decref(result);
    if (false) return FD_FALSE;
    else i++;}
  return FD_TRUE;
}

/** SOME **/

static lisp lisp_some_lexpr(lisp args)
{
  fd_lisp test, seq; int i=0, len;
  fd_get_args("SOME?",args,&test,FD_VOID,&seq,FD_VOID,NULL);
  len=seq_length(seq); 
  while (i < len) {
    lisp elt=seq_elt(seq,i);
    lisp argl=FD_MAKE_LIST1(elt);
    lisp result=fd_apply(test,argl);
    int false=(FD_FALSEP(result));
    decref(argl); decref(result);
    if (false) i++;
    else return FD_TRUE;}
  return FD_FALSE;
}

/** Initialization **/

FDSCRIPT_EXPORT
void fd_initialize_seq_c()
{
  fd_add_cproc(NULL,"LENGTH",1,lisp_seq_length_cproc_cproc);
  fd_add_cproc(NULL,"SEQ->CHOICE",1,elts_to_choice);
  fd_add_alias(NULL,"SEQ->CHOICES","SEQ->CHOICE");
  fd_add_alias(NULL,"SEQ->SET","SEQ->CHOICE");
  fd_add_cproc(NULL,"ELEMENTS",1,elts_to_choice);
  fd_add_alias(NULL,"ELTS","ELEMENTS");
  fd_add_cproc(NULL,"ELT",2,elt);
  fd_add_lexpr(NULL,"SET-ELT!",FD_ND_LEXPR,lisp_set_seq_elt_lexpr);
  fd_add_cproc(NULL,"FIRST",1,seq_first);
  fd_add_cproc(NULL,"SECOND",1,seq_second);
  fd_add_cproc(NULL,"THIRD",1,seq_third);
  fd_add_cproc(NULL,"FOURTH",1,seq_fourth);
  fd_add_cproc(NULL,"FIFTH",1,seq_fifth);

  fd_add_cproc(NULL,"FIND",2,lisp_seq_find_cproc);
  fd_add_alias(NULL,"CL-FIND","FIND");
  fd_add_cproc(NULL,"COUNT",2,lisp_seq_count_cproc);
  fd_add_alias(NULL,"CL-COUNT","COUNT");
  fd_add_lexpr(NULL,"POSITION",FD_ND_LEXPR,lisp_seq_position_lexpr);
  fd_add_alias(NULL,"CL-POSITION","POSITION");
  fd_add_lexpr(NULL,"SUBSEQ",FD_ND_LEXPR,lisp_subseq_lexpr);
  fd_add_cproc(NULL,"REVERSE",1,lisp_seq_reverse_cproc);
  fd_add_cproc(NULL,"REMOVE",2,lisp_seq_remove_cproc);
  fd_add_lexpr(NULL,"MISMATCH",FD_ND_LEXPR,lisp_seq_mismatch_lexpr);
  fd_add_lexpr(NULL,"SEARCH",FD_ND_LEXPR,lisp_seq_search_lexpr);
  fd_add_alias(NULL,"CL-MISMATCH","MISMATCH");
  fd_add_alias(NULL,"CL-SEARCH","SEARCH");

  fd_add_lexpr(NULL,"VECTOR",FD_NORMAL_LEXPR,vector_handler);
  fd_add_cproc(NULL,"VECTOR-LENGTH",1,lisp_vector_length_cproc);
  fd_add_cproc(NULL,"VECTOR-REF",2,lisp_vector_ref_cproc);
  fd_add_cproc(NULL,"VECTOR-SUBST",3,vector_subst);
  fd_add_lexpr(NULL,"VECTOR-SET!",FD_ND_LEXPR,vector_set_lexpr);
  fd_add_cproc(NULL,"VECTOR-FILL!",2,vector_fill);
  fd_add_lexpr(NULL,"MAKE-VECTOR",FD_NORMAL_LEXPR,make_vector_lexpr);
  fd_add_cproc(NULL,"VECTOR->PACKET",1,vector2packet);
  fd_add_cproc(NULL,"PACKET->VECTOR",1,packet2vector);

  fd_add_cproc(NULL,"PACKET-REF",2,lisp_packet_ref_cproc);
  fd_add_cproc(NULL,"PACKET-SET!",3,lisp_packet_set_cproc);

  fd_add_cproc(NULL,"UINT->PACKET",1,lisp_uint_to_packet_cproc);
  fd_add_cproc(NULL,"PACKET->UINT",1,lisp_packet_to_uint_cproc);

  fd_add_lexpr(NULL,"MAKE-INT-VECTOR",FD_NORMAL_LEXPR,lisp_make_int_vector_lexpr);
  fd_add_lexpr(NULL,"INT-VECTOR",FD_NORMAL_LEXPR,lisp_int_vector_lexpr);
  fd_add_lexpr(NULL,"MAKE-SHORT-VECTOR",FD_NORMAL_LEXPR,lisp_make_short_vector_lexpr);
  fd_add_lexpr(NULL,"SHORT-VECTOR",FD_NORMAL_LEXPR,lisp_short_vector_lexpr);
  fd_add_lexpr(NULL,"MAKE-FLOAT-VECTOR",FD_NORMAL_LEXPR,lisp_make_float_vector_lexpr);
  fd_add_lexpr(NULL,"FLOAT-VECTOR",FD_NORMAL_LEXPR,lisp_float_vector_lexpr);
  fd_add_lexpr(NULL,"MAKE-DOUBLE-VECTOR",FD_NORMAL_LEXPR,lisp_make_double_vector_lexpr);
  fd_add_lexpr(NULL,"DOUBLE-VECTOR",FD_NORMAL_LEXPR,lisp_double_vector_lexpr);

  fd_add_special_form(NULL,"DOSEQ",lisp_doseq_handler);

  fd_add_lexpr(NULL,"EVERY?",FD_NORMAL_LEXPR,lisp_every_lexpr);
  fd_add_lexpr(NULL,"SOME?",FD_NORMAL_LEXPR,lisp_some_lexpr);

  fd_register_source_file("seq",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: seq.c,v $
   Revision 1.22  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.21  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.20  2003/09/07 18:25:48  haase
   Added API access to sequence length and element operations

   Revision 1.19  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.18.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.18.2.2  2003/02/10 10:03:29  haase
   Fix removal from empty sequences (esp. strings)

   Revision 1.18.2.1  2003/01/26 20:41:48  haase
   Misc. fixes especially some GC

   Revision 1.18  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.17  2002/05/07 08:06:37  haase
   Removed leak in sequence search function

   Revision 1.16  2002/04/30 13:48:33  haase
   Made packaged format for homogenous vectors be bytes rather than lisp vectors, saving space in the external DType representation

   Revision 1.15  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.14  2002/04/18 20:00:26  haase
   Fixed error where COUNT on ascii strings returned #f rather than zero

   Revision 1.13  2002/04/17 23:06:16  haase
   Fixed bug in new sequence code for strings

   Revision 1.12  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.11  2002/04/05 03:29:10  haase
   Fixed typo introduced into code for FIND in strings

   Revision 1.10  2002/04/04 18:48:53  haase
   Added homongenous vectors of ints, shorts, floats, and doubles.
   Also changed the field "size" in some structs to "length" to indicate
   that the underlying data structure is ordered.

   Revision 1.9  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
