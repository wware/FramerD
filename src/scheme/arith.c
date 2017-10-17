/* C Mode */

/* arith.c
   Arithmetic primitives for FDScript
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

static char vcid[] = "$Id: arith.c,v 1.23 2005/01/14 16:48:49 haase Exp $";

/** Includes and declarations **/
/** Integer operations (with bignums) **/
/** Integer operations (without bignums) **/
/** Some conversions **/
/** Higher order functions **/
/** Addition **/
/** Subtraction **/
/** Multiplication **/
/** Division **/
/** EXPT (incomplete implementation) **/
/** Transcendental functions **/
/** Numeric predicates **/
/** Exactness and Inexactness **/
/** Random numbers **/
/** Numeric comparisons **/
/** Taking apart compound numbers **/
/** Max and min **/
/** Quotient, etc **/
/** Floor, etc. **/
/** Miscellaneous operations **/
/** Numerical i/o **/
/** Generic ordering, sorting **/
/** Initialization **/

#include "fdscript.h"
#include <limits.h>
#include <time.h>
#include <math.h>
#if (USE_BIGNUMS)
#include "bignum.h"
#endif
#if WIN32
#include <process.h>
#endif
#include "framerd/dtcodes.h"

#define INTEGERP FD_INTEGERP
#define BIGNUMP FD_BIGNUMP
#define RATIONALP FD_RATIONALP
#define COMPLEXP FD_COMPLEXP

static double fdtrunc(double x)
{
  if (x < 0.0) return ceil(x); else return floor(x);
}

#if (USE_BIGNUMS)
/** Integer operations (with bignums) **/

static bignum_type big_int_max, big_int_min;

static bignum_type bignum_copy(bignum_type original)
{
  bignum_type zero=bignum_make_zero();
  bignum_type copy=bignum_add(zero,original);
  free(zero);
  return copy;
}

static lisp big2lisp(bignum_type original)
{
  if (((bignum_compare(original,big_int_max)) == bignum_comparison_less) &&
      ((bignum_compare(original,big_int_min)) == bignum_comparison_greater)) {
    int int_value=bignum_to_long(original); free(original);
    return LISPFIX(int_value);}
  else return fd_make_cptr(bigint_type,original);
}

FDSCRIPT_EXPORT
fd_lisp fd_ulong2lisp(unsigned long n)
{
  bignum_type bx=ulong_to_bignum(n);
  return fd_make_cptr(bigint_type,bx);
}

FDSCRIPT_EXPORT
fd_lisp fd_long2lisp(long n)
{
  bignum_type bx=ulong_to_bignum(n);
  return fd_make_cptr(bigint_type,bx);
}

#define sum_overflow(x,y) \
  (((x>0) && (y>0)) ? ((INT_MAX-x) <= y) : \
   (((x<0) && (y>0)) ? ((INT_MIN+y) >= x) : 0))

static lisp int_plus(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    if (!(sum_overflow(ix,iy))) return LISPFIX(ix+iy);
    else {
      bignum_type bx=long_to_bignum(ix);
      bignum_type by=long_to_bignum(iy);
      bignum_type prod=bignum_add(bx,by);
      free(bx); free(by);
      return fd_make_cptr(bigint_type,prod);}}
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum(FIXLISP(x))) : ((bignum_type)CPTR_DATA(x)));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum(FIXLISP(y))) : ((bignum_type)CPTR_DATA(y)));
    bignum_type answer=bignum_add(bx,by);
    if (FIXNUMP(x)) free(bx); if (FIXNUMP(y)) free(by);
    return fd_make_cptr(bigint_type,answer);}
  else if (!(INTEGERP(x))) fd_type_error(_("not an integer"),x);
  else fd_type_error(_("not an integer"),y);
}

static lisp int_minus(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    if (!(sum_overflow(ix,-iy))) return LISPFIX(ix-iy);
    else {
      bignum_type bx=long_to_bignum(ix);
      bignum_type by=long_to_bignum(iy);
      bignum_type prod=bignum_subtract(bx,by);
      free(bx); free(by);
      return fd_make_cptr(bigint_type,prod);}}
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum(FIXLISP(x))) : ((bignum_type)CPTR_DATA(x)));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum(FIXLISP(y))) : ((bignum_type)CPTR_DATA(y)));
    bignum_type answer=bignum_subtract(bx,by);
    if (FIXNUMP(x)) free(bx); if (FIXNUMP(y)) free(by);
    return big2lisp(answer);}
  else if (!(INTEGERP(x))) fd_type_error(_("not an integer"),x);
  else fd_type_error(_("not an integer"),y);
}

#define prod_overflow(x,y) \
  ((x!=0) && ((INT_MAX/((x>0) ? x : -x)) >= ((y>0) ? y : -y)))

static lisp int_times(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    if (prod_overflow(ix,iy)) return LISPFIX(ix*iy);
    else {
      bignum_type bx=long_to_bignum(ix);
      bignum_type by=long_to_bignum(iy);
      bignum_type prod=bignum_multiply(bx,by);
      free(bx); free(by);
      return big2lisp(prod);}}
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum((long)FIXLISP(x))) : ((bignum_type)CPTR_DATA(x)));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum((long)FIXLISP(y))) : ((bignum_type)CPTR_DATA(y)));
    bignum_type answer=bignum_multiply(bx,by);
    if (FIXNUMP(x)) free(bx); if (FIXNUMP(y)) free(by);
    return big2lisp(answer);}
  else if (!(INTEGERP(x))) fd_type_error(_("not an integer"),x);
  else fd_type_error(_("not an integer"),y);
}

static lisp int_quotient(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix/iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntOverflow);}
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum(FIXLISP(x))) : ((bignum_type)CPTR_DATA(x)));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum(FIXLISP(y))) : ((bignum_type)CPTR_DATA(y)));
    bignum_type answer=bignum_quotient(bx,by);
    if (FIXNUMP(x)) free(bx); if (FIXNUMP(y)) free(by);
    return big2lisp(answer);}
  else if (!(INTEGERP(x)))
    fd_type_error(_("not an integer"),x);
  else fd_type_error(_("not an integer"),y);
}

static lisp int_remainder(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix%iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntOverflow);}
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum(FIXLISP(x))) : ((bignum_type)CPTR_DATA(x)));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum(FIXLISP(y))) : ((bignum_type)CPTR_DATA(y)));
    bignum_type answer=bignum_remainder(bx,by);
    if (FIXNUMP(x)) free(bx); if (FIXNUMP(y)) free(by);
    return big2lisp(answer);}
  else if (!(INTEGERP(x))) fd_type_error(_("not an integer"),x);
  else fd_type_error(_("not an integer"),y);
}

static int fix_gcd (int x, int y)
{
  int a;
  if (x < 0) x=-x; if (y < 0) y=-y;
  a= y; while (a != 0) { y = a; a = x % a; x = y; }
  return x;
}

static lisp int_gcd(lisp x,lisp y)
{
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y)))
    return LISPFIX(fix_gcd(FIXLISP(x),FIXLISP(y)));
  else if ((INTEGERP(x)) && (INTEGERP(y))) {
    bignum_type bx=
      ((FIXNUMP(x)) ? (long_to_bignum(FIXLISP(x))) :
       (bignum_copy((bignum_type)CPTR_DATA(x))));
    bignum_type by=
      ((FIXNUMP(y)) ? (long_to_bignum(FIXLISP(y))) :
       (bignum_copy((bignum_type)CPTR_DATA(y))));
    bignum_type ba;
    if (bignum_test(bx) == bignum_comparison_less) {
      bignum_type zero=bignum_make_zero();
      bignum_type neg=bignum_subtract(zero,bx);
      free(bx); free(zero); bx=neg;}
    if (bignum_test(by) == bignum_comparison_less) {
      bignum_type zero=bignum_make_zero();
      bignum_type neg=bignum_subtract(zero,by);
      free(by); free(zero); by=neg;}
    ba=by;
    while (bignum_test(ba) != bignum_comparison_equal) {
      by=ba; ba=bignum_remainder(bx,ba);
      free(bx); bx=by;}
    free(ba);
    return big2lisp(bx);}
  else fd_type_error(_("not an integer"),y);
}

static lisp int_lcm (lisp x, lisp y)
{
  lisp prod=int_times(x,y), gcd=int_gcd(x,y), lcm;
  if (FIXNUMP(prod))
    if (FIXLISP(prod) < 0) prod=LISPFIX(-(FIXLISP(prod)));
    else {}
  else if
    (bignum_test((bignum_type)CPTR_DATA(prod)) == bignum_comparison_less) {
    bignum_type zero=bignum_make_zero();
    bignum_type neg=bignum_subtract(zero,(bignum_type)CPTR_DATA(prod));
    free(zero); decref(prod);
    prod=fd_make_cptr(bigint_type,neg);}
  lcm=int_quotient(prod,gcd);
  decref(prod); decref(gcd);
  return lcm;
}

static void write_bignum_byte(bignum_procedure_context context,int digit)
{
  unsigned char **s=(unsigned char **) context;
  **s=(unsigned char)digit; (*s)++;
}

static int bignum_data(lisp x,void **velts)
{
  /* Estimate initial size and make an array to write bytes
      into; write the bytes and then produce an output array
      starting with a sign byte and followed by the written bytes
      reversed (since bignum_to_igdit_stream writes them backwards). */
  bignum_type b=(bignum_type)CPTR_DATA(x);
  unsigned long nbytes=bignum_length_in_bytes(b);
  unsigned char *bytes=fd_xmalloc(1+nbytes), *scan=bytes;
  unsigned char *vec, *scan_vec;
  unsigned char negativep=(bignum_test(b) == bignum_comparison_less);
  bignum_to_digit_stream(b,256,write_bignum_byte,&scan);
  *velts=vec=fd_xmalloc(1+(scan-bytes));
  vec[0]=negativep; scan_vec=vec+1; scan--;
  while (scan >= bytes) *scan_vec++=*scan--;
  fd_xfree(bytes);
  return scan_vec-vec;
}

static void done_with_bignum_data(lisp x,int size,void **velts)
{
  fd_xfree(*velts);
}
static unsigned int compare_bignums(lisp x,lisp y)
{
  if ((BIGNUMP(x)) && (BIGNUMP(y)))
    if (bignum_equal_p(((bignum_type)(CPTR_DATA(x))),
		       ((bignum_type)(CPTR_DATA(y)))))
      return 1; else return 0;
  else return 0;
}

static unsigned int get_bignum_byte(void *context)
{
  unsigned char **s=(unsigned char **) context;
  unsigned char c=*((*s)++);
  return c;
}

static lisp init_bignum(int size,void *vdigits)
{
  unsigned char *digits=vdigits, *scan=digits+1;
  int negative=digits[0];
  bignum_type b=
    digit_stream_to_bignum(size-1,get_bignum_byte,&scan,256,negative);
  fd_malloc_adjust(-size);
  return fd_make_cptr(bigint_type,b);
}

#define digit2char(d) (((d >= 0) && (d < 10)) ? ('0'+d) : ('a'+(d-10)))
#define char2digit(c) ((isdigit(c)) ? (c-'0') : \
		       ((isxdigit(c)) ? \
			((isupper(c) ? (c-'A') : (c-'a'))) : \
			(fd_raise_detailed_exception(fd_ParseError,"numeric digit"),0)))

static void write_digit_for_bignum(bignum_procedure_context cx,int digit)
{
  fd_string_stream s=(fd_string_stream) cx;
  char ss[2]; ss[1]='\0'; ss[0]=digit2char(digit);
  fd_sputs(s,ss);
}

static void print_bignum(lisp big,fd_string_stream s)
{
  int pos=s->size;
  bignum_type b=(bignum_type)CPTR_DATA(big);
  bignum_to_digit_stream
    (b,10,write_digit_for_bignum,(bignum_procedure_context)s);
  if (bignum_test(b) == bignum_comparison_less)
    fd_sputc(s,'-');
  {
    char *start=s->ptr+pos, *end=s->ptr+s->size-1;
    while (start < end) {
      char swap=*start; *start=*end; *end=swap;
      start++; end--;}
  }
}

static void free_bignum(lisp big)
{
  bignum_type b=(bignum_type)CPTR_DATA(big);
  if (b) free(b);
  fd_qfree(PTR_DATA(big,cptr),sizeof(struct FD_CPTR));
}

static lisp copy_bignum(lisp big)
{
  bignum_type b=(bignum_type)CPTR_DATA(big);
  return fd_make_cptr(bigint_type,bignum_copy(b));
}

static unsigned int hash_bignum(lisp x,fd_hashfn h)
{
  /* Estimate initial size and make an array to write bytes
      into; write the bytes and then produce an output array
      starting with a sign byte and followed by the written bytes
      reversed (since bignum_to_igdit_stream writes them backwards). */
  fd_lisp mystery; struct FD_MYSTERY m; struct FD_CPTR c;
  int hashval;
  bignum_type b=(bignum_type)CPTR_DATA(x);
  unsigned long nbytes=bignum_length_in_bytes(b);
  unsigned char *bytes=fd_xmalloc(1+nbytes), *scan=bytes;
  unsigned char *vec, *scan_vec;
  unsigned char negativep=(bignum_test(b) == bignum_comparison_less);
  bignum_to_digit_stream(b,256,write_bignum_byte,&scan);
  vec=fd_xmalloc(1+(scan-bytes));
  vec[0]=negativep; scan_vec=vec+1; scan--;
  while (scan >= bytes) *scan_vec++=*scan--;
  fd_xfree(bytes);
  m.data.bytes=vec; m.length=scan_vec-vec;
  m.package=dt_extended_numeric; m.code=dt_bignum;
  c.n_refs=1; c.ptr=&m;
  mystery.data.cptr=&c; mystery.type=mystery_type;
  hashval=h(mystery);
  fd_xfree(vec);
  return hashval;
}

static unsigned int get_bignum_digit(void *context)
{
  char **s=(char **) context;
  char c=*((*s)++);
  return char2digit(c);
}

static lisp parse_bignum(char *digits,int radix,int negative)
{
  bignum_type b=
    digit_stream_to_bignum
    (strlen(digits),get_bignum_digit,&digits,radix,negative);
  return fd_make_cptr(bigint_type,b);
}

static lisp bignum_parser(char *digits,int radix)
{
  if (*digits == '-')
    return parse_bignum(digits+1,radix,1);
  else return parse_bignum(digits,radix,0);
}

#define integer_sign(x) \
   ((FIXNUMP(x)) ? \
    ((FIXLISP(x) < 0) ? -1 : ((FIXLISP(x) == 0) ? 0 : 1)) : \
    (BIGNUMP(x)) ? \
    ((bignum_test((bignum_type)CPTR_DATA(x)) \
      ==  bignum_comparison_less) ? -1 : \
     (((bignum_test((bignum_type)CPTR_DATA(x)) \
	==  bignum_comparison_equal) ? 0 : 1))) : \
    88)

#else

/** Integer operations (without bignums) **/

static lisp int_plus(lisp x,lisp y) {
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix+iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntegerOverflow);}
  else if (!(FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else fd_type_error(_("not a fixnum"),y);
}

static lisp int_minus(lisp x,lisp y) {
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix-iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntegerOverflow);}
  else if (!(FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else fd_type_error(_("not a fixnum"),y);
}

static lisp int_times(lisp x,lisp y) {
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix*iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntegerOverflow);}
  else if (!(FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else fd_type_error(_("not a fixnum"),y);
}

static lisp int_quotient(lisp x,lisp y) {
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix/iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntegerOverflow);}
  else if (!(FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else fd_type_error(_("not a fixnum"),y);
}

static lisp int_remainder(lisp x,lisp y) {
  errno=0;
  if ((FIXNUMP(x)) && (FIXNUMP(y))) {
    int ix=FIXLISP(x), iy=FIXLISP(y);
    int io=ix%iy;
    if (errno == 0) return LISPFIX(io);
    else fd_raise_exception(fd_IntegerOverflow);}
  else if (!(FIXNUMP(x)))
    fd_type_error(_("not a fixnum"),x);
  else fd_type_error(_("not a fixnum"),y);
}

static int fix_gcd (int x, int y)
{
  int a;
  if (x < 0) x=-x; if (y < 0) y=-y;
  a= y; while (a != 0) { y = a; a = x % a; x = y; }
  return x;
}

static int fix_lcm (int x, int y)
{
  int prod=x*y, gcd=fix_gcd(x,y);
  if (prod < 0) prod=-prod;
  return prod/gcd;
}

static lisp int_gcd(lisp x,lisp y)
{
  return LISPFIX(fix_gcd(FIXLISP(x),FIXLISP(y)));
}

static lisp int_lcm(lisp x,lisp y)
{
  return LISPFIX(fix_lcm(FIXLISP(x),FIXLISP(y)));
}

#define integer_sign(x) \
   ((FIXNUMP(y)) ? ((FIXLISP(y) < 0) ? -1 : (FIXLISP(y) == 0) ? 0 :1) : \
     88)

#endif


/** Some conversions **/

static lisp make_rational(lisp num,lisp denom)
{
  if ((FIXNUMP(num)) && (FIXNUMP(denom))) {
    int in=FIXLISP(num), id=FIXLISP(denom), igcd=fix_gcd(in,id);
    in=in/igcd; id=id/igcd;
    if (id == 1) return LISPFIX(in);
    else if (id < 0)
      return fd_make_rational(LISPFIX(-in),LISPFIX(-id));
    else return fd_make_rational(LISPFIX(in),LISPFIX(id));}
  else if ((INTEGERP(num)) && (INTEGERP(denom))) {
    lisp gcd=int_gcd(num,denom);
    lisp new_num=int_quotient(num,gcd);
    lisp new_denom=int_quotient(denom,gcd);
    lisp new_rat;
    decref(gcd); 
    if (((FIXNUMP(new_denom)) && (FIXLISP(new_denom) == 1)))
      new_rat=incref(new_num);
    else new_rat=fd_make_rational(new_num,new_denom);
    return new_rat;}
  else fd_raise_exception("Non integral components for rational");
}

static lisp make_complex(lisp real,lisp imag)
{
  if (LISP_ZEROP(imag)) return incref(real);
  else return fd_make_complex(incref(real),incref(imag));
}

static double to_float(lisp x)
{
  if (FLONUMP(x)) return FLOATLISP(x);
  else if (FIXNUMP(x)) return (double) (FIXLISP(x));
  else if (BIGNUMP(x)) 
    return bignum_to_double((bignum_type)CPTR_DATA(x));
  else if (RATIONALP(x)) {
    double fnum=to_float((FD_NUMERATOR(x)));
    double fden=to_float((FD_DENOMINATOR(x)));
    return fnum/fden;}
  else fd_type_error(_("Can't coerce to float"),x);
}

FDSCRIPT_EXPORT double fd_to_float(lisp x) { return to_float(x); }

/** Higher order functions **/

static lisp reduce(lisp start,lisp rest,lisp (*op)(lisp,lisp))
{
  if (FD_EMPTY_LISTP(rest))
    if (CHOICEP(start)) return fd_return_proper_choice(start);
    else return start;
  else {
    lisp next=FD_EMPTY_CHOICE;
    DO_CHOICES(a,start) {
      DO_CHOICES(b,CAR(rest)) {
	lisp one=op(a,b);
	ADD_TO_CHOICE(next,one);}
      END_DO_CHOICES;}
    END_DO_CHOICES;
    decref(start);
    return reduce(next,CDR(rest),op);}
}

static int reduce_bool(lisp args,int (*op)(lisp,lisp))
{
  if ((FD_EMPTY_LISTP(args)) || (FD_EMPTY_LISTP(CDR(args)))) return 1;
  else {
    lisp a=CAR(args), b=CAR(CDR(args)); int so_far_so_good=0;
    if ((CHOICEP(a)) || (CHOICEP(b))) {
      DO_CHOICES(xa,a) {
	DO_CHOICES(xb,b) {
	  if (op(xa,xb)) so_far_so_good=1;}
	END_DO_CHOICES;}
      END_DO_CHOICES;}
    else so_far_so_good=op(a,b);
    if (so_far_so_good) return reduce_bool(CDR(args),op);
    else return 0;}
}

/** Addition **/

static lisp scalar_plus(lisp x,lisp y)
{
  if ((FLONUMP(x)) || (FLONUMP(y))) {
    double fx=to_float(x), fy=to_float(y);
    return LISPFLOAT(fx+fy);}
  else if ((RATIONALP(x)) || (RATIONALP(y))) {
    lisp xnum, xden, ynum, yden;
    lisp new_numP1, new_numP2, new_num, new_denom, result;
    if (RATIONALP(x)) {xnum=FD_NUMERATOR(x); xden=FD_DENOMINATOR(x);}
    else {xnum=x; xden=LISPFIX(1);}
    if (RATIONALP(y)) {ynum=FD_NUMERATOR(y); yden=FD_DENOMINATOR(y);}
    else {ynum=y; yden=LISPFIX(1);}
    new_denom=int_times(xden,yden);
    new_numP1=int_times(xnum,yden); new_numP2=int_times(ynum,xden);
    new_num=int_plus(new_numP1,new_numP2);
    result=make_rational(new_num,new_denom);
    decref(new_numP1); decref(new_numP2);
    decref(new_denom); decref(new_num);
    return result;}
  else return int_plus(x,y);
}

static lisp complex_plus(lisp x,lisp y)
{
  if ((COMPLEXP(x)) || (COMPLEXP(y))) {
    lisp realx, realy, imagx, imagy, realr, imagr, result;
    if (COMPLEXP(x)) {realx=FD_REALPART(x); imagx=FD_IMAGPART(x);}
    else {realx=x; imagx=LISPFIX(0);}
    if (COMPLEXP(y)) {realy=FD_REALPART(y); imagy=FD_IMAGPART(y);}
    else {realy=y; imagy=LISPFIX(0);}
    realr=scalar_plus(realx,realy); imagr=scalar_plus(imagx,imagy);
    result=make_complex(realr,imagr);
    return result;}
  else return scalar_plus(x,y);
}

FDSCRIPT_EXPORT lisp fd_plus(lisp x,lisp y) {
  return complex_plus(x,y);}

static lisp lisp_generic_plus_lexpr(lisp args)
{
  return reduce(LISPFIX(0),args,complex_plus);
}

/** Subtraction **/

static lisp scalar_minus(lisp x,lisp y)
{
  if ((FLONUMP(x)) || (FLONUMP(y))) {
    double fx=to_float(x), fy=to_float(y);
    return LISPFLOAT(fx-fy);}
  else if ((RATIONALP(x)) || (RATIONALP(y))) {
    lisp xnum, xden, ynum, yden;
    lisp new_numP1, new_numP2, new_num, new_denom, result;
    if (RATIONALP(x)) {xnum=FD_NUMERATOR(x); xden=FD_DENOMINATOR(x);}
    else {xnum=x; xden=LISPFIX(1);}
    if (RATIONALP(y)) {ynum=FD_NUMERATOR(y); yden=FD_DENOMINATOR(y);}
    else {ynum=y; yden=LISPFIX(1);}
    new_numP1=int_times(xnum,yden); new_numP2=int_times(ynum,xden);
    new_denom=int_times(xden,yden);
    new_num=int_minus(new_numP1,new_numP2);
    result=make_rational(new_num,new_denom);
    decref(new_numP1); decref(new_numP2);
    decref(new_denom); decref(new_num);
    return result;}
  else return int_minus(x,y);
}
    
static lisp complex_minus(lisp x,lisp y)
{
  if ((COMPLEXP(x)) || (COMPLEXP(y))) {
    lisp realx, realy, imagx, imagy, realr, imagr;
    if (COMPLEXP(x)) {realx=FD_REALPART(x); imagx=FD_IMAGPART(x);}
    else {realx=x; imagx=LISPFIX(0);}
    if (COMPLEXP(y)) {realy=FD_REALPART(y); imagy=FD_IMAGPART(y);}
    else {realy=y; imagy=LISPFIX(0);}
    realr=scalar_minus(realx,realy); imagr=scalar_minus(imagx,imagy);
    return make_complex(realr,imagr);}
  else return scalar_minus(x,y);
}

FDSCRIPT_EXPORT lisp fd_minus(lisp x,lisp y) {
  return complex_minus(x,y);}

static lisp lisp_generic_minus_lexpr(lisp args)
{
  lisp start, rest;
  if (FD_EMPTY_LISTP(CDR(args))) {start=LISPFIX(0); rest=args;}
  else {start=incref(CAR(args)); rest=CDR(args);}
  return reduce(start,rest,complex_minus);
}

/** Multiplication **/

static lisp scalar_times(lisp x,lisp y)
{
  if ((FLONUMP(x)) || (FLONUMP(y))) {
    double fx=to_float(x), fy=to_float(y);
    return LISPFLOAT(fx*fy);}
  else if ((RATIONALP(x)) || (RATIONALP(y))) {
    lisp xnum, xden, ynum, yden;
    lisp new_denom, new_num, result;
    if (RATIONALP(x)) {xnum=FD_NUMERATOR(x); xden=FD_DENOMINATOR(x);}
    else {xnum=x; xden=LISPFIX(1);}
    if (RATIONALP(y)) {ynum=FD_NUMERATOR(y); yden=FD_DENOMINATOR(y);}
    else {ynum=y; yden=LISPFIX(1);}
    new_denom=int_times(xden,yden);
    new_num=int_times(xnum,ynum);
    result=make_rational(new_num,new_denom);
    decref(new_num); decref(new_denom);
    return result;}
  else return int_times(x,y);
}
    
static lisp complex_times(lisp x,lisp y)
{
  if ((COMPLEXP(x)) || (COMPLEXP(y))) {
    lisp realx, realy, imagx, imagy;
    lisp t1, t2, t3, t4, realr, imagr, result;
    if (COMPLEXP(x)) {realx=FD_REALPART(x); imagx=FD_IMAGPART(x);}
    else {realx=x; imagx=LISPFIX(0);}
    if (COMPLEXP(y)) {realy=FD_REALPART(y); imagy=FD_IMAGPART(y);}
    else {realy=y; imagy=LISPFIX(0);}
    t1=scalar_times(realx,realy); t2=scalar_times(imagx,imagy);
    t3=scalar_times(realx,imagy); t4=scalar_times(imagx,realy);
    realr=scalar_minus(t1,t2); imagr=scalar_plus(t3,t4);    
    result=make_complex(realr,imagr);
    decref(t1); decref(t2); decref(t3); decref(t4);
    decref(realr); decref(imagr);
    return result;}
  else return scalar_times(x,y);
}

FDSCRIPT_EXPORT lisp fd_times(lisp x,lisp y) {
  return complex_times(x,y);}

static lisp lisp_generic_times_lexpr(lisp args)
{
  return reduce(LISPFIX(1),args,complex_times);
}

/** Division **/

static lisp scalar_div(lisp x,lisp y)
{
  if ((FLONUMP(x)) || (FLONUMP(y))) {
    double fx=to_float(x), fy=to_float(y);
    return LISPFLOAT(fx/fy);}
  else {
    lisp xnum, xden, ynum, yden;
    lisp new_num, new_denom, result;
    if (RATIONALP(x)) {xnum=FD_NUMERATOR(x); xden=FD_DENOMINATOR(x);}
    else {xnum=x; xden=LISPFIX(1);}
    if (RATIONALP(y)) {ynum=FD_NUMERATOR(y); yden=FD_DENOMINATOR(y);}
    else {ynum=y; yden=LISPFIX(1);}
    new_denom=int_times(xden,ynum);
    new_num=int_times(xnum,yden);
    result=make_rational(new_num,new_denom);
    decref(new_num); decref(new_denom);
    return result;}
}
    
static lisp complex_div(lisp x,lisp y)
{
  if ((COMPLEXP(x)) || (COMPLEXP(y))) {
    lisp realx, realy, imagx, imagy;
    lisp t1, t2, t3, t4, t5, t6,t7, t8, realr, imagr, result;
    lisp t1pt2, t3pt4, t5pt6, t7pt8, negt5pt6;
    if (COMPLEXP(x)) {realx=FD_REALPART(x); imagx=FD_IMAGPART(x);}
    else {realx=x; imagx=LISPFIX(0);}
    if (COMPLEXP(y)) {realy=FD_REALPART(y); imagy=FD_IMAGPART(y);}
    else {realy=y; imagy=LISPFIX(0);}
    t1=scalar_times(realx,realy); t2=scalar_times(imagx,imagy);
    t1pt2=scalar_plus(t1,t2);
    t3=scalar_times(imagx,imagx); t4=scalar_times(imagy,imagy);
    t3pt4=scalar_plus(t3,t4);
    t5=scalar_times(realx,imagy); t6=scalar_times(realy,imagx);
    t5pt6=scalar_plus(t5,t6);
    t7=scalar_times(imagx,imagx); t8=scalar_times(imagy,imagy);
    t7pt8=scalar_plus(t7,t8);
    negt5pt6=scalar_minus(LISPFIX(0),t5pt6);
    realr=make_rational(t1pt2,t3pt4); imagr=make_rational(negt5pt6,t7pt8);
    result=make_complex(realr,imagr);
    decref(t1); decref(t2); decref(t3); decref(t4);
    decref(t5); decref(t6); decref(t7); decref(t8);
    decref(t1pt2); decref(t3pt4); decref(t5pt6); decref(t7pt8);
    decref(negt5pt6); decref(realr); decref(imagr);
    return result;}
  else return scalar_div(x,y);
}

FDSCRIPT_EXPORT lisp fd_div(lisp x,lisp y) {
  return complex_div(x,y);}

static lisp lisp_generic_div_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(CDR(args))) 
    return reduce(LISPFIX(1),args,complex_div);
  else return reduce(incref(CAR(args)),CDR(args),
		     complex_div);
}

/** EXPT (incomplete implementation) **/

static lisp lisp_expt_cproc(lisp base,lisp expt)
{
  if (FIXNUMP(expt)) {
    int i=1, abs_expt=FIXLISP(expt);
    lisp v=fd_incref(base);
    if (abs_expt < 0) abs_expt=-abs_expt;
    while (i < abs_expt) {
      lisp nv=complex_times(v,base);
      decref(v); v=nv; i++;}
    if (FIXLISP(expt) < 0)
      return fd_make_rational(LISPFIX(1),v);
    else return v;}
  else {
    double fb=to_float(base), fe=to_float(expt);
    double answer=pow(fb,fe);
    return LISPFLOAT(answer);}
}

/** Transcendental functions **/

#define TRANSCENDENTAL(lisp_name,c_name) \
static lisp lisp_name(lisp arg)      \
{                                    \
  double f=to_float(arg);            \
  double answer=c_name(f);           \
  return LISPFLOAT(answer);   \
}

TRANSCENDENTAL(lisp_log_cproc,log);
TRANSCENDENTAL(lisp_exp_cproc,exp);
TRANSCENDENTAL(lisp_sin_cproc,sin);
TRANSCENDENTAL(lisp_cos_cproc,cos);
TRANSCENDENTAL(lisp_tan_cproc,tan);
TRANSCENDENTAL(lisp_asin_cproc,asin);
TRANSCENDENTAL(lisp_acos_cproc,acos);
TRANSCENDENTAL(lisp_atan_cproc,atan);

static fd_lisp lisp_atan2_cproc(fd_lisp arg1,fd_lisp arg2)
{
  double f1=to_float(arg1), f2=to_float(arg2);
  double answer=atan2(f1,f2);
  return LISPFLOAT(answer);
}

static lisp lisp_ilog_cproc(lisp num)
{
  if (FIXNUMP(num)) {
    int n=FIXLISP(num), l=0, p=1;
    while (p < n) {p=p*2; l=l+1;}
    return LISPFIX(l);}
  else fd_type_error(_("not a fixnum"),num);
}

static lisp lisp_sqrt_cproc(lisp num)
{
  double fnum=to_float(num), root;
  int negative=0;
  if (fnum < 0) {fnum=-fnum; negative=1;}
  root=sqrt(fnum);
  if (FIXNUMP(num)) {
    int root_as_int=(int)root;
    if (root_as_int*root_as_int == FIXLISP(num))
      return LISPFIX(root_as_int);}
  if (negative)
    return make_complex(LISPFIX(0),LISPFLOAT(root));
  else return LISPFLOAT(root);
}

/** Numeric predicates **/

static lisp lisp_numberp_cproc(lisp x)
{
  if ((INTEGERP(x)) || (FLONUMP(x)) || (RATIONALP(x)) || (COMPLEXP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_complexp_cproc(lisp x)
{
  if ((INTEGERP(x)) || (FLONUMP(x)) || (RATIONALP(x)) ||
      (COMPLEXP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_realp_cproc(lisp x)
{
  if ((INTEGERP(x)) || (FLONUMP(x)) || (RATIONALP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_rationalp_cproc(lisp x)
{
  if ((INTEGERP(x)) || (FLONUMP(x)) || (RATIONALP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_integerp_cproc(lisp x)
{
  if (INTEGERP(x)) return FD_TRUE;
  else return FD_FALSE;
}

static int oddp(lisp x)
{
  if (FIXNUMP(x))
    if ((FIXLISP(x))%2) return 1;
    else return 0;
#if (USE_BIGNUMS)
  else if (BIGNUMP(x)) {
    bignum_type b1=(bignum_type)CPTR_DATA(x);
    bignum_type b2=long_to_bignum(2);
    bignum_type rem=bignum_remainder(b1,b2);
    enum bignum_comparison comparison=bignum_test(rem);
    free(b2); free(rem);
    if (comparison == bignum_comparison_equal) return 0;
    else return 1;}
#endif
  else return 0;
}

static lisp lisp_oddp_cproc(lisp x)
{
  if (oddp(x)) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_evenp_cproc(lisp x)
{
  if (oddp(x)) return FD_FALSE; else return FD_TRUE;
}

static int inexactp(lisp x)
{
  if (FLONUMP(x)) return 1;
  else if (COMPLEXP(x))
    if ((FLONUMP(FD_REALPART(x))) || (FLONUMP(FD_IMAGPART(x))))
      return 1;
    else return 0;
  else return 0;
}

static lisp lisp_inexactp_cproc(lisp x)
{
  if (inexactp(x)) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_exactp_cproc(lisp x)
{
  if (inexactp(x)) return FD_FALSE; else return FD_TRUE;
}

/** Exactness and Inexactness **/

static lisp lisp_to_inexact_cproc(lisp x)
{
  if (COMPLEXP(x)) 
    return make_complex(LISPFLOAT(to_float(FD_REALPART(x))),
			LISPFLOAT(to_float(FD_IMAGPART(x))));
  else return LISPFLOAT(to_float(x));
}

static lisp lisp_to_exact_cproc(lisp x)
{
  if (COMPLEXP(x)) 
    return make_complex(lisp_to_exact_cproc(FD_REALPART(x)),
			lisp_to_exact_cproc(FD_IMAGPART(x)));
  else if (FLONUMP(x)) {
    double d=FLOATLISP(x);
    int i=(int) d;
    if (errno == ERANGE) fd_raise_exception(fd_FlonumOverflow);
    else return LISPFIX(i);}
  else return incref(x);
}

/** Random numbers **/

static int random_seed, random_seed_initialized=0;

static lisp lisp_random_cproc(lisp range)
{
  if (random_seed_initialized == 0) {
    random_seed=fd_random(); fd_set_random(random_seed);
     random_seed_initialized=1;}
  if (FIXNUMP(range)) {
    int r=fd_random();
    return LISPFIX(r%FIXLISP(range));}
  else fd_type_error(_("range must be fixnum"), range);
}

static lisp lisp_random_seed_cproc()
{
  if (random_seed_initialized == 0) {
    random_seed=fd_random(); fd_set_random(random_seed);
    random_seed_initialized=1;}
  return LISPFIX(random_seed);
}

static lisp lisp_set_random_seed_cproc(lisp x)
{
  random_seed=FIXLISP(x); fd_set_random(random_seed);
  random_seed_initialized=1;
  return FD_TRUE;
}

static lisp lisp_randomize_lexpr(lisp args)
{
  lisp seed_arg=fd_get_arg(args,0,FD_FALSE); int seed;
  if (FIXNUMP(seed_arg)) seed=FIXLISP(seed_arg);
  else if (FD_FALSEP(seed_arg))
    seed=((int)time(NULL)+(int)getpid());
  else fd_type_error(_("seed must be fixnum"),seed_arg);
  fd_set_random(seed);
  return LISPFIX(seed);
}

/** Numeric comparisons **/

static int positivep(lisp x)
{
  if (FIXNUMP(x)) return (FIXLISP(x) > 0);
#if (USE_BIGNUMS)
  else if (BIGNUMP(x))
    return (bignum_test((bignum_type)CPTR_DATA(x)) == bignum_comparison_greater);
#endif
  else if (FLONUMP(x)) return (FLOATLISP(x) > 0.0);
  else if (RATIONALP(x)) return (positivep(FD_NUMERATOR(x)));
  else if (COMPLEXP(x)) return (positivep(FD_REALPART(x)));
  else fd_type_error(_("not a number"),x);
}

static int negativep(lisp x)
{
  if (FIXNUMP(x)) return (FIXLISP(x) < 0);
#if (USE_BIGNUMS)
  else if (BIGNUMP(x))
    return
      (bignum_test((bignum_type)(CPTR_DATA(x))) == bignum_comparison_less);
#endif
  else if (FLONUMP(x)) return (FLOATLISP(x) < 0.0);
  else if (RATIONALP(x)) return (negativep(FD_NUMERATOR(x)));
  else if (COMPLEXP(x)) return (negativep(FD_REALPART(x)));
  else fd_type_error(_("not a number"),x);
}

static lisp lisp_positivep_cproc(lisp x)
{ if (positivep(x)) return FD_TRUE; else return FD_FALSE;}

static lisp lisp_negativep_cproc(lisp x)
{ if (negativep(x)) return FD_TRUE; else return FD_FALSE;}

static int eqv(lisp n1,lisp n2)
{
  if ((LISP_EQ(n1,n2))) return 1;
  else if ((FD_XPROCP(n1)) && (FD_XPROCP(n2))) {
    /* Kludge for circular GC fix */
    fd_sproc s1=FD_GET_SPROC(n1), s2=FD_GET_SPROC(n2);
    if (!(LISP_EQ(s1->lambda,s2->lambda)) ) return 0;
    else if (s1->env != s2->env) return 0;
    else return 1;}
  else if ((FIXNUMP(n2)) && (FIXNUMP(n1))) return 0;
  else if ((FLONUMP(n2)) && (FLONUMP(n1)))
    if ((FLOATLISP(n1)) == (FLOATLISP(n2))) return 1;
    else return 0;
  else if ((NUMBERP(n2)) && (NUMBERP(n1))) {
    lisp diff=complex_minus(n1,n2);
    int zerop=LISP_ZEROP(diff); decref(diff);
    if (zerop) return 1; else return 0;}
  else if ((FD_PACKETP(n1)) && (FD_PACKETP(n2)))
    if ((FD_PACKET_LENGTH(n1) == FD_PACKET_LENGTH(n2)) &&
	((memcmp(FD_PACKET_DATA(n1),FD_PACKET_DATA(n2),FD_PACKET_LENGTH(n1)))
	 == 0))
      return 1;
    else return 0;
  else return 0;
}

static int equal(lisp n1,lisp n2)
{
  if ((LISP_EQ(n1,n2))) return 1;
  else if ((FD_XPROCP(n1)) && (FD_XPROCP(n2))) {
    /* Kludge for circular GC fix */
    fd_sproc s1=FD_GET_SPROC(n1), s2=FD_GET_SPROC(n2);
    if (!(LISP_EQ(s1->lambda,s2->lambda)) ) return 0;
    else if (s1->env != s2->env) return 0;
    else return 1;}
  else if ((FIXNUMP(n2)) && (FIXNUMP(n1))) return 0;
  else if ((FLONUMP(n2)) && (FLONUMP(n1)))
    if ((FLOATLISP(n1)) == (FLOATLISP(n2))) return 1;
    else return 0;
  else if ((NUMBERP(n2)) && (NUMBERP(n1))) {
    lisp diff=complex_minus(n1,n2);
    int zerop=LISP_ZEROP(diff); decref(diff);
    if (zerop) return 1; else return 0;}
  else if ((FD_PACKETP(n1)) && (FD_PACKETP(n2)))
    if ((FD_PACKET_LENGTH(n1) == FD_PACKET_LENGTH(n2)) &&
	((memcmp(FD_PACKET_DATA(n1),FD_PACKET_DATA(n2),FD_PACKET_LENGTH(n1)))
	 == 0))
      return 1;
    else return 0;
  else return fd_lisp_equal(n1,n2);
}

static int greater_than(lisp n1,lisp n2)
{
  if ((FIXNUMP(n1)) && (FIXNUMP(n2)))
    return (FIXLISP(n1) > FIXLISP(n2));
  else if ((FLONUMP(n1)) && (FLONUMP(n2)))
    return ((FLOATLISP(n1)) > (FLOATLISP(n2)));
  else {
    lisp diff=complex_minus(n1,n2);
    int pos=positivep(diff); decref(diff);
    return pos;}
}

FDSCRIPT_EXPORT int fd_compare(lisp n1,lisp n2)
{
  if ((FIXNUMP(n1)) && (FIXNUMP(n2)))
    if (FIXLISP(n1) > FIXLISP(n2)) return 1;
    else if (FIXLISP(n1) == FIXLISP(n2)) return 0;
    else return -1;
  else if ((FLONUMP(n1)) && (FLONUMP(n2)))
    if (FLOATLISP(n1) > FLOATLISP(n2)) return 1;
    else if (FLOATLISP(n1) == FLOATLISP(n2)) return 0;
    else return -1;
  else {
    lisp diff=complex_minus(n1,n2);
    int pos=positivep(diff), neg=negativep(diff); decref(diff);
    if (pos) return 1;
    else if (neg) return -1;
    else return 0;}
}


static int less_than(lisp n1,lisp n2)
{
  if ((FIXNUMP(n1)) && (FIXNUMP(n2)))
    return (FIXLISP(n1) < FIXLISP(n2));
  else if ((FLONUMP(n1)) && (FLONUMP(n2)))
    return ((FLOATLISP(n1)) < (FLOATLISP(n2)));
  else {
    lisp diff=complex_minus(n1,n2);
    int neg=negativep(diff); decref(diff);
    return neg;}
}  

static int less_than_or_equal(lisp n1,lisp n2)
{
  if (greater_than(n1,n2)) return 0; else return 1;
}

static int greater_than_or_equal(lisp n1,lisp n2)
{
  if (less_than(n1,n2)) return 0; else return 1;
}

static lisp lisp_eqv_lexpr(lisp args)
{
  DOLIST(elt,args)
    if (FD_EMPTYP(elt)) return FD_EMPTY_CHOICE;
  if (reduce_bool(args,eqv)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_equal_cproc(lisp arg1,lisp arg2)
{
  if (equal(arg1,arg2)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_gt_lexpr(lisp args)
{
  DOLIST(elt,args)
    if (FD_EMPTYP(elt)) return FD_EMPTY_CHOICE;
  if (reduce_bool(args,greater_than)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_lt_lexpr(lisp args)
{
  DOLIST(elt,args)
    if (FD_EMPTYP(elt)) return FD_EMPTY_CHOICE;
  if (reduce_bool(args,less_than)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_lte_lexpr(lisp args)
{
  DOLIST(elt,args)
    if (FD_EMPTYP(elt)) return FD_EMPTY_CHOICE;
  if (reduce_bool(args,less_than_or_equal)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_gte_lexpr(lisp args)
{
  DOLIST(elt,args)
    if (FD_EMPTYP(elt)) return FD_EMPTY_CHOICE;
  if (reduce_bool(args,greater_than_or_equal)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_zerop_cproc(lisp n)
{
  if ((FIXNUMP(n)) && (FIXLISP(n) == 0)) return FD_TRUE;
  else return FD_FALSE;
}

/** Taking apart compound numbers **/

static lisp lisp_numerator_cproc(lisp x)
{
  if (RATIONALP(x)) return incref(FD_NUMERATOR(x));
  else return incref(x);
}

static lisp lisp_denominator_cproc(lisp x)
{
  if (RATIONALP(x)) return incref(FD_NUMERATOR(x));
  else return LISPFIX(1);
}

static lisp lisp_realpart_cproc(lisp x)
{
  if (COMPLEXP(x)) return incref(FD_REALPART(x));
  else return incref(x);
}

static lisp lisp_imagpart_cproc(lisp x)
{
  if (COMPLEXP(x)) return incref(FD_IMAGPART(x));
  else return LISPFIX(0);
}

static lisp lisp_make_rectangular_cproc(lisp x,lisp y)
{
  return make_complex(incref(x),incref(y));
}

static lisp lisp_magnitude_cproc(lisp x)
{
  if (COMPLEXP(x)) {
    lisp squared=
      complex_plus(scalar_times(FD_REALPART(x),FD_REALPART(x)),
		   scalar_times(FD_IMAGPART(x),FD_IMAGPART(x)));
    lisp root=lisp_sqrt_cproc(squared); decref(squared);
    return root;}
  else if (positivep(x)) return incref(x);
  else return scalar_minus(LISPFIX(0),x);
}

/** Max and min **/

static lisp complex_min(lisp x,lisp y)
{
  int gthan=greater_than(x,y);
  int inexact=((inexactp(x)) || (inexactp(y)));
  if (inexact)
    if (gthan) return lisp_to_inexact_cproc(y);
    else return lisp_to_inexact_cproc(x);
  else if (gthan)
    return incref(y);
  else return incref(x);
}
    
static lisp lisp_generic_min_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) return FD_EMPTY_CHOICE;
  else {
    lisp start=incref(CAR(args));
    return reduce(start,CDR(args),complex_min);}
}

static lisp complex_max(lisp x,lisp y)
{
  int gthan=greater_than(x,y);
  int inexact=((inexactp(x)) || (inexactp(y)));
  if (inexact)
    if (gthan) return lisp_to_inexact_cproc(x);
    else return lisp_to_inexact_cproc(y);
  else if (gthan)
    return incref(x);
  else return incref(y);
}
    
static lisp lisp_generic_max_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) return FD_EMPTY_CHOICE;
  else {
    lisp start=incref(CAR(args));
    return reduce(start,CDR(args),complex_max);}
}

/** Quotient, etc **/

static lisp lisp_modulo_cproc(lisp x,lisp y)
{
  lisp remainder=int_remainder(x,y);
  int ysign=integer_sign(y), remsign=integer_sign(remainder);
  if ((remsign == 0) || (ysign == remsign)) return remainder;
  else {
    lisp mod=int_plus(y,remainder);
    decref(remainder);
    return mod;}
}

static lisp lisp_gcd(lisp n1,lisp n2)
{
  return int_gcd(n1,n2);
}

static lisp lisp_nary_gcd_lexpr(lisp args)
{
  return reduce(LISPFIX(0),args,lisp_gcd);
}

static lisp lisp_lcm(lisp n1,lisp n2)
{
  return int_lcm(n1,n2);
}

static lisp lisp_nary_lcm_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(args)) return LISPFIX(1);
  else if (FD_EMPTY_LISTP(CDR(args))) return (CAR(args));
  else return reduce(incref(CAR(args)),CDR(args),lisp_lcm);
}

/** Floor, etc. **/

static lisp lisp_floor_cproc(lisp x)
{
  if (FIXNUMP(x)) return incref(x);
  else if (FLONUMP(x)) {
    double fnum=floor(FLOATLISP(x));
    int as_int=(int)fnum;
    if (errno == ERANGE) fd_raise_exception(fd_IntOverflow);
    else return LISPFIX(as_int);}
  else if (RATIONALP(x)) {
    lisp q=int_quotient(FD_NUMERATOR(x),FD_DENOMINATOR(x));
    if (negativep(q)) return scalar_minus(q,LISPFIX(1));
    else return q;}
  else if (COMPLEXP(x)) {
    lisp mag=lisp_magnitude_cproc(x);
    lisp mag_floor=lisp_floor_cproc(mag);
    decref(mag);
    return mag_floor;}
  else fd_type_error(_("not a number"),x);
}

static lisp lisp_ceiling_cproc(lisp x)
{
  if (FIXNUMP(x)) return incref(x);
  else if (FLONUMP(x)) {
    double fl=ceil(FLOATLISP(x));
    int as_int=(int)fl;
    if (errno == ERANGE) fd_raise_exception(fd_IntOverflow);
    else return LISPFIX(as_int);}
  else if (RATIONALP(x)) {
    lisp q=int_quotient(FD_NUMERATOR(x),FD_DENOMINATOR(x));
    if (negativep(q)) return q;
    else return scalar_plus(q,LISPFIX(1));}
  else if (COMPLEXP(x)) {
    lisp mag=lisp_magnitude_cproc(x);
    lisp mag_floor=lisp_ceiling_cproc(mag);
    decref(mag);
    return mag_floor;}
  else fd_type_error(_("not a number"),x);
}

static lisp lisp_truncate_cproc(lisp x)
{
  if (FIXNUMP(x)) return incref(x);
  else if (FLONUMP(x)) {
    double fl=fdtrunc(FLOATLISP(x));
    int as_int=(int)fl;
    if (errno == ERANGE) fd_raise_exception(fd_IntOverflow);
    else return LISPFIX(as_int);}
  else if (RATIONALP(x)) {
    lisp q=int_quotient(FD_NUMERATOR(x),FD_DENOMINATOR(x));
    return q;}
  else if (COMPLEXP(x)) {
    lisp mag=lisp_magnitude_cproc(x);
    lisp mag_floor=lisp_ceiling_cproc(mag);
    decref(mag);
    return mag_floor;}
  else fd_type_error(_("not a number"),x);
}

static int fix_round(int x,int f)
{
  int q=x/f, r=x%f;
  if ((r<0) ? (-r>=f/2) : (r>=f/2))
    if (q == 0)
      if (x<0) return -f; else return f;
    else return f*(q+1);
  else return f*q;
}

static double doround(double f)
{
  double ft=fdtrunc(f);
  if (f < 0)
    {if (ft-f >= 0.5) ft=ft-1.0;
    else if ((ft-f == 0.5) && (((int)ft)%2 == -1)) ft=ft-1.0;}
  else if (f-ft >= 0.5) ft=ft+1.0;
  else if ((f-ft == 0.5) && (((int)ft)%2 == 1)) ft=ft+1.0;
  return ft;
}

#define getnumerator(x) \
  ((FD_RATIONALP(x)) ? (FD_NUMERATOR(x)) : (x))
#define getdenominator(x) \
  ((FD_RATIONALP(x)) ? (FD_DENOMINATOR(x)) : (FD_LISPFIX(1)))

static lisp scalar_round(lisp x,lisp factor)
{
  if ((FIXNUMP(x)) && (FIXNUMP(factor)))
    return FD_LISPFIX(fix_round(FD_FIXLISP(x),FD_FIXLISP(factor)));
  else if ((FD_FLONUMP(x)) || (FD_FLONUMP(factor))) {
    double fx=to_float(x), fct=to_float(factor);
    double f=doround(fx/fct), ft=fdtrunc(f), answer;
    answer=ft*fct;
    if (FD_FIXNUMP(factor)) {
      double fl=fdtrunc(answer);
      int as_int=(int)fl;
      return FD_LISPFIX(as_int);}
    else return FD_LISPFLOAT(answer);}
  else if ((FD_INTEGERP(x)) || (RATIONALP(x))) {
    lisp num=int_times(getnumerator(x),getdenominator(factor));
    lisp den=int_times(getdenominator(x),getnumerator(factor));
    int negative=negativep(x);
    lisp q=int_quotient(num,den);
    lisp r=int_remainder(num,den);
    lisp halfway=int_quotient(den,((negative) ? (LISPFIX(-2)) : (LISPFIX(2))));
    lisp answer, temp;
    if (negative)
      if (greater_than(r,halfway)) temp=fd_incref(q);
      else temp=scalar_minus(q,LISPFIX(1));
    else if (less_than(r,halfway)) temp=fd_incref(q);
    else temp=scalar_plus(q,LISPFIX(1));
    answer=scalar_times(temp,factor); decref(temp);
    decref(q); decref(r); decref(temp); decref(halfway); 
    return answer;}
  else if (COMPLEXP(x)) {
    lisp mag=lisp_magnitude_cproc(x);
    lisp mag_floor=scalar_round(mag,factor);
    decref(mag);
    return mag_floor;}
  else fd_type_error(_("not a number"),x);
}

static fd_lisp lisp_round_lexpr(fd_lisp args)
{
  fd_lisp x=fd_get_arg(args,0,FD_VOID);
  fd_lisp factor=fd_get_arg(args,1,FD_LISPFIX(1));
  return scalar_round(x,factor);
}

static fd_lisp lisp_scalerep_cproc(fd_lisp v,fd_lisp scale)
{
  if ((FD_FIXNUMP(scale)) && (FD_FIXLISP(scale)<0)) {
    int divisor=-FD_FIXLISP(scale);
    if (FD_FIXNUMP(v)) 
      return FD_MAKE_PAIR(fd_incref(v),fd_incref(scale));
    else if (FD_FLONUMP(v)) {
      double fv=FD_FLOATLISP(v);
      double scaled=fv*divisor;
      double rounded=doround(scaled);
      int asint=fdtrunc(rounded);
      return FD_MAKE_PAIR(FD_LISPFIX(asint),fd_incref(scale));}
    else {
      fd_lisp invscale=fd_make_rational(FD_LISPFIX(1),FD_LISPFIX(divisor));
      fd_lisp rounded=scalar_round(v,invscale);
      fd_lisp multiplied=scalar_times(rounded,FD_LISPFIX(divisor));
      fd_decref(rounded); fd_decref(invscale);
      return FD_MAKE_PAIR(multiplied,fd_incref(scale));}}
  else {
    fd_lisp bucket=scalar_round(v,scale);
    return FD_MAKE_PAIR(bucket,fd_incref(scale));}
}

/** Miscellaneous operations **/

static lisp float_divide(lisp a1,lisp a2)
{
  if ((FIXNUMP(a1)) && (FIXNUMP(a2)))
    return LISPFLOAT(((double)FIXLISP(a1))/((double)FIXLISP(a2)));
  else if ((FLONUMP(a1)) && (FLONUMP(a2)))
    return LISPFLOAT(FLOATLISP(a1)/FLOATLISP(a2));
  else if ((FIXNUMP(a1)) && (FLONUMP(a2)))
    return LISPFLOAT(FIXLISP(a1)/FLOATLISP(a2));
  else if ((FLONUMP(a1)) && (FIXNUMP(a2)))
    return LISPFLOAT(FLOATLISP(a1)/FIXLISP(a2));
  else fd_type_error(_("not a number"),a1);
}

static lisp lisp_rationalize_cproc(lisp x,lisp y)
{
  return make_rational(incref(x),incref(y));
}

static lisp lisp_plusone_cproc(lisp x)
{
  return complex_plus(x,LISPFIX(1));
}

static lisp lisp_minusone_cproc(lisp x)
{
  return complex_minus(x,LISPFIX(1));
}

/** Numerical i/o **/

static lisp lisp_number_to_string_lexpr(lisp args)
{
  int radix;
  lisp num=fd_get_arg(args,0,FD_VOID);
  lisp radish=fd_get_arg(args,1,LISPFIX(10));
  lisp answers=FD_EMPTY_CHOICE;
  if (!(NUMBERP(radish)))
    fd_type_error(_("radix not a number"),radish);
  else radix=fd_lisp2int(radish);
  if (!(NUMBERP(num)))
    fd_type_error(_("arg not a number"),num);
  else if (radix != 10)
    if (FIXNUMP(num)) {
      char buf[32]; lisp answer;
      if (radix == 8) {
	sprintf(buf,"%o",FIXLISP(num));
	answer=fd_make_string(buf);}
      else if (radix == 16) {
	sprintf(buf,"%x",FIXLISP(num));      
	answer=fd_make_string(buf);}
      else answer=(FD_EMPTY_CHOICE);
      ADD_TO_CHOICE(answers,answer);}
    else fd_raise_exception(_("No radix handling for non fixnums"));
  else {
    fd_u8char *string=fd_object_to_string(num);
    lisp answer=fd_copy_string(string); fd_xfree(string);
    ADD_TO_CHOICE(answers,answer);}
  return answers;
}

static lisp lisp_string_to_number_lexpr(lisp args)
{
  lisp strings=fd_get_arg(args,0,FD_VOID);
  lisp radishes=fd_get_arg(args,1,LISPFIX(10));
  lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(string,strings) {
    DO_CHOICES(radish,radishes) {
      int radix;
      if (!(NUMBERP(radish)))
	fd_type_error(_("radix not a number"),radish);
      else radix=fd_lisp2int(radish);
      if (!(STRINGP(string)))
	fd_type_error(_("not a string"),string);
      else if (STRING_LENGTH(string) == 0) {
	ADD_TO_CHOICE(answers,FD_FALSE);}
      else {
	lisp v=fd_parse_number(STRING_DATA(string),radix);
	if (NUMBERP(v)) {ADD_TO_CHOICE(answers,v);}
	else {
	  ADD_TO_CHOICE(answers,FD_FALSE);
	  decref(v);}}}
    END_DO_CHOICES;}
  END_DO_CHOICES;
  return answers;
}

static lisp lisp_inexact_to_string_lexpr(lisp args)
{
  char buf[256];
  lisp number=fd_get_arg(args,0,FD_VOID);
  lisp precision=fd_get_arg(args,1,FD_VOID);
  if (!((FD_FIXNUMP(precision)) &&
	(FD_FIXLISP(precision) > 0) &&
	(FD_FIXLISP(precision) < 12)))
    fd_type_error(_("precision not a fixnum in []"),precision);
  else {
    char fmt[24]; 
    double q=to_float(number);
    sprintf(fmt,"%%.%df",FD_FIXLISP(precision));
    sprintf(buf,fmt,q);
    return fd_make_string(buf);}
}

/** Generic ordering, sorting **/

static int compare_sets(lisp set1,lisp set2);
static int compare_slotmaps(lisp sm1,lisp sm2);

static int generic_gt(lisp x,lisp y)
{
  if ((CHOICEP(x)) || (CHOICEP(y)))
    if ((CHOICE_SIZE(x)) > (CHOICE_SIZE(y))) return 1;
    else if ((CHOICE_SIZE(x)) < (CHOICE_SIZE(y))) return -1;
    else return compare_sets(x,y);
  else if (NUMBERP(x))
    if (!(NUMBERP(y))) return -1;
    else if ((FIXNUMP(x)) && (FIXNUMP(y)))
      if ((x.data.fixnum) > (y.data.fixnum)) return 1;
      else if ((x.data.fixnum) < (y.data.fixnum)) return -1;
      else return 0;
    else if ((FLONUMP(x)) && (FLONUMP(y)))
      if ((FD_FLOATLISP(x)) > (FD_FLOATLISP(y))) return 1;
      else if ((FD_FLOATLISP(x)) < (FD_FLOATLISP(y))) return -1;
      else return 0;
    else if (equal(x,y)) return 0;
    else if (greater_than(x,y)) return 1;
    else return -1;
  else if (NUMBERP(y)) return 1;
  else if (PTR_TYPE(x) > PTR_TYPE(y)) return 1;
  else if (PTR_TYPE(x) < PTR_TYPE(y)) return -1;
  else {
    switch (PTR_TYPE(x)) {
    case character_type: case immediate_type: case fixnum_type:
      if ((x.data.fixnum) > (y.data.fixnum)) return 1;
      else if ((x.data.fixnum) < (y.data.fixnum)) return -1;
      else return 0;
    case string_type: case qstring_type: case packet_type:
      if (FD_STRING_LENGTH(x)==FD_STRING_LENGTH(y))
	if (FD_STRING_LENGTH(x)==0) return 0;
	else return memcmp(STRING_DATA(x),STRING_DATA(y),FD_STRING_LENGTH(x));
      else if (FD_STRING_LENGTH(x)>FD_STRING_LENGTH(y)) return 1;
      else return -1;
    case object_type:
      return FD_COMPARE_OIDS(OID_ADDR(x),OID_ADDR(y));
    case symbol_type:
      return strcmp(SYMBOL_NAME(x),SYMBOL_NAME(y));
    case pair_type: {
      lisp xcar=CAR(x), ycar=CAR(y);
      int comparison=generic_gt(xcar,ycar);
      if (comparison == 0) {
	lisp xcdr=CDR(x), ycdr=CDR(y);
	return generic_gt(xcdr,ycdr);}
      else return comparison;}
    case vector_type:
      if (VECTOR_LENGTH(x) > (VECTOR_LENGTH(y))) return 1;
      else if (VECTOR_LENGTH(x) < (VECTOR_LENGTH(y))) return -1;
      else {
	int i=0, limit=VECTOR_LENGTH(x);
	while (i < limit) {
	  lisp xelt=VECTOR_REF(x,i); 
	  lisp yelt=VECTOR_REF(y,i);
	  int comparison=generic_gt(xelt,yelt);
	  if (comparison != 0) return comparison;
	  else i++;}
	return 0;}
    case slotmap_type:
      return compare_slotmaps(x,y);
    case lrecord_type:
      return generic_gt(LRECORD_DATA(x),LRECORD_DATA(y));
    case record_type:
      if (((long) (CPTR_DATA(x))) > ((long) (CPTR_DATA(y)))) return 1;
      else if (((long) (CPTR_DATA(x))) < ((long) (CPTR_DATA(y))))
	return -1;
      else return 0;
    case rational_type: case complex_type:
      return 0; /* Should never be reached */
    default: fd_raise_exception("G>: big problems");}}
}

typedef struct GSORT_ENTRY {lisp key; lisp obj;} *gsort_entry;

static int compare_gsort_entries(const void *a,const void *b)
{
  return generic_gt(((gsort_entry)a)->key,((gsort_entry)b)->key);
}

static lisp lisp_any_gt_cproc(lisp x,lisp y)
{
  if (generic_gt(x,y) > 0) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_any_lt_cproc(lisp x,lisp y)
{
  if (generic_gt(x,y) < 0) return FD_TRUE;
  else return FD_FALSE;
}

static int lisp_order(const void *x,const void *y)
{
  return generic_gt(*((lisp *)x),*((lisp *)y));
}

static int compare_sets(lisp set1,lisp set2)
{
  int i=0, size=CHOICE_SIZE(set1);
  lisp *setv1=fd_malloc(sizeof(lisp)*size);
  lisp *setv2=fd_malloc(sizeof(lisp)*size);
  {int i=0; DO_CHOICES(x,set1) {setv1[i]=x; i++;} END_DO_CHOICES;}
  {int i=0; DO_CHOICES(x,set2) {setv2[i]=x; i++;} END_DO_CHOICES;}
  qsort(setv1,size,sizeof(lisp),lisp_order);
  qsort(setv2,size,sizeof(lisp),lisp_order);
  i=0; while (i < size) {
    int comparison=generic_gt(setv1[i],setv2[i]);
    if (comparison != 0) {
      fd_free(setv1,sizeof(lisp)*size);
      fd_free(setv2,sizeof(lisp)*size);
      return comparison;}
    else i++;}
  fd_free(setv1,sizeof(lisp)*size);
  fd_free(setv2,sizeof(lisp)*size);
  return 0;
}

static int slotmap_order(const void *v1,const void *v2)
{
  fd_lisp *lv1=(fd_lisp *)v1, *lv2=(fd_lisp *)v2;
  return generic_gt(lv1[0],lv2[0]);
}

static int compare_slotmaps(fd_lisp x,fd_lisp y)
{
  if (SLOTMAP_SIZE(x) > (SLOTMAP_SIZE(y))) return 1;
  else if (SLOTMAP_SIZE(x) < (SLOTMAP_SIZE(y))) return -1;
  else {
    int xsize, ysize;
    fd_lisp *xslots, *yslots, *xscan, *xlimit, *yscan;
    xsize=_fd_slotmap_data(x,(void **)&xslots);
    ysize=_fd_slotmap_data(y,(void **)&yslots);
    if (xsize != ysize) {
      _fd_done_with_slotmap_data(xslots,xsize);
      _fd_done_with_slotmap_data(yslots,ysize); 
      if (xsize > ysize) return 1; else return -1;}
    qsort(xslots,xsize/2,sizeof(fd_lisp)*2,slotmap_order);
    qsort(yslots,ysize/2,sizeof(fd_lisp)*2,slotmap_order);
    xscan=xslots; xlimit=xscan+xsize; yscan=yslots;
    while (xscan < xlimit) {
      int comparison=generic_gt(*xscan,*yscan);
      if (comparison) {
	_fd_done_with_slotmap_data(xslots,xsize);
	_fd_done_with_slotmap_data(yslots,ysize); 
	return comparison;}
      else xscan++; yscan++;}
    _fd_done_with_slotmap_data(xslots,xsize);
    _fd_done_with_slotmap_data(yslots,ysize); 
    return 0;}
}

static lisp keyget(lisp v,lisp key)
{
  if (FD_FALSEP(key)) return incref(v);
  else if (FD_PRIM_TYPEP(key,hashtable_type)) {
    fd_hashtable h=FD_CPTR_DATA(key);
    return fd_hashtable_get(h,v,FD_EMPTY_CHOICE);}
  else if ((FD_SYMBOLP(key)) || (FD_OIDP(key))) 
    return fd_frame_get(v,key);
  else if ((FD_PRIM_TYPEP(key,cproc_type)) ||
	   (FD_PRIM_TYPEP(key,rproc_type)) ||
	   (FD_XPROCP(key))) {
    lisp arg=FD_MAKE_LIST1(incref(v)), result;
    result=fd_apply(key,arg); decref(arg);
    return result;}
  else if (FD_VECTORP(key)) {
    fd_lisp kvec=fd_make_vector(FD_VECTOR_LENGTH(key));
    int i=0, lim=FD_VECTOR_LENGTH(key); while (i < lim) {
      FD_VECTOR_SET(kvec,i,keyget(v,FD_VECTOR_REF(key,i))); i++;}
    return kvec;}
  else fd_type_error(_("Invalid key function"),key);
}

static lisp dosort(lisp set,lisp proc)
{
  int i=0, limit=CHOICE_SIZE(set);
  lisp vector=fd_make_vector(limit);
  struct GSORT_ENTRY *array=fd_malloc(sizeof(struct GSORT_ENTRY)*limit);
  DO_CHOICES(r,set) {
    fd_lisp key=keyget(r,proc);
    array[i].key=key; array[i].obj=incref(r); i++;}
  END_DO_CHOICES;
  qsort((void *)array,limit,sizeof(struct GSORT_ENTRY),compare_gsort_entries);
  i=0; while (i < limit) {
    decref(array[i].key);
    FD_VECTOR_SET(vector,i,array[i].obj);
    i++;}
  fd_free(array,sizeof(struct GSORT_ENTRY)*limit);
  return vector;
}

static lisp lisp_smallest_by_lexpr(fd_lisp args)
{
  
  fd_lisp among=fd_get_arg(args,0,FD_VOID);
  fd_lisp keyfn=fd_get_arg(args,1,FD_FALSE);
  fd_lisp smallest=FD_EMPTY_CHOICE, min=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(each,among) {
    fd_lisp keyval=keyget(each,keyfn);
    if (FD_EMPTYP(min)) {
      min=keyval; smallest=fd_incref(each);}
    else {
      int comparison=generic_gt(keyval,min);
      if (comparison > 0) fd_decref(keyval);
      else if (comparison < 0) {
	fd_decref(smallest); fd_decref(min);
	min=keyval; smallest=fd_incref(each);}
      else {
	FD_ADD_TO_CHOICE(smallest,fd_incref(each));
	fd_decref(keyval);}}}
  FD_END_DO_CHOICES;
  fd_decref(min);
  return smallest;
}

static lisp lisp_largest_by_lexpr(fd_lisp args)
{
  
  fd_lisp among=fd_get_arg(args,0,FD_VOID);
  fd_lisp keyfn=fd_get_arg(args,1,FD_FALSE);
  fd_lisp smallest=FD_EMPTY_CHOICE, min=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(each,among) {
    fd_lisp keyval=keyget(each,keyfn);
    if (FD_EMPTYP(min)) {
      min=keyval; smallest=fd_incref(each);}
    else {
      int comparison=generic_gt(keyval,min);
      if (comparison < 0) fd_decref(keyval);
      else if (comparison > 0) {
	fd_decref(smallest); fd_decref(min);
	min=keyval; smallest=fd_incref(each);}
      else {
	FD_ADD_TO_CHOICE(smallest,fd_incref(each));
	fd_decref(keyval);}}}
  FD_END_DO_CHOICES;
  fd_decref(min);
  return smallest;
}

static lisp lisp_sortby_lexpr(lisp args)
{
  return dosort(fd_get_arg(args,1,FD_VOID),
		fd_get_arg(args,0,FD_VOID));
}
static lisp lisp_sorted_lexpr(lisp args)
{
  return dosort(fd_get_arg(args,0,FD_VOID),
		fd_get_arg(args,1,FD_FALSE));
}

/** Logical ops on fixnums **/

static lisp lisp_fixior_cproc(lisp x,lisp y)
{
  int ix, iy;
  if (FIXNUMP(x)) ix=FIXLISP(x);
  else fd_type_error(_("Not a fixnum"),x);
  if (FIXNUMP(x)) iy=FIXLISP(y);
  else fd_type_error(_("Not a fixnum"),y);
  return FD_LISPFIX(ix|iy);
}

static lisp lisp_fixand_cproc(lisp x,lisp y)
{
  int ix, iy;
  if (FIXNUMP(x)) ix=FIXLISP(x);
  else fd_type_error(_("Not a fixnum"),x);
  if (FIXNUMP(x)) iy=FIXLISP(y);
  else fd_type_error(_("Not a fixnum"),y);
  return FD_LISPFIX(ix&iy);
}

static lisp lisp_fixxor_cproc(lisp x,lisp y)
{
  int ix, iy;
  if (FIXNUMP(x)) ix=FIXLISP(x);
  else fd_type_error(_("Not a fixnum"),x);
  if (FIXNUMP(x)) iy=FIXLISP(y);
  else fd_type_error(_("Not a fixnum"),y);
  return FD_LISPFIX(ix^iy);
}

/** Initialization **/

static void initialize_bignums()
{
#if (USE_BIGNUMS)
  struct FD_TYPE_REGISTRY *r=fd_register_typecode((bigint_type));
  r->package_code=dt_extended_numeric; r->subcode=dt_small_bignum;
  r->print_fcn=print_bignum; r->gc_fcn=free_bignum;
  r->package_data_fcn=bignum_data; /* r->hash_fcn=hash_bignum; */
  r->package_data_done_fcn=done_with_bignum_data;
  r->compare_fcn=compare_bignums; r->copy_fcn=copy_bignum;
  r->package_restore_fcn=init_bignum;
  fd_set_bignum_parser(bignum_parser);
  big_int_max=long_to_bignum(INT_MAX);
  big_int_min=long_to_bignum(INT_MIN);
#endif
}

void fd_initialize_arith_c()
{
  initialize_bignums();
  fd_add_lexpr(NULL,"+",FD_ND_LEXPR,lisp_generic_plus_lexpr);
  fd_add_lexpr(NULL,"-",FD_ND_LEXPR,lisp_generic_minus_lexpr);
  fd_add_lexpr(NULL,"*",FD_ND_LEXPR,lisp_generic_times_lexpr);
  fd_add_lexpr(NULL,"/",FD_ND_LEXPR,lisp_generic_div_lexpr);
  fd_add_lexpr(NULL,"MIN",FD_ND_LEXPR,lisp_generic_min_lexpr);
  fd_add_lexpr(NULL,"MAX",FD_ND_LEXPR,lisp_generic_max_lexpr);

  fd_add_cproc(NULL,"QUOTIENT",2,int_quotient);
  fd_add_cproc(NULL,"REMAINDER",2,int_remainder);
  fd_add_cproc(NULL,"MODULO",2,lisp_modulo_cproc);
  fd_add_cproc(NULL,"FLOAT/",2,float_divide);

  fd_add_cproc(NULL,"ABS",1,lisp_magnitude_cproc);
  fd_add_cproc(NULL,"NUMERATOR",1,lisp_numerator_cproc);
  fd_add_cproc(NULL,"DENOMINATOR",1,lisp_denominator_cproc);
  fd_add_cproc(NULL,"REAL-PART",1,lisp_realpart_cproc);
  fd_add_cproc(NULL,"IMAG-PART",1,lisp_imagpart_cproc);
  fd_add_cproc(NULL,"MAGNITUDE",1,lisp_magnitude_cproc);
  fd_add_cproc(NULL,"MAKE-RECTANGULAR",2,lisp_make_rectangular_cproc);
  fd_add_cproc(NULL,"RATIONALIZE",2,lisp_rationalize_cproc);

  fd_add_cproc(NULL,"FLOOR",1,lisp_floor_cproc);

  fd_add_lexpr(NULL,"=",FD_ND_LEXPR,lisp_eqv_lexpr);
  fd_add_lexpr(NULL,">",FD_ND_LEXPR,lisp_gt_lexpr);
  fd_add_lexpr(NULL,">=",FD_ND_LEXPR,lisp_gte_lexpr);
  fd_add_lexpr(NULL,"=>",FD_ND_LEXPR,lisp_gte_lexpr);
  fd_add_lexpr(NULL,"<",FD_ND_LEXPR,lisp_lt_lexpr);
  fd_add_lexpr(NULL,"<=",FD_ND_LEXPR,lisp_lte_lexpr);
  fd_add_lexpr(NULL,"=<",FD_ND_LEXPR,lisp_lte_lexpr);
  fd_add_lexpr(NULL,"EQV?",FD_ND_LEXPR,lisp_eqv_lexpr);
  /* Note that these override the definitions in mini.c */
  fd_add_cproc(NULL,"EQUAL?",2,lisp_equal_cproc);
  fd_add_cproc(NULL,"EQUAL",2,lisp_equal_cproc);

  fd_add_cproc(NULL,"ZERO?",1,lisp_zerop_cproc);
  fd_add_cproc(NULL,"NUMBER?",1,lisp_numberp_cproc);
  fd_add_cproc(NULL,"NEGATIVE?",1,lisp_negativep_cproc);
  fd_add_cproc(NULL,"POSITIVE?",1,lisp_positivep_cproc);
  fd_add_cproc(NULL,"EXACT?",1,lisp_exactp_cproc);
  fd_add_cproc(NULL,"INEXACT?",1,lisp_inexactp_cproc);
  fd_add_cproc(NULL,"INTEGER?",1,lisp_integerp_cproc);
  fd_add_cproc(NULL,"RATIONAL?",1,lisp_rationalp_cproc);
  fd_add_cproc(NULL,"COMPLEX?",1,lisp_complexp_cproc);
  fd_add_cproc(NULL,"REAL?",1,lisp_realp_cproc);
  fd_add_cproc(NULL,"ODD?",1,lisp_oddp_cproc);
  fd_add_cproc(NULL,"EVEN?",1,lisp_evenp_cproc);

  fd_add_cproc(NULL,"INEXACT->EXACT",1,lisp_to_exact_cproc);
  fd_add_cproc(NULL,"EXACT->INEXACT",1,lisp_to_inexact_cproc);

  fd_add_cproc(NULL,"ILOG",1,lisp_ilog_cproc);

  fd_add_cproc(NULL,"LOG",1,lisp_log_cproc);
  fd_add_cproc(NULL,"EXP",1,lisp_exp_cproc);
  fd_add_cproc(NULL,"SIN",1,lisp_sin_cproc);
  fd_add_cproc(NULL,"COS",1,lisp_cos_cproc);
  fd_add_cproc(NULL,"TAN",1,lisp_tan_cproc);
  fd_add_cproc(NULL,"ASIN",1,lisp_asin_cproc);
  fd_add_cproc(NULL,"ACOS",1,lisp_acos_cproc);
  fd_add_cproc(NULL,"ATAN",1,lisp_atan_cproc);
  fd_add_cproc(NULL,"ATAN2",2,lisp_atan2_cproc);
  fd_add_cproc(NULL,"SQRT",1,lisp_sqrt_cproc);

  fd_add_cproc(NULL,"EXPT",2,lisp_expt_cproc);

  fd_add_lexpr(NULL,"GCD",FD_NORMAL_LEXPR,lisp_nary_gcd_lexpr);
  fd_add_lexpr(NULL,"LCM",FD_NORMAL_LEXPR,lisp_nary_lcm_lexpr);
  fd_add_cproc(NULL,"FLOOR",1,lisp_floor_cproc);
  fd_add_cproc(NULL,"CEILING",1,lisp_ceiling_cproc);
  fd_add_cproc(NULL,"TRUNCATE",1,lisp_truncate_cproc);
  fd_add_lexpr(NULL,"ROUND",FD_NORMAL_LEXPR,lisp_round_lexpr);
  fd_add_cproc(NULL,"SCALEREP",2,lisp_scalerep_cproc);

  fd_add_cproc(NULL,"1+",1,lisp_plusone_cproc);
  fd_add_cproc(NULL,"-1+",1,lisp_minusone_cproc);
  fd_add_alias(NULL,"1-","-1+");

  fd_add_cproc(NULL,"RANDOM",1,lisp_random_cproc);
  fd_add_cproc(NULL,"RANDOM-SEED",1,lisp_random_seed_cproc);
  fd_add_cproc(NULL,"SET-RANDOM-SEED!",1,lisp_set_random_seed_cproc);
  fd_add_lexpr(NULL,"RANDOMIZE!",FD_ND_LEXPR,lisp_randomize_lexpr);

  fd_add_lexpr(NULL,"NUMBER->STRING",FD_NORMAL_LEXPR,lisp_number_to_string_lexpr);
  fd_add_lexpr(NULL,"STRING->NUMBER",FD_NORMAL_LEXPR,lisp_string_to_number_lexpr);
  fd_add_lexpr(NULL,"INEXACT->STRING",FD_NORMAL_LEXPR,lisp_inexact_to_string_lexpr);

  fd_add_lexpr(NULL,"SMALLEST",FD_ND_LEXPR,lisp_smallest_by_lexpr);
  fd_add_lexpr(NULL,"LARGEST",FD_ND_LEXPR,lisp_largest_by_lexpr);

  fd_add_cproc(NULL,"ANY>?",2,lisp_any_gt_cproc);
  fd_add_cproc(NULL,"ANY<?",2,lisp_any_lt_cproc);
  fd_add_lexpr(NULL,"SORTBY",FD_ND_LEXPR,lisp_sortby_lexpr);
  fd_add_lexpr(NULL,"SORTED",FD_ND_LEXPR,lisp_sorted_lexpr);

  fd_add_cproc(NULL,"%LOGIOR",2,lisp_fixior_cproc);
  fd_add_cproc(NULL,"%LOGAND",2,lisp_fixand_cproc);
  fd_add_cproc(NULL,"%LOGXOR",2,lisp_fixxor_cproc);

  fd_add_cproc(NULL,"ANY<?",2,lisp_any_lt_cproc);

  fd_register_source_file("arith",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: arith.c,v $
   Revision 1.23  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.22  2004/09/15 17:11:40  haase
   Fixed bug in possible mystery (bignum) hashing

   Revision 1.21  2004/09/15 12:38:06  haase
   Fixed some errors in generic sorting for packets, strings, etc

   Revision 1.20  2004/09/08 18:35:27  haase
   Made bignum hashing defer to packet hashes

   Revision 1.19  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.18  2004/06/22 16:17:58  haase
   Fix rounding with rationals

   Revision 1.17  2004/06/18 17:38:11  haase
   Fixed rounding problem

   Revision 1.16  2004/06/17 15:56:36  haase
   Added additional factor argument to ROUND and added SCALEREP procedure for scaled representations of numbers

   Revision 1.15  2004/06/08 19:53:40  haase
   Fix to new definition of atan2

   Revision 1.14  2004/06/08 15:54:50  haase
   added atan2

   Revision 1.13  2004/05/10 15:55:04  haase
   Added real hash function for bignums

   Revision 1.12  2004/03/03 18:49:29  haase
   Allowed sort keys to be composed into vectors of sort keys

   Revision 1.11  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.10  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.9.2.1  2003/01/26 20:56:06  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.9  2002/06/18 17:33:47  haase
   Made smallest/largest work on arbitrary objects and take an optional sort key argument

   Revision 1.8  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.7  2002/04/03 18:16:39  haase
   Added READ-4BYTES primitive

   Revision 1.6  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
