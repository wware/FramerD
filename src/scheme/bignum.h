/* -*-C-*-

$Id: bignum.h,v 1.1.1.1 2001/10/29 20:49:21 haase Exp $

Copyright (c) 1989-1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

#ifndef _BIGNUMH_IS_LOADED
#define _BIGNUMH_IS_LOADED

/* External Interface to Bignum Code */

/* The `unsigned long' type is used for the conversion procedures
   `bignum_to_long' and `long_to_bignum'.  Older implementations of C
   don't support this type; if you have such an implementation you can
   disable these procedures using the following flag (alternatively
   you could write alternate versions that don't require this type). */
/* #define BIGNUM_NO_ULONG */

#include "ansidecl.h"

#ifdef MIT_SCHEME

typedef SCHEME_OBJECT bignum_type;
#define BIGNUM_OUT_OF_BAND SHARP_F

#else

/* In MIT Scheme, was typedef long * bignum_type; */

/*FRAMERD likes 4 byte chunks:*/
typedef int * bignum_type;

#define BIGNUM_OUT_OF_BAND ((bignum_type) 0)

#endif

enum bignum_comparison
{
  bignum_comparison_equal, bignum_comparison_less, bignum_comparison_greater
};

typedef PTR bignum_procedure_context;
extern bignum_type EXFUN (bignum_make_zero, (void));
extern bignum_type EXFUN (bignum_make_one, (int negative_p));
extern int EXFUN (bignum_equal_p, (bignum_type, bignum_type));
extern enum bignum_comparison EXFUN (bignum_test, (bignum_type));
extern enum bignum_comparison EXFUN (bignum_compare,
				     (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_add, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_subtract, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_negate, (bignum_type));
extern bignum_type EXFUN (bignum_multiply, (bignum_type, bignum_type));
extern int EXFUN (bignum_divide,
		  (bignum_type numerator, bignum_type denominator,
		   bignum_type * quotient, bignum_type * remainder));
extern bignum_type EXFUN (bignum_quotient, (bignum_type, bignum_type));
extern bignum_type EXFUN (bignum_remainder, (bignum_type, bignum_type));
#ifndef BIGNUM_NO_ULONG
extern bignum_type EXFUN (long_to_bignum, (long));
extern bignum_type EXFUN (ulong_to_bignum, (unsigned long));
extern long EXFUN (bignum_to_long, (bignum_type));
extern unsigned long EXFUN (bignum_to_ulong, (bignum_type));
#endif /* not BIGNUM_NO_ULONG */
extern bignum_type EXFUN (double_to_bignum, (double));
extern double EXFUN (bignum_to_double, (bignum_type));
extern int EXFUN (bignum_fits_in_word_p,
		  (bignum_type, long word_length, int twos_complement_p));
extern unsigned long EXFUN (bignum_length_in_bytes, (bignum_type));
extern bignum_type EXFUN (bignum_length_in_bits, (bignum_type));
extern bignum_type EXFUN (bignum_length_upper_limit, (void));
extern bignum_type EXFUN (digit_stream_to_bignum,
			  (unsigned int n_digits,
			   unsigned int EXFUN ((*producer), ()),
			   bignum_procedure_context context,
			   unsigned int radix, int negative_p));
extern void EXFUN (bignum_to_digit_stream,
		   (bignum_type, unsigned int radix,
		    void EXFUN((*consumer), ()),
		    bignum_procedure_context context));
extern long EXFUN (bignum_max_digit_stream_radix, (void));


#endif /*_BIGNUMH_IS_LOADED */
