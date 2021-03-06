/* -*-C-*-

$Id: bignmint.h,v 1.1.1.1 2001/10/29 20:49:17 haase Exp $

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

/* Internal Interface to Bignum Code */

#undef BIGNUM_ZERO_P
#undef BIGNUM_NEGATIVE_P

/* The memory model is based on the following definitions, and on the
   definition of the type `bignum_type'.  The only other special
   definition is `CHAR_BIT', which is defined in the Ansi C header
   file "limits.h". */

/*MIT Scheme used longs for these.  FRAMERD likes 4byte chunks.*/
typedef int bignum_digit_type;
typedef int bignum_length_type;

#ifdef MIT_SCHEME

/* BIGNUM_ALLOCATE allocates a (length + 1)-element array of
   `bignum_digit_type'; deallocation is the responsibility of the
   user (in Scheme, the garbage collector handles this). */
#define BIGNUM_ALLOCATE(length)						\
  (allocate_non_marked_vector						\
   (TC_BIG_FIXNUM, (BIGNUM_LENGTH_TO_GC_LENGTH (length)), 1))

/* BIGNUM_TO_POINTER casts a bignum object to a digit array pointer. */
#define BIGNUM_TO_POINTER(bignum)					\
  ((bignum_digit_type *) (VECTOR_LOC ((bignum), 0)))

/* BIGNUM_REDUCE_LENGTH allows the memory system to reclaim some
   space when a bignum's length is reduced from its original value. */
#define BIGNUM_REDUCE_LENGTH(target, source, length)			\
{									\
  SET_VECTOR_LENGTH ((source), (BIGNUM_LENGTH_TO_GC_LENGTH (length)));	\
  (target) = (source);							\
}

#define BIGNUM_LENGTH_TO_GC_LENGTH(length)				\
  (BYTES_TO_WORDS (((length) + 1) * (sizeof (bignum_digit_type))))

/* BIGNUM_DEALLOCATE is called when disposing of bignums which are
   created as intermediate temporaries; Scheme doesn't need this. */
#define BIGNUM_DEALLOCATE(bignum)

/* If BIGNUM_FORCE_NEW_RESULTS is defined, all bignum-valued operations
   return freshly-allocated results.  This is useful for some kinds of
   memory deallocation strategies. */
#define BIGNUM_FORCE_NEW_RESULTS

/* BIGNUM_EXCEPTION is invoked to handle assertion violations. */
#define BIGNUM_EXCEPTION error_external_return

#else /* not MIT_SCHEME */

#define BIGNUM_ALLOCATE bignum_malloc
#define BIGNUM_TO_POINTER(bignum) ((bignum_digit_type *) (bignum))
#define BIGNUM_REDUCE_LENGTH(target, source, length)			\
  (target) = (bignum_realloc ((source), (length)))
#define BIGNUM_DEALLOCATE free
#define BIGNUM_FORCE_NEW_RESULTS
#define BIGNUM_EXCEPTION abort
#define fast register
extern void free ();
extern void abort ();

#endif /* MIT_SCHEME */

#define BIGNUM_DIGIT_LENGTH (((sizeof (bignum_digit_type)) * CHAR_BIT) - 2)
#define BIGNUM_HALF_DIGIT_LENGTH (BIGNUM_DIGIT_LENGTH / 2)
#define BIGNUM_RADIX (((unsigned long) 1) << BIGNUM_DIGIT_LENGTH)
#define BIGNUM_RADIX_ROOT (((unsigned long) 1) << BIGNUM_HALF_DIGIT_LENGTH)
#define BIGNUM_DIGIT_MASK	 (BIGNUM_RADIX - 1)
#define BIGNUM_HALF_DIGIT_MASK	 (BIGNUM_RADIX_ROOT - 1)

#define BIGNUM_START_PTR(bignum)					\
  ((BIGNUM_TO_POINTER (bignum)) + 1)

#define BIGNUM_SET_HEADER(bignum, length, negative_p)			\
  (* (BIGNUM_TO_POINTER (bignum))) =					\
    ((length) | ((negative_p) ? BIGNUM_RADIX : 0))

#define BIGNUM_LENGTH(bignum)						\
  ((bignum_length_type)((* (BIGNUM_TO_POINTER (bignum))) & BIGNUM_DIGIT_MASK))

#define BIGNUM_NEGATIVE_P(bignum)					\
  (((* (BIGNUM_TO_POINTER (bignum))) & BIGNUM_RADIX) != 0)

#define BIGNUM_ZERO_P(bignum)						\
  ((BIGNUM_LENGTH (bignum)) == 0)

#define BIGNUM_REF(bignum, index)					\
  (* ((BIGNUM_START_PTR (bignum)) + (index)))

#ifdef BIGNUM_FORCE_NEW_RESULTS
#define BIGNUM_MAYBE_COPY bignum_copy
#else
#define BIGNUM_MAYBE_COPY(bignum) bignum
#endif

/* These definitions are here to facilitate caching of the constants
   0, 1, and -1. */
#define BIGNUM_ZERO bignum_make_zero
#define BIGNUM_ONE bignum_make_one

#define HD_LOW(digit) ((digit) & BIGNUM_HALF_DIGIT_MASK)
#define HD_HIGH(digit) ((digit) >> BIGNUM_HALF_DIGIT_LENGTH)
#define HD_CONS(high, low) (((high) << BIGNUM_HALF_DIGIT_LENGTH) | (low))

#define BIGNUM_BITS_TO_DIGITS(n)					\
  (((n) + (BIGNUM_DIGIT_LENGTH - 1)) / BIGNUM_DIGIT_LENGTH)

#define BIGNUM_DIGITS_FOR_LONG						\
  (BIGNUM_BITS_TO_DIGITS ((sizeof (long)) * CHAR_BIT))

#ifndef BIGNUM_DISABLE_ASSERTION_CHECKS

#define BIGNUM_ASSERT(expression)					\
{									\
  if (! (expression))							\
    BIGNUM_EXCEPTION ();						\
}

#endif /* not BIGNUM_DISABLE_ASSERTION_CHECKS */
