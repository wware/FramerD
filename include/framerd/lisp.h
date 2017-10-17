/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: lisp.h,v 1.17 2007/06/30 16:21:06 haase Exp $

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

  This is the file lisp.h supporting LISP operations from C.

*************************************************************************/

#ifndef FRAMERD_LISP_H /* If defined, skip the file */
#define FRAMERD_LISP_H

/* Lisp objects */

/* A lisp pointer is a struct consisting of a type field and a data field.
   The type is of enum FD_PTR_TYPE and the data is of union FD_PTR_DATA */

/* Type codes less than FD_ATOMIC_LIMIT denote atoms for which EQ? implies
     EQUAL? (e.g. you can just compare the data portion of the pointers)
   Type codes less than FD_SMALL_TYPE_LIMIT only use the lower 4 bytes of the
     data portion (the others may be random, so you need to ignore them) */
#define FD_ATOMIC_LIMIT 8
#define FD_ATOMICP(x) (FD_PTR_TYPE(x) < FD_ATOMIC_LIMIT)
#define FD_GC_LIMIT 7
#define FD_SMALL_TYPE_LIMIT 4
#define FD_SMALL_TYPEP(x) (FD_PTR_TYPE(x) < FD_SMALL_TYPE_LIMIT)

/* These are codes for the the basic types; those with codes < FD_ATOMIC_LIMIT
    are "atomic": == on their data is the same as EQUAL */
typedef enum FD_PTR_TYPE {
  bad_type=0,
  /* Atomic types which are also small */
  fixnum_type=1, immediate_type=2, character_type=3, 
  /* Atomic types which use the whole data section */
  symbol_type=4, object_type=5, 
  zstring_type=6,
  /* Types beyond this are consed and need to be GC'd */
  qstring_type=7, 
  /* Non-atomic types which may need to be gc'd and have refcounts */
  string_type=8, pair_type=9, vector_type=10, slotmap_type=11,
  bigint_type=12, rational_type=13, complex_type=14,
  record_type=16, lrecord_type=17,
  /* Various choice types indicating properties of the choice's elements */
  choice_type=18, proper_choice_type=19, quoted_choice_type=20,
  /* Evaluator types */
  tail_call_type=21, cproc_type=22, sproc_type=23, ssproc_type=24, 
  continuation_type=25, gproc_type=26, rproc_type=27, multiple_value_type=28,
  mutex_type=29, delay_type=30, env_type=31,
  /* FramerD types */
  dtype_server_type=32, pool_type=33, index_type=34,
  hashtable_type=35, hashset_type=36,
  input_file_type=37, output_file_type=38, io_file_type=39,
  input_string_type=40, output_string_type=41, 
  packet_type=42, mystery_type=43,
  segmented_stream_type=44, record_stream_type=45, tx_closure_type=46,
  flonum_type=47, int_vector_type=48, short_vector_type=49,
  double_vector_type=50, float_vector_type=51
 } fd_lisp_type;

/* What sits in the data portion of a pointer */
typedef union FD_DATA {
  long fixnum;
  int any_small;
  struct FD_DOUBLE *fdouble;
  struct FD_PAIR *pair;
  struct FD_STRING *string;
  struct FD_CHOICE *choice;
  struct FD_SYMBOL *symbol;
#if (FD_LIGHTWEIGHT_OIDS)
  unsigned long oidaddr;
#else
  struct FD_CONSOID *oid;
#endif
  struct FD_LRECORD *lrecord;
  struct FD_RECORD *record;
  struct FD_VECTOR *vector;
  struct FD_INT_VECTOR *ivector;
  struct FD_SHORT_VECTOR *svector;
  struct FD_FLOAT_VECTOR *fvector;
  struct FD_DOUBLE_VECTOR *dvector;
  struct FD_RPROC *rproc;
  struct FD_SPROC *sproc;
  struct FD_CPROC  *cproc;
  struct FD_CPTR *cptr;
  struct FD_SLOTMAP  *slotmap;
  struct FD_SSPROC *ssproc;
  struct FD_COMPLEX_NUMBER *complex_number;
  struct FD_RATIONAL_NUMBER *rational_number;
  struct FD_ACONS *acons;
  void *any;}
 fd_data;

/* A lisp pointer */
typedef struct FD_LISP_PTR {
  enum FD_PTR_TYPE type; union FD_DATA data;} fd_lisp;
struct FD_FAT_PTR { int type; union FD_DATA data; };

/* Making tagged pointers from untagged ones, checking tags, etc */
#define FD_PTR_TYPE(x) ((x).type)
#define FD_NPTR_TYPE(x) ((int)(FD_PTR_TYPE(x))) /* Stands for "numeric pointer type" */
#define FD_PRIM_TYPEP(x,prim_type) ((FD_NPTR_TYPE(x)) == prim_type)
#define FD_PTYPEP(x,prim_type) (FD_PRIM_TYPEP(x,prim_type))
#define FD_CHECK_PTR(x) (((x).type) ? (x) : (fd_raise_exception(fd_BadType),x))
#define FD_CHECK_LISP(x) (FD_CHECK_PTR(x))
/*  struct FD_FAT_PTR *l=(struct FD_FAT_PTR *)&(_result); l->type=0; / */
#if HAVE_CONSTRUCTOR_EXPRESSIONS
#define FD_RETURN_LISP_IMMEDIATE(tp,fld,d) \
   return (fd_lisp) { type: tp, data: { fld: d } };
#define FD_RETURN_LISP(tp,fld,d) \
   return (fd_lisp) { type: tp, data: { fld: d } };
#else
#define FD_RETURN_LISP_IMMEDIATE(tp,fld,d) \
   fd_lisp _result; \
   FD_SET_PRIM_TYPE(_result,tp); _result.data.fld=d; return _result;
#define FD_RETURN_LISP(tp,fld,d) \
   fd_lisp _result; \
   FD_SET_PRIM_TYPE(_result,tp); _result.data.fld=d; return _result;
#endif
#define FD_PTR_DATA(x,fld) ((x).data.fld)
#define FD_SET_PTR_DATA(x,fld,v) (x).data.fld=v

/* This forces the high bits of the type field to be zero'd, which may be
    neccessary for some memory checkers which complain that the field
    is uninitialized even though all of the useful bits are. */
#ifdef ZERO_HIGH_TYPE
#define FD_SET_PRIM_TYPE(x,prim_type) \
  ((struct FD_FAT_PTR)x).type=0; x.type=prim_type;
#else
#define FD_SET_PRIM_TYPE(x,prim_type) x.type=prim_type
#endif

/* Comparing pointers */

#if (SIZE_OF_VOID_P > 4)
#define FD_LISP_EQ(x,y) \
  ((FD_PTR_TYPE(x) == FD_PTR_TYPE(y)) && \
   (((FD_NPTR_TYPE(x)) < FD_SMALL_TYPE_LIMIT) ? ((x).data.fixnum == (y).data.fixnum) : \
    ((x).data.any == (y).data.any)))
#define FD_LISP_EQUAL(k1,k2) \
  ((FD_NPTR_TYPE(k1) == FD_NPTR_TYPE(k2)) ?                                      \
   (((FD_NPTR_TYPE(k1) < FD_SMALL_TYPE_LIMIT) ?                                  \
     ((k1).data.fixnum == (k2).data.fixnum) :                                    \
     ((k1).data.any == (k2).data.any)) ||                                        \
    ((FD_NPTR_TYPE(k1) >= FD_ATOMIC_LIMIT) && (fd_lisp_equal(k1,k2)))) :         \
   /* We handle these specially because they're the cases where different base   \
      types can still be equal.  And while we're special-casing it for strings,  \
      we just make the comparison direct rather than going through fd_lisp_equal.\
   */                                                                            \
   ((FD_PRIM_TYPEP(k1,qstring_type) || (FD_PRIM_TYPEP(k1,string_type))) &&       \
    (FD_PRIM_TYPEP(k2,qstring_type) || (FD_PRIM_TYPEP(k2,string_type))) &&       \
    (FD_STRING_LENGTH(k1) == FD_STRING_LENGTH(k2)) &&                            \
    (strcmp(FD_STRING_DATA(k1),FD_STRING_DATA(k2)) == 0)) ||                     \
   ((FD_PRIM_TYPEP(k1,proper_choice_type) || (FD_PRIM_TYPEP(k1,choice_type))) && \
    (FD_PRIM_TYPEP(k2,proper_choice_type) || (FD_PRIM_TYPEP(k2,choice_type))) && \
    (fd_lisp_equal(k1,k2))))
typedef unsigned long long fd_intptr;
#else
#define FD_LISP_EQ(x,y) \
  (((x).data.any == (y).data.any) && (FD_NPTR_TYPE(x) == FD_NPTR_TYPE(y)))
#define FD_LISP_EQUAL(k1,k2) \
  ((FD_NPTR_TYPE(k1) == FD_NPTR_TYPE(k2)) ?                                      \
   (((FD_NPTR_TYPE(k1) < FD_SMALL_TYPE_LIMIT) ?                                  \
     ((k1).data.fixnum == (k2).data.fixnum) :                                    \
     ((k1).data.any == (k2).data.any)) ||                                        \
    ((FD_NPTR_TYPE(k1) >= FD_ATOMIC_LIMIT) && (fd_lisp_equal(k1,k2)))) :         \
   /* We handle these specially because they're the cases where different base   \
      types can still be equal.  And while we're special-casing it for strings,  \
      we just make the comparison direct rather than going through fd_lisp_equal.\
   */                                                                            \
   ((FD_PRIM_TYPEP(k1,qstring_type) || (FD_PRIM_TYPEP(k1,string_type))) &&       \
    (FD_PRIM_TYPEP(k2,qstring_type) || (FD_PRIM_TYPEP(k2,string_type))) &&       \
    (FD_STRING_LENGTH(k1) == FD_STRING_LENGTH(k2)) &&                            \
    (strcmp(FD_STRING_DATA(k1),FD_STRING_DATA(k2)) == 0)) ||                     \
   ((FD_PRIM_TYPEP(k1,proper_choice_type) || (FD_PRIM_TYPEP(k1,choice_type))) && \
    (FD_PRIM_TYPEP(k2,proper_choice_type) || (FD_PRIM_TYPEP(k2,choice_type))) && \
    (fd_lisp_equal(k1,k2))))
typedef unsigned long long fd_intptr;
#endif

DTYPES_EXPORT int fd_lisp_equal(fd_lisp x,fd_lisp y);


/* Fixnums */

#define FD_FIXNUMP(x) (FD_PRIM_TYPEP(x,fixnum_type))
#define FD_FIXLISP(x) (x.data.fixnum) 
#define fd_fixlisp(x) \
  ((FD_FIXNUMP(x)) ? (FD_FIXLISP(x)) : \
   (fd_type_error(_("fd_lisp2int: not a fixnum"),x),0))
#define fd_lisp2int(x) fd_fixlisp(x)
STATIC_INLINE fd_lisp FD_LISPFIX(int i) UNUSED;
STATIC_INLINE fd_lisp FD_LISPFIX(int i) {
  fd_lisp l; FD_SET_PRIM_TYPE(l,fixnum_type); l.data.fixnum=i; return l;}

/* Immediate values */

/* Immediate values are used to represent ascii and unicode characters.
   When the lower 11 bits of an immediate are the CHAR_IMMEDIATE tag
   (32<<3+IMMEDIATE_TAG=81), bits 27 to 11 consitute a unicode char.  If
   the high order 8 bits are zero, this is an ascii character. */

#define FD_IMMEDIATEP(x) (FD_PRIM_TYPEP(x,immediate_type))
#define FD_TEST_IMMEDIATE(x,code) \
  (((x).data.fixnum == code) && (FD_IMMEDIATEP(x)))
STATIC_INLINE fd_lisp FD_MAKE_IMMEDIATE(int code)
{
  FD_RETURN_LISP_IMMEDIATE(immediate_type,fixnum,code);
}

#define FD_FALSE_CODE 0
#define FD_TRUE_CODE  1
#define FD_EMPTY_LIST_CODE  2
#define FD_VOID_CODE  3
#define FD_EMPTY_CHOICE_CODE 4
#define FD_QUOTED_EMPTY_CHOICE_CODE 5
#define FD_LISP_EOF_CODE 6

DTYPES_EXPORT fd_lisp FD_FALSE, FD_TRUE, FD_EMPTY_LIST, FD_EOF_OBJECT;
DTYPES_EXPORT fd_lisp FD_VOID, FD_EMPTY_CHOICE, FD_QUOTED_EMPTY_CHOICE;

#define FD_MAKE_FALSE (FD_MAKE_IMMEDIATE(FD_FALSE_CODE))
#define FD_FALSEP(x) (FD_TEST_IMMEDIATE((x),FD_FALSE_CODE))

#define FD_MAKE_TRUE  (FD_MAKE_IMMEDIATE(FD_TRUE_CODE))
#define FD_TRUEP(x) (FD_TEST_IMMEDIATE((x),FD_TRUE_CODE))

#define FD_MAKE_EMPTY_LIST  (FD_MAKE_IMMEDIATE(FD_EMPTY_LIST_CODE))
#define FD_EMPTY_LISTP(x) (FD_TEST_IMMEDIATE((x),FD_EMPTY_LIST_CODE))

#define FD_MAKE_VOID  (FD_MAKE_IMMEDIATE(FD_VOID_CODE))
#define FD_VOIDP(x) (FD_TEST_IMMEDIATE((x),FD_VOID_CODE))

#define FD_MAKE_EMPTY_CHOICE (FD_MAKE_IMMEDIATE(FD_EMPTY_CHOICE_CODE))
#define FD_EMPTYP(x) (FD_TEST_IMMEDIATE((x),FD_EMPTY_CHOICE_CODE))

#define FD_MAKE_QUOTED_EMPTY_CHOICE (FD_MAKE_IMMEDIATE(FD_QUOTED_EMPTY_CHOICE_CODE))
#define FD_QUOTED_EMPTY_CHOICEP(x) \
  (FD_TEST_IMMEDIATE((x),FD_QUOTED_EMPTY_CHOICE_CODE))
#define FD_QUOTED_CHOICEP(x) \
  ((PRIM_TYPEP(x,quoted_choice_type)) || FD_QUOTED_EMPTY_CHOICEP(x))

#define FD_FILEP(x) \
  ((FD_PRIM_TYPEP(x,input_file_type)) || \
   (FD_PRIM_TYPEP(x,output_file_type)) || \
   (FD_PRIM_TYPEP(x,io_file_type)))
#define FD_INPUT_FILEP(x) \
  ((FD_PRIM_TYPEP(x,input_file_type)) || \
   (FD_PRIM_TYPEP(x,io_file_type)))
#define FD_OUTPUT_FILEP(x) \
  ((FD_PRIM_TYPEP(x,output_file_type)) || \
   (FD_PRIM_TYPEP(x,io_file_type)))

#define FD_MAKE_EOF_OBJECT (FD_MAKE_IMMEDIATE(FD_LISP_EOF_CODE))
#define FD_EOF_OBJECTP(x) (FD_TEST_IMMEDIATE(x,FD_LISP_EOF_CODE))

#define FD_INPUT_STRINGP(x) (FD_PRIM_TYPEP(x,input_string_type))
#define FD_OUTPUT_STRINGP(x) (FD_PRIM_TYPEP(x,output_string_type))

#define FD_CHARACTERP(x) (FD_PRIM_TYPEP(x,character_type))
#define FD_CHAR_CODE(x) (FD_PTR_DATA(x,fixnum))
#define fd_char_code(x) \
  ((FD_CHARACTERP(x)) ? (FD_CHAR_CODE(x)) : \
   (fd_type_error(_("fd_char_code: not a character"),x),0))
#define FD_CODE_CHAR(x) fd_make_character(x)

#define FD_XPROCP(x) \
  ((FD_PRIM_TYPEP(x,sproc_type)) || (FD_PRIM_TYPEP(x,ssproc_type)) || \
   (FD_PRIM_TYPEP(x,gproc_type)))
#define FD_APPLICABLEP(x) \
  ((FD_PRIM_TYPEP(x,sproc_type)) || (FD_PRIM_TYPEP(x,ssproc_type)) || \
   (FD_PRIM_TYPEP(x,gproc_type)) || (FD_PRIM_TYPEP(x,cproc_type)))


/* Really should be in cons.h, but more useful here */

DTYPES_EXPORT fd_u8char *fd_object_to_string(fd_lisp object);
DTYPES_EXPORT fd_lisp fd_parse_string(fd_u8char *string);
DTYPES_EXPORT fd_lisp fd_parse_arg(char *string);

#if (FD_SOURCE)
typedef struct FD_LISP_PTR lisp;

#define RETURN_LISP FD_RETURN_LISP
#define RETURN_LISP_IMMEDIATE FD_RETURN_LISP
#define PRIM_TYPEP FD_PRIM_TYPEP 
#define PTR_TYPE FD_PTR_TYPE 
#define ATOMICP FD_ATOMICP

#define FIXNUMP FD_FIXNUMP
#define CHARACTERP FD_CHARACTERP
#define CHOICEP FD_CHOICEP
#define PAIRP FD_PAIRP

#define PTR_DATA FD_PTR_DATA
#define FIXLISP FD_FIXLISP
#define LISPFIX FD_LISPFIX
#define CHAR_CODE FD_CHAR_CODE
#define char_code fd_char_code
#define CODE_CHAR FD_CODE_CHAR

#define LISP_EQ FD_LISP_EQ
#define LISP_EQUAL FD_LISP_EQUAL

#endif /* (FD_SOURCE) */

#endif /* FRAMERD_LISP_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: lisp.h,v $
   Revision 1.17  2007/06/30 16:21:06  haase
   Various 64 bit fixes, together with stuff for repacking indices with less than 10 keys

   Revision 1.16  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.15  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.14  2003/10/20 09:25:44  haase
   Added FD_CHECK_PTR/FD_CHECK_LISP

   Revision 1.13  2003/09/10 17:58:29  haase
   Made slotmaps grow linearly in size

   Revision 1.12  2003/09/10 11:20:00  haase
   Minor rearrangements of source code

   Revision 1.11  2003/09/05 12:59:24  haase
   Added macro for setting LISP pointer data

   Revision 1.10  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.6  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.9.2.5  2003/08/02 21:31:32  haase
   Added FD_APPLICABLEP

   Revision 1.9.2.4  2003/01/26 20:32:14  haase
   Introduced zstrings

   Revision 1.9.2.3  2002/09/26 02:12:50  haase
   Cleanups and addition of FD_QSTRINGP

   Revision 1.9.2.2  2002/08/10 18:03:22  haase
   Made code use constructor expressions if available

   Revision 1.9.2.1  2002/08/09 16:42:35  haase
   Moving towards more direct FD_RETURN_LISP

   Revision 1.9  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.8  2002/04/04 18:48:51  haase
   Added homongenous vectors of ints, shorts, floats, and doubles.
   Also changed the field "size" in some structs to "length" to indicate
   that the underlying data structure is ordered.

   Revision 1.7  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
