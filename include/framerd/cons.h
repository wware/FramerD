/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: cons.h,v 1.49 2005/12/05 04:09:39 haase Exp $

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

*************************************************************************/

#ifndef FRAMERD_CONS_H /* If defined, skip the file */
#define FRAMERD_CONS_H

#include "framerd/common.h"

#define FD_DEBUGGING_ALLOCATION 0


/* Exceptions */

DTYPES_EXPORT fd_exception
  fd_NoDTypeRep,
  fd_DanglerCref,
  fd_DanglerOp,
  fd_RecordRegistryOverflow,
  fd_HashTableOverflow,
  fd_InvalidDType,
  fd_Unknown_DTCode,
  fd_ConfigSyntaxError,
  fd_ParseError,
  fd_Unknown_Record_Type,
  fd_Invalid_Cons_Format;


/* Locks for shared data */

/* We keep an array of mutexes for locking conses and lock
   based on an offset mask of the pointer address.  We use this
   to lock ref count increases. */
#if (FD_THREADS_ENABLED) 
#define FD_N_CONS_LOCKS 128
#define FD_CONS_LOCK_MASK (0x7F)
DTYPES_EXPORT fd_mutex fd_cons_locks[FD_N_CONS_LOCKS];
#define FD_LOCK_CONS(ptr) \
  fd_lock_mutex(&(fd_cons_locks[(((long)ptr)>>4)&FD_CONS_LOCK_MASK]))
#define FD_UNLOCK_CONS(ptr) \
  fd_unlock_mutex(&(fd_cons_locks[(((long)ptr)>>4)&FD_CONS_LOCK_MASK]))
#define FD_WITH_CONS_LOCKED(ptr) \
  FD_WITH_MUTEX_LOCKED(&(fd_cons_locks[(((long)ptr)>>4)&FD_CONS_LOCK_MASK]))
#define FD_END_WITH_CONS_LOCKED(ptr) \
  FD_END_WITH_MUTEX_LOCKED(&(fd_cons_locks[(((long)ptr)>>4)&FD_CONS_LOCK_MASK]))
#else /* No threads, no need for pointer locking */
#define FD_LOCK_CONS(ptr) 0
#define FD_UNLOCK_CONS(ptr) 0
#define FD_WITH_CONS_LOCKED(ptr)
#define FD_END_WITH_CONS_LOCKED(ptr)
#endif

/* The same thing, but just for cells (OIDs and symbols). */
#if (FD_THREADS_ENABLED) 
#define FD_N_CELL_LOCKS 64
#define FD_CELL_LOCK_MASK (0x3F)
DTYPES_EXPORT fd_mutex fd_cell_locks[FD_N_CELL_LOCKS];
#define FD_LOCK_CELL(ptr) \
  fd_lock_mutex(&(fd_cell_locks[(((ptr).data.fixnum)>>4)&FD_CELL_LOCK_MASK]))
#define FD_UNLOCK_CELL(ptr) \
  fd_unlock_mutex(&(fd_cell_locks[(((ptr).data.fixnum)>>4)&FD_CELL_LOCK_MASK]))
#define FD_WITH_CELL_LOCKED(ptr) \
  FD_WITH_MUTEX_LOCKED(&(fd_cell_locks[(((ptr).data.fixnum)>>4)&FD_CELL_LOCK_MASK]))
#define FD_END_WITH_CELL_LOCKED(ptr) \
  FD_END_WITH_MUTEX_LOCKED(&(fd_cell_locks[(((ptr).data.fixnum)>>4)&FD_CELL_LOCK_MASK]))
#else /* No threads, no need for pointer locking */
#define FD_LOCK_CELL(ptr) 0
#define FD_UNLOCK_CELL(ptr) 0
#define FD_WITH_CELL_LOCKED(ptr)
#define FD_END_WITH_CELL_LOCKED(ptr)
#endif


/* All the things made out of "conses" */

/* Type for newly allocated cons */
typedef struct FD_ACONS {
  fd_refcount n_refs; fd_lisp car, cdr;} *acons;
typedef struct FD_PAIR {
  fd_refcount n_refs; fd_lisp car, cdr;} *fd_pair; /* CONSes */
typedef struct FD_CHOICE { /* Non-deterministic values */
  fd_refcount n_refs; 
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif  
  unsigned int sorted, size, limit; 
  fd_lisp_type elt_type; unsigned short busy;
  union CHOICE_ELTS {
    fd_lisp *lisp; union FD_DATA *data;} elements;} *fd_choice;
/* Interned symbols */
typedef struct FD_SYMBOL {
  fd_u8char *name; fd_lisp value;} *fd_lisp_symbol ;
typedef struct FD_STRING { /* Strings (utf-8 encoded unicode) */
  fd_refcount n_refs; int length;
  uchar utf8; fd_u8char *data;} *fd_lisp_string;

typedef struct FD_DOUBLE {
  fd_refcount n_refs; double d;} *fd_double; /* Doubles */

struct FD_RATIONAL_NUMBER {
  fd_refcount n_refs; fd_lisp numerator, denominator;};
struct FD_COMPLEX_NUMBER {
  fd_refcount n_refs; fd_lisp real, imag;};

typedef struct FD_CPROC { /* Primitive C procedure callable from FDScript */
  fd_refcount n_refs;
  fd_u8char *name; int n_args; int direct_call; fd_lisp (*func)(void);} *fd_cproc;
typedef struct FD_SPROC { /* Compound Scheme procedure */
  fd_refcount n_refs; struct FD_LISPENV *env; fd_lisp lambda;} *fd_sproc;
typedef struct FD_RPROC { /* Remote procedure */
  fd_refcount n_refs; struct FD_SERVER *server; fd_lisp remote_op;} *fd_rproc;
typedef struct FD_SSPROC { /* Synchronized compound Scheme procedure */
  fd_refcount n_refs; struct FD_LISPENV *env; fd_lisp lambda; 
#if (FD_THREADS_ENABLED)
  fd_mutex lock;
#endif
  } *fd_ssproc;

typedef struct FD_CPTR { /* Generic refcounted pointer */
  fd_refcount n_refs; void *ptr;} *cptr;

typedef struct GCCONS {void *next;} *gccons; /* Cons on the free list */
typedef struct FD_VECTOR { /* A fixed size vector */
  fd_refcount n_refs; unsigned int length; fd_lisp *elements;} *lisp_vector;
struct FD_INT_VECTOR { /* A fixed size vector of ints */
  fd_refcount n_refs; unsigned int length; int *elements;};
struct FD_SHORT_VECTOR { /* A fixed size vector of shorts */
  fd_refcount n_refs; unsigned int length; short *elements;};
struct FD_FLOAT_VECTOR { /* A fixed size vector of floats */
  fd_refcount n_refs; unsigned int length; float *elements;};
struct FD_DOUBLE_VECTOR { /* A fixed size vector of doubles */
  fd_refcount n_refs; unsigned int length; double *elements;};

typedef struct FD_RECORD { /* A record whose data is a C pointer */
  fd_refcount n_refs; fd_lisp tag; void *data;} *record;
typedef struct FD_LRECORD { /* A record whose data is a LISP pointer */
  fd_refcount n_refs; fd_lisp tag; fd_lisp data;} *lrecord;

/* Other types */
typedef struct FD_MYSTERY { /* Used for unknown packaged DTypes */
  unsigned char package; unsigned char code;
  unsigned int length;
  union {char *bytes; fd_lisp *dtypes;} data;} *fd_mystery;
typedef struct FD_SLOTMAP { /* Vector of slots and values used by frames */
  fd_refcount n_refs;
  unsigned int size, limit;
  char modified, shared_schema;
  fd_lisp *schema, *values;
#if FD_STICKY_SLOTMAPS
  /* Sticky slotmaps are used for debugging frame operations because the keep track of whether
     an OID has officially "let go" of the slotmap which is its value.  They are normally turned off. */
  int sticky;
#endif
#if FD_THREADS_ENABLED
  fd_mutex lock;
#endif
  } *fd_slotmap;

struct FD_STRING_ISTREAM {uchar *original, *ptr;};

/* The type FD_CONS refers to all of these uniformly. */
typedef union FD_CONS { /* Union of various CONS types, used in GC code */
  struct FD_PAIR pair; 
  struct FD_SYMBOL symbol; 
  struct FD_CHOICE choice;
  struct FD_STRING string; 
  struct FD_RECORD record;
  struct FD_LRECORD lrecord;
  struct FD_VECTOR vector;
  struct FD_SPROC sproc;
  struct FD_RPROC rproc;
  struct FD_CPTR cptr;
  struct GCCONS gc;
  struct FD_COMPLEX_NUMBER complex_number;
  struct FD_RATIONAL_NUMBER rational_number;
#if (!(FD_LIGHTWEIGHT_OIDS))
  struct FD_CONSOID oid;
#endif
} *fd_consptr;

#if FD_THREADS_ENABLED
struct FD_LISP_QUEUE { int n, lim; fd_lisp *refs; fd_mutex lock; };
#else
struct FD_LISP_QUEUE { int n, lim; fd_lisp *refs; };
#endif
DTYPES_EXPORT void fd_init_queue(struct FD_LISP_QUEUE *q);
DTYPES_EXPORT void fd_destroy_queue(struct FD_LISP_QUEUE *q);
DTYPES_EXPORT void fd_add_to_queue(struct FD_LISP_QUEUE *q,fd_lisp item);
DTYPES_EXPORT void fd_simplify_queue(struct FD_LISP_QUEUE *q);

#define PTR_REFCOUNT(x) ((FD_PTR_DATA(x,acons))->n_refs)

#define FD_MAX_TYPECODE 128


/* Supporting allocation of conses */

/* These handle non-adamantine types. */
DTYPES_EXPORT fd_lisp _fd_copy_lisp_proc(fd_lisp x);

/* These free and copy objects */
#define fd_copy_lisp(x) ((FD_ATOMICP(x)) ? (x) : (_fd_copy_lisp_proc(x)))

DTYPES_EXPORT fd_lisp _fd_incref_cons(fd_lisp x);

STATIC_INLINE fd_lisp fd_incref(fd_lisp x)
{
  if ((FD_NPTR_TYPE(x)) < FD_GC_LIMIT) return x;
  else return _fd_incref_cons(x);
}

DTYPES_EXPORT void _fd_decref_cons(fd_lisp x);

STATIC_INLINE void fd_decref(fd_lisp x)
{
  if ((FD_NPTR_TYPE(x)) < FD_GC_LIMIT) return;
  else _fd_decref_cons(x);
}

#define fd_refcount(x) ((FD_PTR_DATA((x),acons))->n_refs)

/* Gets a big byte buffer */
DTYPES_EXPORT char *fd_get_big_buffer(unsigned int *bufsize);

/* Used to round up malloc'd memory to consistent sizes */

#define fd_roundup(n,block) ((n>32) ? ((((n)/block)+1)*block) : (n))
#define FD_STRING_CHUNK 64
#define fd_string_roundup(n) \
  ((n>32) ? ((((n)/FD_STRING_CHUNK)+1)*FD_STRING_CHUNK) : (n))


/* Startup and shutdown */

DTYPES_EXPORT void fd_initialize_dtypes(void);
extern void fd_setup_signal_handlers(void);
extern void fd_initialize_exception_handling(void);
extern void fd_initialize_heap(void);
extern void fd_initialize_symbol_table(void);
extern void fd_initialize_oid_table(void);
extern void fd_initialize_io_stuff(void);
extern void fd_initialize_xdata(void);
extern void fd_initialize_network(void);


/* Data consing functions */

/* These make new conses */
DTYPES_EXPORT fd_lisp _FD_MAKE_PAIR(fd_lisp car,fd_lisp cdr);
DTYPES_EXPORT fd_lisp _FD_MAKE_LIST1(fd_lisp car);
DTYPES_EXPORT fd_lisp FD_MAKE_LIST(int length,...);
DTYPES_EXPORT fd_lisp fd_cons(char *format,...);
DTYPES_EXPORT fd_lisp fd_init_string (fd_u8char *string,int size);
DTYPES_EXPORT fd_lisp fd_make_string(char *string);
DTYPES_EXPORT fd_lisp fd_make_substring(fd_u8char *start,fd_u8char *end);
DTYPES_EXPORT fd_lisp fd_copy_string(fd_u8char *string);
DTYPES_EXPORT fd_lisp fd_lower_string(fd_u8char *string);
DTYPES_EXPORT fd_lisp fd_make_choice(void);
DTYPES_EXPORT fd_lisp fd_init_choice(int size);
DTYPES_EXPORT fd_lisp fd_init_oid_choice(int size);
DTYPES_EXPORT fd_lisp fd_make_cptr(fd_lisp_type tp,void *data);
DTYPES_EXPORT fd_lisp fd_make_record(fd_lisp type_name,void *data);
DTYPES_EXPORT fd_lisp fd_make_erecord(fd_lisp type_name,void *data);
DTYPES_EXPORT fd_lisp fd_make_lrecord(fd_lisp type_name,fd_lisp data);
DTYPES_EXPORT fd_lisp fd_make_rational(fd_lisp num,fd_lisp denom);
DTYPES_EXPORT fd_lisp fd_make_complex(fd_lisp real,fd_lisp imag);
DTYPES_EXPORT fd_lisp fd_quote_lisp(fd_lisp x);

DTYPES_EXPORT fd_lisp _fd_quote_choice(fd_lisp x);
DTYPES_EXPORT fd_lisp _fd_unquote_choice(fd_lisp x);

/* These return immediates */
DTYPES_EXPORT fd_lisp fd_make_flonum(double f);
DTYPES_EXPORT fd_lisp fd_make_character(unsigned int c);

/* These do lookups */
DTYPES_EXPORT fd_lisp fd_make_symbol(const fd_u8char *name);
DTYPES_EXPORT fd_lisp fd_probe_symbol(const fd_u8char *name);
/* This makes an uppercase symbol */
DTYPES_EXPORT fd_lisp fd_intern(const fd_u8char *name,int len);


/* Other, non-core, data types */

DTYPES_EXPORT fd_lisp fd_make_ascii(char s);
/* In some implementations, might return a record */
DTYPES_EXPORT fd_lisp fd_make_fixnum(int i);

DTYPES_EXPORT fd_lisp fd_make_vector(int size);
DTYPES_EXPORT fd_lisp fd_init_vector(int size,fd_lisp *elts);

DTYPES_EXPORT fd_lisp fd_make_int_vector(int size,int *data);
DTYPES_EXPORT fd_lisp fd_make_short_vector(int size,short *data);
DTYPES_EXPORT fd_lisp fd_make_float_vector(int size,float *data);
DTYPES_EXPORT fd_lisp fd_make_double_vector(int size,double *data);

DTYPES_EXPORT fd_lisp fd_make_packet(int size,unsigned char *data);
DTYPES_EXPORT fd_lisp fd_parse_packet(fd_u8char *string);

DTYPES_EXPORT fd_lisp fd_make_slotmap(int size);

DTYPES_EXPORT fd_lisp fd_make_timestamp(time_t moment);
DTYPES_EXPORT time_t fd_timestamp_time(fd_lisp timestamp);

/* CPTRs are void * pointers with a reference counter */
DTYPES_EXPORT fd_lisp fd_copy_cptr(fd_lisp x);
DTYPES_EXPORT void fd_free_cptr(fd_lisp x);
DTYPES_EXPORT unsigned int fd_compare_cptrs(fd_lisp x,fd_lisp y);

DTYPES_EXPORT fd_lisp fd_make_error(fd_lisp data);
DTYPES_EXPORT fd_lisp fd_make_exception(fd_lisp data);


/* Hash tables */

#ifndef FD_HASHTABLE_DEFINED
#define FD_HASHTABLE_DEFINED 1
typedef struct FD_HASHTABLE {
#if FD_THREADS_ENABLED
  fd_rwlock lock;
#endif
  unsigned int n_slots, n_keys; struct FD_PAIR **table;} *fd_hashtable;
#endif
typedef struct FD_HASHSET {
#if FD_THREADS_ENABLED
  fd_rwlock lock;
#endif  
  unsigned int n_slots, n_keys; signed char need_gc;
  fd_lisp *table;} *fd_hashset;

DTYPES_EXPORT unsigned int fd_hash_lisp(fd_lisp x);

DTYPES_EXPORT unsigned int fd_select_table_size(unsigned int min);

DTYPES_EXPORT fd_hashtable fd_make_hashtable(int size);
DTYPES_EXPORT void fd_init_hashtable(fd_hashtable h,int size);
DTYPES_EXPORT void fd_reinit_hashtable(fd_hashtable h,int size,int locked);
DTYPES_EXPORT fd_lisp fd_hashtable_get(fd_hashtable h,fd_lisp key,fd_lisp dflt);
DTYPES_EXPORT int fd_hashtable_test(fd_hashtable h,fd_lisp key,fd_lisp value);

DTYPES_EXPORT fd_lisp fd_hashtable_strget(fd_hashtable h,fd_u8char *key,int len);
DTYPES_EXPORT void fd_hashtable_set(fd_hashtable h,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_hashtable_set_nc(fd_hashtable h,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_hashtable_init_value
  (fd_hashtable h,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_hashtable_add(fd_hashtable h,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_hashtable_drop(fd_hashtable h,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_hashtable_zap(fd_hashtable h,fd_lisp key);
DTYPES_EXPORT int fd_hashtable_probe(fd_hashtable h,fd_lisp key);
DTYPES_EXPORT void fd_grow_hashtable(fd_hashtable h,int min);
DTYPES_EXPORT void fd_free_hashtable(fd_hashtable h);
DTYPES_EXPORT void fd_cleanup_locked_hashtable(fd_hashtable h);
DTYPES_EXPORT void _fd_hashtable_set_nolock(fd_hashtable h,fd_lisp key,fd_lisp value);

DTYPES_EXPORT void fd_hashtable_increment
                   (fd_hashtable h,fd_lisp key,int delta);
DTYPES_EXPORT void fd_hashtable_increment_existing
                   (fd_hashtable h,fd_lisp key,int delta);
DTYPES_EXPORT fd_lisp fd_hashtable_skim(fd_hashtable h,int max);
DTYPES_EXPORT fd_lisp fd_hashtable_max(fd_hashtable h);
DTYPES_EXPORT void fd_hashtable_dedangle(fd_hashtable h);
DTYPES_EXPORT void fd_hashtable_map(fd_hashtable h,void (*fcn)(fd_lisp key,fd_lisp val,void *datap),void *data);


DTYPES_EXPORT fd_hashset fd_make_hashset(int size);
DTYPES_EXPORT void fd_init_hashset(fd_hashset h,int size);
DTYPES_EXPORT void fd_reinit_hashset(fd_hashset h,int size,int locked);
DTYPES_EXPORT int fd_hashset_get(fd_hashset h,fd_lisp key);
DTYPES_EXPORT int fd_hashset_add(fd_hashset h,fd_lisp key);
DTYPES_EXPORT void fd_hashset_drop(fd_hashset h,fd_lisp key);
DTYPES_EXPORT fd_lisp fd_hashset_elts(fd_hashset h);
DTYPES_EXPORT fd_lisp fd_final_hashset_elts(fd_hashset h);
DTYPES_EXPORT void fd_grow_hashset(fd_hashset h,int min);
DTYPES_EXPORT void fd_free_hashset(fd_hashset size);
DTYPES_EXPORT void fd_hashset_map(fd_hashset h,void (*fcn)(fd_lisp key,void *datap),void *data);

DTYPES_EXPORT int _fd_hashset_add_nc(fd_hashset h,fd_lisp key);

DTYPES_EXPORT int fd_hashset_strget(fd_hashset h,fd_u8char *keystring,int len);
DTYPES_EXPORT fd_lisp fd_hashset_probe(fd_hashset h,fd_lisp key);
DTYPES_EXPORT fd_lisp fd_hashset_intern(fd_hashset h,fd_lisp key);
DTYPES_EXPORT fd_lisp fd_hashset_intern_string
  (fd_hashset h,fd_u8char *keystring,int len);


DTYPES_EXPORT fd_lisp fd_make_qstring(fd_u8char *data,int len);
DTYPES_EXPORT fd_lisp fd_qify_string(struct FD_STRING *str);

DTYPES_EXPORT fd_lisp fd_make_zstring(fd_u8char *data,int len);
DTYPES_EXPORT fd_lisp fd_zify_string(struct FD_STRING *str);

/* These handle the tables for symbols and objects */
DTYPES_EXPORT void fd_grow_oid_table(unsigned int size);
DTYPES_EXPORT fd_hashset fd_oid_table(void);
DTYPES_EXPORT fd_hashset fd_symbol_table(void);
DTYPES_EXPORT void fd_for_all_symbols(void (*fcn)(fd_lisp symbol));


/* Macros for accessing LISP stuff */

#define FD_PAIRP(x) (FD_PRIM_TYPEP(x,pair_type))
#define FD_CAR(x) (((FD_PTR_DATA(x,pair))->car))
#define fd_car_noref(x) \
  ((FD_PAIRP(x)) ? (FD_CAR(x)) : \
   (fd_type_error(_("not a pair"),x),\
    FD_VOID))
#define fd_car(x) \
  ((FD_PAIRP(x)) ? (fd_incref(FD_CAR(x))) : \
   (fd_type_error(_("not a pair"),x),\
    FD_VOID))
#define FD_CDR(x)  (((FD_PTR_DATA(x,pair))->cdr))
#define fd_cdr_noref(x) \
  ((FD_PAIRP(x)) ? (FD_CDR(x)) : \
   (fd_type_error(_("not a pair"),x),\
    FD_VOID))
#define fd_cdr(x) \
  ((FD_PAIRP(x)) ? (fd_incref(FD_CDR(x))) : \
   (fd_type_error(_("not a pair"),x),\
    FD_VOID))

#define FD_DOLIST(x,l) \
  fd_lisp _tmp=l, x; \
  while ((FD_PAIRP(_tmp)) ? \
         (x=FD_CAR(_tmp),_tmp=FD_CDR(_tmp),1) : 0)

#define FD_RPLACA(p,newcar) (FD_PTR_DATA(p,pair)->car)=newcar
#define FD_RPLACD(p,newcdr) (FD_PTR_DATA(p,pair)->cdr)=newcdr
DTYPES_EXPORT unsigned int fd_list_length(fd_lisp x);

#define FD_STRINGP(x)         ((FD_PRIM_TYPEP(x,string_type)) || (FD_PRIM_TYPEP(x,qstring_type)) || (FD_PRIM_TYPEP(x,zstring_type)))
#define FD_ASCII_STRINGP(x)   ((FD_STRINGP(x)) && (FD_PTR_DATA(x,string)->utf8 == 0))
#define FD_UNICODE_STRINGP(x) ((FD_STRINGP(x)) && (FD_PTR_DATA(x,string)->utf8))
#define FD_QSTRINGP(x)        (FD_PRIM_TYPEP(x,qstring_type))
#define FD_ZSTRINGP(x)        (FD_PRIM_TYPEP(x,zstring_type))

#define FD_STRING_LENGTH(x) (FD_PTR_DATA(x,string)->length)
#define fd_strlen(x) \
  ((FD_STRINGP(x)) ?  (FD_STRING_LENGTH(x)) : \
   (fd_type_error(_("not a string"),x),0))
#define FD_STRING_DATA(x) (FD_PTR_DATA(x,string)->data)
#define fd_strdata(x) \
  ((FD_STRINGP(x)) ? (FD_STRING_DATA(x)) : \
   (fd_type_error(_("not a string"),x),(fd_u8char *)NULL))

#define FD_SYMBOLP(x) (FD_PRIM_TYPEP(x,symbol_type))
#define FD_SYMBOL_NAME(x) (FD_PTR_DATA(x,symbol)->name)
#define fd_symbol_name(x) \
  ((FD_SYMBOLP(x)) ? (FD_SYMBOL_NAME(x)) : \
   (fd_type_error(_("not a symbol"),x),(fd_u8char *)NULL))

DTYPES_EXPORT fd_lisp fd_symbol_value(fd_lisp symbol);
DTYPES_EXPORT fd_lisp _fd_symbol_value_noref(fd_lisp symbol);
DTYPES_EXPORT void fd_set_symbol_value(fd_lisp symbol,fd_lisp value);
DTYPES_EXPORT void _fd_set_symbol_value_noref(fd_lisp symbol,fd_lisp value);

#if (!(FD_THREADS_ENABLED))
#define FD_SYMBOL_VALUE(x) (FD_PTR_DATA(x,symbol)->value)
#else
#define FD_SYMBOL_VALUE(x) _fd_symbol_value_noref(x)
#endif

#if (!(FD_THREADS_ENABLED))
#define FD_SET_SYMBOL_VALUE(x,v) (FD_PTR_DATA(x,symbol)->value)=v
#else
FASTOP void FD_SET_SYMBOL_VALUE(fd_lisp x,fd_lisp v) UNUSED;
FASTOP void FD_SET_SYMBOL_VALUE(fd_lisp x,fd_lisp v) {
  _fd_set_symbol_value_noref(x,v);
}
#endif

/* Numeric macros */

#define FD_FLONUMP(x) (FD_PRIM_TYPEP(x,flonum_type))
#define FD_FLOATLISP(x) (FD_PTR_DATA(x,fdouble)->d)
#define fd_lisp2float(x) \
  ((FD_FLONUMP(x)) ? (FD_FLOATLISP(x)) : \
   (fd_type_error(_("not a flonum"),x),\
    (double)0.0))
#define FD_LISPFLOAT(x) fd_make_flonum(x)

#define LISP_ZEROP(x) \
  (((FD_FIXNUMP(x)) && ((FD_PTR_DATA(x,fixnum)) == 0)) || \
   ((FD_FLONUMP(x)) && ((FD_FLOATLISP(x)) == 0.0)))

#define FD_BIGNUMP(x)  (FD_PRIM_TYPEP(x,bigint_type))

#define FD_INTEGERP(x) ((FD_FIXNUMP(x)) || (FD_BIGNUMP(x)))
#define FD_RATIONALP(x) ((FD_PRIM_TYPEP(x,rational_type)))
#define FD_NUMERATOR(x) (((FD_PTR_DATA(x,rational_number))->numerator))
#define FD_DENOMINATOR(x) (((FD_PTR_DATA(x,rational_number))->denominator))

#define FD_COMPLEXP(x) ((FD_PRIM_TYPEP(x,complex_type)))
#define FD_REALPART(x) (((FD_PTR_DATA(x,complex_number))->real))
#define FD_IMAGPART(x) (((FD_PTR_DATA(x,complex_number))->imag))

#define FD_NUMBERP(x) \
   ((FD_FIXNUMP(x)) || (FD_FLONUMP(x)) || (FD_BIGNUMP(x)) || \
    (FD_RATIONALP(x)) || (FD_COMPLEXP(x)))

/* Choice operations */

#define FD_CHOICEP(x) ((FD_PRIM_TYPEP(x,choice_type)) || \
		      (FD_PRIM_TYPEP(x,proper_choice_type)))
#define FD_SINGLE_VALUEP(x) (!(FD_CHOICEP(x)))
#define FD_CHOICE_SIZE(v) \
  ((FD_EMPTYP(v)) ? 0 : \
   ((FD_CHOICEP(v)) ? (fd_choice_size(FD_PTR_DATA(v,choice))) : 1))
#define FD_CHOICE_ELTS(v) \
  ((FD_EMPTYP(v)) ? NULL : \
   ((FD_CHOICEP(v)) ? ((FD_PTR_DATA(v,choice))->elements) : \
    (&v)))
#define FD_CHOICE_LIMIT(v) \
  ((FD_EMPTYP(v)) ? 0 : \
   ((FD_CHOICEP(v)) ? ((FD_PTR_DATA(v,choice))->limit) : \
    1))
#define fd_choice_size(s) (s->size)
#define fd_choice_limit(s) (s->limit)
#define fd_set_choice_size(s,sz) s->size=sz
#define fd_choice_elt_type(s) s->elt_type

DTYPES_EXPORT void _fd_add_to_choice (fd_lisp x,fd_lisp set);
DTYPES_EXPORT fd_lisp fd_remove_from_choice (fd_lisp x,fd_lisp choice);
DTYPES_EXPORT fd_lisp _fd_binary_choice (fd_lisp x,fd_lisp y);
DTYPES_EXPORT fd_lisp fd_list_to_choice (fd_lisp lst);
DTYPES_EXPORT fd_lisp fd_merge_choices(fd_lisp x,fd_lisp y);
DTYPES_EXPORT fd_lisp _fd_merge_choices(fd_lisp x,fd_lisp y);
DTYPES_EXPORT int fd_choice_containsp(fd_lisp x,fd_lisp c);
DTYPES_EXPORT int fd_choice_overlapsp(fd_lisp x,fd_lisp c);
DTYPES_EXPORT int fd_proper_choicep(fd_lisp value);
DTYPES_EXPORT fd_lisp fd_return_proper_choice(fd_lisp values);
DTYPES_EXPORT fd_lisp fd_return_sorted_choice(fd_lisp values);
DTYPES_EXPORT fd_lisp fd_intersect_choices(fd_lisp *choices,int size);
DTYPES_EXPORT int fd_sort_choice(fd_lisp arg);
DTYPES_EXPORT void fd_sort_hashtable_values(fd_hashtable h);


/* CPTR access */

#define FD_CPTR_DATA(x) ((FD_PTR_DATA(x,cptr))->ptr)


/* Record Macros */

#define FD_RECORDP(x) (FD_PRIM_TYPEP(x,record_type))
#define FD_LRECORDP(x) (FD_PRIM_TYPEP(x,lrecord_type))
#define FD_RECORD_TAG(x) (FD_PTR_DATA(x,record)->tag)
#define FD_LRECORD_TAG(x) (FD_PTR_DATA(x,lrecord)->tag)
#define FD_RECORD_DATA(x) (FD_PTR_DATA(x,record)->data)
#define FD_LRECORD_DATA(x) (FD_PTR_DATA(x,lrecord)->data)
#define FD_RECORD_TYPEP(x,tg) \
  ((FD_RECORDP(x)) && (FD_LISP_EQ((FD_PTR_DATA(x,record)->tag),tg)))
#define FD_LRECORD_TYPEP(x,tg) \
  ((FD_LRECORDP(x)) && (FD_LISP_EQ((FD_PTR_DATA(x,record)->tag),tg)))
#define fd_record_tag(x) \
  ((FD_RECORDP(x)) ? (FD_RECORD_TAG(x)) \
   : (fd_type_pointer("record_tag: pointer is not a record",(x)),\
      FD_VOID))
#define fd_record_data(x,tag) \
  ((FD_RECORDP(x)) ? \
   ((FD_RECORD_TYPEP(x,tag)) ? (FD_RECORD_DATA(x)) : \
    (fd_record_type_error(x,tag),((void *)NULL))) \
   : (fd_type_error("not a record",x),((void *)NULL)))
#define fd_lrecord_tag(x) \
  ((FD_LRECORDP(x)) ? (FD_LRECORD_TAG(x)) \
   : (fd_type_pointer("record_tag: pointer is not an lrecord",(x)),\
      FD_VOID))
#define fd_lrecord_data(x,tag) \
  ((FD_LRECORDP(x)) ? \
   ((FD_LRECORD_TYPEP(x,tag)) ? (fd_incref(FD_LRECORD_DATA(x))) : \
    (fd_record_type_error(x,tag),(FD_VOID))) \
   : (fd_type_error("not a record",x),(FD_VOID)))


/* Record types */

typedef unsigned int(fd_hashfn)(fd_lisp);

struct FD_TYPE_REGISTRY {
  fd_lisp tag; fd_lisp_type typecode;
  fd_lisp (*copy_fcn)(fd_lisp x);
  void (*gc_fcn)(fd_lisp x);
  int (*incref_fcn)(fd_lisp x);
  int (*decref_fcn)(fd_lisp x);
  unsigned int (*hash_fcn)(fd_lisp x,fd_hashfn h);
  void (*print_fcn)(fd_lisp x,fd_string_stream s);
  unsigned int (*compare_fcn)(fd_lisp x,fd_lisp y);
  fd_lisp compound_tag;
  fd_lisp (*compound_dump_fcn)(fd_lisp x);
  fd_lisp (*compound_restore_fcn)(fd_lisp x);
  fd_lisp (*reader_restore_fcn)(fd_lisp x);
  unsigned char package_code, subcode;
  int (*package_data_fcn)(fd_lisp obj,void **x);
  void (*package_data_done_fcn)(fd_lisp obj,int n_elts,void **x);
  fd_lisp (*package_restore_fcn)(int size,void *v);};

DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_typecode_registry[];

DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_lookup_record(fd_lisp tag);
DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_register_record(fd_lisp tag);
DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_register_typecode(fd_lisp_type tp);
DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_lookup_package_code
   (unsigned char package_code,unsigned char subcode);
DTYPES_EXPORT struct FD_TYPE_REGISTRY *fd_lookup_compound(fd_lisp tag);
#define fd_lookup_typecode(x) \
  ((x < FD_MAX_TYPECODE) ? (fd_typecode_registry[(int)x]) : \
   (fd_raise_exception(fd_BadType),(struct FD_TYPE_REGISTRY *)NULL))

/* Procedures for built in record types */

DTYPES_EXPORT fd_lisp fd_make_hashtable_for_lisp(int size);
DTYPES_EXPORT fd_lisp fd_hashtable_to_alist(fd_lisp hashset);
DTYPES_EXPORT fd_lisp fd_alist_to_hashtable(fd_lisp elts);

DTYPES_EXPORT fd_lisp fd_make_hashset_for_lisp(int size);
DTYPES_EXPORT fd_lisp fd_lisp_hashset_elts(fd_lisp hashset);
DTYPES_EXPORT fd_hashset fd_choice_to_hashset(fd_lisp elts);


/* Vectors */

#define FD_VECTORP(x) (FD_PRIM_TYPEP(x,vector_type))
#define FD_VECTOR_LENGTH(v) ((FD_PTR_DATA(v,vector))->length)
#define FD_VECTOR_ELEMENTS(v) ((FD_PTR_DATA(v,vector))->elements)
#define FD_VECTOR_REF(v,i) (((FD_PTR_DATA(v,vector))->elements)[i])
#define FD_VECTOR_SET(v,i,x) (((FD_PTR_DATA(v,vector))->elements)[i])=x

#define fd_vector_length(v) \
     ((FD_VECTORP(v)) ? (FD_VECTOR_LENGTH(v)) : \
      (fd_type_error(_("not a vector"),v),\
       0))
#define fd_vector_ref(v,i) \
     ((FD_VECTORP(v)) ? \
      ((i <= (FD_VECTOR_LENGTH(v))) ? (FD_VECTOR_REF(v,i)) : \
       (fd_raise_lisp_exception(fd_RangeError,fd_int2str(i),v),\
	FD_VOID)) : \
       (fd_type_error(_("not a vector"),v),\
	FD_VOID))



/* Homogenous Vectors */

#define FD_INT_VECTORP(x) (FD_PRIM_TYPEP(x,int_vector_type))
#define FD_SHORT_VECTORP(x) (FD_PRIM_TYPEP(x,short_vector_type))
#define FD_FLOAT_VECTORP(x) (FD_PRIM_TYPEP(x,float_vector_type))
#define FD_DOUBLE_VECTORP(x) (FD_PRIM_TYPEP(x,double_vector_type))
#define FD_HVECTORP(x) \
  (FD_INT_VECTORP(x) || FD_SHORT_VECTORP(x) || \
   FD_FLOAT_VECTORP(x) || FD_DOUBLE_VECTORP(x))
#define FD_HVECTOR_REF(x,tfield,i) \
  ((FD_PTR_DATA(x,tfield))->elements[i])
#define FD_HVECTOR_SET(x,tfield,i,v) \
  ((FD_PTR_DATA(x,tfield))->elements[i])=v

#define FD_HVECTOR_LENGTH(v) ((FD_PTR_DATA(v,vector))->length)

#define fd_hvector_length(v) \
     ((FD_HVECTORP(v)) ? (FD_HVECTOR_LENGTH(v)) : \
      (fd_type_error(_("not a vector"),v),\
       0))
#define fd_vector_ref(v,i) \
     ((FD_VECTORP(v)) ? \
      ((i <= (FD_VECTOR_LENGTH(v))) ? (FD_VECTOR_REF(v,i)) : \
       (fd_raise_lisp_exception(fd_RangeError,fd_int2str(i),v),\
	FD_VOID)) : \
       (fd_type_error(_("not a vector"),v),\
	FD_VOID))




/* Slotmaps */

/* I'm numbering slots from zero */
#define FD_SLOTMAPP(x) (FD_PRIM_TYPEP(x,slotmap_type))
#define FD_SLOTMAP_PTR(x) (FD_PTR_DATA((x),slotmap))
#define FD_SLOTMAP_SIZE(sm) ((FD_SLOTMAP_PTR(sm))->size)
#define FD_SLOTMAP_KEY(sm,i) (((FD_SLOTMAP_PTR(sm))->schema)[i])
#define FD_SLOTMAP_VALUE(sm,i) (((FD_SLOTMAP_PTR(sm))->values)[i])
DTYPES_EXPORT fd_lisp fd_slotmap_get(fd_slotmap sm,fd_lisp key,fd_lisp dflt);
DTYPES_EXPORT int fd_slotmap_test(fd_slotmap sm,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_slotmap_set(fd_slotmap sm,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_slotmap_add(fd_slotmap sm,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_slotmap_remove(fd_slotmap sm,fd_lisp key,fd_lisp value);
DTYPES_EXPORT void fd_slotmap_zap(fd_slotmap sm,fd_lisp key);
DTYPES_EXPORT int _fd_slotmap_data(fd_lisp,void **);
DTYPES_EXPORT void _fd_done_with_slotmap_data(fd_lisp *,int);
DTYPES_EXPORT fd_lisp fd_copy_slotmap(fd_lisp slotmap);

#define FD_DO_SLOTMAP(s,v,obj) \
  fd_lisp _doslots_ov=fd_incref(obj);  \
  fd_lisp s, v, *_doslots_slots;  \
  int _size=_fd_slotmap_data(_doslots_ov,(void **)&_doslots_slots); \
  fd_lisp *_doslots_scan=_doslots_slots; \
  fd_lisp *_doslots_limit=_doslots_slots+_size; \
  while ((_doslots_scan<_doslots_limit) ? \
	 (s=*_doslots_scan++,v=*_doslots_scan++,1) : \
	 (fd_decref(_doslots_ov),_fd_done_with_slotmap_data(_doslots_slots,_size),0))

#if FD_STICKY_SLOTMAPS
#define set_sticky(sm,v) (sm)->sticky=1
#define test_sticky(sm) (sm->sticky)
#else
#define set_sticky(sm,v) 
#define test_sticky(sm) (0)
#endif


/* Other predicates and macros */

DTYPES_EXPORT fd_lisp fd_exception_tag, fd_error_tag;

#define FD_PACKETP(x) (FD_PRIM_TYPEP(x,packet_type))
#define FD_PACKET_LENGTH(v) ((FD_PTR_DATA(v,string))->length)
#define FD_PACKET_DATA(v)   ((FD_PTR_DATA(v,string))->data)

#define fd_byte_length(x) \
  ((FD_STRINGP(x) || (FD_PACKETP(x))) ? (FD_STRING_LENGTH(x)) : \
   (fd_type_error(_("not a string or packet"),x),0))
#define fd_byte_data(x) \
  ((FD_STRINGP(x) || (FD_PACKETP(x))) ? (FD_STRING_DATA(x)) : \
   (fd_type_error(_("not a string or packet"),x),(uchar *)NULL))

#define FD_ERRORP(x) (FD_LRECORD_TYPEP(x,fd_error_tag))
#define FD_EXCEPTIONP(x) (FD_LRECORD_TYPEP(x,fd_exception_tag))
#define FD_ERROR_DATA(x) (FD_LRECORD_DATA(x))
#define FD_EXCEPTION_DATA(x) (FD_LRECORD_DATA(x))


/* Macros for dealing with choice enumeration and accumulation */

DTYPES_EXPORT fd_lisp *_fd_copy_lispv(fd_lisp *v,int size,int limit);
DTYPES_EXPORT
union FD_DATA *_fd_copy_datav(union FD_DATA *v,int size,int limit);


STATIC_INLINE fd_lisp _cons_ptr(fd_lisp_type t,union FD_DATA d)
{
  fd_lisp x; FD_SET_PRIM_TYPE(x,t); x.data=d; return x;
}

/* This is really hairy, mostly to stay as a macro and deal with the
   complexity of choice structures.  I want to keep it as a macro because
   the most common cliche has the `body' accessing some local variables
   and C doesn't have lexical closures. */
#define FD_DO_CHOICES(x,l) \
  fd_lisp_type _elt_type; int _old_limit; \
  fd_lisp x, _tmp=l, *_scan, *_limit, *_lisp; \
  union FD_DATA *_dscan, *_dlimit, *_data; \
  fd_choice _ch=NULL; \
  FD_UNWIND_PROTECT { \
      if (FD_EMPTYP(_tmp)) {_scan=&_tmp; _limit=_scan; _elt_type=0;} \
      else if (FD_CHOICEP(_tmp)) {\
	_ch=FD_PTR_DATA(_tmp,choice);\
	fd_lock_mutex(&(_ch->lock)); _old_limit=_ch->limit; \
	if (_ch->busy)  \
	 if ((_elt_type=_ch->elt_type) != 0) { \
	   _dscan=_data=_fd_copy_datav(_ch->elements.data,_ch->size,_ch->limit); \
	   _dlimit=_dscan+_ch->size; FD_SET_PRIM_TYPE(x,_elt_type);} \
	 else {\
	   _scan=_lisp=_fd_copy_lispv(_ch->elements.lisp,_ch->size,_ch->limit); \
	   _limit=_scan+_ch->size;} \
	else if ((_elt_type=_ch->elt_type) != 0) {\
	  FD_SET_PRIM_TYPE(x,_elt_type); _dscan=_data=_ch->elements.data; \
	  _dlimit=_dscan+_ch->size; _ch->busy=1;} \
	else {\
	 _scan=_lisp=_ch->elements.lisp; \
	 _limit=_scan+_ch->size; \
	 _ch->busy=1;} \
	fd_unlock_mutex(&(_ch->lock));} \
      else {_scan=&_tmp; _limit=_scan+1; _elt_type=0;} \
      while ((_elt_type) ? \
	     ((_dscan < _dlimit) ? (x.data=*_dscan,_dscan++,1) : (0)) : \
	     ((_scan < _limit) ? (x=*_scan,_scan++,1) : (0)))

#define FD_END_DO_CHOICES \
   } FD_ON_UNWIND { \
   if (_ch) { \
    fd_lock_mutex(&(_ch->lock)); \
    if (_elt_type) \
      if (_data == _ch->elements.data) _ch->busy=0; \
      else fd_free(_data,_old_limit*sizeof(union FD_DATA)); \
    else if (_lisp == _ch->elements.lisp) _ch->busy=0; \
    else fd_free(_lisp,_old_limit*sizeof(fd_lisp)); \
    fd_unlock_mutex(&(_ch->lock));}} FD_END_UNWIND
#define END_FD_DO_CHOICES FD_END_DO_CHOICES

#define fd_first_choice(x) \
  ((CHOICEP(x)) ? (((PTR_DATA(x,choice))->elt_type) ? \
                   (_cons_ptr(((PTR_DATA(x,choice))->elt_type), \
                              ((PTR_DATA(x,choice))->elements.data)[0])) : \
                   ((PTR_DATA(x,choice))->elements.lisp)[0]) \
                : (x))

FASTOP
fd_lisp _result_merge(fd_lisp into,fd_lisp result) UNUSED;

FASTOP
fd_lisp _result_merge(fd_lisp into,fd_lisp result)
{
  if (FD_PRIM_TYPEP(result,bad_type)) fd_raise_exception(fd_BadType);
  else if (FD_EMPTYP(result)) return into;
  else if (FD_EMPTYP(into))
    if ((FD_CHOICEP(result)) && ((FD_PTR_DATA(result,acons))->n_refs > 1)) {
      fd_lisp copy=fd_copy_lisp(result); fd_decref(result); return copy;}
    else return result;
  else if ((!(FD_CHOICEP(result))) && (FD_CHOICEP(into))) {
    fd_choice ch=FD_PTR_DATA(into,choice);    
    fd_lock_mutex(&(ch->lock));
    if (ch->elt_type == 0) {
      unsigned int size=ch->size, limit=ch->limit;
      if (size == limit) _fd_add_to_choice(result,into);
      else {ch->elements.lisp[size]=result; ch->size++;}
      FD_SET_PRIM_TYPE(into,choice_type);
      fd_unlock_mutex(&(ch->lock));
      return into;}
    else if (FD_PRIM_TYPEP(result,ch->elt_type)) {
      unsigned int size=ch->size, limit=ch->limit;
      if (size == limit) _fd_add_to_choice(result,into);
      else {ch->elements.data[size]=result.data; ch->size++;}
      FD_SET_PRIM_TYPE(into,choice_type);
      fd_unlock_mutex(&(ch->lock));
      return into;}
    else {
      _fd_add_to_choice(result,into);
      fd_unlock_mutex(&(ch->lock));
      return into;}}
  else return _fd_merge_choices(into,result);
}

#define FD_ADD_TO_CHOICE(into,result) into=_result_merge(into,result)

#define FD_QUOTE_CHOICE(x) _fd_quote_choice(x)
#define FD_UNQUOTE_CHOICE(x) _fd_unquote_choice(x)


/* DTYPE I/O functions */

DTYPES_EXPORT int fd_fwrite_dtype(fd_lisp x,FILE *f);
DTYPES_EXPORT int fd_dwrite_dtype(fd_lisp x,FD_DBUF *f);
DTYPES_EXPORT int fd_fake_dtype(fd_lisp x,unsigned int *loc);

DTYPES_EXPORT int fd_fwrite_dtype_x(fd_lisp x,FILE *f,int capability);
DTYPES_EXPORT int fd_dwrite_dtype_x(fd_lisp x,FD_DBUF *f,int capability);
DTYPES_EXPORT int fd_fake_dtype_x(fd_lisp x,unsigned int *loc,int capability);

DTYPES_EXPORT fd_lisp fd_fread_dtype(FILE *f);
DTYPES_EXPORT fd_lisp fd_dread_dtype(FD_DBUF *f);

DTYPES_EXPORT unsigned int fd_dtype_size(fd_lisp x);
DTYPES_EXPORT int fd_validate_dtype(unsigned char *ptr,unsigned char *end);

DTYPES_EXPORT void fd_write_dtype_to_file(fd_lisp v,char *filename);
DTYPES_EXPORT void fd_add_dtype_to_file(fd_lisp v,char *filename);
DTYPES_EXPORT fd_lisp fd_read_dtype_from_file(char *filename);
DTYPES_EXPORT fd_lisp fd_read_dtypes_from_file(char *filename);


/* TEXT(LISP) I/O functions */

DTYPES_EXPORT fd_lisp fd_stream_string(struct FD_STRING_STREAM *s);

DTYPES_EXPORT fd_lisp fd_parse_lisp_from_string(fd_u8char **string);
DTYPES_EXPORT fd_lisp fd_parse_lisp_from_stream(FILE *stream);
DTYPES_EXPORT fd_lisp fd_parse_number(fd_u8char *string,int base);


DTYPES_EXPORT void fd_fprintf(FILE *f,char *format_string,...);
DTYPES_EXPORT void fd_printf(fd_string_stream s,char *format_string,...);
DTYPES_EXPORT void fd_notify(char *format_string,...);
DTYPES_EXPORT void fd_warn(char *format_string,...);
DTYPES_EXPORT void fd_set_notifier(void (*nf)(char *msg));
DTYPES_EXPORT void fd_set_notify_stream(FILE *f);
DTYPES_EXPORT void fd_inhibit_herald(int inhibit);

DTYPES_EXPORT void fd_default_print_oid(fd_lisp obj,fd_string_stream ss);
DTYPES_EXPORT fd_lisp fd_default_parse_oid(fd_u8char *string);
DTYPES_EXPORT void fd_configure_oid_io
  (void ((*print_fcn)(fd_lisp x,fd_string_stream ss)),
   fd_lisp ((*parse_fcn)(fd_u8char *string)),
   fd_lisp ((*lookup_fcn)(fd_lisp name)));
DTYPES_EXPORT void fd_print_lisp_to_string(fd_lisp x,fd_string_stream s);
DTYPES_EXPORT void fd_print_lisp (fd_lisp x, FILE * stream);
DTYPES_EXPORT void fd_print_lisp_to_stdout(fd_lisp dtype);

DTYPES_EXPORT void fd_set_bignum_parser(fd_lisp (*fcn)(char *,int));

DTYPES_EXPORT unsigned int fd_pprint_lisp_to_string
  (fd_lisp x,fd_string_stream s,unsigned int pos,unsigned int indent,unsigned int limit);
DTYPES_EXPORT void fd_pprint_lisp(fd_lisp x,FILE *stream,int width);
DTYPES_EXPORT void fd_pprint_lisp_with_offset
  (fd_lisp x,FILE *stream,int width,int offset);
DTYPES_EXPORT fd_u8char *fd_ppstring(fd_lisp x,int width);

DTYPES_EXPORT void fd_set_charset(fd_unichar_t *charset);
DTYPES_EXPORT fd_unichar_t *fd_get_charset(char *charset_spec);
DTYPES_EXPORT char *fd_filestring(char *filename);

DTYPES_EXPORT void fd_set_super_pool_aliasing(FD_OID oid1,FD_OID oid2);

#if (FD_INLINE_CONSING)
STATIC_INLINE fd_lisp FD_MAKE_PAIR(fd_lisp car,fd_lisp cdr)
{
  fd_pair _c=fd_malloca(struct FD_PAIR);
  _c->car = car;
  _c->cdr = cdr;
  if ((FD_PRIM_TYPEP(car,bad_type)) || (FD_PRIM_TYPEP(cdr,bad_type)))
    fd_raise_exception(fd_BadType);
  else {RETURN_LISP(pair_type,pair,_c);}
}
#define FD_MAKE_LIST1(x) FD_MAKE_PAIR(x,FD_EMPTY_LIST)
#else
#define FD_MAKE_PAIR _FD_MAKE_PAIR
#define FD_MAKE_LIST1 _FD_MAKE_LIST1
#endif

#if (FD_SOURCE)
#define incref fd_incref
#define decref fd_decref
#define copy_lisp fd_copy_lisp
#define DO_CHOICES FD_DO_CHOICES
#define END_DO_CHOICES END_FD_DO_CHOICES
#define ADD_TO_CHOICE FD_ADD_TO_CHOICE
#define CHOICE_SIZE FD_CHOICE_SIZE
#define CHOICE_ELTS FD_CHOICE_ELTS
#define PAIRP FD_PAIRP
#define CAR FD_CAR
#define CDR FD_CDR
#define DOLIST FD_DOLIST
#define RPLACA FD_RPLACA
#define RPLACD FD_RPLACD
#define ASCII_STRINGP FD_ASCII_STRINGP
#define UNICODE_STRINGP FD_UNICODE_STRINGP
#define STRINGP FD_STRINGP
#define STRING_LENGTH FD_STRING_LENGTH
#define STRING_DATA FD_STRING_DATA
#define SYMBOLP FD_SYMBOLP
#define SYMBOL_NAME FD_SYMBOL_NAME
#define SYMBOL_VALUE FD_SYMBOL_VALUE
#define FLONUMP FD_FLONUMP
#define FLOATLISP FD_FLOATLISP
#define LISPFLOAT FD_LISPFLOAT
#define NUMBERP FD_NUMBERP
#define VECTORP FD_VECTORP
#define VECTOR_LENGTH FD_VECTOR_LENGTH 
#define VECTOR_REF FD_VECTOR_REF 
#define SLOTMAPP FD_SLOTMAPP
#define SLOTMAP_SIZE FD_SLOTMAP_SIZE
#define SLOTMAP_KEY FD_SLOTMAP_KEY
#define SLOTMAP_VALUE FD_SLOTMAP_VALUE
#define SLOTMAP_PTR FD_SLOTMAP_PTR
#define DO_SLOTS FD_DO_SLOTS
#define SLOTP FD_SLOTP

#define RECORDP FD_RECORDP
#define LRECORDP FD_LRECORDP

#define RECORD_TYPEP FD_RECORD_TYPEP
#define LRECORD_TYPEP FD_LRECORD_TYPEP

#define PACKETP FD_PACKETP
#define PACKET_LENGTH FD_PACKET_LENGTH
#define PACKET_DATA FD_PACKET_DATA
#define EXCEPTIONP FD_EXCEPTIONP
#define ERRORP FD_ERRORP
#define ERROR_DATA FD_ERROR_DATA
#define EXCEPTION_DATA FD_EXCEPTION_DATA
#define RECORD_TAG FD_RECORD_TAG
#define LRECORD_TAG FD_LRECORD_TAG
#define RECORD_DATA FD_RECORD_DATA
#define LRECORD_DATA FD_LRECORD_DATA
#define CPTR_DATA FD_CPTR_DATA
#endif /* (FD_SOURCE) */

#endif /* ndef FRAMERD_CONS_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: cons.h,v $
   Revision 1.49  2005/12/05 04:09:39  haase
   Updates for compiling with gcc 4.0

   Revision 1.48  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.47  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.46  2004/09/28 23:39:39  haase
   Added non-symbol oid lookup

   Revision 1.45  2004/09/17 07:30:34  haase
   Made file index caching not copy the retrieved choice

   Revision 1.44  2004/07/27 16:16:30  haase
   Optimizing choice operations

   Revision 1.43  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.42  2004/07/19 16:57:09  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.41  2004/05/15 21:36:02  haase
   Made FD_FLOATLISP work outside of FramerD

   Revision 1.40  2004/05/14 14:41:55  haase
   Made preloading be an option for all kinds of indices

   Revision 1.39  2004/03/31 21:04:11  haase
   Various unthreaded compile fixes

   Revision 1.38  2004/03/31 11:19:55  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.37  2004/03/31 03:13:10  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.36  2004/03/30 10:55:01  haase
   Defined _x versions of DTYPE writing to handle extended capabilities

   Revision 1.35  2004/03/30 08:10:07  haase
   Support for using schemas

   Revision 1.34  2004/03/29 15:03:08  haase
   added fd_copy_slotmap

   Revision 1.33  2004/01/10 21:11:14  haase
   Added fd_simplify_queue to remove duplicates, sort, etc.

   Revision 1.32  2003/11/26 12:35:30  haase
   Added fd_hashtable_dedangle to remove dangling keys from hashtables

   Revision 1.31  2003/11/22 21:08:41  haase
   Changed CONS type to FD_CONS

   Revision 1.30  2003/10/05 06:37:48  haase
   Made FD_DO_SLOTS use interleaved data function

   Revision 1.29  2003/09/09 00:33:17  haase
   Whitespace changes

   Revision 1.28  2003/08/31 16:58:21  haase
   Declarations and minor amendations

   Revision 1.27  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.26.2.6  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.26.2.5  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.26.2.4  2003/08/02 13:39:56  haase
   Added LISP pointer queues

   Revision 1.26.2.3  2003/06/29 19:34:29  haase
   Added FD_DO_SLOTMAP and used it in slotmap printing

   Revision 1.26.2.2  2003/01/26 20:32:06  haase
   Introduced zstrings

   Revision 1.26.2.1  2002/09/26 02:12:50  haase
   Cleanups and addition of FD_QSTRINGP

   Revision 1.26  2002/07/24 02:05:47  haase
   Removed 'new' symbols from include files to allow inclusion in C++ files

   Revision 1.25  2002/07/05 21:20:07  uid59704
   Fixed GC contract of fd_hashset_intern and added fd_hashset_intern string

   Revision 1.24  2002/07/03 06:04:21  haase
   Added a C-level debugging feature for the GC where some slotmaps can be
   declared "sticky" meaning that an error is signalled when the are GCd.  This
   sticky bit is set whenever a slotmap is stored under an OID and cleared
   by the procedures for swapping out OIDs.

   Revision 1.23  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.22  2002/05/26 06:16:32  haase
   Typo fix to fd_byte_data

   Revision 1.21  2002/05/26 05:56:14  haase
   Added error checking fd_byte_length and fd_byte_data (for packets and strings)

   Revision 1.20  2002/05/19 10:12:55  haase
   Added fd_intern for making uppercase symbols

   Revision 1.19  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.18  2002/04/20 19:47:48  haase
   Renamed fd_hashset_zap to fd_hashset_drop

   Revision 1.17  2002/04/19 19:30:59  haase
   Added framework for read/write locks on hashtables

   Revision 1.16  2002/04/04 18:48:51  haase
   Added homongenous vectors of ints, shorts, floats, and doubles.
   Also changed the field "size" in some structs to "length" to indicate
   that the underlying data structure is ordered.

   Revision 1.15  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
