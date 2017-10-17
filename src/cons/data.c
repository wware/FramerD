/* Mode: C */

/* data.c
   This file implements the basic Lisp datatypes in C:
     strings, fixnums, conses, objects, records, etc.
   Details of the data structures can be found in the header files
    lisp.h and cons.h 

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

static char vcid[] = "$Id: data.c,v 1.19 2005/01/14 16:48:44 haase Exp $";

/** Data structure and variable declarations **/
/** Utility functions for responsible malloc and reference counting **/
/** Threadsafe OID access **/
/** Allocation debugging support functions **/
/** Generating more conses **/
/** Atom construction functions **/
/** List construction functions **/
/** String functions **/
/** Record construction functions **/
/** Choices: Non deterministic sets **/
/** Record functions **/
/** LISP Vectors **/
/** Garbage collection **/
/** Copying lisp objects **/
/** Comparing lisp objects **/
/** Iterating over structures **/
/** Dealing with big buffers **/
/** Module registry **/
/** Initializing the heap, symbols, etc... **/

#include "dtypes.h"

#include <limits.h>
#include <time.h>
#include <signal.h>
#include <stdarg.h>

/* Declarations for object consing */

/* Conses are allocated in blocks in order to avoid malloc overhead on
   each cons. */

#if FD_THREADS_ENABLED
fd_mutex fd_cell_locks[FD_N_CELL_LOCKS];
fd_mutex fd_cons_locks[FD_N_CONS_LOCKS];
#endif

/* Comparing atoms */

#define COMPARE_NUMS(x,y) (((x)==(y)) ? (0) : (((x)>(y)) ? 1 : -1))
#define COMPARE_ATOMS(a1,a2) \
  (((PTR_TYPE(a1)) == (PTR_TYPE(a2))) ? \
   ((FD_SMALL_TYPEP(a1)) ? \
    (COMPARE_NUMS(PTR_DATA(a1,any_small),PTR_DATA(a2,any_small))) : \
    (COMPARE_NUMS(PTR_DATA(a1,any),PTR_DATA(a2,any)))) \
   : (COMPARE_NUMS((PTR_TYPE(a1)),(PTR_TYPE(a2)))))

/** Data structure and variable declarations **/

#define RECORD_REGISTRY_SIZE 256

/* Exceptions */

fd_exception
  fd_Type_Error=_("Type Error"),
  fd_Out_Of_Bounds=_("Reference out of bounds"),
  fd_DanglerCREF=_("GC error: incref for a dangling pointer"),
  fd_DanglerOp=_("GC error: operating on a dangling pointer"),
  fd_BadType=_("Bad type code (corrupted data)"),
  fd_RecordRegistryOverflow=_("Too may kinds of records registered (sorry)");

static void validate_utf8(fd_u8char *s)
{
  int c;
  while ((c=fd_sgetc(&s))>=0);
}

/** Utility functions for reference counting **/

DTYPES_EXPORT
/* _fd_incref_cons:
     Arguments: a lisp pointer
     Returns: its argument

  Increments the reference count associated with a cons
*/
lisp _fd_incref_cons(lisp x)
{
  acons c=PTR_DATA(x,acons);
  if (PTR_TYPE(x)>=64) {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
    if ((rec) && (rec->decref_fcn)) {
      rec->decref_fcn(x);
      return;}}
  if (c->n_refs <= 0) fd_raise_exception(fd_DanglerCREF);
  FD_LOCK_CONS(c);
  c->n_refs++;
  FD_UNLOCK_CONS(c);
  return x;
}

/** Atom construction functions **/

DTYPES_EXPORT
/* fd_make_character:
    Arguments: a character
    Returns: the LISP version of the character */
lisp fd_make_character(unsigned int c)
{
  RETURN_LISP_IMMEDIATE(character_type,fixnum,c);
}

/** List construction functions **/

static lisp empty_list;

DTYPES_EXPORT
/* _FD_MAKE_PAIR:
    Arguments: two lisp objects
    Returns: a cons pair whose CAR and CDR are the arguments
     the arguments are not incref'd
*/
lisp _FD_MAKE_PAIR (lisp x, lisp y)
{
  fd_pair c=fd_malloca(struct FD_PAIR);
  c->car = x;
  c->cdr = y;
  if ((x.type == bad_type) || (y.type == bad_type))
    fd_raise_exception(fd_BadType);
  else {RETURN_LISP(pair_type,pair,c);}
}

DTYPES_EXPORT
/* fd_make_pair:
    Arguments: two lisp objects
    Returns: a cons pair whose CAR and CDR are the arguments
     the arguments *are* incref'd
*/
lisp fd_make_pair (lisp x, lisp y)
{
  return FD_MAKE_PAIR(incref(x),incref(y));
}

DTYPES_EXPORT
/* _FD_MAKE_LIST1:
    Arguments: a lisp object
    Returns: a list whose first and only element is the argument
*/
lisp _FD_MAKE_LIST1 (lisp x)
{
  return FD_MAKE_PAIR(x,FD_EMPTY_LIST);
}

DTYPES_EXPORT
/* FD_MAKE_LIST:
    Arguments: an integer followed by several elements
    Returns: a list of <length> elements in order
*/
lisp FD_MAKE_LIST (int length,...)
{
  int i = 0;
  lisp empty_list = (FD_EMPTY_LIST);
  lisp result = empty_list, next, prev;
  va_list args;
  va_start (args, length);
  while (i < length)
    {
      result = FD_MAKE_PAIR (va_arg (args, lisp), result);
      i++;
    }
  /* Reverse in place */
  prev = empty_list;
  next = result;
  while (!(FD_EMPTY_LISTP(next)))
    {
      fd_pair p = PTR_DATA (next, pair);
      lisp temp = p->cdr;
      p->cdr = prev;
      prev = next;
      next = temp;
    }
  return prev;
}

DTYPES_EXPORT
/* fd_list_length:
    Arguments: a lisp object
    Returns: the number of elements in the object, if it is a list,
     or 1 otherwise.
*/
unsigned int fd_list_length(lisp x)
{
  int l=0; while (PAIRP(x)) {x=CDR(x); l++;}
  return l;
}

/** String functions **/

DTYPES_EXPORT
/* fd_init_string:
    Arguments: a null-terminated utf8 string
    Returns: a lisp object describing the string

This uses the actual string argument (so it shouldn't be
stack consed or subsequently freed by the caller).

*/
lisp fd_init_string (fd_u8char *string,int size)
{
  fd_lisp_string c=fd_malloca(struct FD_STRING);
  fd_u8char *scan=string, *limit;
  if (size < 0) size=strlen(string); limit=scan+size;
  fd_malloc_adjust(size+1);
  c->length=size; c->data=string; c->utf8=0;
#if 0
  validate_utf8(string);
#endif
  while (scan < limit) 
    if (*scan >= 0x80) {c->utf8=1; break;} else scan++;
  {RETURN_LISP(string_type,string,c);}
}

DTYPES_EXPORT
/* fd_copy_string:
    Arguments: a null-terminated utf8 string
    Returns: a lisp object describing the string

This copies the string argument and also determines
if it is UTF-8 or not.
*/
lisp fd_copy_string(fd_u8char *string)
{
  fd_lisp_string c=fd_malloca(struct FD_STRING);
  fd_u8char *scan=string, *limit;
  c->utf8=0; while (*scan) { /* Assume null-terminated */
    if (*scan >= 0x80) c->utf8=1; scan++;}
  c->length=scan-string; c->data=fd_xmalloc(c->length+1);
  memcpy(c->data,string,c->length+1);
  fd_malloc_adjust(c->length+1);
#if 0
  validate_utf8(string);
#endif
  {RETURN_LISP(string_type,string,c);}
}

DTYPES_EXPORT
/* fd_make_string:
    Arguments: a null-terminated localized C string
    Returns: a lisp object describing the string

This does UTF-8 conversion and doesn't use the direct
pointer to string.
 */
lisp fd_make_string (char *string)
{
  return fd_init_string(fd_xstring(string),-1);
}

DTYPES_EXPORT
/* fd_make_substring:
    Arguments: two pointers into the same utf-8 string
    Returns: a lisp object describing the substring between them
*/
lisp fd_make_substring (fd_u8char *start,fd_u8char *end)
{
  int len = end-start;
  if (len < 0)
    fd_raise_exception(_("fd_make_substring: args are backwards"));
  else {
    fd_u8char *copy=fd_xmalloc(len+1);
    strncpy (copy, start, end-start); copy[end-start]='\0';
    return fd_init_string(copy,len);}
}

DTYPES_EXPORT
/* fd_stream_string:
    Arguments: a pointer to a string stream
    Returns: a lisp string containing the contents of the stream

This sets the string stream's pointer to NULL and size to zero, so that
subsequent modifications signal errors.
*/
lisp fd_stream_string(struct FD_STRING_STREAM *s)
{
  fd_u8char *str=s->ptr; int size=s->size;
  s->ptr[s->size]=0; s->ptr=NULL; s->size=0;
  return fd_init_string(str,size);
}

FASTOP int uni_sgetc(fd_u8char **ss)
{
  if (**ss < 0x80) return *((*ss)++);
  else return _fd_sgetc(ss);
}

DTYPES_EXPORT
/* fd_lower_string:
    Arguments: a null-terminated utf-8 C string
    Returns: a lisp object describing the lower-cased version of the string

This is useful for canonicalizing strings to look things up in hashtables,
etc. */
lisp fd_lower_string (fd_u8char *string)
{
  fd_u8char *scan=string;
  struct FD_STRING_STREAM ss; int c;
  FD_INITIALIZE_STRING_STREAM(&ss,32);
  while (*scan) {
    if (*scan < 0x80) c=tolower(*scan++);
    else c=fd_tolower(fd_sgetc(&scan));
    fd_sputc(&ss,c);}
  return fd_stream_string(&ss);
}


DTYPES_EXPORT
/* fd_utf8_strlen:
    Arguments: a pointer to a UTF-encoded string and a length
    Returns: an integer indicating the number of unicode characters
in the string it represents

*/
int fd_utf8_strlen(fd_u8char *str,int slen)
{
  fd_u8char *scan=str, *limit=str+slen; int len=0;
  while (scan < limit) {len++; uni_sgetc(&scan);}
  return len;
}

DTYPES_EXPORT
/* fd_utf8_substring:
    Arguments: a pointer to a UTF-encoded string and an integer
    Returns: the substring starting at the interger-th character

*/
char *fd_utf8_substring(fd_u8char *str,int index)
{
  fd_u8char *scan=str, *last=scan; int count=index;
  while ((count > 0) && (uni_sgetc(&scan) >= 0)) {
    last=scan; count--;}
  if (count == 0) return last;
  else return NULL;
}

DTYPES_EXPORT
/* fd_utf8_string_ref:
    Arguments: a pointer to a UTF-encoded string
    Returns: returns the first unicode character in the string

*/
int fd_utf8_string_ref(fd_u8char *str)
{
  int c=uni_sgetc(&str);
  return c;
}

/** Record construction functions **/

DTYPES_EXPORT
/* fd_make_cptr:
    Arguments: a lisp type and a data pointer
    Returns: a lisp pointer with the type and an allocated refcounter for
    the cpointer */
fd_lisp fd_make_cptr(fd_lisp_type tp,void *data)
{
  cptr c=fd_malloca(struct FD_CPTR); c->ptr = data;
  {RETURN_LISP(tp,cptr,c);}
}

DTYPES_EXPORT
/* fd_make_record:
    Arguments: a type specifier (a lisp object), and a pointer to some data
    Returns: a lisp record
*/
lisp fd_make_record (lisp type_name, void *data)
{
  record c=fd_malloca(struct FD_RECORD);
  c->tag = type_name; c->data = data;
  {RETURN_LISP(record_type,record,c);}
}

DTYPES_EXPORT
/* fd_make_lrecord:
    Arguments: a type specifier (a lisp object), and another lisp object
    Returns: a lisp record
*/
lisp fd_make_lrecord (lisp type_name, lisp data)
{
  lrecord c=fd_malloca(struct FD_LRECORD);
  c->tag = type_name; c->data = data;
  {RETURN_LISP(lrecord_type,lrecord,c);}
}

/** Record functions **/

/* The record registry is a data structure used to describe extended types
   implemented as tagged records.  Each record entry contains:
   * the record tag used for the object
   * a copy function
   * a gc function
   * a print function
   * a comparison function
   in addition, a record can have a dtype format as either
   a COMPOUND or a PACKAGED TYPE (or possibly but rarely both).
   To support COMPOUND types, the record entry also contains:
   * a compound tag which can be written as a DTYPE
   * a "canonicalizer" for generating a lisp object from the record
   which can be written as a dtype.
   * an "interpreter" for for generating equivalent records from
   lisp objects generated by the canonicalizer
   To support packaged types, the record entry also contains:
   * a package code
   * a subcode
   * an init function which takes a size and a pointer to
   either a vector of objects or vector of bytes and returns
   a record instance
   * an elements function which returns a size and leaves a pointer
   to a vector of objects or bytes in a given location.
   * an "free elements" function which is called when the deposited
   array is no longer needed
   
   New types are declared by calling fd_register_record and then setting
   fields in the returned object.  Functions also exist for finding records
   with particular properties (compound tags, package codes, subcodes, etc).
*/

static struct FD_TYPE_REGISTRY record_registry[RECORD_REGISTRY_SIZE];
struct FD_TYPE_REGISTRY *fd_typecode_registry[FD_MAX_TYPECODE];
static int n_registered_records=0;

DTYPES_EXPORT
/* fd_lookup_compound:
   Arguments: a lisp tag
   Returns: a pointer to a record entry or NULL
   Returns the record entry for types with the specified compound tag 
*/
struct FD_TYPE_REGISTRY *fd_lookup_compound(lisp tag)
{
  int i=0; while (i < n_registered_records)
    if (LISP_EQ(record_registry[i].compound_tag,tag))
      return &(record_registry[i]);
    else i++;
  return NULL;
}

DTYPES_EXPORT
/* fd_lookup_package_code:
   Arguments: a package code and subcode
   Returns: a pointer to a record entry or NULL
   Returns the record entry for types with the specified
   package code and subcode.  Note that this looks up the
   subcode for the "short" version of the packaged data.
*/
struct FD_TYPE_REGISTRY *fd_lookup_package_code
(unsigned char package_code,unsigned char subcode)
{
  int i=0; while (i < n_registered_records)
    if ((record_registry[i].package_code == package_code) &&
	(record_registry[i].subcode == (subcode&(~0x40))))
      return &(record_registry[i]);
    else i++;
  return NULL;
}

DTYPES_EXPORT
/* fd_lookup_record:
      Arguments: a lisp pointer
      Returns: a pointer to a record entry or NULL
    Returns the record entry for types whose record tags
     are the argument given to the function.
*/
struct FD_TYPE_REGISTRY *fd_lookup_record(lisp tag)
{
  int i=0; while (i < n_registered_records)
    if (LISP_EQ(record_registry[i].tag,tag))
      return &(record_registry[i]);
    else i++;
  return NULL;
}

DTYPES_EXPORT
/* fd_register_record:
      Arguments: a lisp pointer
      Returns: a pointer to a record entry
    Returns a record entry for types with a particular record tag.
    This creates an entry if one does not already exist.  Also, if
    the tag is FD_VOID, it always creates a new entry.  (This is
    useful for types which don't have tags but need special methods
    like non-deterministic sets.
*/
struct FD_TYPE_REGISTRY *fd_register_record(lisp tag)
{
  int i=0;
  if (FD_EMPTYP(tag)) i=n_registered_records;
  else while (i < n_registered_records)
    if (LISP_EQ(record_registry[i].tag,tag))
      return &(record_registry[i]);
    else i++;
  if (n_registered_records < RECORD_REGISTRY_SIZE) {
    record_registry[i].tag=incref(tag);
    record_registry[i].copy_fcn=NULL;
    record_registry[i].gc_fcn=NULL;
    record_registry[i].incref_fcn=NULL;
    record_registry[i].decref_fcn=NULL;
    record_registry[i].hash_fcn=NULL;
    record_registry[i].print_fcn=NULL;
    record_registry[i].compare_fcn=NULL;
    
    record_registry[i].compound_tag=(FD_VOID);
    record_registry[i].compound_dump_fcn=NULL;
    record_registry[i].compound_restore_fcn=NULL;
    record_registry[i].reader_restore_fcn=NULL;
    
    record_registry[i].package_code=0;
    record_registry[i].subcode=0; 
    record_registry[i].package_data_fcn=NULL;
    record_registry[i].package_data_done_fcn=NULL;
    record_registry[i].package_restore_fcn=NULL;
    n_registered_records++;
    return &(record_registry[i]);}
  else  
    fd_raise_exception(fd_RecordRegistryOverflow);
  return NULL; /* Never reached */
}

DTYPES_EXPORT
/* fd_register_typecode:
      Arguments: a lisp typecode
      Returns: a pointer to a record entry
    Returns a record entry for types with a particular typecode.
    This creates an entry if one does not already exist.  Also, if
    the tag is FD_VOID, it always creates a new entry.  (This is
    useful for types which don't have tags but need special methods
    like non-deterministic sets.
*/
struct FD_TYPE_REGISTRY *fd_register_typecode(fd_lisp_type tp)
{
  if (((int)tp) >= FD_MAX_TYPECODE)
    fd_raise_exception(fd_BadType);
  if (fd_typecode_registry[(int)tp])
    return (fd_typecode_registry[(int)tp]);
  else {
    struct FD_TYPE_REGISTRY *new=fd_register_record(FD_EMPTY_CHOICE);
    (fd_typecode_registry[(int)tp])=new;
    return new;}
}

/** LISP Vectors **/

DTYPES_EXPORT
/* fd_make_vector:
      Arguments: a C integer
      Returns: a vector of a fixed size, initialized to FD_EMPTY_CHOICE
    Makes a vector of the given size.  A vector is implemented as a record
     with the tag VECTOR_TAG and a pointer to an array of size and elements.
*/
lisp fd_make_vector (int size)
{
  lisp_vector v=fd_malloca(struct FD_VECTOR);
  if (size) v->elements=fd_malloc(size*sizeof(lisp));
  else v->elements=NULL;
  v->length=size;
  {/* Initialize the new vector to empty choices */
    lisp *scan = v->elements, *limit = scan + size;
    while (scan < limit) *scan++ = (FD_EMPTY_CHOICE);}
  {RETURN_LISP(vector_type,vector,v);}
}

DTYPES_EXPORT
/* fd_init_vector:
      Arguments: a C integer and a pointer to a vector of LISP objects
      Returns: a vector of a fixed size with the given elements
    Makes a vector of the given size with particular elements.  Called
     by dtio.c to make vectors when reading dtypes.
*/
lisp fd_init_vector (int size,lisp *elts)
{
  lisp_vector v=fd_malloca(struct FD_VECTOR);
  v->elements=elts; v->length=size;
  {RETURN_LISP(vector_type,vector,v);}
}

/* Copies a vector, including copying its elements */
static lisp copy_vector(lisp vec)
{
  lisp_vector v=PTR_DATA(vec,vector), copy;
  lisp *read=v->elements, *limit=read+v->length, *write;
  copy=fd_malloca(struct FD_VECTOR);
  copy->length=v->length;
  if (copy->length == 0) copy->elements=NULL;
  else {
    copy->elements=write=fd_malloc(sizeof(lisp)*v->length);
    while (read < limit) {/* Copy the elements */
      lisp v=*read; *write=incref(v); read++; write++;}}
  {RETURN_LISP(vector_type,vector,copy);}
}

/* Frees a vector, including freeing its elements */
static void free_vector(lisp vec)
{
  lisp_vector v=PTR_DATA(vec,vector);
  if (v->elements) {
    lisp *scan=v->elements, *limit=scan+v->length;
    while (scan < limit) {decref(*scan); scan++;}
    fd_free(v->elements,v->length*sizeof(lisp));}
  fd_qfree(v,sizeof(struct FD_VECTOR)); 
}

/** Garbage collection **/

FASTOP unsigned int decrefs(lisp x)
{
  acons c=PTR_DATA(x,acons);
  if (PTR_TYPE(x)>=64) {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
    if ((rec) && (rec->decref_fcn)) {
      return rec->decref_fcn(x);}}
  if (c->n_refs <= 0) fd_raise_exception("Double GC");
  FD_LOCK_CONS(c);
  c->n_refs--;
  FD_UNLOCK_CONS(c);
  return c->n_refs;
}

DTYPES_EXPORT
/* fd_free_proc:
     Arguments: a lisp object
     Returns: nothing
   If x is reclaimable (e.g. not a symbol, object, integer, etc), reclaim
      the storage used by x for future objects.
   This is used by the inline fd_decref which doesn't bother invoking
    it on non-reclaimable objects.
 */
void fd_free_proc (lisp x)
{
  switch (PTR_TYPE (x)) {
  case fixnum_type:
  case immediate_type:
  case symbol_type:
  case object_type:
  case character_type:
  case zstring_type:
    return;
  case flonum_type: {
    fd_qfree(x.data.fdouble,sizeof(struct FD_DOUBLE));
    break;}
  case string_type: case qstring_type: {
    fd_lisp_string  s = PTR_DATA (x, string);
    fd_xfree(s->data); fd_malloc_adjust(-((s->length)+1));
    fd_qfree(s,sizeof(struct FD_STRING));
    break;}
  case choice_type: case proper_choice_type: case quoted_choice_type: {
    fd_choice ch=PTR_DATA(x,choice); 
    if (ch->elt_type == 0) {
      lisp *elts=ch->elements.lisp, *limit=elts+ch->size;
      while (elts < limit) {decref(*elts); elts++;}}
    else if (ch->elt_type >= FD_ATOMIC_LIMIT) {
      union FD_DATA *elts=ch->elements.data, *limit=elts+ch->size;
      lisp x; x.type=ch->elt_type;
      while (elts < limit) {x.data=*elts++; decref(x);}}
    if (ch->elt_type)
      fd_free(ch->elements.data,sizeof(union FD_DATA)*ch->limit);
    else fd_free(ch->elements.lisp,sizeof(fd_lisp)*ch->limit);
#if FD_THREADS_ENABLED
    pthread_mutex_destroy(&(ch->lock));
#endif
    ch->elements.lisp=NULL; ch->size=0;
    fd_qfree(ch,sizeof(struct FD_CHOICE));
    break;}
  case pair_type: {
    /* This hairy bit of code takes constant stack size
       in the length of the list and avoids stack overflows
       on freeing long lists. */
    lisp n;
    fd_pair p = PTR_DATA (x, pair); int list_valid=1;
    while (list_valid) {
      n=p->cdr; decref(p->car); fd_qfree(p,sizeof(struct FD_PAIR)); 
      if (PRIM_TYPEP(n,pair_type))
	if (decrefs(n) == 0) p=PTR_DATA(n,pair);
	else {n=FD_VOID; break;}
      else list_valid=0;}
    decref(n);
    break;}
  case vector_type: case tail_call_type: case multiple_value_type: {
    free_vector(x); return;}
  case record_type:  {
    lisp tag=RECORD_TAG(x);
    struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
    if (r && (r->gc_fcn)) {r->gc_fcn(x); return;}
    else {
      decref(tag);
      fd_qfree(PTR_DATA(x, record), sizeof(struct FD_RECORD));
      return;}}
  case lrecord_type: {
    lrecord lr=PTR_DATA(x,lrecord);
    decref(lr->tag); decref(lr->data);
    fd_qfree(lr,sizeof(struct FD_LRECORD)); return;}
  case mystery_type: {
    struct FD_MYSTERY *m=(struct FD_MYSTERY *)FD_CPTR_DATA(x);
    if (m->code&0x80) fd_free(m->data.dtypes,sizeof(fd_lisp)*m->length);
    else fd_free(m->data.bytes,m->length);
    fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
    fd_free(m,sizeof(struct FD_MYSTERY));
    return;}
  case bad_type: fd_raise_exception(fd_BadType); return;
  default: {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
    if ((rec) && (rec->gc_fcn)) rec->gc_fcn(x);
    return;}
  }
}

DTYPES_EXPORT
/* _fd_decref_cons:
     Arguments: a lisp object
     Returns: void
   Increments the GC count for x and reclaims it if appropriate.
 */
void _fd_decref_cons (lisp x)
{
  acons c;
  if (PTR_TYPE(x) < FD_GC_LIMIT) return;
  if (PTR_TYPE(x)>=64) {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
    if ((rec) && (rec->decref_fcn)) {
      rec->decref_fcn(x);
      return;}}
  /* Check GC ing */
  c=PTR_DATA(x,acons);
  if (c->n_refs <= 0) fd_raise_exception("Double GC");
  FD_LOCK_CONS(c);
  if (c->n_refs <= 0) {
    FD_UNLOCK_CONS(c);
    fd_raise_exception(_("Double GC"));}
  else c->n_refs--;
  FD_UNLOCK_CONS(c);
  if (c->n_refs) return;
  else fd_free_proc(x);
}


 /** Copying lisp objects **/

DTYPES_EXPORT
/* _fd_copy_lisp_proc:
     Arguments: a lisp object
     Returns: a copy of the object
   This doesn't bother copying fixnums, symbols, objects, or immediates.
   The macro fd_incref is the identify for such objects and calls
   _fd_copy_lisp_proc for everything else.
*/
lisp _fd_copy_lisp_proc (lisp x)
{
  switch (PTR_TYPE (x)) {
  case fixnum_type:
  case immediate_type:
  case symbol_type:
  case object_type:
  case character_type:
  case zstring_type:
    return x;
  case flonum_type: {
    double d=FD_FLOATLISP(x);
    return fd_make_flonum(d);}
  case string_type: case qstring_type: {
    fd_lisp_string  s = PTR_DATA (x, string), c=fd_malloca(struct FD_STRING); 
    int l=s->length; fd_u8char *copy=fd_xmalloc(l+1);
    fd_malloc_adjust(l+1); strcpy (copy, s->data);
    c->length = s->length; c->data = copy; c->utf8=s->utf8;
    {RETURN_LISP (string_type,string,c);}}
  case choice_type: case proper_choice_type: case quoted_choice_type: {
    fd_choice original=PTR_DATA(x,choice), new=fd_malloca(struct FD_CHOICE);
    int size=original->size, limit=original->limit;
#if FD_THREADS_ENABLED
    fd_init_mutex(&(new->lock));
#endif
    new->size=size; new->limit=limit; new->sorted=original->sorted;
    new->elt_type=original->elt_type; new->busy=0;
    if (original->elt_type) {
      new->elements.data=fd_malloc(sizeof(union FD_DATA)*limit);
      if (original->elt_type < FD_ATOMIC_LIMIT)
	memcpy(new->elements.data,original->elements.data,
	       sizeof(union FD_DATA)*fd_choice_size(original));
      else {
	lisp tmp_ptr; 
	union FD_DATA *read=original->elements.data,
	  *limit=read+original->size, *write=new->elements.data;
	tmp_ptr.type=original->elt_type;
	while (read < limit) {
	  tmp_ptr.data=*read++; incref(tmp_ptr);
	  *write++=tmp_ptr.data;}}}
    else {
      lisp *read=original->elements.lisp, *limit=read+original->size,
	*write=new->elements.lisp=fd_malloc(sizeof(lisp)*original->limit);
      while (read < limit) {*write=incref(*read); write++; read++;}}
    {RETURN_LISP(PTR_TYPE(x),choice,new);}}
  case pair_type: {
    lisp first=FD_MAKE_PAIR(copy_lisp(CAR(x)),empty_list), last=first;
    lisp scan=CDR(x);
    while (PAIRP(scan)) {
      lisp new=FD_MAKE_PAIR(copy_lisp(CAR(scan)),empty_list);
      RPLACD(last,new); last=new; scan=CDR(scan);}
    RPLACD(last,copy_lisp(scan));
    return first;}
  case vector_type: case tail_call_type: case multiple_value_type:
    return copy_vector(x);
  case record_type: {
    lisp tag=RECORD_TAG(x);
    struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
    if (r && (r->copy_fcn)) return r->copy_fcn(x);
    else return fd_incref(x);}
  case lrecord_type: {
    lisp tag=LRECORD_TAG(x);
    struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
    if (r && (r->copy_fcn)) return r->copy_fcn(x);
    else {
      lrecord new=fd_malloca(struct FD_LRECORD);
      new->tag=copy_lisp(LRECORD_TAG(x));
      new->data=copy_lisp(LRECORD_DATA(x));
      {RETURN_LISP(lrecord_type,lrecord,new);}}}
  case bad_type: fd_raise_exception(fd_BadType);
  default: {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
    if ((rec) && (rec->copy_fcn))
      return rec->copy_fcn(x);
    else return incref(x);}
  } /*switch PTR_TYPE(x)*/
}

/** Comparing lisp objects **/

DTYPES_EXPORT
/* fd_lisp_equal:
     Arguments: two lisp objects
     Returns: the integer 1 if they're EQUAL, 0 if they're not.
*/
int fd_lisp_equal (lisp key0, lisp key1)
{
  if (ATOMICP(key0))
    if ((key0.data.any) == (key1.data.any)) return 1;
    else if (((PRIM_TYPEP(key0,qstring_type)) && (PRIM_TYPEP(key0,string_type))))
      if ((strcmp(STRING_DATA(key0),STRING_DATA(key1))) == 0) return 1;
      else return 0;
    else return 0;
  else if ((PTR_TYPE(key0)) == (PTR_TYPE(key1)) &&
	   (PTR_DATA(key0,acons) == PTR_DATA(key1,acons)))
    return 1;
  else switch (PTR_TYPE(key0)) {
  case string_type:
    if (!(FD_STRINGP(key1))) return 0;
    else if (STRING_LENGTH(key0) != STRING_LENGTH(key1)) return 0;
    else return (strcmp(STRING_DATA(key0),STRING_DATA(key1))== 0);
  case pair_type:
    if (PTR_TYPE(key1) != pair_type) return 0;
    else if (LISP_EQUAL(CAR(key0),CAR(key1)))
      return (LISP_EQUAL(CDR(key0),CDR(key1)));
    else return 0;
  case choice_type: case proper_choice_type: case quoted_choice_type:
    if (((PTR_TYPE(key0)) != choice_type) &&
	((PTR_TYPE(key1)) != choice_type) &&
	(CHOICE_SIZE(key0) != CHOICE_SIZE(key1)))
      /* If you know the size is right (proper or sorted) but the
	 sizes are different, you know the sets can't be equal */
      return 0;
    /* If key0 is a valid choice (> 1 element) and key1 is not,
       you know they're not equal.  Note that the case where key0
       may not be a proper choice should be handled by the code above */
    else if (!(CHOICEP(key1))) return 0;
    else {
      struct FD_HASHSET hs1, hs2, *s1=&hs1, *s2=&hs2;
      int same_size=0; int missing_elt=0;
      fd_init_hashset(s1,CHOICE_SIZE(key0));
      fd_init_hashset(s2,CHOICE_SIZE(key1));
      {DO_CHOICES(r,key0) fd_hashset_add(s1,r); END_DO_CHOICES;}
      {DO_CHOICES(r,key1) /* choice trouble */ 
	 if ((fd_hashset_get(s1,r)) == 0) {missing_elt=1; break;}
	 else fd_hashset_add(s2,r);
       END_DO_CHOICES;}
      if (hs1.n_keys == hs2.n_keys) same_size=1;
      fd_free_hashset(s1);  fd_free_hashset(s2); 
      if (missing_elt) return 0;
      else return same_size;}
  case vector_type:  case tail_call_type: case multiple_value_type:
    if (PTR_TYPE(key0) != PTR_TYPE(key1)) return 0;
    else if (VECTOR_LENGTH(key0) == VECTOR_LENGTH(key1)) {
      int i=0, size=VECTOR_LENGTH(key1);
      while (i < size)
	if (LISP_EQUAL(VECTOR_REF(key0,i),VECTOR_REF(key1,i))) i++;
	else return 0;
      return 1;}
    else return 0;
  case record_type: case lrecord_type:
    if (PTR_TYPE(key0) != PTR_TYPE(key1)) return 0;
    else {
      lisp tag=RECORD_TAG(key0);
      struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
      if (r && (r->compare_fcn)) {
	return r->compare_fcn(key0,key1);}
      else if (PTR_TYPE(key0) == lrecord_type)
	if ((FD_LISP_EQUAL(LRECORD_TAG(key0),LRECORD_TAG(key1))) &&
	    (FD_LISP_EQUAL(LRECORD_DATA(key0),LRECORD_DATA(key1))))
	  return 1;
	else return 0;
      else return 0;}
  case character_type: case immediate_type: case fixnum_type:
    if (PTR_TYPE(key0) != PTR_TYPE(key1)) return 0;
    else return ((key0.data.fixnum) == (key1.data.fixnum));
  case symbol_type: case object_type: 
    if (PTR_TYPE(key0) != PTR_TYPE(key1)) return 0;
    else return ((key0.data.any) == (key1.data.any));
  case flonum_type:
    if (PTR_TYPE(key0) != PTR_TYPE(key1)) return 0;
    else {
      double d1=FD_FLOATLISP(key0), d2=FD_FLOATLISP(key1);
      return (d1 == d2);}
  case bad_type: fd_raise_exception(fd_BadType);
  default: {
    struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(key0));
    if ((rec) && (rec->compare_fcn))
      return rec->compare_fcn(key0,key1);
    else return 0;}
  }
  return 0; /* Should never be reached */
}

/** Iterating over structures **/

DTYPES_EXPORT
/* fd_for_elts:
    Arguments: a function which returns void from a lisp object and
               a list of lisp objects
    Returns: nothing
   This applies the function to all the elements of the list.
*/
void fd_for_elts(void (*fcn)(lisp x),lisp elts)
{
  while (PRIM_TYPEP(elts,pair_type))
    {fcn(CAR(elts)); elts=CDR(elts);}
  if (!(FD_EMPTY_LISTP(elts))) fcn(elts);
}

DTYPES_EXPORT
/* fd_memberp:
     Arguments: a lisp object and a list of such objects
     Returns: 1 if the object is in the list and 0 otherwise
*/
int fd_memberp(lisp x,lisp list)
{
  if (FD_EMPTY_LISTP(list)) return 0;
  else if (PRIM_TYPEP(list,pair_type))
    {DOLIST(elt,list) if (LISP_EQUAL(elt,x)) return 1;
     return 0;}
  else fd_type_error(_("fd_memberp: not a list"),list);
  return 0; /* Never reached */
} 

/** Dealing with LISP queues **/

DTYPES_EXPORT
/* fd_init_queue:
    Arguments: a pointer to a lisp queue structure
    Returns: nothing
 Initializes a lisp queue structure
*/
void fd_init_queue(struct FD_LISP_QUEUE *q)
{
  q->n=0; q->lim=0; q->refs=NULL; fd_init_mutex(&(q->lock));
}

DTYPES_EXPORT
/* fd_destroy_queue:
    Arguments: a pointer to a lisp queue structure
    Returns: nothing
 Removes storage used by a lisp queue structure.
 This does not free the structure itself, since it might
  be statically allocated or part of another structure.
*/
void fd_destory_queue(struct FD_LISP_QUEUE *q)
{
  if (q->refs) fd_free(q->refs,sizeof(fd_lisp)*q->lim);
  q->n=0; q->lim=0;
  fd_destroy_mutex(&(q->lock));
}

DTYPES_EXPORT
/* fd_add_to_queue:
    Arguments: a pointer to a lisp queue structure and a lisp pointer
    Returns: nothing
 Adds the item to the lisp cue, incref'ing it.
*/
void fd_add_to_queue(struct FD_LISP_QUEUE *q,fd_lisp item)
{
  fd_lock_mutex(&(q->lock));
  if (q->n >= q->lim) {
    int new_size=((q->lim) ? (q->lim*2) : 64);
    q->refs=fd_realloc(q->refs,new_size*sizeof(fd_lisp),q->lim*sizeof(fd_lisp));
    q->lim=new_size;}
  if (!(FD_LISP_EQ(q->refs[q->n],item))) q->refs[q->n++]=fd_incref(item);
  fd_unlock_mutex(&(q->lock));
}

STATIC_INLINE void swap(fd_lisp *a,fd_lisp *b)
{
  fd_lisp t;
  t = *a;
  *a = *b;
  *b = t;
}

static void sort_oidv(fd_lisp *v,int n)
{
  unsigned i, j, ln, rn;
  while (n > 1) {
    swap(&v[0], &v[n/2]);
    for (i = 0, j = n; ; ) {
      do --j; while (v[j].data.any > v[0].data.any);
      do ++i; while (i < j && v[i].data.any < v[0].data.any);
      if (i >= j) break; swap(&v[i], &v[j]);}
    swap(&v[j], &v[0]);
    ln = j;
    rn = n - ++j;
    if (ln < rn) {
      sort_oidv(v, ln); v += j; n = rn;}
    else {sort_oidv(v + j, rn); n = ln;}}
}
static int compress_oidv(fd_lisp *v,int n)
{
  int removals=0;
  fd_lisp *read=v, *limit=v+n;
  fd_lisp current=*read++, *write=read;
  while (read < limit)
    if (read->data.any == current.data.any) {
      read++; removals++;}
    else if (read == write) {current=*read; read++; write++;}
    else {
      write->data.any=read->data.any; current=*read; read++; write++;}
  return n-removals;
}

DTYPES_EXPORT
/* fd_simplify_queue:
    Arguments: a pointer to a lisp queue structure
    Returns: nothing
 Sorts and removes duplicates from the lisp queue.
*/
void fd_simplify_queue(struct FD_LISP_QUEUE *q)
{
  fd_lock_mutex(&(q->lock));
  if (q->n > 1) {
    sort_oidv(q->refs,q->n);
    q->n=compress_oidv(q->refs,q->n);}
  fd_unlock_mutex(&(q->lock));
}

/** Dealing with big buffers **/

DTYPES_EXPORT
/* fd_get_big_buffer:
     Arguments: a pointer to an int
     Returns: a pointer to a large char array

 Allocates a big buffer, with the size determined by
the environment variable FD_BUFFER_SIZE or the runtime
define FD_BIGBUFF_DEFAULT.  This value is stored in the int
pointed to by the argument. */
char *fd_get_big_buffer(unsigned int *bufsize)
{
  lisp big_buf_init=fd_getenv("FD_BUFFER_SIZE");
  unsigned int bigbuf;
  if (FIXNUMP(big_buf_init)) bigbuf=FIXLISP(big_buf_init);
  else if (STRINGP(big_buf_init))
    bigbuf=strtol(STRING_DATA(big_buf_init),NULL,10);
  else bigbuf=FD_BIGBUFF_DEFAULT;
  *bufsize=bigbuf;
  if (bigbuf) return fd_xmalloc(bigbuf);
  else return NULL;
}

/** Module registry **/

#if FD_THREADS_ENABLED
fd_mutex module_registry_lock;
#endif

static lisp modules;
static int modules_initialized=0;

DTYPES_EXPORT
/* fd_get_modules:
    Arguments: none
    Returns: a list of modules
 Make a copy of all the registered modules. */
lisp fd_get_modules()
{
  if (modules_initialized) {
    lisp answer;
    lock_mutex(&module_registry_lock);
    answer=copy_lisp(modules);
    unlock_mutex(&module_registry_lock);
    return answer;}
  else return FD_EMPTY_LIST;
}

DTYPES_EXPORT
/* fd_register_source_file:
    Arguments: three strings -- name, date, and details
    Returns: void
 Registers a module.  Name is the filename (basename), date is the
 compilation date for the file, and details is typically the RCSID tag. */
void fd_register_source_file(char *name,char *date,char *details)
{
  if (modules_initialized == 0) {
    modules_initialized=1; modules=FD_EMPTY_LIST;}
  lock_mutex(&module_registry_lock);
  modules=
    FD_MAKE_PAIR
    (FD_MAKE_LIST(3,fd_make_string(name),fd_make_string(date),
		  fd_make_string(details)),
     modules);
  unlock_mutex(&module_registry_lock);  
}

DTYPES_EXPORT 
/* fd_source_file_registeredp:
    Arguments: a filename (basename)
    Returns: 1 if the module has been registered, 0 otherwise
*/
int fd_source_file_registeredp(char *name)
{
  char *copy=fd_strdup(name), *scan=copy; lisp sym; int found=0;
  while (*scan) {*scan=toupper(*scan); scan++;}
  sym=fd_make_symbol(copy);
  lock_mutex(&module_registry_lock);
  {DOLIST(elt,modules) 
     if (LISP_EQ(CAR(elt),sym)) {found=1; break;}}
  fd_xfree(copy); unlock_mutex(&module_registry_lock);
  return found;
}

/** Initializing the heap, symbols, etc... **/

static int heap_initialized=0;


/* fd_initialize_data_c
     Arguments: none
     Returns: nothing
  Initializes heap and related variables
  Sets the variable fd_dtypes_initialized to avoid calling itself twice.
*/
void fd_initialize_data_c ()
{
  if (heap_initialized) return;
#if FD_THREADS_ENABLED
  fd_init_mutex(&module_registry_lock);
  {int i=0;
   while (i < FD_N_CELL_LOCKS) {
     fd_init_mutex(&(fd_cell_locks[i])); i++;}}
  {int i=0;
   while (i < FD_N_CONS_LOCKS) {
     fd_init_mutex(&(fd_cons_locks[i])); i++;}}
#endif  
  empty_list=(FD_EMPTY_LIST);
  heap_initialized=1;

  {int i=0; while (i < FD_MAX_TYPECODE) fd_typecode_registry[i++]=NULL;}

  fd_register_source_file("data",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: data.c,v $
   Revision 1.19  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.18  2004/09/08 18:36:51  haase
   Added GC of mystery types

   Revision 1.17  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.16  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.15  2004/01/10 21:11:16  haase
   Added fd_simplify_queue to remove duplicates, sort, etc.

   Revision 1.14  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.13  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.12.2.4  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.12.2.3  2003/08/02 13:39:56  haase
   Added LISP pointer queues

   Revision 1.12.2.2  2003/01/26 20:34:35  haase
   Misc. fixes, introduced zstrings

   Revision 1.12.2.1  2002/08/09 19:13:11  haase
   Moved towards cleaner RETURN_LISP

   Revision 1.12  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.11  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.10  2002/04/17 11:46:11  haase
   Switched internal UTF-8 representation to real UTF8

   Revision 1.9  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.8  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
