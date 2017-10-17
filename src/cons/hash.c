/* Mode: C */

/* hash.c
   This file implements various hash-based search functions, including
   fast hash tables, hash sets, symbol tables, and object tables for
   FramerD.
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

static char vcid[]="$Id: hash.c,v 1.42 2007/06/30 16:21:06 haase Exp $";

/** Macros and Utilities **/
/** Hash functions **/
/** Hashtables **/
/** Hashsets **/
/** Freeing hashtables and hashsets **/
/** Interning Symbols **/
/** Interning OIDs **/
/** The initialization function **/

#define FD_INLINE_STRING_STREAMS 1

#include "dtypes.h"

fd_exception fd_HashTableOverflow=_("Hash table too large");
static fd_exception ephemeral_misuse=_("Misuse of ephemeral hash table");

static void initialize_symbol_table(void);
static void initialize_qstring_table(void);

/** Macros and Utilities **/

/* This makes a number fit into 24 bits and use it in keeping hash
    codes down.  */
#define MAGIC_MULTIPLIER 2654435769UL /* 6125423371 */
#define MAGIC_MODULUS 16777213
#define DEBUGGING_HASHING 0

#define twist_word(x) ((x>>16)|(x<<16))
#define jumble_word(x) \
  (((x&0xFF000000)>>16) | ((x&0x0000FF00)>>8) | \
   ((x&0x000000FF)<<16) | ((x&0x00FF0000)<<8))

#define hash_combine(x,y) ((twist_word(x))^(y))
#define hash_combine2(x,y) ((twist_word(y))^(x))
#define hash_fixdata(x) \
  ((((unsigned int)((x)%(MAGIC_MODULUS)))*(MAGIC_MULTIPLIER))%(MAGIC_MODULUS))
#define hash_lisp(x) \
  ((PTR_TYPE(x)<FD_ATOMIC_LIMIT) ? \
   (hash_fixdata((x).data.fixnum)) :\
   (hash_lisp_proc(x)))
static unsigned int hash_lisp_proc(lisp x);

#if 0
FASTOP unsigned int get_chain(unsigned int hash,int size)
{
  int chain=(hash%(size-2));
  if (chain == 0) return size-2;
  else return chain;
}
#else
FASTOP unsigned int get_chain(unsigned int hash,int size)
{
  /* This just does linear hashing */
  return 1;
}
#endif

/* These are all the higher of prime pairs around powers of 2.  They are
    used to select good hashtable sizes. */
static unsigned int hashtable_sizes[]=
 {19, 43, 73, 139, 271, 523, 1033, 2083, 4129, 8221, 16453,
  32803, 65539, 131113, 262153, 524353, 1048891, 2097259,
  4194583, 8388619, 16777291, 32000911, 64000819, 128000629,
  256001719, 0};

/* This finds a hash table size which is larger than min */
DTYPES_EXPORT
/* fd_select_table_size:
     Arguments: an unsigned int (min)
     Returns: an unsigned int greater than min
  Selects a hashtable/index size greater than min. */
unsigned int fd_select_table_size(unsigned int min)
{
  unsigned int i=0;
  while (hashtable_sizes[i])
    if (hashtable_sizes[i] > min) return hashtable_sizes[i];
    else i++;
  fd_raise_exception(fd_HashTableOverflow);
  return 0; /* Never reached */
}

static void grow_hashtable(struct FD_HASHTABLE *h,int minsize);
static void grow_hashset(struct FD_HASHSET *h,int minsize);

/** Hash functions **/

/* hash_string: (static, inline)
     arguments: a string
     Returns: an integer computed from the characters of the string
*/
FASTOP int hash_string (const char *string, int size)
{
  unsigned int value;	 /* Used to compute the hash value.  */
  int index;             /* Used to cycle through random values. */

  value = 0x238F13AF * size;
  for (index = 0; index < size; index++)
    value = (value + (string[index] << (index * 5 % 24)))
      & 0x7FFFFFFF;

  value = (1103515243 * value + 12345) & 0x7FFFFFFF;
  return value;
}

/* hash_utf8_string: (static, inline)
     arguments: a string
     Returns: an integer computed from the characters of the string
*/
static int hash_utf8_string (unsigned char *string,int size)
{
  unsigned int value;	 /* Used to compute the hash value.  */
  int index=0;           /* Used to cycle through random values. */
  fd_u8char *scan=string, *limit=string+size;   /* Used to read from string */

  value = 0x238F13AF * size;
  for (index = 0; scan < limit; index++) {
    int ch=fd_sgetc(&scan);
    value = (value + (ch << (index * 5 % 24)))
      & 0x7FFFFFFF;}
  value = (1103515243 * value + 12345) & 0x7FFFFFFF;
  return value;
}

/* hash_elts: (static)
     arguments: a pointer to a vector of LISP pointers and a size
     Returns: combines the elements' hashes into a single hash
*/
static unsigned int hash_elts(lisp *x,unsigned int n)
{
  lisp *limit=x+n; int sum=0;
  while (x < limit) {
    unsigned int h=hash_lisp(*x);
    sum=hash_combine(sum,h); sum=sum%(MAGIC_MODULUS); x++;}
  return sum;
}

/* hash_record: (static)
     arguments: a LISP pointer to a record object
     Returns: computes the objects hash code using the function
 in the registry
*/
static unsigned int hash_record(lisp x)
{
  lisp tag=RECORD_TAG(x);
  struct FD_TYPE_REGISTRY *r=fd_lookup_record(tag);
  if ((r) && (r->hash_fcn)) return r->hash_fcn(x,hash_lisp_proc);
  else return hash_lisp(tag);
}

/* hash_lisp_proc:
    Arguments: a lisp pointer
    Returns: a hash code for the object
*/
static unsigned int hash_lisp_proc(lisp x)
{
  switch (PTR_TYPE(x)) {
  case choice_type: case proper_choice_type: {
    unsigned int max=0;
    DO_CHOICES(r,x) {
      unsigned int h=hash_lisp(r); if (h > max) max=h;} END_DO_CHOICES;
    return max;}
  case quoted_choice_type: {
    unsigned int max=0; fd_lisp unq=FD_UNQUOTE_CHOICE(x);
    DO_CHOICES(r,unq) {
      unsigned int h=hash_lisp(r); if (h > max) max=h;} END_DO_CHOICES;
    return max;}
  case pair_type: {
    lisp ptr=x; unsigned int sum=0;
    /* The shift here is introduced to make the hash function asymmetric */
    while (PTR_TYPE(ptr) == pair_type) {
      unsigned int h=hash_lisp(CAR(ptr));
      sum=((sum<<4)+h)%(MAGIC_MODULUS);
      ptr=CDR(ptr);}
    if (!(FD_EMPTY_LISTP(ptr))) {
      unsigned int cdr_hash=hash_lisp(ptr);
      sum=(sum+(flip_word(cdr_hash)>>8))%(MAGIC_MODULUS);}
    return sum;}
  case string_type: {
    fd_lisp_string  s=PTR_DATA(x,string);
    if (s->utf8)
      return hash_utf8_string(STRING_DATA(x),STRING_LENGTH(x));
    else return hash_string(STRING_DATA(x),STRING_LENGTH(x));}
  case vector_type: case tail_call_type: case multiple_value_type: {
    lisp_vector v=PTR_DATA(x,vector);
    return hash_elts(v->elements,v->length);}
  case slotmap_type: {
    fd_slotmap sm=PTR_DATA(x,slotmap);
    int i=0, limit=sm->size, sum=0;
    while (i < limit) {
      sum=sum+hash_lisp(sm->schema[i]);
      sum=sum+hash_lisp(sm->values[i]);
      i++;}
    if (limit == 0) return 1;
    return (sum/limit)%MAGIC_MODULUS;}
  case record_type:
    return hash_record(x);
  case lrecord_type:
    return (((hash_lisp(LRECORD_TAG(x)))%(MAGIC_MODULUS))+
	    ((hash_lisp(LRECORD_DATA(x)))%(MAGIC_MODULUS)))%(MAGIC_MODULUS);
  case symbol_type: case object_type: case fixnum_type: case immediate_type:
  case qstring_type: case zstring_type:
  case character_type:
    return hash_fixdata((x).data.fixnum);
  case bad_type:
    fd_raise_exception(fd_BadType);
  case flonum_type: {
    double d=FD_FLOATLISP(x); return (((int)d)%(MAGIC_MODULUS));}
  default: {
      struct FD_TYPE_REGISTRY *rec=fd_lookup_typecode(PTR_TYPE(x));
      if ((rec) && (rec->hash_fcn))
	return rec->hash_fcn(x,hash_lisp_proc);
      else if ((rec) && (rec->compare_fcn == fd_compare_cptrs))
	return ((fd_intptr)(CPTR_DATA(x)))%(MAGIC_MODULUS);
      else return ((int)((x.data.fixnum)%(MAGIC_MODULUS)));}
  }
}

DTYPES_EXPORT
/* fd_hash_lisp:
    Arguments: a lisp pointer
    Returns: an unsigned int

  Returns the hash for a lisp object.  This hash is *not*
  portable across sessions. */
unsigned int fd_hash_lisp(lisp x)
{
  return hash_lisp(x);
}

/** Hashtables **/

/* hashtable_get: (static)
    Arguments: a pointer to a hashtable, a key, and a pointer to an uint
    Returns: a pointer to a PAIR struct or NULL

   This is the core loop for hashtables, which looks down the hashtable
   using dual hashing and either returns the entry it finds or NULL.  In
   either case, it deposits the slot at which it stopped into the uint
   pointer it is passed as its last argument.  This is either where the
   pair lives or where it should go.

   Empty slots in a hashtable are indicated by a NULL; full slots contain
   pairs of key and value.
*/
FASTOP
struct FD_PAIR *hashtable_get
               (struct FD_HASHTABLE *h,lisp key,unsigned int *slot)
{
  int size=h->n_slots; struct FD_PAIR **table=h->table; int probes=1;
  unsigned int hash=hash_lisp(key), probe=hash%size;
  unsigned int chain=get_chain(hash,size);
  struct FD_PAIR *entry;
  if (ATOMICP(key))
    while ((entry=table[probe]))
      if ((LISP_EQ(entry->car,key))) {
	*slot=probe; return entry;}
      else {
	probe=(probe+chain)%size; probes++;
#if DEBUGGING_HASHING
	fd_warn(_("Miss %d: %q/%q hash=%d/%d chain=%d n=%d/%d"),
		probes,key,entry->car,hash,hash_lisp(entry->car),chain,
		h->n_keys,size);
#endif
      }
  else while ((entry=table[probe]))
    if (LISP_EQUAL(key,entry->car)) {
      *slot=probe;
#if DEBUGGING_HASHING
      if (probes>2)
	fd_warn(_("%d probes for %q, hash=%d/%d"),probes,key,hash,size);
#endif
      return entry;}
    else {
      probe=(probe+chain)%size; probes++;
#if DEBUGGING_HASHING
      fd_warn(_("Miss %d: %q/%q hash=%d/%d chain=%d n=%d/%d"),
	      probes,key,entry->car,hash,hash_lisp(entry->car),chain,
	      h->n_keys,size);
#endif
    }
#if DEBUGGING_HASHING
  if (probes <= 2)
    fd_warn(_("Retrieved %q (hash=%d) with just %d probes"),key,hash,probes);
#endif
  *slot=probe;
  return NULL;
}

DTYPES_EXPORT
/* fd_hashtable_get:
    Arguments: a pointer to a hashtable, a lisp key, and a default
    Returns: a lisp object

   Returns the value associated with the key in the hashtable or the
    given default value if there is no such assocation.  Note that
    this does *not* copy the value returned from the table.
*/
lisp fd_hashtable_get(fd_hashtable h,lisp key,lisp dflt)
{
  unsigned int slot_no; struct FD_PAIR *entry; lisp answer;
  fd_read_lock(&(h->lock));
  if (h->n_keys == 0) {
    unlock_mutex(&(h->lock)); return incref(dflt);}
  entry=hashtable_get(h,key,&slot_no);
  if (entry) answer=incref(entry->cdr); else answer=incref(dflt);
  fd_read_unlock(&(h->lock));
  return answer;
}

DTYPES_EXPORT
/* fd_hashtable_test:
    Arguments: a pointer to a hashtable, a lisp key, and a lisp value
    Returns: 1 or 0

   Returns 1 if the value is one of the values associated with the key
*/
int fd_hashtable_test(fd_hashtable h,lisp key,lisp value)
{
  unsigned int slot_no; struct FD_PAIR *entry; int found;
  fd_read_lock(&(h->lock));
  if (h->n_keys == 0) {
    unlock_mutex(&(h->lock)); return 0;}
  entry=hashtable_get(h,key,&slot_no);
  if (entry)
    if (FD_CHOICEP(entry->cdr))
      found=(fd_choice_containsp(value,entry->cdr));
    else found=FD_LISP_EQUAL(value,entry->cdr);
  else found=0;
  fd_read_unlock(&(h->lock));
  return found;
}

DTYPES_EXPORT
/* fd_hashtable_probe:
    Arguments: a pointer to a hashtable, a lisp key
    Returns: 1 or 0

   Returns 1 if the key is associated with some value in the table.
*/
int fd_hashtable_probe(fd_hashtable h,lisp key)
{
  unsigned int slot_no, answer; fd_pair p;
  fd_read_lock(&(h->lock));
  if (h->n_keys == 0) {
    unlock_mutex(&(h->lock)); return 0;}
  p=hashtable_get(h,key,&slot_no);
  if (p)
    if (!(FD_VOIDP(p->cdr))) answer=1; else answer=0;
  else answer=0;
  fd_read_unlock(&(h->lock));
  return answer;
}

static void hashtable_set(fd_hashtable h,lisp key,lisp value)
{
  unsigned int slot_no; struct FD_PAIR *entry;
  entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    decref(entry->cdr);
    if (CHOICEP(value))
      entry->cdr=copy_lisp(value);
    else entry->cdr=incref(value);}
  else if (h->n_keys*2 > h->n_slots) {
    grow_hashtable(h,h->n_slots); hashtable_set(h,key,value);}
  else {
    fd_pair p=fd_malloca(struct FD_PAIR);
    p->car=incref(key);
    if (CHOICEP(value))
      p->cdr=copy_lisp(value); else p->cdr=incref(value);
    h->table[slot_no]=p; h->n_keys++;}
}

static void hashtable_set_nc(fd_hashtable h,lisp key,lisp value)
{
  unsigned int slot_no; struct FD_PAIR *entry;
  entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    decref(entry->cdr);
    entry->cdr=incref(value);}
  else if (h->n_keys*2 > h->n_slots) {
    grow_hashtable(h,h->n_slots); hashtable_set(h,key,value);}
  else {
    fd_pair p=fd_malloca(struct FD_PAIR);
    p->car=incref(key);
    if (CHOICEP(value))
      p->cdr=copy_lisp(value); else p->cdr=incref(value);
    h->table[slot_no]=p; h->n_keys++;}
}

static void hashtable_init_value(fd_hashtable h,lisp key,lisp value)
{
  unsigned int slot_no; struct FD_PAIR *entry;
  entry=hashtable_get(h,key,&slot_no);
  if (entry) return;
  else if (h->n_keys*2 > h->n_slots) {
    grow_hashtable(h,h->n_slots); hashtable_set(h,key,value);}
  else {
    fd_pair p=fd_malloca(struct FD_PAIR);
    p->car=incref(key);
    if (CHOICEP(value))
      p->cdr=copy_lisp(value); else p->cdr=incref(value);
    h->table[slot_no]=p; h->n_keys++;}
}

DTYPES_EXPORT
/* fd_hashtable_set:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Associates the value with the key in the hashtable.  If the key is new
   it copies both key and value; if the key already had an association, that
   value is freed and a copy of the new value replaces it.
*/
void fd_hashtable_set(fd_hashtable h,lisp key,lisp value)
{
  fd_write_lock(&(h->lock));
  hashtable_set(h,key,value);
  fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_hashtable_set_nc:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Associates the value with the key in the hashtable.  If the key is new
   it copies the key; this will not choice a choice value.
*/
void fd_hashtable_set_nc(fd_hashtable h,lisp key,lisp value)
{
  fd_write_lock(&(h->lock));
  hashtable_set_nc(h,key,value);
  fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_hashtable_init:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Associates the value with the key in the hashtable if it does not already
   have an associated value.
*/
void fd_hashtable_init_value(fd_hashtable h,lisp key,lisp value)
{
  fd_write_lock(&(h->lock));
  hashtable_init_value(h,key,value);
  fd_write_unlock(&(h->lock));
}


DTYPES_EXPORT
/* fd_hashtable_set_nolock:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Associates the value with the key in the hashtable.  If the key is new
   it copies both key and value; if the key already had an association, that
   value is freed and a copy of the new value replaces it.
*/
void _fd_hashtable_set_nolock(fd_hashtable h,lisp key,lisp value)
{
  hashtable_set(h,key,value);
}

DTYPES_EXPORT
/* fd_hashtable_zap:
    Arguments: a pointer to a hashtable and a lisp key
    Returns: nothing

   Removes all values associated key in the hashtable.
*/
void fd_hashtable_zap(fd_hashtable h,lisp key)
{
  fd_hashtable_set(h,key,(FD_EMPTY_CHOICE));
}

static void hashtable_add(fd_hashtable h,lisp key,lisp value)
{
  struct FD_PAIR *entry; unsigned int slot_no; 
  entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    if (FD_EMPTYP(entry->cdr))
      if (CHOICEP(value))
	entry->cdr=copy_lisp(value);
      else entry->cdr=incref(value);
    else if (CHOICEP(entry->cdr)) {
      lisp v=entry->cdr;
      DO_CHOICES(r,value) {
	lisp nv=incref(r); _fd_add_to_choice(nv,v);}
      END_DO_CHOICES;}
    else if (CHOICEP(value)) {
      lisp new=copy_lisp(value); _fd_add_to_choice(entry->cdr,new);
      entry->cdr=new;}
    else if (LISP_EQUAL(entry->cdr,value)) {}
    else entry->cdr=_fd_binary_choice(entry->cdr,incref(value));}
  else if (h->n_keys*2 > h->n_slots) {
    grow_hashtable(h,h->n_slots);
    hashtable_add(h,key,value);
    return;}
  else {
    fd_pair p=fd_malloca(struct FD_PAIR);
    p->car=incref(key);
    if (CHOICEP(value)) p->cdr=copy_lisp(value);
    else p->cdr=incref(value);
    h->table[slot_no]=p; h->n_keys++;}
}

DTYPES_EXPORT
/* fd_hashtable_add:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Adds a value to the values associated with key in the hashtable.  If the
    key is already associated with multiple values, a copy of the new value
    is added to it.  If there is one association, a new non-deterministic
    set is created.  And if there is not association, this is just the
    same as fd_hashtable_set.
*/
void fd_hashtable_add(fd_hashtable h,lisp key,lisp value)
{
  if (FD_EMPTYP(value)) return;
  fd_write_lock(&(h->lock));
  hashtable_add(h,key,value);
  fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_hashtable_drop:
    Arguments: a pointer to a hashtable, a lisp key, and a value
    Returns: nothing

   Removes a value from the values associated with key in the hashtable.
*/
void fd_hashtable_drop(fd_hashtable h,lisp key,lisp value)
{
  unsigned int slot_no; 
  struct FD_PAIR *entry;
  if (FD_EMPTYP(value)) return;
  fd_write_lock(&(h->lock));
  entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    if (FD_EMPTYP(entry->cdr)) {}
    else if (CHOICEP(entry->cdr))
      entry->cdr=fd_remove_from_choice(value,entry->cdr);
    else if ((CHOICEP(value)) ?
	     (fd_choice_containsp(entry->cdr,value)) :
	     (FD_LISP_EQUAL(value,entry->cdr))) {
      decref(entry->cdr); entry->cdr=FD_EMPTY_CHOICE;}}
  fd_write_unlock(&(h->lock));
}

static int hashtable_increment(fd_hashtable h,lisp key,int increment)
{
  unsigned int slot_no; 
  struct FD_PAIR *entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    if (FD_EMPTYP(entry->cdr))
      entry->cdr=LISPFIX(increment);
    else if (FIXNUMP(entry->cdr)) 
      entry->cdr=LISPFIX(FIXLISP(entry->cdr)+increment);
    else return -1;}
  else if (h->n_keys*2 > h->n_slots) {
    grow_hashtable(h,h->n_slots);
    return hashtable_increment(h,key,increment);}
  else {
    fd_pair p=fd_malloca(struct FD_PAIR);
    p->car=incref(key);
    p->cdr=LISPFIX(increment);
    h->table[slot_no]=p; h->n_keys++;
    return 1;}
}

DTYPES_EXPORT
/* fd_hashtable_increment:
    Arguments: a pointer to a hashtable, a lisp key, and an int
    Returns: nothing

   Increments the value associated with the key by a number, simply
    storing the number if no value is currently associated.
*/
void fd_hashtable_increment(fd_hashtable h,lisp key,int increment)
{
  int retcode=-1;
  if (increment == 0) return;
  fd_write_lock(&(h->lock));
  retcode=hashtable_increment(h,key,increment);
  fd_write_unlock(&(h->lock));
  if (retcode < 0)
    fd_type_error(_("fd_hashtable_increment: association is not a number"),
		  key);
}

static int hashtable_increment_existing(fd_hashtable h,lisp key,int increment)
{
  unsigned int slot_no; 
  struct FD_PAIR *entry=hashtable_get(h,key,&slot_no);
  if (entry) {
    if (FD_EMPTYP(entry->cdr))
      entry->cdr=LISPFIX(increment);
    else if (FIXNUMP(entry->cdr)) 
      entry->cdr=LISPFIX(FIXLISP(entry->cdr)+increment);
    else return -1;
    return 1;}
  return 0;
}

DTYPES_EXPORT
/* fd_hashtable_increment_existing:
    Arguments: a pointer to a hashtable, a lisp key, and an int
    Returns: nothing

   Increments the value associated with the key by a number, doing
nothing if the key does not exist.
*/
void fd_hashtable_increment_existing(fd_hashtable h,lisp key,int increment)
{
  int retcode=-1;
  if (increment == 0) return;
  fd_write_lock(&(h->lock));
  retcode=hashtable_increment_existing(h,key,increment);
  fd_write_unlock(&(h->lock));
  if (retcode < 0)
    fd_type_error
      (_("fd_hashtable_increment_existing: association is not a number"),
       key);
}

/* Internal function for growing a hashtable to a new size.
   (Not threadsafe).  */
static void grow_hashtable(struct FD_HASHTABLE *h,int minsize)
{
  unsigned int new_size=fd_select_table_size(minsize);
  if (new_size > h->n_slots) {
    struct FD_PAIR **new_table=fd_malloc(sizeof(struct FD_PAIR *)*new_size);
    struct FD_PAIR **scan=h->table, **limit=h->table+h->n_slots;
    fd_pair *nscan=new_table, *nlimit=nscan+new_size;
    while (nscan < nlimit) *nscan++=NULL;
    while (scan < limit)
      if (*scan) {
	struct FD_PAIR *p=*scan++;
	unsigned int new_hash=hash_lisp(p->car);
	unsigned int probe=new_hash%new_size;
	unsigned int chain=get_chain(new_hash,new_size);
	while (new_table[probe]) probe=(probe+chain)%new_size;
	new_table[probe]=p;}
      else scan++;
    fd_free(h->table,h->n_slots*sizeof(struct FD_PAIR *));
    h->n_slots=new_size; h->table=new_table;}
}

DTYPES_EXPORT
/* fd_grow_hashtable:
     Arguments: a pointer to a hashtable and an int minsize
     Returns: nothing
  Grows hashtable to have at least minsize slots */
void fd_grow_hashtable(struct FD_HASHTABLE *h,int minsize)
{
  fd_write_lock(&(h->lock));
  grow_hashtable(h,minsize);
  fd_write_unlock(&(h->lock));
}


/* hashtable_get: (static)
    Arguments: a pointer to a hashtable, a key, and a pointer to an uint
    Returns: a pointer to a PAIR struct or NULL

   This is the core loop for hashtables, which looks down the hashtable
   using dual hashing and either returns the entry it finds or NULL.  In
   either case, it deposits the slot at which it stopped into the uint
   pointer it is passed as its last argument.  This is either where the
   pair lives or where it should go.

   Empty slots in a hashtable are indicated by a NULL; full slots contain
   pairs of key and value.
*/
DTYPES_EXPORT
fd_lisp fd_hashtable_strget(struct FD_HASHTABLE *h,fd_u8char *key,int glen)
{
  int size=h->n_slots; struct FD_PAIR **table=h->table;
  int probes=1, len=((glen<0) ? (strlen(key)) : (glen));
  unsigned int hash=hash_utf8_string(key,len), probe=hash%size;
  unsigned int chain=get_chain(hash,size);
  struct FD_PAIR *entry;
  while ((entry=table[probe]))
    if ((FD_STRINGP(entry->car)) && (FD_STRING_LENGTH(entry->car) == len) &&
	((strncmp(key,FD_STRING_DATA(entry->car),len)) == 0))
      return fd_incref(entry->cdr);
    else {
      probe=(probe+chain)%size; probes++;}
  return FD_EMPTY_CHOICE;
}

DTYPES_EXPORT
/* fd_init_hashtable:
     Arguments: a pointer to a hashtable and an int minsize
     Returns: nothing

  Initializes the table for use with at least minsize slots.
  This can be used for either a malloc'd hashtable or a hashtable
  on the stack. */
void fd_init_hashtable(fd_hashtable h,int minsize)
{
  int size=fd_select_table_size(minsize); fd_pair *scan, *limit;
  scan=h->table=fd_malloc(sizeof(struct FD_PAIR *)*size);
  h->n_keys=0; h->n_slots=size; limit=scan+size;
  while (scan < limit) *scan++=NULL;
#if FD_THREADS_ENABLED
  fd_init_rwlock(&(h->lock));
#endif
}

DTYPES_EXPORT
/* fd_make_hashtable:
     Arguments: an int minsize
     Returns: a pointer to a hashtable
  This mallocs a new hashtable and initializes it to have at least minsize
   elements. */
struct FD_HASHTABLE *fd_make_hashtable(int minsize)
{
  struct FD_HASHTABLE *new=fd_malloc(sizeof(struct FD_HASHTABLE));
  fd_init_hashtable(new,minsize);
  return new;
}

DTYPES_EXPORT
/* fd_cleanup_locked_hashtable:
     Arguments: a pointer to a hashtable
     Returns: nothing
  Removes empty elements from the hashtable. */
void fd_cleanup_locked_hashtable(fd_hashtable h)
{
  fd_pair *new=fd_malloc(sizeof(fd_pair)*h->n_slots);
  fd_pair *table=h->table, *scan=table, *limit=scan+h->n_slots;
  h->table=new; memset(h->table,0,sizeof(fd_pair)*h->n_slots); h->n_keys=0;
  while (scan < limit) {
    fd_pair p=*scan++;
    if (p) {
      if ((FD_EMPTYP(p->cdr)) || (FD_VOIDP(p->cdr))) {
	decref(p->car); fd_qfree(p,sizeof(struct FD_PAIR));}
      else {
	fd_lisp key=p->car; unsigned int slot_no;
	struct FD_PAIR *entry=hashtable_get(h,key,&slot_no);
	assert(entry == NULL);
	new[slot_no]=p; h->n_keys++;}}}
  fd_free(table,sizeof(fd_pair)*h->n_slots);
}

/** Hashsets **/

/* Hashsets are fast lookup tables for LISP values which just record
   membership, not associations.  Table elements indicate emptiness 
   by the value FD_EMPTY_CHOICE and removed values by FD_VOID */

/* hashset_get: (static)
    Arguments: a pointer to a hashset, a key, and a pointer to an uint
    Returns: 1 or 0

   This is the core loop for hashsets, which looks down the hashset
   using dual hashing and returns 1 if it finds the entry.  In
   either case, it deposits the slot at which it stopped into the uint
   pointer it is passed as its last argument.  This is either where the
   key lives or where it should go if added.

   Since NULL is a LISP zero, empty slots in a hashset are indicated by
   FD_EMPTY_CHOICE and deleted items are indicated by FD_VOID.
*/
FASTOP 
int hashset_get(struct FD_HASHSET *h,lisp key,unsigned int *slot)
{
  int size=h->n_slots; lisp *table=h->table;
  unsigned int hash=hash_lisp(key), probe=hash%size;
  unsigned int chain=get_chain(hash,size);
  if (ATOMICP(key))
    while (!(FD_EMPTYP(table[probe])))
      if (LISP_EQ(table[probe],key))
	{*slot=probe; return 1;}
      else {probe=(probe+chain)%size;}
  else while (!(FD_EMPTYP(table[probe])))
    if (LISP_EQUAL(key,table[probe])) {
      *slot=probe; return 1;}
    else {probe=(probe+chain)%size;}
  *slot=probe;
  return 0;
}

FASTOP 
/* hashset_strget:
   Arguments: A pointer to a hashset, a string pointer, a length, and a pointer to an integer slot. */
int hashset_strget(struct FD_HASHSET *h,fd_u8char *keystring,int len,unsigned int *slot)
{
  lisp *table=h->table;
  unsigned int size=h->n_slots; 
  unsigned int hash=hash_utf8_string(keystring,len);
  unsigned int probe=hash%size, chain=get_chain(hash,size);
  while (!(FD_EMPTYP(table[probe]))) {
    lisp entry=table[probe];
    if ((STRINGP(entry)) && ((STRING_LENGTH(entry)) == len) &&
	((strncmp(STRING_DATA(entry),keystring,len) == 0) )) {
      *slot=probe; return 1;}
    else probe=(probe+chain)%size;}
  *slot=probe;
  return 0;
}

DTYPES_EXPORT
/* fd_hashset_get:
    Arguments: a pointer to a hashtable and a lisp key
    Returns: 1 or 0

   Returns 1 if the given key is in the hashset.
*/
int fd_hashset_get(fd_hashset h,lisp key)
{
  unsigned int ignored;
  fd_read_lock(&(h->lock));
  if (h->table == NULL) {fd_read_unlock(&(h->lock)); return 0;}
  else {
    int result;
    result=hashset_get(h,key,&ignored);
    fd_read_unlock(&(h->lock));
    return result;}
}

DTYPES_EXPORT
/* fd_hashset_strget:
    Arguments: a pointer to a hashtable and a string
    Returns: 1 or 0

   Returns 1 if the a LISP copy of string is in the hashset.
*/
int fd_hashset_strget(fd_hashset h,fd_u8char *keystring,int len)
{
  unsigned int ignored; 
  fd_read_lock(&(h->lock));
  if (len < 0) len=strlen(keystring);
  if (h->table == NULL) {fd_read_unlock(&(h->lock)); return 0;}
  else {
    int result;
    result=hashset_strget(h,keystring,len,&ignored);
    fd_read_unlock(&(h->lock));
    return result;}
}

DTYPES_EXPORT
/* fd_hashset_intern_string:
    Arguments: a pointer to a hashtable and a string
    Returns: 1 or 0

   Returns the string object equal to KEYSTRING in H, creating a new
string object if neccessary.
*/
fd_lisp fd_hashset_intern_string(fd_hashset h,fd_u8char *keystring,int len)
{
  unsigned int slot_no; 
  fd_read_lock(&(h->lock));
  if (len < 0) len=strlen(keystring);
  if (h->table == NULL) {
    fd_read_unlock(&(h->lock));
    fd_raise_exception(_("Uninitialized hash table!"));}
  else {
    int result=hashset_strget(h,keystring,len,&slot_no);
    if (result) {
      fd_lisp entry=h->table[slot_no]; fd_incref(entry);
      fd_read_unlock(&(h->lock));
      return entry;}
    else {
      fd_lisp new_string=fd_copy_string(keystring);
      h->table[slot_no]=incref(new_string);
      fd_read_unlock(&(h->lock));
      return new_string;}}
}

DTYPES_EXPORT
/* _fd_hashset_add_nc:
    Arguments: a pointer to a hashset and a lisp key
    Returns: 1 if the value wasn't already there

   Adds a key (not a copy!) to the hashset.
*/
int _fd_hashset_add_nc(fd_hashset h,lisp key)
{
  unsigned int slot_no; int there;
  fd_write_lock(&(h->lock));
  if (h->table == NULL) fd_init_hashset(h,0);
  there=hashset_get(h,key,&slot_no);
  if (there) {fd_write_unlock(&(h->lock)); return 0;}
  if (h->n_keys*2 > h->n_slots) {
    grow_hashset(h,h->n_slots);
    fd_write_unlock(&(h->lock));
    return _fd_hashset_add_nc(h,key);}
  else if (h->need_gc) {
    fd_write_unlock(&(h->lock));
    fd_raise_exception(ephemeral_misuse);}
  else {
    h->table[slot_no]=key; h->n_keys++;
    fd_write_unlock(&(h->lock));}
  return 1;
}

DTYPES_EXPORT
/* fd_hashset_add:
    Arguments: a pointer to a hashset and a lisp key
    Returns: 1 if the value wasn't already there

   Adds a cref of a key to the hashset (if it doesn't already contain it).
*/
int fd_hashset_add(fd_hashset h,lisp key)
{
  unsigned int slot_no; int there;
  fd_write_lock(&(h->lock));
  if (h->table == NULL) fd_init_hashset(h,0);
  there=hashset_get(h,key,&slot_no);
  if (there) {
    fd_write_unlock(&(h->lock));
    return 0;}
  if ((h->need_gc == 0) && (PTR_TYPE(key) > 3)) h->need_gc=1;
  if (h->n_keys*2 > h->n_slots) {
    grow_hashset(h,h->n_slots);
    fd_write_unlock(&(h->lock));
    return fd_hashset_add(h,key);}
  else {
    if (FD_PRIM_TYPEP(key,bad_type))  fd_raise_exception(fd_BadType);
    h->table[slot_no]=incref(key); h->n_keys++;
    fd_write_unlock(&(h->lock));
    return 1;}
}

DTYPES_EXPORT
/* fd_hashset_intern:
    Arguments: a pointer to a hashset and a lisp key
    Returns: a value EQUAL to the key in the hashset

   Adds a cref of a key to the hashset (if it doesn't already contain it),
otherwise returns the pointer already there
*/
lisp fd_hashset_intern(fd_hashset h,lisp key)
{
  unsigned int slot_no; int there;
  fd_write_lock(&(h->lock));
  if (h->table == NULL) fd_init_hashset(h,0);
  there=hashset_get(h,key,&slot_no);
  if (there) {
    lisp value=fd_incref(h->table[slot_no]);
    fd_write_unlock(&(h->lock));
    return value;}
  if ((h->need_gc == 0) && (PTR_TYPE(key) > 3)) h->need_gc=1;
  if (h->n_keys*2 > h->n_slots) {
    grow_hashset(h,h->n_slots);
    fd_write_unlock(&(h->lock));
    return fd_hashset_intern(h,key);}
  else {
    h->table[slot_no]=incref(key); h->n_keys++;
    incref(key); fd_write_unlock(&(h->lock));
    return key;}
}

DTYPES_EXPORT
/* fd_hashset_probe:
    Arguments: a pointer to a hashset and a lisp key
    Returns: a value EQUAL to the key in the hashset

   Returns the key equal to KEY in hashset if it exists
or the empty choice otherwise
*/
lisp fd_hashset_probe(fd_hashset h,lisp key)
{
  unsigned int slot_no; int there;
  fd_read_lock(&(h->lock));
  if (h->table == NULL) fd_init_hashset(h,0);
  there=hashset_get(h,key,&slot_no);
  if (there) {
    lisp value=h->table[slot_no];
    fd_incref(value);
    fd_read_unlock(&(h->lock));
    return value;}
  else {
    fd_read_unlock(&(h->lock));
    return (FD_EMPTY_CHOICE);}
}

DTYPES_EXPORT
/* fd_hashset_drop:
    Arguments: a pointer to a hashtable and a lisp key
    Returns: nothing

   "Removes" the key from the hashset by replacing it with FD_VOID.
   Note that this doesn't save any space, but the search algorithm will
   just skip over it rather than returning it.
*/
void fd_hashset_drop(fd_hashset h,lisp key)
{
  unsigned int slot_no; 
  if (h->table == NULL) return;
  else {
    fd_write_lock(&(h->lock));
    if (hashset_get(h,key,&slot_no)) 
		h->table[slot_no]=(FD_VOID);
    fd_write_unlock(&(h->lock));}
}

/* Internal function for growing a hashtable to a new size.
   (Not threadsafe).  */
static void grow_hashset(fd_hashset h,int minsize)
{
  unsigned int new_size=fd_select_table_size(minsize);
  if (new_size > h->n_slots) {
    lisp *new_table=fd_malloc(sizeof(lisp)*new_size);
    lisp *scan=h->table, *limit=h->table+h->n_slots;
    lisp *nscan=new_table, *nlimit=nscan+new_size;
    while (nscan < nlimit) *nscan++=(FD_EMPTY_CHOICE);
    while (scan < limit)
      if (!((FD_VOIDP(*scan)) || (FD_EMPTYP(*scan)))) {
	lisp elt=*scan;
	unsigned int new_hash=hash_lisp(elt);
	unsigned int probe=new_hash%new_size;
	unsigned int chain=get_chain(new_hash,new_size);
	while (!(FD_EMPTYP(new_table[probe])))
	  probe=(probe+chain)%new_size;
	new_table[probe]=elt; scan++;}
      else scan++;
    fd_free(h->table,h->n_slots*sizeof(lisp));
    h->n_slots=new_size; h->table=new_table;}
}

DTYPES_EXPORT
/* fd_grow_hashset:
     Arguments: a pointer to a hashset and an int minsize
     Returns: nothing
  Grows the hashset to have at least minsize slots */
void fd_grow_hashset(fd_hashset h,int minsize)
{
  fd_write_lock(&(h->lock));
  grow_hashset(h,minsize);
  fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_init_hashset:
     Arguments: a pointer to a hashset and an int minsize
     Returns: nothing

  Initializes the table for use with at least minsize slots.
  This can be used for either a malloc'd hashtable or a hashtable
  on the stack. */
void fd_init_hashset(fd_hashset h,int minsize)
{
  int size=fd_select_table_size(minsize);
  h->n_keys=0; h->n_slots=size; h->need_gc=0;
  h->table=fd_malloc(sizeof(lisp)*size);
  {lisp *scan=h->table, *limit=scan+size;
   while (scan < limit) *scan++=(FD_EMPTY_CHOICE);}
#if FD_THREADS_ENABLED
  fd_init_rwlock(&(h->lock));
#endif
}

DTYPES_EXPORT
/* fd_make_hashset:
     Arguments: an int minsize
     Returns: a pointer to a hashtable
  This mallocs a new hashset and initializes it to have at least minsize
   elements. */
struct FD_HASHSET *fd_make_hashset(int minsize)
{
  struct FD_HASHSET *new=fd_malloc(sizeof(struct FD_HASHSET));
  fd_init_hashset(new,minsize);
  return new;
}

DTYPES_EXPORT
/* fd_hashset_elts:
     Arguments: a pointer to a hashset
     Returns: a lisp object (possibly a non-deterministic set)
  This returns all the values in the hashset. */
lisp fd_hashset_elts(fd_hashset h)
{
  lisp *scan=h->table, *limit=scan+h->n_slots;
  lisp answer=(FD_EMPTY_CHOICE);
  fd_read_lock(&(h->lock));
  while (scan < limit)
    if (!((FD_EMPTYP(*scan)) || (FD_VOIDP(*scan)))) {
      lisp v=*scan++; ADD_TO_CHOICE(answer,incref(v));}
    else scan++;
  fd_read_unlock(&(h->lock));
  if (PRIM_TYPEP(answer,choice_type)) answer.type=proper_choice_type;
  return answer;
}

DTYPES_EXPORT
/* fd_choice_to_hashset:
     Arguments: a pointer to a hashset
     Returns: a lisp object (possibly a non-deterministic set)
  This returns all the values in the hashset. */
fd_hashset fd_choice_to_hashset(lisp values)
{
  fd_hashset h=fd_make_hashset(CHOICE_SIZE(values)*2);
  DO_CHOICES(v,values) fd_hashset_add(h,v); END_DO_CHOICES;
  return h;
}

/** Freeing hashtables and hashsets **/

DTYPES_EXPORT
/* fd_free_hashtable:
    Arguments: a pointer to a hashtable
    Returns: nothing
  Frees the memory taken by a hashtable and its elements.  Note that
this does not free the hashtable itself, since it might be on the stack. */
void fd_free_hashtable(struct FD_HASHTABLE *h)
{
  int i=0, limit=h->n_slots;
  struct FD_PAIR **table=h->table;
  if (fd_normal_exit) return;
  fd_write_lock(&(h->lock));
  while (i < limit)
    if (table[i]) {
      struct FD_PAIR *p=table[i++];
      decref(p->car); decref(p->cdr);
      fd_qfree(p,sizeof(struct FD_PAIR));}
    else i++;
  fd_free(h->table,sizeof(struct FD_PAIR *)*h->n_slots);
  h->table=NULL; h->n_slots=0; h->n_keys=0;
  fd_write_unlock(&(h->lock));
#if FD_THREADS_ENABLED
  fd_destroy_rwlock(&(h->lock));
#endif
}

DTYPES_EXPORT
/* fd_reinit_hashtable:
     Arguments: a pointer to a hashtable and an int minsize
     Returns: nothing

  Reinitializes the table for use with at least minsize slots.
  This leaves the tables mutex untouched, since someone may be
   waiting on it. */
void fd_reinit_hashtable(fd_hashtable h,int minsize,int locked)
{
  if (locked == 0) fd_write_lock(&(h->lock));
  /* Phase 1: Cleanup */
  {
    int i=0, limit=h->n_slots;
    struct FD_PAIR **table=h->table;
    if (table == NULL) {
      if (locked == 0) fd_write_unlock(&(h->lock));
      return;}
    while (i < limit)
      if (table[i]) {
	struct FD_PAIR *p=table[i++];
	decref(p->car); decref(p->cdr);
	fd_qfree(p,sizeof(struct FD_PAIR));}
      else i++;
    fd_free(h->table,sizeof(struct FD_PAIR *)*h->n_slots);
    h->table=NULL; h->n_slots=0; h->n_keys=0;}
  /* Phase 2: Setup again */
  {
    int size=fd_select_table_size(minsize); fd_pair *scan, *limit;
    scan=h->table=fd_malloc(sizeof(struct FD_PAIR *)*size);
    h->n_keys=0; h->n_slots=size; limit=scan+size;
    while (scan < limit) *scan++=NULL;}
  if (locked == 0) fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_free_hashset:
    Arguments: a pointer to a hashset
    Returns: nothing
  Frees the memory taken by a hashset and its elements.  Note that
this does not free the hashtable itself, since it might be on the stack. */
void fd_free_hashset(struct FD_HASHSET *h)
{
  int i=0, limit=h->n_slots;
  lisp *table=h->table;
  fd_write_lock(&(h->lock));
  if (fd_normal_exit) return;
  if (table == NULL) return;
  if (h->need_gc == 1)
    while (i < limit) {lisp v=table[i]; decref(v); i++;}
  fd_free(h->table,sizeof(lisp)*h->n_slots);
  h->table=NULL; h->n_slots=0; h->n_keys=0;
  fd_write_unlock(&(h->lock));
#if FD_THREADS_ENABLED
  fd_destroy_rwlock(&(h->lock));
#endif
}

DTYPES_EXPORT
/* fd_reinit_hashset:
     Arguments: a pointer to a hashtable and an int minsize
     Returns: nothing

  Reinitializes the table for use with at least minsize slots.
  This leaves the tables mutex untouched, since someone may be
   waiting on it. */
void fd_reinit_hashset(fd_hashset h,int minsize,int locked)
{
  if (locked == 0) fd_write_lock(&(h->lock));
  /* Phase 1: Cleanup */
  {
    int i=0, limit=h->n_slots;
    lisp *table=h->table;
    if (table == NULL) return;
    if (h->need_gc == 1)
      while (i < limit) {lisp v=table[i]; decref(v); i++;}
    fd_free(h->table,sizeof(lisp)*h->n_slots);
    h->table=NULL; h->n_slots=0; h->n_keys=0;}
  /* Phase 2: Setup again */
  {
    lisp *scan, *limit;
    int size=fd_select_table_size(minsize);
    h->n_keys=0; h->n_slots=size; h->need_gc=0;
    h->table=fd_malloc(sizeof(lisp)*size);
    scan=h->table; limit=scan+size;
    while (scan < limit) *scan++=(FD_EMPTY_CHOICE);}
  if (locked == 0) fd_write_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_final_hashset_elts:
     Arguments: a pointer to a hashset
     Returns: a lisp object (possibly a non-deterministic set)
  This returns all the values in the hashset and frees the hashset. */
lisp fd_final_hashset_elts(fd_hashset h)
{
  lisp *scan=h->table, *limit=scan+h->n_slots;
  lisp answer=(FD_EMPTY_CHOICE);
  fd_write_lock(&(h->lock));
  while (scan < limit)
    if (!((FD_EMPTYP(*scan)) || (FD_VOIDP(*scan)))) {
      lisp v=*scan++; ADD_TO_CHOICE(answer,v);}
    else scan++;
  fd_free(h->table,sizeof(lisp)*h->n_slots);
  h->table=NULL; h->n_slots=0; h->n_keys=0;
  fd_write_unlock(&(h->lock));
#if FD_THREADS_ENABLED
  fd_destroy_rwlock(&(h->lock));
#endif
  if (PRIM_TYPEP(answer,choice_type)) answer.type=proper_choice_type;
  return answer;
}

DTYPES_EXPORT
/* fd_hashset_map
     Arguments: a hashtable and a void-returning function of two lisp arguments
     Returns: nothing
  Applies the function to every key and value in the hash table. Keys and values are not incref'd. */
void fd_hashset_map(fd_hashset h,void (*fcn)(fd_lisp key,void *datap),void *data)
{
  fd_lisp *scan, *limit;
  fd_read_lock(&(h->lock));
  scan=h->table; limit=scan+h->n_slots;
  while (scan < limit)
    if (FD_VOIDP(*scan)) scan++;
    else {fcn(*scan,data); scan++;}
  fd_read_unlock(&(h->lock));
}


/** Using hashtables as score keepers **/

DTYPES_EXPORT
/* fd_hashtable_skim:
     Arguments: a hashtable and an integral threshold
     Returns: a lisp pointer

Returns (as a choice) all the keys in the hashtable whose
values are greater than the numeric threshold.  */
lisp fd_hashtable_skim(fd_hashtable h,int threshold)
{
  int size=h->n_slots; struct FD_PAIR **scan=h->table, **max=scan+size;
  lisp answer=FD_EMPTY_CHOICE;
  while (scan < max) 
    if (*scan) {
      struct FD_PAIR *entry=*scan;
      if (FIXNUMP(entry->cdr)) {
	if (FIXLISP(entry->cdr) > threshold) {
	  ADD_TO_CHOICE(answer,incref(entry->car));}}
      else if (FD_EMPTYP(entry->cdr)) {}
      else fd_type_error(_("key isn't a fixnum"),entry->car);
      scan++;}
    else scan++;
  return answer;
}

DTYPES_EXPORT
/* fd_hashtable_max:
     Arguments: a hashtable
     Returns: a lisp pointer

Returns the keys in the hashtable whose values are numerically
maximum. */
lisp fd_hashtable_max(fd_hashtable h)
{
  int size=h->n_slots; struct FD_PAIR **scan=h->table, **max=scan+size;
  int threshold, threshold_needs_setting=1;
  lisp answer=FD_EMPTY_CHOICE;
  while (scan < max) 
    if (*scan) {
      struct FD_PAIR *entry=*scan;
      if (FIXNUMP(entry->cdr)) {
	if (threshold_needs_setting) {
	  threshold=FIXLISP(entry->cdr); threshold_needs_setting=0;}
	if (FIXLISP(entry->cdr) == threshold) {
	  ADD_TO_CHOICE(answer,incref(entry->car));}
	else if (FIXLISP(entry->cdr) > threshold) {
	  threshold=FIXLISP(entry->cdr);
	  decref(answer); answer=incref(entry->car);}}
      else if (FD_EMPTYP(entry->cdr)) {}
      else fd_type_error(_("key isn't a fixnum"),entry->car);
      scan++;}
    else scan++;
  return answer;
}

DTYPES_EXPORT
/* fd_hashtable_map
     Arguments: a hashtable and a void-returning function of two lisp arguments
     Returns: nothing
  Applies the function to every key and value in the hash table. Keys and values are not incref'd. */
void fd_hashtable_map(fd_hashtable h,void (*fcn)(fd_lisp key,fd_lisp val,void *datap),void *data)
{
  struct FD_PAIR **scan, **limit;
  fd_read_lock(&(h->lock));
  scan=h->table; limit=scan+h->n_slots;
  while (scan < limit)
    if (*scan == NULL) scan++;
    else {fcn((*scan)->car,(*scan)->cdr,data); scan++;}
  fd_read_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_hashtable_dedangle
     Arguments: a hashtable 
     Returns: nothing
  Removes all keys whose only pointer is in the hashtable. */
void fd_hashtable_dedangle(fd_hashtable h)
{
  struct FD_PAIR **scan, **limit;
  fd_read_lock(&(h->lock));
  scan=h->table; limit=scan+h->n_slots;
  while (scan < limit)
    if (*scan == NULL) scan++;
    else if (FD_ATOMICP((*scan)->car)) scan++;
    else if (fd_refcount((*scan)->car) > 1) scan++;
    else {
      fd_lisp key=(*scan)->car, val=(*scan)->cdr;
      (*scan)->car=FD_VOID; (*scan)->cdr=FD_VOID;
      fd_decref(key); fd_decref(val);
      scan++;}
  fd_read_unlock(&(h->lock));
}

/** Interning Symbols **/

/* Symbol tables are just hashsets that don't use == but do string
    compares to make sure that symbols are appropriately interned. */

static struct FD_HASHSET symbol_table;
static void grow_symbol_table();
static int symbol_table_initialized=0;

DTYPES_EXPORT
/* fd_symbol_table:
    Arguments: none
    Returns: a pointer to the hashset being used as a symbol table. */
fd_hashset fd_symbol_table() {return &symbol_table;}

DTYPES_EXPORT
/* fd_make_symbol:
    Arguments: a string
    Returns: a lisp pointer to a symbol with that name

  This is basically the same loop as above with a substitution of
   a strcmp for the == test.
*/
lisp fd_make_symbol(const fd_u8char *pname)
{
  int size; lisp *table;
  unsigned int hash, probe, chain;
  if (symbol_table_initialized == 0) initialize_symbol_table();
  fd_write_lock(&(symbol_table.lock));
  size=symbol_table.n_slots; table=symbol_table.table;
  hash=hash_string(pname,strlen(pname));
  probe=hash%size; chain=get_chain(hash,size);
  while (SYMBOLP(table[probe]))
    if (strcmp(SYMBOL_NAME(table[probe]),pname) == 0) {
      fd_write_unlock(&(symbol_table.lock)); return table[probe];}
    else {probe=(probe+chain)%size;}
  if (symbol_table.n_keys > ((symbol_table.n_slots)/2)) {
    grow_symbol_table();
    fd_write_unlock(&(symbol_table.lock));
    return fd_make_symbol(pname);}
  else {
    fd_lisp_symbol consed=fd_malloca(struct FD_SYMBOL);
    consed->name=fd_strdup(pname); consed->value=(FD_VOID);
    table[probe].type=symbol_type;
    table[probe].data.symbol=consed;
    symbol_table.n_keys++;
    fd_write_unlock(&(symbol_table.lock));
    return table[probe];}
}

DTYPES_EXPORT
/* fd_probe_symbol:
    Arguments: a string
    Returns: a lisp pointer to a symbol with that name

  This is like fd_make_symbol but doesn't make the symbol, only
returning it if it exists.
*/
lisp fd_probe_symbol(const fd_u8char *pname)
{
  int size; lisp *table;
  unsigned int hash, probe, chain;
  if (symbol_table_initialized == 0) initialize_symbol_table();
  fd_read_lock(&(symbol_table.lock));
  size=symbol_table.n_slots; table=symbol_table.table;
  hash=hash_string(pname,strlen(pname));
  probe=hash%size; chain=get_chain(hash,size);
  while (SYMBOLP(table[probe]))
    if (strcmp(SYMBOL_NAME(table[probe]),pname) == 0)
      {fd_read_unlock(&(symbol_table.lock)); return table[probe];}
    else {probe=(probe+chain)%size;}
  fd_read_unlock(&(symbol_table.lock));
  return FD_EMPTY_CHOICE;
}

static void grow_symbol_table()
{
  unsigned int new_size=fd_select_table_size(symbol_table.n_slots);
  lisp *new_table=fd_malloc(sizeof(lisp)*new_size);
  lisp *scan=symbol_table.table, *limit=scan+symbol_table.n_slots;
  lisp *nscan=new_table, *nlimit=nscan+new_size;
  while (nscan < nlimit) *nscan++=(FD_VOID);
  while (scan < limit)
    if (SYMBOLP(*scan)) {
      lisp elt=*scan; char *pname=SYMBOL_NAME(elt);
      unsigned int new_hash=hash_string(pname,strlen(pname));
      unsigned int probe=new_hash%new_size, chain=get_chain(new_hash,new_size);
      while (SYMBOLP(new_table[probe])) probe=(probe+chain)%new_size;
      new_table[probe]=elt; scan++;}
    else scan++;
  fd_notify(_("Growing symbol table from %d/%d"),
	    symbol_table.n_keys,symbol_table.n_slots);
  fd_free(symbol_table.table,symbol_table.n_slots*sizeof(lisp));
  symbol_table.n_slots=new_size;
  symbol_table.table=new_table;
}

/* initialize_symbol_table:
    Arguments: None
    Returns: nothing
  Initializes the symbol table.  */
void initialize_symbol_table()
{
  lisp *scan, *limit;
  if (symbol_table_initialized) return;
  else symbol_table_initialized=1;
  symbol_table.n_keys=0; symbol_table.n_slots=4129;
  symbol_table.table=fd_malloc(4129*sizeof(lisp));
  scan=symbol_table.table; limit=scan+4129;
  while (scan < limit) *scan++=(FD_VOID);
#if FD_THREADS_ENABLED
  fd_init_rwlock(&(symbol_table.lock));
#endif
}

DTYPES_EXPORT
/* fd_for_all_symbols:
     Arguments: a function on lisp pointers which returns void
     Returns: nothing
  Applies the function to every symbol in the symbol table. */
void fd_for_all_symbols(void (*fcn)(lisp symbol))
{
  lisp *scan, *limit;
  fd_read_lock(&(symbol_table.lock));
  scan=symbol_table.table; limit=scan+symbol_table.n_slots;
  while (scan < limit)
    if (SYMBOLP(*scan)) fcn(*scan++); else scan++;
  fd_read_unlock(&(symbol_table.lock));
}

DTYPES_EXPORT
/* fd_symbol_value:
     Arguments: a LISP symbol
     Returns: the symbol's value 

    Threadsafe accessor for the value slot which increfs the value it returns

*/
lisp fd_symbol_value(lisp x)
{
  lisp v;
  if (!(SYMBOLP(x))) fd_type_error(_("pointer not a symbol"),x);
  FD_LOCK_CELL(x);
  v=(PTR_DATA(x,symbol)->value);
  FD_UNLOCK_CELL(x);
  return fd_incref(v);
}

DTYPES_EXPORT
/* fd_symbol_value:
     Arguments: a LISP symbol
     Returns: the symbol's value 

    Threadsafe accessor for the value slot which doesn't incref the value it returns

*/
lisp _fd_symbol_value_noref(lisp x)
{
  lisp v;
  if (!(SYMBOLP(x))) fd_type_error(_("pointer not a symbol"),x);
  FD_LOCK_CELL(x);
  v=(PTR_DATA(x,symbol)->value);
  FD_UNLOCK_CELL(x);
  return v;
}

DTYPES_EXPORT
/* fd_set_symbol_value:
     Arguments: a LISP symbol, another LISP pointer
     Returns: nothing

    Threadsafe modifier for the value slot of a symbol.

*/
void fd_set_symbol_value(lisp x,lisp v)
{
  lisp oldv;
  if (!(SYMBOLP(x))) fd_type_error(_("arg not a symbol"),x);
  incref(v);
  FD_LOCK_CELL(x);
  oldv=PTR_DATA(x,symbol)->value;
  PTR_DATA(x,symbol)->value=v;
  FD_UNLOCK_CELL(x);
  decref(oldv);
}

DTYPES_EXPORT
/* _fd_set_symbol_value_noref:
     Arguments: a LISP symbol, another LISP pointer
     Returns: nothing

    Threadsafe modifier for the value slot of a symbol
which does not do any refcounting or uncounting.

*/
void _fd_set_symbol_value_noref(lisp x,lisp v)
{
  lisp oldv;
  if (!(SYMBOLP(x))) fd_type_error(_("arg not a symbol"),x);
  FD_LOCK_CELL(x);
  PTR_DATA(x,symbol)->value=v;
  FD_UNLOCK_CELL(x);
}

DTYPES_EXPORT
/* fd_intern:
    Arguments: a pointer to a string and a int length
    Returns: a lisp pointer to a symbol whose name is the string capitalized

    Returns an interned symbol whose name is a capitalized version of name.
*/
fd_lisp fd_intern(const fd_u8char *name,int len)
{
  if (len <= 0) fd_raise_exception(_("FD_INTERN: invalid length"));
  else if (len < 127) {
    fd_u8char buf[128]; struct FD_STRING_STREAM uname;
    fd_u8char *scan=(fd_u8char *)name, *limit=scan+len; int c=fd_sgetc(&scan);
    FD_INITIALIZE_FIXED_STRING_STREAM(&uname,128,buf);
    while ((c >= 0) && (scan < limit)) {
      fd_sputc(&uname,fd_toupper(c)); c=fd_sgetc(&scan);}
    if (c>=0) fd_sputc(&uname,fd_toupper(c));
    return fd_make_symbol(uname.ptr);}
  else {
    struct FD_STRING_STREAM uname; fd_lisp result;
    fd_u8char *scan=(fd_u8char *)name, *limit=scan+len;
    int c=fd_sgetc(&scan);
    FD_INITIALIZE_STRING_STREAM(&uname,len+8);
    while ((c >= 0) && (scan < limit)) {
      fd_sputc(&uname,fd_toupper(c)); c=fd_sgetc(&scan);}
    if (c>=0) fd_sputc(&uname,fd_toupper(c));
    result=fd_make_symbol(uname.ptr);
    fd_xfree(uname.ptr);
    return result;}
}

static void free_symbol_data()
{
  lisp *scan, *limit;
  fd_read_lock(&(symbol_table.lock));
  scan=symbol_table.table; limit=scan+symbol_table.n_slots;
  while (scan < limit)
    if (SYMBOLP(*scan)) {
      fd_decref(scan->data.symbol->value);
      (*scan).data.symbol->value=FD_VOID; scan++;}
    else scan++;
  /* fd_free(symbol_table.table,sizeof(lisp)*symbol_table.n_slots); */
  fd_read_unlock(&(symbol_table.lock));
}

/** QStrings **/

/* QStrings are strings which have been interned, so == is the same as EQUAL */

static struct FD_HASHSET qstring_table;
static int qstring_table_initialized=0;
static void grow_qstring_table(fd_hashset h,int minsize);

DTYPES_EXPORT
/* fd_make_qstring:
    Arguments: a string and a length
    Returns: a lisp pointer to a qstring that is EQUAL to the string

   If the length is negative, it is computed.
*/
lisp fd_make_qstring(fd_u8char *string_data,int len)
{
  int pos; int result;
  fd_write_lock(&(qstring_table.lock));
  if (len < 0) len=strlen(string_data);
  result=hashset_strget(&qstring_table,string_data,len,&pos);
  if (result) {
    lisp qs=fd_incref(qstring_table.table[pos]);
    fd_write_unlock(&(qstring_table.lock));
    return qs;}
  else if (qstring_table.n_keys*2 > qstring_table.n_slots) {
    grow_qstring_table(&qstring_table,qstring_table.n_slots);
    fd_write_unlock(&(qstring_table.lock));
    return fd_make_qstring(string_data,len);}
  else {
    fd_lisp qs=fd_make_substring(string_data,string_data+len);
    qs.type=qstring_type;
    qstring_table.table[pos]=qs; qstring_table.n_keys++;
    fd_write_unlock(&(qstring_table.lock));
    return fd_incref(qs);}
}

lisp fd_qify_string(struct FD_STRING *str)
{
  int pos; int result;
  fd_u8char *string_data=str->data; int len=str->length;
  fd_write_lock(&(qstring_table.lock));
  if (len < 0) len=strlen(string_data);
  result=hashset_strget(&qstring_table,string_data,len,&pos);
  if (result) {
    lisp qs=fd_incref(qstring_table.table[pos]);
    fd_write_unlock(&(qstring_table.lock));
    return qs;}
  else if (qstring_table.n_keys*2 > qstring_table.n_slots) {
    grow_qstring_table(&qstring_table,qstring_table.n_slots);
    fd_write_unlock(&(qstring_table.lock));
    return fd_qify_string(str);}
  else {
    fd_lisp qs; qs.type=qstring_type; qs.data.string=str;
    qstring_table.table[pos]=qs; qstring_table.n_keys++;
    fd_write_unlock(&(qstring_table.lock));
    return fd_incref(qs);}
}

/* initialize_qstring_table:
    Arguments: None
    Returns: nothing
  Initializes the qstring table.  */
void initialize_qstring_table()
{
  lisp *scan, *limit;
  if (qstring_table_initialized) return;
  else {
    qstring_table_initialized=1;
    fd_init_hashset(&qstring_table,128);}
}

/* grow_qstring_table:
    Arguments: None
    Returns: nothing
  Grows the qstring table.  This has to be separate from
  regular grow_hashtable because the qstring table does
  string hashing even on qstrings, while regular hashtables
  use pointer hashing on qstrings. */
static void grow_qstring_table(fd_hashset h,int minsize)
{
  unsigned int new_size=fd_select_table_size(minsize);
  if (new_size > h->n_slots) {
    lisp *new_table=fd_malloc(sizeof(lisp)*new_size);
    lisp *scan=h->table, *limit=h->table+h->n_slots;
    lisp *nscan=new_table, *nlimit=nscan+new_size;
    while (nscan < nlimit) *nscan++=(FD_EMPTY_CHOICE);
    while (scan < limit)
      if (!((FD_VOIDP(*scan)) || (FD_EMPTYP(*scan)))) {
	lisp elt=*scan;
	unsigned int new_hash=
	  hash_utf8_string(STRING_DATA(elt),STRING_LENGTH(elt));
	unsigned int probe=new_hash%new_size;
	unsigned int chain=get_chain(new_hash,new_size);
	while (!(FD_EMPTYP(new_table[probe])))
	  probe=(probe+chain)%new_size;
	new_table[probe]=elt; scan++;}
      else scan++;
    fd_free(h->table,h->n_slots*sizeof(lisp));
    h->n_slots=new_size; h->table=new_table;}
}

/** ZStrings **/

/* ZStrings are strings which have been interned, so == is the same as EQUAL */

static struct FD_HASHSET zstring_table;
static int zstring_table_initialized=0;
static void grow_zstring_table(fd_hashset h,int minsize);

DTYPES_EXPORT
/* fd_make_zstring:
    Arguments: a string and a length
    Returns: a lisp pointer to a zstring that is EQUAL to the string

   If the length is negative, it is computed.
   ZStrings are different from Qstrings in that their status is preserved by dtype write/read.
*/
lisp fd_make_zstring(fd_u8char *string_data,int len)
{
  int pos; int result;
  fd_write_lock(&(zstring_table.lock));
  if (len < 0) len=strlen(string_data);
  result=hashset_strget(&zstring_table,string_data,len,&pos);
  if (result) {
    lisp zs=fd_incref(zstring_table.table[pos]);
    fd_write_unlock(&(zstring_table.lock));
    return zs;}
  else if (zstring_table.n_keys*2 > zstring_table.n_slots) {
    grow_zstring_table(&zstring_table,zstring_table.n_slots);
    fd_write_unlock(&(zstring_table.lock));
    return fd_make_zstring(string_data,len);}
  else {
    fd_lisp zs=fd_make_substring(string_data,string_data+len);
    zs.type=zstring_type;
    zstring_table.table[pos]=zs; zstring_table.n_keys++;
    fd_write_unlock(&(zstring_table.lock));
    return zs;}
}

DTYPES_EXPORT
/* fd_zify_string
    Arguments: a pointer to a string struct
    Returns: a lisp pointer to a zstring which is either the given pointer or another one

   If the length is negative, it is computed.
   ZStrings are different from Qstrings in that their status is preserved by dtype write/read.
*/
lisp fd_zify_string(struct FD_STRING *str)
{
  int pos; int result;
  fd_u8char *string_data=str->data; int len=str->length;
  fd_write_lock(&(zstring_table.lock));
  if (len < 0) len=strlen(string_data);
  result=hashset_strget(&zstring_table,string_data,len,&pos);
  if (result) {
    lisp zs=fd_incref(zstring_table.table[pos]);
    fd_write_unlock(&(zstring_table.lock));
    return zs;}
  else if (zstring_table.n_keys*2 > zstring_table.n_slots) {
    grow_zstring_table(&zstring_table,zstring_table.n_slots);
    fd_write_unlock(&(zstring_table.lock));
    return fd_zify_string(str);}
  else {
    /* Make a string type so fd_incref() will increase the reference count. */
    fd_lisp zs; FD_SET_PRIM_TYPE(zs,string_type); zs.data.string=str;
    fd_incref(zs);
    /* Now switch to zstring_type which isn't reference counted;
       the hash table owns the reference which will never be released. */
    FD_SET_PRIM_TYPE(zs,zstring_type);
    zstring_table.table[pos]=zs; zstring_table.n_keys++;
    fd_write_unlock(&(zstring_table.lock));
    return zs;}
}

/* initialize_zstring_table:
    Arguments: None
    Returns: nothing
  Initializes the zstring table.  */
void initialize_zstring_table()
{
  lisp *scan, *limit;
  if (zstring_table_initialized) return;
  else {
    zstring_table_initialized=1;
    fd_init_hashset(&zstring_table,128);}
}

/* grow_zstring_table:
    Arguments: None
    Returns: nothing
  Grows the zstring table.  This has to be separate from
  regular grow_hashtable because the zstring table does
  string hashing even on zstrings, while regular hashtables
  use pointer hashing on zstrings. */
static void grow_zstring_table(fd_hashset h,int minsize)
{
  unsigned int new_size=fd_select_table_size(minsize);
  if (new_size > h->n_slots) {
    lisp *new_table=fd_malloc(sizeof(lisp)*new_size);
    lisp *scan=h->table, *limit=h->table+h->n_slots;
    lisp *nscan=new_table, *nlimit=nscan+new_size;
    while (nscan < nlimit) *nscan++=(FD_EMPTY_CHOICE);
    while (scan < limit)
      if (!((FD_VOIDP(*scan)) || (FD_EMPTYP(*scan)))) {
	lisp elt=*scan;
	unsigned int new_hash=
	  hash_utf8_string(STRING_DATA(elt),STRING_LENGTH(elt));
	unsigned int probe=new_hash%new_size;
	unsigned int chain=get_chain(new_hash,new_size);
	while (!(FD_EMPTYP(new_table[probe])))
	  probe=(probe+chain)%new_size;
	new_table[probe]=elt; scan++;}
      else scan++;
    fd_free(h->table,h->n_slots*sizeof(lisp));
    h->n_slots=new_size; h->table=new_table;}
}

/** The initialization function **/

DTYPES_EXPORT void fd_initialize_hash_c()
{
  initialize_symbol_table();
  initialize_qstring_table();
  initialize_zstring_table();
  fd_add_full_free_fn(free_symbol_data);
  fd_register_source_file("hash",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: hash.c,v $
   Revision 1.42  2007/06/30 16:21:06  haase
   Various 64 bit fixes, together with stuff for repacking indices with less than 10 keys

   Revision 1.41  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.40  2004/10/04 15:42:12  haase
   A little more early type checking

   Revision 1.39  2004/09/17 07:30:36  haase
   Made file index caching not copy the retrieved choice

   Revision 1.38  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.37  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.36  2004/05/14 14:41:57  haase
   Made preloading be an option for all kinds of indices

   Revision 1.35  2004/03/31 21:03:38  haase
   Fixed fd_intern to use the length argument

   Revision 1.34  2004/02/18 16:21:07  haase
   Fixed dedangling to actually work

   Revision 1.33  2003/11/26 12:35:33  haase
   Added fd_hashtable_dedangle to remove dangling keys from hashtables

   Revision 1.32  2003/08/31 16:56:29  haase
   Added hashset/hashtable map

   Revision 1.31  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.30.2.7  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.30.2.6  2003/06/29 19:32:41  haase
   Fixed multiplicative hashing constant to fit in 32 bits

   Revision 1.30.2.5  2003/01/26 20:34:35  haase
   Misc. fixes, introduced zstrings

   Revision 1.30.2.4  2002/09/26 02:11:57  haase
   Removed redundant cast

   Revision 1.30.2.3  2002/08/12 18:44:20  haase
   Fixed bug in growing of QSTRING table

   Revision 1.30.2.2  2002/08/10 17:50:42  haase
   Fixed bug in symbol table cleanup

   Revision 1.30.2.1  2002/08/09 19:14:10  haase
   Added freefn for cleaning up the symbol table

   Revision 1.30  2002/07/23 21:43:41  haase
   Add explicit cast to (int)

   Revision 1.29  2002/07/05 21:20:07  uid59704
   Fixed GC contract of fd_hashset_intern and added fd_hashset_intern string

   Revision 1.28  2002/07/03 21:48:46  haase
   Removed immediate exit from fd_reinit_hashset while exiting

   Revision 1.27  2002/07/02 16:45:49  haase
   Reordered locks and increfs to avoid thread race conditions

   Revision 1.26  2002/05/19 10:12:55  haase
   Added fd_intern for making uppercase symbols

   Revision 1.25  2002/05/13 07:30:50  haase
   Doc fixes from Mario

   Revision 1.24  2002/04/27 03:39:33  haase
   More changes for furthre use of rwlock s

   Revision 1.23  2002/04/26 16:33:50  haase
   Fixed error in multiplicative hashing add

   Revision 1.22  2002/04/26 16:32:16  haase
   Added multiplicative hashing for fixnum like keys

   Revision 1.21  2002/04/20 19:47:48  haase
   Renamed fd_hashset_zap to fd_hashset_drop

   Revision 1.20  2002/04/19 19:30:59  haase
   Added framework for read/write locks on hashtables

   Revision 1.19  2002/04/19 13:19:51  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.18  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.17  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
