/* Mode: C */

/* choices.c
   This file implements choices.

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

static char vcid[] = "$Id: choices.c,v 1.22 2005/01/14 16:48:44 haase Exp $";

#include "dtypes.h"

#include <limits.h>
#include <time.h>
#include <signal.h>
#include <stdarg.h>

#define LINEAR_HASHET_TRADEOFF (32)
static void check_type(fd_lisp x)
{
  if (FD_PRIM_TYPEP(x,bad_type)) fd_raise_exception(fd_BadType);
}
#define CHECK_TYPE(x) check_type(x)

/* Comparing atoms */

#define COMPARE_NUMS(x,y) (((x)==(y)) ? (0) : (((x)>(y)) ? 1 : -1))
#define COMPARE_ATOMS(a1,a2) \
  (((PTR_TYPE(a1)) == (PTR_TYPE(a2))) ? \
   ((FD_SMALL_TYPEP(a1)) ? \
    (COMPARE_NUMS(PTR_DATA(a1,any_small),PTR_DATA(a2,any_small))) : \
    (COMPARE_NUMS(PTR_DATA(a1,any),PTR_DATA(a2,any)))) \
   : (COMPARE_NUMS((PTR_TYPE(a1)),(PTR_TYPE(a2)))))

#define next_choice_size(x) ((x>=0x40000) ? (x+0x40000) : (x*2))

/** Choices: Non deterministic sets **/

/* Choices (non deterministic sets) are just growable vectors which have
    special functions for merging and augmenting. */

DTYPES_EXPORT
/* fd_make_choice:
     Arguments: none
     Returns: An empty non-deterministic set
   Non deterministic sets are sets of objects which can be dynamically
   extended while retaining their identity.  They represent sets and
   are used in ambiguous evaluation, multi-valued slots, and set operations.
*/
lisp fd_make_choice ()
{
  fd_choice ch=fd_malloca(struct FD_CHOICE);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ch->lock));
#endif
  ch->sorted=0; ch->size=0; ch->limit=0; ch->busy=0;
  ch->elt_type=0; ch->elements.data=NULL;
  {RETURN_LISP(proper_choice_type,choice,ch);}
}

DTYPES_EXPORT
/* fd_init_choice:
     Arguments: a size
     Returns: An empty non-deterministic set
   This returns an empty set with reserved space for a certain number
    of elements.
*/
lisp fd_init_choice(int n)
{
  fd_choice ch=fd_malloca(struct FD_CHOICE);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ch->lock));
#endif
  ch->sorted=0; ch->size=0; ch->limit=n; ch->elt_type=0; ch->busy=0;
  ch->elements.lisp = fd_malloc(sizeof(fd_lisp)*n);    
  {RETURN_LISP(proper_choice_type,choice,ch);}
}

DTYPES_EXPORT
/* fd_init_oid_choice:
     Arguments: a size
     Returns: An empty non-deterministic set with a homogenous type of OID
   This returns an empty set with reserved space for a certain number
    of elements.  The set is initialized as a homogenous choice of OIDS.
*/
lisp fd_init_oid_choice(int n)
{
  fd_choice ch=fd_malloca(struct FD_CHOICE);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ch->lock));
#endif
  ch->sorted=0; ch->size=0; ch->limit=n; ch->elt_type=object_type; ch->busy=0;
  ch->elements.lisp = fd_malloc(sizeof(union FD_DATA)*n);    
  {RETURN_LISP(proper_choice_type,choice,ch);}
}

/** Utility functions **/

static fd_lisp *copy_lispv(fd_lisp *v,int size,int limit)
{
  fd_lisp *copy=fd_malloc(sizeof(fd_lisp)*limit);
  memcpy(copy,v,sizeof(fd_lisp)*size);
  return copy;
}
DTYPES_EXPORT
fd_lisp *_fd_copy_lispv(fd_lisp *v,int size,int limit)
{ return copy_lispv(v,size,limit); }

static union FD_DATA *copy_datav(union FD_DATA *v,int size,int limit)
{
  union FD_DATA *copy=fd_malloc(sizeof(union FD_DATA)*limit);
  memcpy(copy,v,sizeof(union FD_DATA)*size);
  return copy;
}
DTYPES_EXPORT
union FD_DATA *_fd_copy_datav(union FD_DATA *v,int size,int limit)
{ return copy_datav(v,size,limit); }

/* Note that all of the static functions here are not threadsafe (they
   don't do locking) and the locking is always done in the externally
   exported entry points.   This is so they can call each other with
   impunity.  */
STATIC_INLINE void grow_choice(fd_choice ch,int new_limit)
{
  if (ch->elt_type == 0) {
    if (ch->busy) {
      lisp *nlisp=copy_lispv(ch->elements.lisp,ch->size,new_limit);
      ch->elements.lisp=nlisp; ch->limit=new_limit; ch->busy=0;}
    else {
      ch->elements.lisp=fd_realloc
	(ch->elements.lisp,sizeof(lisp)*new_limit,sizeof(lisp)*ch->limit);
      ch->limit=new_limit;}}
  else if (ch->busy) {
    union FD_DATA *ndata=copy_datav(ch->elements.data,ch->size,new_limit);
    ch->elements.data=ndata; ch->limit=new_limit; ch->busy=0;}
  else {
    ch->elements.data=fd_realloc
      (ch->elements.data,
       sizeof(union FD_DATA)*new_limit,
       sizeof(union FD_DATA)*ch->limit);
    ch->limit=new_limit;}
}

/** Operations between heterogenous and homogenous choices **/

DTYPES_EXPORT
/* _fd_make_choice_heterogenous:
     Arguments: a pointer to a choice
     Returns: nothing
  Makes a homogenous choice into a heterogenous one
*/
void _fd_make_choice_heterogenous(fd_choice ch)
{
  int size=ch->size; int olimit=ch->limit;
  fd_lisp_type elt_type=ch->elt_type;
  lisp *ldata, *write;
  union FD_DATA *read, *limit;
  if (ch->elt_type == 0) return;
  read=ch->elements.data; limit=read+size;
  if (size == ch->limit) {
    write=ldata=fd_malloc(sizeof(lisp)*size*2);
    ch->size=size; ch->limit=size*2;}
  else {
    write=ldata=fd_malloc(sizeof(lisp)*ch->limit);}
  while (read < limit) {
    write->type=elt_type;
    write->data=*read++;
    write++;}
  if (ch->busy) ch->busy=0;
  else fd_free(ch->elements.data,sizeof(union FD_DATA *)*olimit);
  ch->elements.lisp=ldata; ch->elt_type=0;
}

DTYPES_EXPORT
/* _fd_make_choice_homogenous:
     Arguments: a pointer to a choice
     Returns: nothing
  Makes a heterogenous choice into a homogenous one
*/
void _fd_make_choice_homogenous(fd_choice ch)
{
  int size=ch->size, abort=0; 
  lisp *read=ch->elements.lisp, *limit=read+size;
  union FD_DATA *data, *write;
  fd_lisp_type elt_type;
  if (ch->elt_type) return;
  else elt_type=read[0].type;
  data=fd_malloc(ch->limit*sizeof(union FD_DATA)); write=data;
  while (read < limit) 
    if (read->type == elt_type) {
      *write=read->data; write++; read++;}
    else {abort=1; break;}
  if (abort) {
    fd_free(data,ch->limit*sizeof(union FD_DATA));
    fd_raise_exception("Attempt to make heterogenous choice homogenous");}
  else {
    if (ch->busy) ch->busy=0;
    else fd_free(ch->elements.lisp,sizeof(lisp)*ch->limit);
    ch->elt_type=elt_type; ch->elements.data=data;}
}

/** Sorting Choices **/

#if 1
STATIC_INLINE void swap(union FD_DATA *a,union FD_DATA *b)
{
  union FD_DATA t;
  t = *a;
  *a = *b;
  *b = t;
}

void big_sort(union FD_DATA *v,int n)
{
  unsigned i, j, ln, rn;
  while (n > 1) {
    swap(&v[0], &v[n/2]);
    for (i = 0, j = n; ; ) {
      do --j; while (v[j].any > v[0].any);
      do ++i; while (i < j && v[i].any < v[0].any);
      if (i >= j) break; swap(&v[i], &v[j]);}
    swap(&v[j], &v[0]);
    ln = j;
    rn = n - ++j;
    if (ln < rn) {
      big_sort(v, ln); v += j; n = rn;}
    else {big_sort(v + j, rn); n = ln;}}
}
void small_sort(union FD_DATA *v,int n)
{
  unsigned i, j, ln, rn;
  while (n > 1) {
    swap(&v[0], &v[n/2]);
    for (i = 0, j = n; ; ) {
      do --j; while (v[j].any_small > v[0].any_small);
      do ++i; while (i < j && v[i].any_small < v[0].any_small);
      if (i >= j) break; swap(&v[i], &v[j]);}
    swap(&v[j], &v[0]);
    ln = j;
    rn = n - ++j;
    if (ln < rn) {
      small_sort(v, ln); v += j; n = rn;}
      else {
	small_sort(v + j, rn); n = ln;}}
}
#else
static int compare_big(const void *v1,const void *v2)
{
  union FD_DATA *d1=(union FD_DATA *)v1, *d2=(union FD_DATA *)v2;
  if (d1->any == d2->any) return 0;
  else if (d1->any < d2->any) return -1;
  else return 1;
}
static int compare_small(const void *v1,const void *v2)
{
  union FD_DATA *d1=(union FD_DATA *)v1, *d2=(union FD_DATA *)v2;
  if (d1->any_small == d2->any_small) return 0;
  else if (d1->any_small < d2->any_small) return -1;
  else return 1;
}
static void big_sort(union FD_DATA *elts, int n)
{
  qsort(elts,n,sizeof(union FD_DATA),compare_big);
}
static void small_sort(union FD_DATA *elts, int n)
{
  qsort(elts,n,sizeof(union FD_DATA),compare_small);
}
#endif

STATIC_INLINE void compress_sorted_choice(fd_choice ch)
{
  if ((ch->elt_type) && (ch->sorted)) {
    union FD_DATA *read=ch->elements.data, *limit=read+ch->size;
    union FD_DATA current=*read++, *write=read;
    int removals=0;
    if (ch->elt_type < FD_SMALL_TYPE_LIMIT) 
      while (read < limit)
	if (read->any_small == current.any_small) {read++; removals++;}
	else if (read == write) {current=*read; read++; write++;}
	else {
	  write->any_small=read->any_small; current=*read;
	  read++; write++;}
    else while (read < limit)
      if (read->any == current.any) {
	read++; removals++;}
      else if (read == write) {current=*read; read++; write++;}
      else {
	write->any=read->any; current=*read; read++; write++;}
    ch->size=ch->size-removals;}
  ch->sorted=ch->size;
}

static int sort_choice(fd_choice ch)
{
  if (ch->sorted == ch->size) return 1;
  else if (ch->elt_type == 0) return 0;
  else if (ch->elt_type >= FD_ATOMIC_LIMIT) return 0;
  if (ch->busy) {
    ch->elements.data=copy_datav(ch->elements.data,ch->size,ch->limit);
    ch->busy=0;}
  if (ch->elt_type < FD_SMALL_TYPE_LIMIT)
    small_sort(ch->elements.data,ch->size);
  else big_sort(ch->elements.data,ch->size);
  ch->sorted=ch->size; compress_sorted_choice(ch); 
  return 1;
}

DTYPES_EXPORT
/* fd_sort_choice
     Arguments: a pointer to a choice
     Returns: 1/0
  Sorts the elements of a homogenous choice, returns 1 if
the choice was successfully sorted, 0 otherwise
*/
int fd_sort_choice(fd_lisp arg)
{
  if (CHOICEP(arg)) {
    fd_choice ch=PTR_DATA(arg,choice);
    int proper;
    fd_lock_mutex(&(ch->lock));
    proper=sort_choice(ch);
    fd_unlock_mutex(&(ch->lock));
    return proper;}
  else return 1;
}

static
/* item_in_choicep:
     Arguments: two lisp objects
     Returns: 1 or 0

  Returns 1 if the first argument is equal to or a member
of the choice which is the second argument.
*/
int item_in_choicep(lisp x,lisp c)
{
  if (!(CHOICEP(c))) return LISP_EQUAL(x,c);
  else if (ATOMICP(x)) {
    fd_choice ch=PTR_DATA(c,choice);
    if ((ch->elt_type) && (PTR_TYPE(x) != ch->elt_type)) return 0;
    else if (ch->elt_type) {
      /* We invest a lot of cleverness in searching for atoms
	 in homogenous choices, starting with a binary search of the
	 sorted portion and the doing a linear search of whatever has
	 been added since it was last sorted. */
      union FD_DATA *scan, *limit, key;
      fd_lock_mutex(&(ch->lock));
      scan=ch->elements.data+ch->sorted;
      limit=ch->elements.data+ch->size; key=x.data;
      /* Look in the sorted section using a binary search */
      if (ch->sorted) {
	union FD_DATA *bot=ch->elements.data, *top=bot+ch->sorted-1;
	union FD_DATA *middle=bot+(top-bot)/2, key=x.data;
	if (ch->elt_type < FD_SMALL_TYPE_LIMIT) 
	  while (top >= bot)
	    if (key.any_small == middle->any_small) {
	      fd_unlock_mutex(&(ch->lock)); return 1;}
	    else if (key.any_small > middle->any_small) {
	      bot=middle+1; middle=bot+(top-bot)/2;}
	    else {top=middle-1; middle=bot+(top-bot)/2;}
	else while (top >= bot)
	  if (key.any == middle->any) {
	    fd_unlock_mutex(&(ch->lock)); return 1;}
	  else if (key.any > middle->any) {
	    bot=middle+1; middle=bot+(top-bot)/2;}
	  else {top=middle-1; middle=bot+(top-bot)/2;}}
      /* Look among the remaining elements with a linear search,
         using different loops for small (<= int) and large (> int) types */
      if (ch->elt_type < FD_SMALL_TYPE_LIMIT) 
	while (scan < limit)
	  if (key.any_small == scan->any_small) {
	    fd_unlock_mutex(&(ch->lock)); return 1;}
	  else scan++;
      else while (scan < limit)
	if (key.any == scan->any) {
	  fd_unlock_mutex(&(ch->lock)); return 1;}
	else scan++;
      fd_unlock_mutex(&(ch->lock));
      return 0;}
    else { /* At this point, ch must be heterogenous.  We do a linear search with
	      the choice locked, using EQ for comparison. */
      fd_choice ch=PTR_DATA(c,choice); lisp *scan, *limit;
      lock_mutex(&(ch->lock));
      scan=ch->elements.lisp; limit=scan+ch->size;
      while (scan < limit)
	if (LISP_EQ(x,*scan)) {
	  unlock_mutex(&(ch->lock)); return 1;}
	else scan++;
      unlock_mutex(&(ch->lock)); return 0;}}
  /* This may make a private copy of c (DO_CHOICES), so we only use it if c is big.  We don't
     do this same branch for the atomic case above, since copying a homogenous atomic choice
     and doing a linear search should take about the same amount of time. */
  else if (CHOICE_SIZE(c) > 32) {
    int found=0; 
    DO_CHOICES(elt,c) {
      if (LISP_EQUAL(x,elt)) {found=1; break;}}
    END_DO_CHOICES;
    return found;}
  else { /* If the choice is small, we do a linear search with the choice locked */
    fd_choice ch=PTR_DATA(c,choice);
    lock_mutex(&(ch->lock));
    if (ch->elt_type) {
      union FD_DATA *scan=ch->elements.data, *limit=scan+ch->size;
      lisp _tmp; _tmp.type=ch->elt_type;
      while (scan < limit) {
	_tmp.data=*scan++;
	if (LISP_EQUAL(x,_tmp)) {
	  unlock_mutex(&(ch->lock)); return 1;}}
      unlock_mutex(&(ch->lock)); return 0;}
    else {
      lisp *scan=ch->elements.lisp, *limit=scan+ch->size;
      while (scan < limit)
	if (LISP_EQUAL(x,*scan)) {
	  unlock_mutex(&(ch->lock)); return 1;}
	else scan++;
      unlock_mutex(&(ch->lock)); return 0;}}
}

/** Operations on choices **/

DTYPES_EXPORT
/* _fd_add_to_choice:
     Arguments: a lisp object and a lisp non-deterministic set
     Returns: nothing
   Adds the object (not a copy!) to the non-deterministic set.
*/
void _fd_add_to_choice (lisp x,lisp set)
{
  fd_choice ch=PTR_DATA(set,choice);
  /* If it's a heterogenous choice, it's easy */
  if (ch->elt_type == 0) {
    int size=ch->size;
    if (size == ch->limit) grow_choice(ch,next_choice_size(ch->limit));
    ch->elements.lisp[size]=x; ch->size++; ch->sorted=0;}
  /* If the new element's type is the same as the element type, it's simple,
     but just a little different. */
  else if (ch->elt_type == PTR_TYPE(x)) {
    int size=ch->size, grown=0;
    if (size == ch->limit) grow_choice(ch,next_choice_size(ch->limit));
    ch->elements.data[size]=x.data; ch->size++; ch->sorted=0;
    if ((grown) && (ch->elt_type < FD_ATOMIC_LIMIT)) (void) sort_choice(ch);}
  /* Otherwise, we have to convert the homogenous choice into a
     heterogenous choice, and then add the element. */
  else {
    int size=ch->size;
    /* This will leave space for an element */
    _fd_make_choice_heterogenous(ch); 
    ch->elements.lisp[size]=x; ch->size++;}
}

DTYPES_EXPORT
/* fd_remove_from_choice:
   Arguments: a lisp object and a lisp non-deterministic set
   Returns: a non-deterministic set without any occurences of the object

*/
lisp fd_remove_from_choice(lisp x,lisp choice)
{
  if (CHOICEP(choice))
    if (CHOICEP(x)) {
      /* Straightforward, iteration over elements of choice, adding
	 all except x to answer */
      lisp answer=FD_EMPTY_CHOICE;
      DO_CHOICES(each,choice)
	if (item_in_choicep(each,x)) {}
	else {ADD_TO_CHOICE(answer,incref(each));}
      END_DO_CHOICES;
      return answer;}
    else {
      /* Straightforward, iteration over elements of choice, adding
	 all except x to answer */
      lisp answer=FD_EMPTY_CHOICE;
      DO_CHOICES(each,choice)
	if (LISP_EQUAL(each,x)) {}
	else {ADD_TO_CHOICE(answer,incref(each));}
      END_DO_CHOICES;
      return answer;}
  else if (LISP_EQUAL(choice,x)) return FD_EMPTY_CHOICE;
  else return incref(choice);
}

DTYPES_EXPORT
/* _fd_binary_choice:
     Arguments: two lisp objects
     Returns: a non-deterministic set containing copies of them
   This is useful because the transition from simple value to nd-value
    usually starts with a set of two elements.
*/
lisp _fd_binary_choice (lisp x,lisp y)
{
  if (FD_EMPTYP(x)) return y; 
  else if (FD_EMPTYP(y)) return x;
  else if (LISP_EQUAL(x,y)) return x;
  else {
    fd_choice ch; int homogenous=((PTR_TYPE(x) == PTR_TYPE(y)) ? 1 : 0);
    ch=fd_malloca(struct FD_CHOICE);
#if FD_THREADS_ENABLED
  fd_init_mutex(&(ch->lock));
#endif
    ch->size=2; ch->limit=8; ch->busy=0;
    if (homogenous) {
      ch->elt_type=PTR_TYPE(x);
      ch->elements.data=fd_malloc(sizeof(union FD_DATA)*8);
      if (ch->elt_type >= FD_ATOMIC_LIMIT) {
	ch->sorted=0;
	ch->elements.data[0]=x.data; ch->elements.data[1]=y.data;}
      else {
	ch->sorted=2;
	if ((ch->elt_type < FD_SMALL_TYPE_LIMIT) ?
	    (x.data.any_small < y.data.any_small) :
	    (x.data.any < y.data.any)) {
	  ch->elements.data[0]=x.data; ch->elements.data[1]=y.data;}
	else {ch->elements.data[1]=x.data; ch->elements.data[0]=y.data;}}}
    else {
      ch->elt_type=0; ch->sorted=0; ch->busy=0; /* Did ralphc mean to remove this? */
      ch->elements.lisp=fd_malloc(sizeof(lisp)*8);
      ch->elements.lisp[0]=x; ch->elements.lisp[1]=y;}
    /* We know that this is a proper choice (no duplicates)
       because we got past the call to LISP_EQUAL above */
    {RETURN_LISP(proper_choice_type,choice,ch);}}
}

DTYPES_EXPORT
/* fd_list_to_choice:
     Arguments: a lisp list
     Returns: an non-deterministic set whose elements are the elements of
       the list it is given
  This copies (rather than just crefs) the elements it is given
*/
lisp fd_list_to_choice (lisp lst)
{
  if (FD_EMPTY_LISTP(lst)) return (FD_EMPTY_CHOICE);
  else if ((PRIM_TYPEP(lst,pair_type)) && (FD_EMPTY_LISTP(CDR(lst))))
    return incref(CAR(lst));
  else {
    lisp answer=(FD_EMPTY_CHOICE);
    fd_lisp_type type_info=PTR_TYPE(CAR(lst));
    DOLIST(elt,lst) {
      if (PTR_TYPE(elt) != type_info) type_info=0;
      ADD_TO_CHOICE(answer,copy_lisp(elt));}
    if (type_info) _fd_make_choice_homogenous(PTR_DATA(answer,choice));
    return answer;}
}

static void homogenous_merge(fd_choice ch1,fd_choice ch2,int need_incref)
{
  int combined_size=ch1->size+ch2->size, nlimit=ch1->limit;
  union FD_DATA *read, *limit, *write;
  while (combined_size >= nlimit) nlimit=next_choice_size(nlimit);
  if (nlimit > ch1->limit) grow_choice(ch1,nlimit);
  if (need_incref)
    if (ch1->elt_type < FD_ATOMIC_LIMIT) need_incref=0;
  read=ch2->elements.data; limit=read+ch2->size;
  write=ch1->elements.data+ch1->size;
  if (need_incref) {
    lisp tmp_ptr; tmp_ptr.type=ch1->elt_type;
    while (read < limit) {
      tmp_ptr.data=*read; incref(tmp_ptr); *write++=*read++;}}
  else while (read < limit) *write++=*read++;
  ch1->size=ch1->size+ch2->size; ch1->sorted=0;
}

static void heterogenous_merge(fd_choice ch1,fd_choice ch2,int need_incref)
{
  int combined_size=ch1->size+ch2->size, nlimit=ch1->limit;
  fd_lisp *read, *limit, *write;
  while (combined_size >= nlimit) nlimit=next_choice_size(nlimit);
  if (nlimit > ch1->limit) grow_choice(ch1,nlimit);
  read=ch2->elements.lisp; limit=read+ch2->size;
  write=ch1->elements.lisp+ch1->size;
  if (need_incref) {
    while (read < limit) {incref(*read); *write++=*read++;}}
  else while (read < limit) *write++=*read++;
  ch1->size=ch1->size+ch2->size; ch1->sorted=0;
}

/* This adds to ch1 the elements of ch2 where ch1 is heterogenous
   and ch2 is homogenous. */
static void merge_homo_into_hetero(fd_choice ch1,fd_choice ch2,int need_incref)
{
  int combined_size=ch1->size+ch2->size;
  fd_lisp *write; union FD_DATA *read, *limit;
  if (combined_size < ch1->limit) {}
  else if (combined_size < ch1->limit*2) grow_choice(ch1,ch1->limit*2);
  else grow_choice(ch1,ch2->limit*2);
  read=ch2->elements.data; limit=read+ch2->size;
  write=ch1->elements.lisp+ch1->size;
  if (need_incref) {
    lisp tmp_ptr; tmp_ptr.type=ch2->elt_type;
    while (read < limit) {
      tmp_ptr.data=*read++; incref(tmp_ptr); *write++=tmp_ptr;}}
  else {
    lisp tmp_ptr; tmp_ptr.type=ch2->elt_type;    
    while (read < limit) {tmp_ptr.data=*read++; *write++=tmp_ptr;}}
  ch1->size=ch1->size+ch2->size; ch1->sorted=0;
}

DTYPES_EXPORT
/* _fd_merge_choices:
      Arguments: two objects
      Returns: a non-deterministic set which contains the elements of both

This implicitly frees y (its second argument), while adding its elements
to x.

*/
lisp _fd_merge_choices(lisp x,lisp y)
{
  if (FD_EMPTYP(x)) return y;
  else if (FD_EMPTYP(y)) return x;
  else if ((CHOICEP(x)) && (CHOICEP(y))) {
    fd_choice ch1=PTR_DATA(x,choice), ch2=PTR_DATA(y,choice);
    fd_lock_mutex(&(ch1->lock)); fd_lock_mutex(&(ch2->lock));
    if ((ch1->elt_type == 0) && (ch2->elt_type == 0)) {
      int ch2_will_be_free=((PTR_DATA(y,acons))->n_refs == 1);
      heterogenous_merge(ch1,ch2,(!(ch2_will_be_free)));
      if (ch2_will_be_free) {
	fd_free(ch2->elements.lisp,ch2->limit*sizeof(lisp));
	fd_unlock_mutex(&(ch2->lock));
#if FD_THREADS_ENABLED
	fd_destroy_mutex(&(ch2->lock));
#endif
	fd_qfree(ch2,sizeof(struct FD_CHOICE));}
      else decref(y);
      fd_unlock_mutex(&(ch1->lock));
      if (ch2_will_be_free == 0) fd_unlock_mutex(&(ch2->lock));
      x.type=choice_type; /* x may no longer be proper */
      return x;}
    else if (ch1->elt_type == ch2->elt_type) {
      int ch2_will_be_free=((PTR_DATA(y,acons))->n_refs == 1);
      homogenous_merge(ch1,ch2,(!(ch2_will_be_free)));
      if (ch2_will_be_free) {
	fd_free(ch2->elements.data,ch2->limit*sizeof(union FD_DATA));
	fd_unlock_mutex(&(ch2->lock));
#if FD_THREADS_ENABLED
	fd_destroy_mutex(&(ch2->lock));
#endif
	fd_qfree(ch2,sizeof(struct FD_CHOICE));}
      else decref(y);
      fd_unlock_mutex(&(ch1->lock));
      if (ch2_will_be_free == 0) fd_unlock_mutex(&(ch2->lock));
      x.type=choice_type; /* x may no longer be proper */
      return x;}
    else if (ch1->elt_type) {
      int ch2_will_be_free=((PTR_DATA(y,acons))->n_refs == 1);
      _fd_make_choice_heterogenous(ch1);
      if (ch2->elt_type)
	merge_homo_into_hetero(ch1,ch2,(!(ch2_will_be_free)));
      else heterogenous_merge(ch1,ch2,(!(ch2_will_be_free)));
      if (ch2_will_be_free) {
	fd_free(ch2->elements.lisp,sizeof(fd_lisp)*ch2->limit);
	fd_unlock_mutex(&(ch2->lock));
#if FD_THREADS_ENABLED
	fd_destroy_mutex(&(ch2->lock));
#endif
	fd_qfree(ch2,sizeof(struct FD_CHOICE));}
      else decref(y);
      fd_unlock_mutex(&(ch1->lock));
      if (ch2_will_be_free == 0) fd_unlock_mutex(&(ch2->lock));
      x.type=choice_type; /* x may no longer be proper */
      return x;}
    else {
      int ch2_will_be_free=((PTR_DATA(y,acons))->n_refs == 1);
      merge_homo_into_hetero(ch1,ch2,(!(ch2_will_be_free)));
      if (ch2_will_be_free) {
	fd_free(ch2->elements.data,sizeof(union FD_DATA)*ch2->limit);
	fd_unlock_mutex(&(ch2->lock));
#if FD_THREADS_ENABLED
	fd_destroy_mutex(&(ch2->lock));
#endif
	fd_qfree(ch2,sizeof(struct FD_CHOICE));}
      else decref(y);
      fd_unlock_mutex(&(ch1->lock));
      if (ch2_will_be_free == 0) fd_unlock_mutex(&(ch2->lock));
      x.type=choice_type; /* x may no longer be proper */
      return x;}}
  else if (CHOICEP(x)) {
    fd_choice ch1=PTR_DATA(x,choice);
    fd_lock_mutex(&(ch1->lock)); 
    _fd_add_to_choice(y,x);
    fd_unlock_mutex(&(ch1->lock)); 
    x.type=choice_type; /* x may no longer be proper */
    return x;}
  else if (CHOICEP(y)) { /* We may need to copy y */
    lisp copy;
    /* Same logic as below:
         if we have a copy, we don't have to worry about locking,
	 while if we don't need a copy, we don't have to worry about
	 locking either (I hope). */
    if ((PTR_DATA(y,acons)->n_refs) > 1) {
      copy=copy_lisp(y); fd_decref(y);}
    else copy=y;
    _fd_add_to_choice(x,copy);
    /* No longer guaranteed to be proper */
    copy.type=choice_type;
    return copy;}
  else if (LISP_EQUAL(x,y)) {decref(y); return x;}
  else return _fd_binary_choice(x,y);
}

DTYPES_EXPORT
/* fd_merge_choices:
      Arguments: two objects
      Returns: a non-deterministic set which contains the elements of both

Merges two choices, being smart about sorted choices.  This may side
effect the first argument.

*/
lisp fd_merge_choices(lisp x,lisp y)
{
  if (FD_EMPTYP(x)) return y;
  else if (FD_EMPTYP(y)) return x;
  else if ((CHOICEP(x)) && (CHOICEP(y))) {
    lisp result;
    result=_fd_merge_choices(x,incref(y));
    return result;}
  else if (CHOICEP(x)) {
    fd_choice ch1=PTR_DATA(x,choice);
    fd_lock_mutex(&(ch1->lock));
    _fd_add_to_choice(incref(y),x);
    fd_unlock_mutex(&(ch1->lock));
    /* No longer guaranteed to be sorted or proper */
    x.type=choice_type; 
    return x;}
  else if (CHOICEP(y)) { /* We may need to copy y */
    lisp copy;
    /* If we copy y, we don't have to worry about locking it.
       If we don't need to copy y, we don't need to worry about
       locking it either (I think) */
    if ((PTR_DATA(y,acons)->n_refs) > 1) copy=copy_lisp(y);
    else copy=incref(y);
    _fd_add_to_choice(x,copy);
    /* No longer guaranteed to be sorted or proper */
    copy.type=choice_type;
    return copy;}
  else if (LISP_EQUAL(x,y)) return x;
  else return _fd_binary_choice(x,incref(y));
}

DTYPES_EXPORT
/* fd_choice_containsp:
     Arguments: two lisp objects
     Returns: 1 or 0

  Returns 1 if the first argument is a subset of the second.
of the choice which is the second argument.
*/
int fd_choice_containsp(lisp sub,lisp super)
{
  int found=1;
  if (FD_EMPTYP(sub)) return 1;
  else if ((CHOICEP(sub)) && (CHOICEP(super))) {
    FD_DO_CHOICES(elt,sub)
      if (!(item_in_choicep(elt,super))) {
	found=0; break;}
    END_FD_DO_CHOICES;
    return found;}
  /* If X is a choice, but C isn't x can be in C. */
  else if (CHOICEP(sub)) return 0; 
  else if (CHOICEP(super))
    return item_in_choicep(sub,super);
  else return FD_LISP_EQUAL(sub,super);
}

DTYPES_EXPORT
/* fd_choice_overlapsp:
     Arguments: two lisp objects
     Returns: 1 or 0

  Returns 1 if any elements of the first argument overlap elements of the second.
*/
int fd_choice_overlapsp(lisp ch1,lisp ch2)
{
  int found=0;
  if ((CHOICE_SIZE(ch2) < (CHOICE_SIZE(ch1)))) {
    fd_lisp tmp=ch1; ch1=ch2; ch2=tmp;}
  if ((CHOICEP(ch1)) && (CHOICEP(ch2))) {
    FD_DO_CHOICES(elt,ch1)
      if (item_in_choicep(elt,ch2)) {
	found=1; break;}
    END_FD_DO_CHOICES;
    return found;}
  /* If X is a choice, but C isn't x can be in C. */
  else if (CHOICEP(ch1)) return 0; 
  else if (CHOICEP(ch2))
    return item_in_choicep(ch1,ch2);
  else if ((FD_EMPTYP(ch1)) || (FD_QUOTED_EMPTY_CHOICEP(ch1)))
    return 0;
  else return FD_LISP_EQUAL(ch1,ch2);
}

#define PROPER_CHOICE_LINEAR_THRESHOLD 8

static void insert_choice_into_hashset(fd_choice ch,fd_hashset h)
{
  if (ch->elt_type) {
    lisp x; union FD_DATA *scan=ch->elements.data, *limit=scan+ch->size;
    x.type=ch->elt_type; while (scan < limit) {
      x.data=*scan++; fd_hashset_add(h,x);}}
  else {
    lisp *scan=ch->elements.lisp, *limit=scan+ch->size;
    while (scan < limit) {fd_hashset_add(h,*scan); scan++;}}
}

static lisp convert_hashset_to_choice(fd_hashset h)
{
  if (h->n_keys == 1) {
    lisp *scan=h->table, *limit=scan+h->n_slots;
    while (scan < limit) 
      if (!(FD_EMPTYP(*scan))) {
	lisp x=*scan; fd_free(h->table,sizeof(fd_lisp)*h->n_slots);
	return x;}
      else scan++;
    return FD_VOID;}
  else {
    lisp answer=fd_init_choice(h->n_keys), *scan=h->table, *limit=scan+h->n_slots;
    fd_lock_mutex(&(h->lock));
    while (scan < limit)
      if (!(FD_VOIDP(*scan))) {ADD_TO_CHOICE(answer,*scan); scan++;}
      else scan++;
    fd_unlock_mutex(&(h->lock));
    fd_free(h->table,sizeof(fd_lisp)*h->n_slots);
    return answer;}
}

DTYPES_EXPORT
/* fd_sort_hashtable_values:
     Arguments: a pointer to a ahshtable structure
     Returns: void
     This makes each value in the hashtable be a sorted choice. */
void fd_sort_hashtable_values(fd_hashtable h)
{
  struct FD_PAIR **scan, **limit;
  fd_read_lock(&(h->lock));
  scan=h->table; limit=scan+h->n_slots;
  while (scan < limit)
    if (*scan == NULL) scan++;
    else if (FD_CHOICEP((*scan)->cdr)) {
      fd_choice ch=PTR_DATA(((*scan)->cdr),choice);
      if ((ch->elt_type) && (ch->elt_type < FD_ATOMIC_LIMIT)) {
	fd_lock_mutex(&(ch->lock));
	(void) sort_choice(ch);
	fd_unlock_mutex(&(ch->lock));
	FD_SET_PRIM_TYPE(((*scan)->cdr),proper_choice_type);
	scan++;}
      else scan++;}
    else scan++;
  fd_read_unlock(&(h->lock));
}

DTYPES_EXPORT
/* fd_return_proper_choice:
    Arguments: a lisp pointer
    Returns: another lisp pointer

If the argument is a non-deterministic set, this returns a "proper set"
which contains no duplicate elements.
*/
lisp fd_return_proper_choice(lisp values)
{
  if (PRIM_TYPEP(values,choice_type)) {
    fd_choice ch=PTR_DATA(values,choice);
    if (ch->sorted == ch->size) return values;
    else if ((ch->elt_type) && (ch->elt_type < FD_ATOMIC_LIMIT)) {
      fd_lock_mutex(&(ch->lock));
      (void) sort_choice(ch);
      fd_unlock_mutex(&(ch->lock));
      FD_SET_PRIM_TYPE(values,proper_choice_type);
      return values;}
    else {
      struct FD_HASHSET s; 
      unsigned int size=CHOICE_SIZE(values);
      fd_init_hashset(&s,((size+1)*9)/7); 
      fd_lock_mutex(&(ch->lock));
      insert_choice_into_hashset(ch,&s);
      fd_unlock_mutex(&(ch->lock));
      decref(values);
      return convert_hashset_to_choice(&s);}}
  else return values;
}

DTYPES_EXPORT
/* fd_return_sorted_choice:
    Arguments: a lisp pointer
    Returns: another lisp pointer

If the argument is a non-deterministic set which can be sorted, this returns
a sorted version which contains no duplicate elements.  It also reduces a set of
one element to just that element.
*/
lisp fd_return_sorted_choice(lisp values)
{
  if (PRIM_TYPEP(values,choice_type)) {
    fd_choice ch=PTR_DATA(values,choice);
    if ((ch->elt_type) && (ch->elt_type < FD_ATOMIC_LIMIT)) {
      fd_lock_mutex(&(ch->lock));
      if (ch->sorted != ch->size) (void) sort_choice(ch);
      if (ch->size == 1) {
	fd_unlock_mutex(&(ch->lock));
	{
	  fd_lisp v; FD_DO_CHOICES(x,values) v=x; FD_END_DO_CHOICES;
	  fd_incref(v); fd_decref(values);
	  return v;}}
      else {
	FD_SET_PRIM_TYPE(values,proper_choice_type);
	fd_unlock_mutex(&(ch->lock));
	return values;}}
    else if (ch->size == 1) {
      fd_lisp v; FD_DO_CHOICES(x,values) v=x; FD_END_DO_CHOICES;
      fd_incref(v); fd_decref(values);
      return v;}
    else return values;}
  else return values;
}

DTYPES_EXPORT
/* fd_proper_choicep:
     Arguments: a lisp pointer
     Returns: 1 if the argument is a proper choice, 0 otherwise

 A proper choice has no duplicated elements. Note that
 a non-choice lisp pointer is always "proper" since it has
 only one element. */
int fd_proper_choicep(lisp value)
{
  if (FD_PRIM_TYPEP(value,proper_choice_type)) return 1;
  else if (CHOICEP(value)) {
    fd_choice ch=PTR_DATA(value,choice);
    int proper;
    fd_lock_mutex(&(ch->lock));
    proper=sort_choice(ch);
    fd_unlock_mutex(&(ch->lock));
    if (proper) return 1;
    else {
      struct FD_HASHSET h;
      fd_init_hashset(&h,CHOICE_SIZE(value)*2);
      {DO_CHOICES(c,value) fd_hashset_add(&h,c); END_DO_CHOICES}
      if (((int)h.n_keys) == CHOICE_SIZE(value)) {
	fd_free_hashset(&h); return 1;}
      else {fd_free_hashset(&h); return 0;}}}
  else return 1;
}

/** Doing fast intersections **/

static void free_lookups(fd_hashset *lookups,int size)
{
  int i=0; while (i < size)
    if (lookups[i]) {
      fd_free_hashset(lookups[i]);
      fd_free(lookups[i],sizeof(struct FD_HASHSET));
      i++;}
     else i++;
   fd_free(lookups,sizeof(fd_hashset)*size);
}

DTYPES_EXPORT
/* fd_intersect_choices:
     Arguments: a C array of LISP pointers and a size
     Returns: a lisp object

 Returns the intersection of all the choices in the array.  This
 is optimized to take advantage of sorted choices. */
lisp fd_intersect_choices(lisp *choices,int size)
{
  /* First, we find the smallest set.  That's what we'll use for our outer loop. */
  int smallest=0, smallest_size=FD_CHOICE_SIZE(choices[0]); int i=1;
  while (i < size) {
    lisp choice_set=choices[i]; int set_size;
    if ((choice_set.type == choice_type) ||
	(choice_set.type == proper_choice_type)) {
      set_size=choice_set.data.choice->size;}
    else set_size=1;
    if (set_size < smallest_size) {
      smallest_size=set_size; smallest=i++;}
    else i++;}
  /* If the smallest is empty, the intersection is trivial */
  if (smallest_size == 0) return FD_EMPTY_CHOICE;
  /* If the smallest is one, there's not advantage to using
     hashsets to represent the other sets. */
  else if (smallest_size == 1) {
    lisp elt=choices[smallest];
    int i=0; while (i < size)
      if (i == smallest) i++;
      else if (fd_choice_containsp(elt,choices[i])) i++;
      else return FD_EMPTY_CHOICE;
    return incref(choices[smallest]);}
  else {
    /* Otherwise, we set up hashsets for all the sets we think
       it is worth it for.  If the set is a sorted choice, a binary search
       will probably be faster.  If the set is too small, a linear search
       will also be faster.  Only if it's a big unsorted set to we
       initialize a hashset. */
    lisp result=FD_EMPTY_CHOICE;
    lisp starting_set=choices[smallest];
    fd_hashset *lookups=fd_malloc(sizeof(fd_hashset)*size);
    int i=0; while (i < size) {
      if (i == smallest) lookups[i++]=NULL;
      else {
	lisp choice_set=choices[i];
	fd_choice ch=PTR_DATA(choice_set,choice);
	if ((ch->elt_type) && (sort_choice(ch)))
	  lookups[i++]=NULL;
	else if (ch->size < LINEAR_HASHET_TRADEOFF)
	  lookups[i++]=NULL;
	else lookups[i++]=fd_choice_to_hashset(choice_set);}}

    /* Iterate over the smallest set and check each member for
       membership in the other sets. */
    {
      DO_CHOICES(each,starting_set) {
	int i=0; while (i < size)
	  if (i == smallest) i++;
	  else {
	    /* Use the hashset if you made it */
	    if (lookups[i]) 
	      if (fd_hashset_get(lookups[i],each)) i++;
	      else break;
	    else if (fd_choice_containsp(each,choices[i])) i++;
	    else break;}
	if (i == size) {ADD_TO_CHOICE(result,incref(each));}}
      END_DO_CHOICES;
      free_lookups(lookups,size);
      return result;}}
}

/* Quoting and unquoting choices */

DTYPES_EXPORT
/* _fd_quote_choice:
     Arguments: a lisp object (possibly a choice)
     Returns: a lisp object

Returns a lisp object will be a quoted choice if appropriate
*/
fd_lisp _fd_quote_choice(fd_lisp x)
{
  if (FD_CHOICEP(x)) {
    RETURN_LISP(quoted_choice_type,choice,PTR_DATA(x,choice));}
  else if (FD_EMPTYP(x)) return FD_QUOTED_EMPTY_CHOICE;
  else return x;
}

DTYPES_EXPORT
/* _fd_unquote_choice:
     Arguments: a lisp object (possibly a choice)
     Returns: a lisp object

Returns a lisp object will be a quoted choice if appropriate
*/
fd_lisp _fd_unquote_choice(fd_lisp x)
{
  if (FD_PRIM_TYPEP(x,quoted_choice_type)) {
    RETURN_LISP(choice_type,choice,PTR_DATA(x,choice));}
  else if (FD_QUOTED_EMPTY_CHOICEP(x))
    return FD_EMPTY_CHOICE;
  else return x;
}

/* Creating choices from DTYPE representations */

static lisp init_choice(int size,void *vdata)
{
  lisp *data=(lisp *)vdata;
  if (size == 0) return (FD_EMPTY_CHOICE);
  else if (size == 1) {
    lisp elt=data[0]; fd_free(vdata,sizeof(lisp));
    return elt;}
  else {
    fd_choice ch=fd_malloca(struct FD_CHOICE); fd_lisp_type homogenous;
    lisp *read, *write, *limit;
#if FD_THREADS_ENABLED
    fd_init_mutex(&(ch->lock));
#endif
    ch->elements.lisp=fd_malloc(sizeof(lisp)*size);
    ch->limit=size; ch->size=size; ch->sorted=0; ch->elt_type=0; ch->busy=0;
    read=data; write=ch->elements.lisp; limit=read+size;
    homogenous=read[0].type;
    while (read < limit) {
      if (read->type != homogenous) homogenous=0;
      *write++=*read++;}
    fd_free(vdata,size*sizeof(lisp));
    if (homogenous) {
      union FD_DATA *nelts=fd_malloc(sizeof(union FD_DATA)*size);
      lisp *scan=ch->elements.lisp, *limit=scan+size;
      union FD_DATA *write=nelts;
      while (scan < limit) *write++=(scan++)->data;
      ch->elt_type=homogenous;
      fd_free(ch->elements.lisp,sizeof(lisp)*size);
      ch->elements.data=nelts;}
#if FD_THREADS_ENABLED
    fd_init_mutex(&(ch->lock));
#endif
    {RETURN_LISP(choice_type,choice,ch);}}
}

/* fd_initialize_choices_c
     Arguments: none
     Returns: nothing
*/
void fd_initialize_choices_c ()
{
  struct FD_TYPE_REGISTRY *r=fd_register_typecode(choice_type);
  r->package_code=dt_framerd; r->subcode=dt_small_set;
  r->package_restore_fcn=init_choice;

  fd_register_source_file("choices",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: choices.c,v $
   Revision 1.22  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.21  2004/10/04 15:42:12  haase
   A little more early type checking

   Revision 1.20  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.19  2004/09/17 08:32:45  haase
   Fixed leak in one add choice case

   Revision 1.18  2004/08/02 21:14:55  haase
   Fixes to values sorting

   Revision 1.17  2004/07/31 14:03:06  haase
   Made choices grow more slowly as they get big

   Revision 1.16  2004/07/27 16:16:36  haase
   Optimizing choice operations

   Revision 1.15  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.14  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.13  2003/09/30 19:08:22  haase
   Cleaned up mutex deletion without unlocking

   Revision 1.12  2003/09/25 19:04:15  haase
   Fixed choice overlap for two empty choices

   Revision 1.11  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.10.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.10.2.2  2003/01/26 20:32:59  haase
   Check some base cases

   Revision 1.10.2.1  2002/08/09 19:13:35  haase
   Fixed some mutex lock/unlock problems

   Revision 1.10  2002/06/18 01:06:52  haase
   Fix fd_remove_from_choice to remove choices from choices

   Revision 1.9  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.8  2002/04/10 15:07:27  haase
   Fixed nasty bug in homogenous/heterogenous choice merging

   Revision 1.7  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
