/* C Mode */

/* hashprims.c
   Access to hashtables from FDScript
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

static char vcid[] = "$Id: hashprims.c,v 1.21 2005/01/14 16:48:46 haase Exp $";

/** Hashtables **/
/** Hashsets **/
/** Hashset compaction **/
/** Initialization **/

#include "fdscript.h"

/** Hashtables **/

static lisp make_hashtable_lexpr(lisp args)
{ 
  lisp lsize; fd_get_args("MAKE-HASHTABLE",args,&lsize,LISPFIX(50),NULL);
  if (FIXNUMP(lsize))
    return fd_make_hashtable_for_lisp(FIXLISP(lsize));
  else fd_type_error(_("size is not a fixnum"),lsize);
}

static lisp hashtable_size(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    return FD_LISPFIX(h->n_keys);}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_slots(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    return FD_LISPFIX(h->n_slots);}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtablep(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp hashtable_get(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    return fd_hashtable_get(h,key,FD_EMPTY_CHOICE);}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_set_lexpr(lisp args)
{
  fd_lisp tables, keys, values;
  fd_get_args("HASHTABLE-SET!",args,
	      &tables,FD_VOID,&keys,FD_VOID,&values,FD_VOID,NULL);
  {FD_DO_CHOICES(table,tables) {
    FD_DO_CHOICES(key,keys) {
      if (PRIM_TYPEP(table,hashtable_type)) {
	fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
	fd_lisp key_copy=copy_lisp(key);
	fd_hashtable_set(h,key,values);
	fd_decref(key_copy);}
      else fd_type_error(_("not a hashtable"),table);}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;}
  return FD_TRUE;
}

static lisp hashtable_add_lexpr(lisp args)
{
  fd_lisp tables, keys, values;
  fd_get_args("HASHTABLE-ADD!",args,&tables,FD_VOID,&keys,FD_VOID,&values,FD_VOID,NULL);
  {FD_DO_CHOICES(table,tables) {
    FD_DO_CHOICES(key,keys) {
      if (PRIM_TYPEP(table,hashtable_type)) {
	fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
	/* We copy the key in case it gets modified later.
	   This used to be implemented by hash.c, but it slows
	   down hashtable operations, so we keep it for access
	   from Scheme but have a warning for the LISP functions. */
	fd_lisp key_copy=copy_lisp(key);
	fd_hashtable_add(h,key_copy,values);
	fd_decref(key_copy);}
      else fd_type_error(_("not a hashtable"),table);}
    END_FD_DO_CHOICES;}
  END_FD_DO_CHOICES;}
  return FD_TRUE;
}

static lisp hashtable_zap(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    fd_hashtable_set(h,key,FD_EMPTY_CHOICE);
    return FD_TRUE;}
  else fd_type_error(_("not a hashtable"),table);
}

static void hash_increment(fd_hashtable h,lisp key,lisp increment)
{
  lisp current=fd_hashtable_get(h,key,FD_VOID);
  if (FD_VOIDP(current)) 
    fd_hashtable_set(h,key,increment);
  else {
    lisp sum=fd_plus(current,increment);
    fd_lisp key_copy=copy_lisp(key);
    fd_hashtable_set(h,key_copy,sum);
    fd_decref(key_copy);
    fd_decref(sum);}
  fd_decref(current);
}

static void hash_increment_existing(fd_hashtable h,lisp key,lisp increment)
{
  lisp current=fd_hashtable_get(h,key,FD_VOID);
  if (FD_VOIDP(current)) return;
  else {
    lisp sum=fd_plus(current,increment);
    fd_lisp key_copy=copy_lisp(key);
    fd_hashtable_set(h,key_copy,sum);
    fd_decref(key_copy);
    fd_decref(sum);}
  fd_decref(current);
}

static lisp hashtable_increment_lexpr(lisp args)
{
  fd_lisp tables, keys, increments;
  fd_get_args("HASHTABLE-INCREMENT!",args,
	      &tables,FD_VOID,&keys,FD_VOID,&increments,FD_LISPFIX(1),NULL);
  {DO_CHOICES(table,tables)
     if (PRIM_TYPEP(table,hashtable_type)) {
       fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
       DO_CHOICES(increment,increments) {
	 fd_grow_hashtable(h,CHOICE_SIZE(keys)*2);
	 {DO_CHOICES(key,keys) {
	   hash_increment(h,key,increment);}
	 END_DO_CHOICES;}}
       END_DO_CHOICES;}
     else fd_type_error(_("not a hashtable"),table);
  END_DO_CHOICES;}
  return FD_TRUE;
}

static lisp hashtable_increment_existing_lexpr(lisp args)
{
  fd_lisp tables, keys, increments;
  fd_get_args("HASHTABLE-INCREMENT-EXISTING!",args,
	      &tables,FD_VOID,&keys,FD_VOID,&increments,FD_LISPFIX(1),NULL);
  {DO_CHOICES(table,tables)
     if (PRIM_TYPEP(table,hashtable_type)) {
       fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
       DO_CHOICES(increment,increments) {
	 fd_grow_hashtable(h,CHOICE_SIZE(keys)*2);
	 {DO_CHOICES(key,keys) {
	   hash_increment_existing(h,key,increment);}
	 END_DO_CHOICES;}}
       END_DO_CHOICES;}
     else fd_type_error(_("not a hashtable"),table);
  END_DO_CHOICES;}
  return FD_TRUE;
}

static
/* hashtable_skim:
     Arguments: a hashtable and a numeric threshold
     Returns: a choice of keys from the hashtable

Returns (as a choice) all the keys in the hashtable whose
values are greater than the numeric threshold.  */
lisp hashtable_skim(lisp table,lisp maxval)
{
  if (!(PRIM_TYPEP(table,hashtable_type)))
    fd_type_error(_("not a hashtable"),table);
  else {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    int size=h->n_slots; struct FD_PAIR **scan=h->table, **max=scan+size;
    lisp answer=FD_EMPTY_CHOICE;
    while (scan < max) 
      if (*scan) {
	struct FD_PAIR *entry=*scan;
	if (FD_EMPTYP(entry->cdr)) {}
	else if (fd_compare(entry->cdr,maxval) >= 0) {
	  ADD_TO_CHOICE(answer,incref(entry->car));}
	scan++;}
      else scan++;
    return answer;}
}

static lisp hashtable_max(lisp table)
{
  if (!(PRIM_TYPEP(table,hashtable_type)))
    fd_type_error(_("not a hashtable"),table);
  else {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    int size=h->n_slots; struct FD_PAIR **scan=h->table, **max=scan+size;
    int threshold_needs_setting=1; lisp threshold=FD_EMPTY_CHOICE;
    lisp answer=FD_EMPTY_CHOICE;
    while (scan < max) 
      if (*scan) {
	struct FD_PAIR *entry=*scan;
	if (FD_EMPTYP(entry->cdr)) {}
	else if (threshold_needs_setting) {
	  threshold=incref(entry->cdr); threshold_needs_setting=0;
	  ADD_TO_CHOICE(answer,incref(entry->car));}
	else {
	  int cmp=fd_compare(entry->cdr,threshold);
	  if (cmp == 0) {
	    ADD_TO_CHOICE(answer,incref(entry->car));}
	  else if (cmp > 0) {
	    decref(answer); answer=incref(entry->car);
	    decref(threshold); threshold=incref(entry->cdr);}}
	scan++;}
      else scan++;
    decref(threshold);
    return answer;}
}

static lisp hashtable_probe(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    if (fd_hashtable_probe(h,key)) return FD_TRUE;
    else return FD_FALSE;}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_keys(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    lisp answer=FD_EMPTY_CHOICE;
    fd_pair *scan, *limit;
    fd_lock_mutex(&(h->lock));
    scan=h->table; limit=scan+h->n_slots;
    while (scan < limit)
      if (*scan) {
	lisp v=(*scan)->car;
	if (!((FD_VOIDP(v)) || (FD_EMPTYP(((*scan)->cdr)))))  {
	  ADD_TO_CHOICE(answer,incref((*scan)->car));}
	scan++;}
      else scan++;
    fd_unlock_mutex(&(h->lock));
    return answer;}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_values(lisp table)
{
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    lisp answer=FD_EMPTY_CHOICE;
    fd_pair *scan, *limit;
    fd_lock_mutex(&(h->lock));
    scan=h->table; limit=scan+h->n_slots;
    while (scan < limit)
      if (*scan) {
	lisp v=(*scan)->car;
	if (!((FD_VOIDP(v)) || (FD_EMPTYP(((*scan)->cdr)))))  {
	  ADD_TO_CHOICE(answer,incref((*scan)->cdr));}
	scan++;}
      else scan++;
    fd_unlock_mutex(&(h->lock));
    return answer;}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_map(lisp proc,lisp table)
{
  fd_lisp results=(FD_EMPTY_CHOICE);
  if (PRIM_TYPEP(table,hashtable_type)) {
    fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
    fd_pair *scan=h->table, *limit=scan+h->n_slots;
    while (scan < limit)
      if (*scan) {
	lisp k=(*scan)->car, v=(*scan)->cdr;
	if (!((FD_VOIDP(k)) || (FD_EMPTYP(v))))  {
	  fd_lisp argl=FD_MAKE_LIST(2,fd_incref(k),fd_incref(v));
	  fd_lisp result=fd_apply(proc,argl);
	  fd_decref(argl);
	  FD_ADD_TO_CHOICE(results,result);}
	scan++;}
      else scan++;
    return results;}
  else fd_type_error(_("not a hashtable"),table);
}

static lisp hashtable_grow(lisp table,lisp new_size)
{
  if (PRIM_TYPEP(table,hashtable_type))
    if (!(FIXNUMP(new_size)))
      fd_raise_exception("Arg isn't a valid hashtable size");
    else {fd_hashtable h=(fd_hashtable)CPTR_DATA(table);
	  fd_grow_hashtable(h,FIXLISP(new_size));
	  return LISPFIX(h->n_slots);}
  else fd_type_error(_("not a hashtable"),table);
}

/** Hashsets **/

static lisp make_hashset_lexpr(lisp args)
{
  lisp lsize; fd_get_args("MAKE-HASHSET",args,&lsize,LISPFIX(50),NULL);
  if (FIXNUMP(lsize))
    return fd_make_hashset_for_lisp(FIXLISP(lsize));
  else fd_type_error(_("size is not a fixnum"),lsize);
}

static lisp hashsetp(lisp table)
{
  if (PRIM_TYPEP(table,hashset_type))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp hashset_size(lisp table)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return FD_LISPFIX(h->n_keys);}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_slots(lisp table)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return FD_LISPFIX(h->n_slots);}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_get(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    if (fd_hashset_get(h,key)) return FD_TRUE;
    else return FD_FALSE;}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_add(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    fd_hashset_add(h,key);
    return FD_TRUE;}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_zap(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    fd_hashset_drop(h,key);
    return FD_TRUE;}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_filter_lexpr(lisp args)
{
  fd_lisp set, elts;
  fd_get_args("HASHSET-FILTER",args,&set,FD_VOID,&elts,FD_VOID,NULL);
  if (PRIM_TYPEP(set,hashset_type)) {
    lisp answer=FD_EMPTY_CHOICE;
    fd_hashset h=(fd_hashset)CPTR_DATA(set);
    DO_CHOICES(elt,elts)
      if (!(fd_hashset_get(h,elt))) {
	ADD_TO_CHOICE(answer,incref(elt));}
    END_DO_CHOICES;
    return answer;}
  else fd_type_error(_("not a hashset"),set);
}

static lisp hashset_accept_lexpr(lisp args)
{
  fd_lisp set, elts;
  fd_get_args("HASHSET-ACCEPT",args,&set,FD_VOID,&elts,FD_VOID,NULL);
  if (PRIM_TYPEP(set,hashset_type)) {
    lisp answer=FD_EMPTY_CHOICE;
    fd_hashset h=(fd_hashset)CPTR_DATA(set);
    DO_CHOICES(elt,elts)
      if (fd_hashset_get(h,elt)) {
	ADD_TO_CHOICE(answer,incref(elt));}
    END_DO_CHOICES;
    return answer;}
  else fd_type_error(_("not a hashset"),set);
}

static lisp choices_to_hashset_lexpr(lisp args)
{
  if (!(FD_PAIRP(args))) fd_raise_exception(fd_TooFewArgs);
  else if (FD_PAIRP(FD_CDR(args)))
    fd_raise_lisp_exception(fd_TooManyArgs,"choices->hashset",args);
  else {
    fd_hashset h=fd_choice_to_hashset(fd_get_arg(args,0,FD_VOID));
    return fd_make_cptr(hashset_type,h);}
}

static lisp hashset_contains_lexpr(lisp args)
{
  fd_lisp set, elts;
  fd_get_args("HASHSET-CONTAINS?",args,&set,FD_VOID,&elts,FD_VOID,NULL);
  if (PRIM_TYPEP(set,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(set); int found=0;
    DO_CHOICES(v,elts)  /* choice trouble */
      if (fd_hashset_get(h,v)) {found=1; break;}
    END_DO_CHOICES;
    if (found) return FD_TRUE; else return FD_FALSE;}
  else fd_type_error(_("not a hashset"),set);
}

static lisp hashset_grow(lisp table,lisp new_size)
{
  if (PRIM_TYPEP(table,hashset_type))
    if (!(FIXNUMP(new_size)))
      fd_type_error(_("new size is not a fixnum"),new_size);
    else {
      fd_hashset h=(fd_hashset)CPTR_DATA(table);
      fd_grow_hashset(h,FIXLISP(new_size));
      return LISPFIX(h->n_slots);}
  else fd_type_error(_("not a hashset"),table);
}

/** Hashset compaction **/

static lisp hashset_intern(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return fd_hashset_intern(h,key);}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_probe(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return fd_hashset_probe(h,key);}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_compact_vector_helper(fd_hashset h,lisp obj);
static lisp hashset_compact_pair_helper(fd_hashset h,lisp obj);

FASTOP lisp hashset_compact(fd_hashset h,lisp obj)
{
  switch (PTR_TYPE(obj)) {
  case string_type: 
    return fd_hashset_intern(h,obj);
  case pair_type:
    return hashset_compact_pair_helper(h,obj);
  case vector_type:
    return hashset_compact_vector_helper(h,obj);
  case qstring_type:
    return incref(obj);
  default: return incref(obj);}
}

static lisp hashset_compact_pair_helper(fd_hashset h,lisp obj)
{
  return FD_MAKE_PAIR(hashset_compact(h,CAR(obj)),
		      hashset_compact(h,CDR(obj)));
}

static lisp hashset_compact_vector_helper(fd_hashset h,lisp obj)
{
  int i=0, l=VECTOR_LENGTH(obj);
  lisp new=fd_make_vector(l);
  lisp *write=FD_VECTOR_ELEMENTS(new), *read=FD_VECTOR_ELEMENTS(obj);
  while (i < l) {
    write[i]=hashset_compact(h,read[i]); i++;}
  return new;
}

static lisp hashset_compact_cproc(lisp table,lisp key)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    return hashset_compact(h,key);}
  else fd_type_error(_("not a hashset"),table);
}

static lisp hashset_map(fd_lisp proc,fd_lisp table)
{
  if (PRIM_TYPEP(table,hashset_type)) {
    fd_lisp results=(FD_EMPTY_CHOICE);
    fd_hashset h=(fd_hashset)CPTR_DATA(table);
    lisp *scan=h->table, *limit=scan+h->n_slots;
    lisp answer=(FD_EMPTY_CHOICE);
    lock_mutex(&(h->lock));
    while (scan < limit)
      if (!((FD_EMPTYP(*scan)) || (FD_VOIDP(*scan)))) {
	lisp v=*scan++, argl=FD_MAKE_LIST1(fd_incref(v));
	lisp result=fd_apply(proc,argl); 
	ADD_TO_CHOICE(results,result); fd_decref(argl);}
      else scan++;
    unlock_mutex(&(h->lock));
    return results;}
  else fd_type_error(_("not a hashset"),table);
}

/** Initialization **/

FDSCRIPT_EXPORT
void fd_initialize_hashprims_c()
{
  fd_add_lexpr(NULL,"MAKE-HASHTABLE",FD_NORMAL_LEXPR,make_hashtable_lexpr);
  fd_add_cproc(NULL,"HASHTABLE?",1,hashtablep);
  fd_add_cproc(NULL,"HASHTABLE-SIZE",1,hashtable_size);
  fd_add_cproc(NULL,"HASHTABLE-SLOTS",1,hashtable_slots);
  fd_add_cproc(NULL,"HASHTABLE-GET",2,hashtable_get);
  fd_add_cproc(NULL,"HASHTABLE-PROBE",2,hashtable_probe);
  fd_add_lexpr(NULL,"HASHTABLE-SET!",FD_ND_LEXPR,hashtable_set_lexpr);
  fd_add_lexpr(NULL,"HASHTABLE-ADD!",FD_ND_LEXPR,hashtable_add_lexpr);
  fd_add_cproc(NULL,"HASHTABLE-ZAP!",2,hashtable_zap);
  fd_add_cproc(NULL,"HASHTABLE-GROW",2,hashtable_grow);
  fd_add_cproc(NULL,"HASHTABLE-KEYS",1,hashtable_keys);
  fd_add_cproc(NULL,"HASHTABLE-VALUES",1,hashtable_values);
  fd_add_cproc(NULL,"HASHTABLE->ALIST",1,fd_hashtable_to_alist);
  fd_add_cproc(NULL,"ALIST->HASHTABLE",1,fd_alist_to_hashtable);

  fd_add_lexpr
    (NULL,"HASHTABLE-INCREMENT!",FD_ND_LEXPR,hashtable_increment_lexpr);
  fd_add_lexpr
    (NULL,"HASHTABLE-INCREMENT-EXISTING!",
     FD_ND_LEXPR,hashtable_increment_existing_lexpr);
  fd_add_cproc(NULL,"HASHTABLE-SKIM",2,hashtable_skim);
  fd_add_cproc(NULL,"HASHTABLE-MAX",1,hashtable_max);

  fd_add_cproc(NULL,"HASHTABLE-MAP",2,hashtable_map);

  fd_add_lexpr(NULL,"MAKE-HASHSET",FD_NORMAL_LEXPR,make_hashset_lexpr);
  fd_add_cproc(NULL,"HASHSET?",1,hashsetp);
  fd_add_cproc(NULL,"HASHSET-SIZE",1,hashset_size);
  fd_add_cproc(NULL,"HASHSET-SLOTS",1,hashset_slots);
  fd_add_cproc(NULL,"HASHSET-GET",2,hashset_get);
  fd_add_cproc(NULL,"HASHSET-ADD!",2,hashset_add);
  fd_add_cproc(NULL,"HASHSET-ZAP!",2,hashset_zap);
  fd_add_cproc(NULL,"HASHSET-GROW",2,hashset_grow);
  fd_add_cproc(NULL,"HASHSET-GROW!",2,hashset_grow);
  fd_add_cproc(NULL,"HASHSET-ELEMENTS",1,fd_lisp_hashset_elts);
  fd_add_cproc(NULL,"HASHSET-ELTS",1,fd_lisp_hashset_elts);
  fd_add_cproc(NULL,"HASHSET-INTERN",2,hashset_intern);
  fd_add_cproc(NULL,"HASHSET-PROBE",2,hashset_probe);
  fd_add_cproc(NULL,"HASHSET-COMPACT",2,hashset_compact_cproc);

  fd_add_lexpr(NULL,"CHOICES->HASHSET",FD_ND_LEXPR,choices_to_hashset_lexpr);
  fd_add_alias(NULL,"VALUES->HASHSET","CHOICES->HASHSET");
  fd_add_lexpr(NULL,"HASHSET-FILTER",FD_ND_LEXPR,hashset_filter_lexpr);
  fd_add_lexpr(NULL,"HASHSET-ACCEPT",FD_ND_LEXPR,hashset_accept_lexpr);
  fd_add_lexpr(NULL,"HASHSET-CONTAINS?",FD_ND_LEXPR,hashset_contains_lexpr);
  fd_add_cproc(NULL,"HASHSET-MAP",2,hashset_map);

  fd_register_source_file("hashprims",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: hashprims.c,v $
   Revision 1.21  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.20  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.19  2004/04/02 12:43:03  haase
   Added hashtable-values

   Revision 1.18  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.17.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.17.2.1  2003/01/26 20:41:47  haase
   Misc. fixes especially some GC

   Revision 1.17  2002/07/05 21:20:07  uid59704
   Fixed GC contract of fd_hashset_intern and added fd_hashset_intern string

   Revision 1.16  2002/07/02 16:47:16  haase
   Changed fd_hashset_probe to incref its return value, obviating the need to do it in the C primitive handler

   Revision 1.15  2002/04/26 16:34:43  haase
   Fixed procedure names in calls to fd_get_args

   Revision 1.14  2002/04/20 19:47:48  haase
   Renamed fd_hashset_zap to fd_hashset_drop

   Revision 1.13  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
