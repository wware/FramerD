/* C Mode */

/* index.c
   Implements generic persistent hash tables DType objects.
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

static char vcid[] = "$Id: index.c,v 1.39 2005/01/14 16:48:46 haase Exp $";

/** Utility definitions **/
/** Registering indices **/
/** Basic operations **/
/** Index control procedures **/
/** Operations over all indices **/
/** Interpreting index specifications **/
/** Initialization **/

#include "framerd.h"

fd_exception
  fd_NoHashMethod=_("Object cannot be hashed to store in a file index"),
  fd_NoKeysMethod=_("Index cannot enumerate keys"),
  fd_BadIndexSpec=_("Index identification is invalid"),
  fd_ReadOnlyIndex=_("Index cannot be modified");
  
#if FD_THREADS_ENABLED
static fd_mutex all_indices_write_lock;
#endif

/** Utility definitions **/

static lisp intern_values(lisp v,fd_hashset h)
{
  if (CHOICEP(v)) {
    lisp results=FD_EMPTY_CHOICE;
    DO_CHOICES(elt,v)
      if (ATOMICP(elt)) {ADD_TO_CHOICE(results,elt);}
      else {
	lisp interned=fd_hashset_intern(h,elt);
	ADD_TO_CHOICE(results,interned);}
    END_DO_CHOICES;
    decref(v); return results;}
  else if (ATOMICP(v)) return v;
  else {
    lisp interned=fd_hashset_intern(h,v);
    decref(v); return interned;}
}

static fd_lisp get_xname(fd_u8char *start,fd_u8char *end)
{
  struct FD_STRING_STREAM ss; fd_u8char *scan=start; fd_lisp sym;
  FD_INITIALIZE_STRING_STREAM(&ss,64);
  while (scan < end) {
    int c=fd_sgetc(&scan); c=fd_toupper(c); fd_sputc(&ss,c);}
  sym=fd_make_symbol(ss.ptr); fd_xfree(ss.ptr);
  return sym;
}


/** Registering indices **/

/* This is very simple.  Indices live on a linked list where each
   points to the next.  We don't currently support deletion of indices,
   so we don't need to worry about locking this data structure. */

fd_index fd_all_indices=NULL;
static int _fd_index_count=0;

/* register_index: (static)
    Arguments: a pointer to an index object
    Returns: nothing
 Updates the linked list to include an index.
 */
void fd_register_index(fd_index x) 
{ 
  lock_mutex(&all_indices_write_lock);
  x->serial=_fd_index_count++; x->next=fd_all_indices; fd_all_indices=x;
  unlock_mutex(&all_indices_write_lock);
}

FRAMERD_EXPORT
/* fd_find_index:
    Arguments: a string
    Returns: a pointer to an index or NULL
 Returns the index whose id matches the given string.
 */
fd_index fd_find_index(fd_u8char *id)
{
  fd_index scan=fd_all_indices;
  while (scan) 
    if (strcmp(scan->id,id) == 0) return scan;
    else scan=scan->next;
  return NULL;
}

FRAMERD_EXPORT
/* fd_for_indices:
  Arguments: a function on pointers to indices
  Returns: nothing

 Calls the function on all registered indices.
*/
void fd_for_indices(void (*fcn)(fd_index p,void *arg),void *arg)
{
  fd_index scan=fd_all_indices;
  while (scan) {fcn(scan,arg); scan=scan->next;}
}

FRAMERD_EXPORT
/* fd_get_index_count:
  Arguments: nothing
  Returns: the number of current indices
*/
int fd_get_index_count()
{
  int i=0;
  fd_index scan=fd_all_indices;
  while (scan) {i++; scan=scan->next;}
  return i;
}

/** Basic operations **/

FRAMERD_EXPORT
/* fd_index_get:
     Arguments: an index pointer, a lisp key, and a default value
     Returns: the values associated with the key in the index

  Returns the values associated with the key in the index or the default
   value if there are none. */
lisp fd_index_get(fd_index x,lisp key,lisp dflt)
{
  if (x->handler->ix_get)
    return x->handler->ix_get(x,key);
  else {
    lisp v=fd_hashtable_get(&(x->cache),key,FD_VOID);
    if (!(FD_VOIDP(v))) return v;
    else if (x->handler->ix_fetch) v=x->handler->ix_fetch(x,key);
    if (fd_hashtable_probe(&(x->adds),key)) {
      /* Get whatever we've changed */
      lisp plus=(fd_hashtable_get(&(x->adds),key,FD_EMPTY_CHOICE));
      /* If we're adding, convert the current void value to the empty choice */
      if (FD_VOIDP(v)) v=FD_EMPTY_CHOICE;
      ADD_TO_CHOICE(v,plus);}
    if (FD_VOIDP(v)) {
      if (x->cache.n_slots) fd_hashtable_set(&(x->cache),key,fd_incref(dflt));
      return incref(dflt);}
    if (fd_hashtable_probe(&(x->drops),key)) {
      /* Note that this is an expensive way to do this. */
      lisp minus=(fd_hashtable_get(&(x->drops),key,FD_EMPTY_CHOICE));
      DO_CHOICES(m,minus) v=fd_remove_from_choice(m,v); END_DO_CHOICES;
      decref(minus);}
    /* Intern the value if you're doing that */
    if (x->interned_values.n_slots)
      v=intern_values(v,&(x->interned_values));
    /* Store it in the cache */    
    if (x->cache.n_slots) fd_hashtable_set(&(x->cache),key,v);
    return v;}
}

FRAMERD_EXPORT
/* fd_index_get_size:
     Arguments: an index pointer, a lisp key
     Returns: the number of values associated with the key in the index

  Returns the number of values associated with the key in the index */
unsigned int fd_index_get_size(fd_index x,lisp key)
{
  if (x->handler->ix_get_size)
    return x->handler->ix_get_size(x,key);
  else {
    lisp v=fd_hashtable_get(&(x->cache),key,FD_VOID), s;
    if (!(FD_VOIDP(v))) {
      int size;
      if (FD_EMPTYP(v)) size=0;
      else if (CHOICEP(v)) size=CHOICE_SIZE(v);
      else size=1;
      decref(v);
      return size;}
    /* The value isn't cached, but maybe the size is... */
    if (x->sizes.n_keys)
      s=fd_hashtable_get(&(x->sizes),key,FD_VOID);
    else s=FD_VOID;
    if (!(FD_VOIDP(s))) {
      /* Now, add the values you have */
      int size;
      lisp plus=fd_hashtable_get(&(x->adds),key,FD_VOID);
      lisp minus=fd_hashtable_get(&(x->drops),key,FD_VOID);
      size=FIXLISP(s)+CHOICE_SIZE(plus)-CHOICE_SIZE(minus);
      decref(s); decref(plus); decref(minus);
      return size;}
    else if (x->handler->ix_fetch_size) {
      unsigned int size=x->handler->ix_fetch_size(x,key);
      lisp plus=fd_hashtable_get(&(x->adds),key,FD_VOID);
      lisp minus=fd_hashtable_get(&(x->drops),key,FD_VOID);
      size=size+CHOICE_SIZE(plus)-CHOICE_SIZE(minus);
      decref(plus); decref(minus);
      if (x->sizes.n_keys)
	fd_hashtable_set(&(x->sizes),key,LISPFIX(size));
      return size;}
    else {
      lisp v=fd_index_get(x,key,FD_EMPTY_CHOICE);
      if (x->sizes.n_keys)
	fd_hashtable_set(&(x->sizes),key,LISPFIX(CHOICE_SIZE(v)));
      decref(v);
      return CHOICE_SIZE(v);}}
}

FRAMERD_EXPORT
/* fd_index_add:
     Arguments: an index pointer, a lisp key, a lisp value
     Returns: an index

  Adds the value to the values associated with the key in the index. */
void fd_index_add(fd_index x,lisp key,lisp value)
{
  if (x->handler->ix_add) {
    x->handler->ix_add(x,key,value);
    return;}
  else if (x->read_only)
    fd_raise_detailed_exception(fd_ReadOnlyIndex,x->id);
  else if (x->interned_values.n_slots)
    value=intern_values(value,&(x->interned_values));
  {DO_CHOICES(each,key) {
    fd_hashtable_add(&(x->adds),each,value);
    if (fd_hashtable_probe(&(x->cache),each))
      fd_hashtable_add(&(x->cache),each,value);
    if (x->sizes.n_keys)
      fd_hashtable_increment_existing(&(x->sizes),each,CHOICE_SIZE(value));}
   END_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_index_drop:
     Arguments: an index pointer, a lisp key, a lisp value
     Returns: an index

  Removes the values from the values associated with the key in the index. */
void fd_index_drop(fd_index x,lisp key,lisp value)
{
  if (x->handler->ix_drop) {
    x->handler->ix_drop(x,key,value);
    return;}
  else if (x->read_only)
    fd_raise_detailed_exception(fd_ReadOnlyIndex,x->id);
  {DO_CHOICES(each,key) {
    fd_hashtable_add(&(x->drops),each,value);
    if (fd_hashtable_probe(&(x->cache),each))
      fd_hashtable_drop(&(x->cache),each,value);
    if (fd_hashtable_probe(&(x->adds),each))
      fd_hashtable_drop(&(x->adds),each,value);
    if (x->sizes.n_keys)
      fd_hashtable_increment_existing
	(&(x->sizes),each,-((int)CHOICE_SIZE(value)));}
  END_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_index_set:
     Arguments: an index pointer, a lisp key, a lisp value
     Returns: an index

  Sets the values associated with the key in the index. */
void fd_index_set(fd_index x,lisp key,lisp value)
{
  lisp current=fd_index_get(x,key,FD_EMPTY_CHOICE);
  {DO_CHOICES(elt,current) {
    if ((fd_choice_containsp(elt,value)) == 0)
      fd_index_drop(x,key,elt);}
   END_DO_CHOICES;}
  {DO_CHOICES(elt,value)
     if (!(fd_choice_containsp(elt,current))) fd_index_add(x,key,elt);
   END_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_commit_index:
     Arguments: a pointer to an index
     Returns: nothing

  Saves the changes made to the specified index. */
void fd_commit_index(fd_index x)
{
  if (fd_ephemeralp()) return;
  UNWIND_PROTECT {
    lock_mutex(&(x->lock));
    if ((x->read_only) ||
	((x->adds.n_keys == 0) && (x->drops.n_keys == 0))) {}
    else if (x->handler->ix_commit) {
      fd_notify("Committing changes to %s (%d adds, %d drops)",
		x->id,x->adds.n_keys,x->drops.n_keys);
      x->handler->ix_commit(x);}}
  ON_UNWIND {
    unlock_mutex(&(x->lock));}
  END_UNWIND;
}

FRAMERD_EXPORT
/* fd_revert_index:
     Arguments: a pointer to an index
     Returns: nothing

  Erases all of the changes made to the specified index. */
void fd_revert_index(fd_index x)
{
  fd_reinit_hashtable(&(x->cache),x->cache_size,0);
  if (x->interned_values.n_slots) {
    fd_reinit_hashset(&(x->interned_values),128,0);}
  fd_reinit_hashtable(&(x->adds),x->adds_size,0);
  fd_reinit_hashtable(&(x->drops),x->drops_size,0);
  if (x->sizes_size>0)
    fd_reinit_hashtable(&(x->sizes),x->sizes_size,0);
} 

FRAMERD_EXPORT
/* fd_index_keys:
     Arguments: a pointer to an index
     Returns: nothing

  Non-deterministically returns all the keys in an index. */
lisp fd_index_keys(fd_index idx)
{
  struct FD_PAIR **scan, **limit;
  lisp keys;
  if (idx->handler->ix_dir) keys=idx->handler->ix_dir(idx);
  else fd_raise_detailed_exception(fd_NoKeysMethod,idx->id);
  /* Add current keys */
  {UNWIND_PROTECT {
    lock_mutex(&(idx->adds.lock));
    scan=idx->adds.table; limit=scan+idx->adds.n_slots;
    while (scan < limit)
      if (*scan) {
	lisp v=incref((*scan)->car); ADD_TO_CHOICE(keys,v); scan++;}
      else scan++;}
  ON_UNWIND {
    unlock_mutex(&(idx->adds.lock));}
  END_UNWIND;}
  return keys;
}

FRAMERD_EXPORT
/* fd_index_prefetch:
     Arguments: an index pointer, and a choice of keys
     Returns: an index

  Prefetches the values for particular keys. */
void fd_index_prefetch(fd_index x,lisp keys)
{
  if (FD_EMPTYP(keys)) return;
  else if (!(FD_CHOICEP(keys))) {
    lisp v=fd_index_get(x,keys,FD_EMPTY_CHOICE); fd_decref(v);}
  else if (x->handler->ix_prefetch) {
    lisp needed=FD_EMPTY_CHOICE;
    fd_hashtable h=&(x->cache);
    DO_CHOICES(key,keys) {
      if (!(fd_hashtable_probe(h,key))) {
	ADD_TO_CHOICE(needed,incref(key));}}
    END_DO_CHOICES;
    if (!(FD_EMPTYP(needed))) x->handler->ix_prefetch(x,needed);
    decref(needed);}
  else {
    DO_CHOICES(key,keys) {
      fd_lisp v=fd_index_get(x,key,FD_VOID);
      fd_decref(v);}
    END_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_close_index:
     Arguments: a pointer to an index
     Returns: nothing

     Closes an index */
void fd_close_index(fd_index idx)
{
  if (idx->handler->ix_close) idx->handler->ix_close(idx);
}

/** Syncing indices **/

FRAMERD_EXPORT
/* fd_sync_index:
     Arguments: a pointer to an index
     Returns: nothing
*/
int fd_sync_index(fd_index x)
{
  if (x->handler->ix_sync)
    return x->handler->ix_sync(x);
  else return -1;
}

FRAMERD_EXPORT
/* fd_sync_indices:
     Arguments: nothing
     Returns: nothing

  Commits all the changes made to all the indices. */
void fd_sync_indices()
{
  fd_index scan=fd_all_indices;
  while (scan) {fd_sync_index(scan); scan=scan->next;}
}

/** Index control procedures **/

FRAMERD_EXPORT
/* fd_set_zipf_threshold:
  Arguments: an index, an integer
  Returns: nothing
 Sets the save threshold for an index, so that keys with fewer
than threshold values will not be written to disk or server
*/
void fd_set_index_zipf_threshold(fd_index ix,int threshold)
{
  ix->zipf_threshold=threshold;
}

FRAMERD_EXPORT
/* fd_index_intern_values:
     Arguments: an index pointer
     Returns: void

  Arranges for an index to intern the values it stores */
void fd_intern_index_values(fd_index x)
{
  if (x->interned_values.n_slots == 0) 
    fd_init_hashset(&(x->interned_values),128);
}

FRAMERD_EXPORT
/* fd_index_set_sizes:
     Arguments: a pointer to an index
     Returns: nothing

  Grows the internal tables for an index to accomodate n keys and n mods  
*/
void fd_index_set_sizes
  (fd_index x,int cache_size,int adds_size,int drops_size,int sizes_size)
{
  if (cache_size > 0) {
    fd_grow_hashtable(&(x->cache),cache_size);
    x->cache_size=cache_size;}
  if (adds_size > 0) {
    fd_grow_hashtable(&(x->adds),adds_size);
    x->adds_size=adds_size;}
  if (drops_size > 0) {
    fd_grow_hashtable(&(x->drops),drops_size);
    x->drops_size=drops_size;}
  if (x->sizes_size < 0)
    fd_warn(_("Can't specify size cache size for a preloaded index"));
  else if (sizes_size > 0) {
    fd_grow_hashtable(&(x->sizes),sizes_size);
    x->sizes_size=sizes_size;}
} 

FRAMERD_EXPORT
/* fd_swap_out_index:
     Arguments: a pointer to an index
     Returns: nothing

  Frees most of the space used by the cached values of an index. */
void fd_swap_out_index(fd_index x)
{
  fd_lock_mutex(&(x->lock));
  if ((x->cache.n_keys) || (x->sizes.n_keys)) {
    fd_notify(_("Swapping out index %s"),x->id);
    fd_reinit_hashtable(&(x->cache),x->cache_size,0);
    if (x->sizes_size > 0)
      fd_reinit_hashtable(&(x->sizes),x->sizes_size,0);}
  if (x->interned_values.n_slots) {
    fd_reinit_hashset(&(x->interned_values),128,0);}
  fd_unlock_mutex(&(x->lock));
}

FRAMERD_EXPORT
/* fd_preload_index:
     Arguments: a pointer to an index
     Returns: void
  This arranges for an index to preload a lot of stuff in order to be
  faster.  For a file index, it preloads all the key sizes, which incidentally
  tells you if a key is in the table, speeding up empty fetches.  */
int fd_preload_index(fd_index x)
{
  if (x->handler->ix_preload)
    return x->handler->ix_preload(x,1);
  else return -1;
}

FRAMERD_EXPORT
/* fd_unpreload_index:
     Arguments: a pointer to an index
     Returns: void
  Reinits the size cache and basically undoes the effects of
  fd_preload_file_index. */
int fd_unpreload_index(fd_index x)
{
  if (x->handler->ix_preload)
    return x->handler->ix_preload(x,0);
  else return -1;
}


/** Operations over all indices **/

FRAMERD_EXPORT
/* fd_commit_indices:
     Arguments: nothing
     Returns: nothing

  Commits all the changes made to all the indices. */
void fd_commit_indices()
{
  fd_index scan=fd_all_indices;
  while (scan) {fd_commit_index(scan); scan=scan->next;}
}

FRAMERD_EXPORT
/* fd_revert_indices:
     Arguments: nothing
     Returns: nothing

  Erases all of the changes made to all the indices. */
void fd_revert_indices()
{
  fd_index scan=fd_all_indices;
  while (scan) {fd_revert_index(scan); scan=scan->next;}
}

FRAMERD_EXPORT
/* fd_swap_out_indices:
     Arguments: nothing
     Returns: nothing

   Frees all the cache storage for indices. */
void fd_swap_out_indices()
{
  fd_index scan=fd_all_indices;
  while (scan) {fd_swap_out_index(scan); scan=scan->next;}
}

/** Interpreting index specifications **/

FRAMERD_EXPORT
/* fd_open_index:
     Arguments: a string
     Returns: an index

  Returns an index object based on the string argument.  If the string
    has the form "port@host" it is taken to be a network index; otherwise,
    it is taken to be a file index which is opened. */
fd_index fd_open_index(fd_u8char *spec)
{
  fd_index answer=NULL; fd_u8char *at=NULL, *amp, *orchar; 
  if (spec == NULL) fd_raise_exception(_("NULL index spec"));
  else if (orchar=strchr(spec,'|')) {
    int n_indices=0, max_indices=32;
    fd_index ix, *indices=fd_xmalloc(sizeof(fd_index)*32);
    char *copy=fd_strdup(spec), *scan=copy, *start=scan;
    while (1) 
      if ((*scan == '|') || (*scan == '\0')) {
	int end=(*scan == '\0');
	*scan=NUL; ix=fd_open_index(start);
	if (n_indices>=max_indices) {
	  indices=fd_realloc(indices,sizeof(fd_index)*max_indices*2,
			     sizeof(fd_index)*max_indices);
	  max_indices=max_indices*2;}
	indices[n_indices++]=ix;
	if (end) break; else {*scan='|'; start=scan+1; scan++;}}
      else scan++;
    ix=(fd_index)fd_make_compound_index(n_indices,indices);
    ix->id=copy;
    return ix;}
  else if (amp=strchr(spec,'&')) {
    char *copy=fd_strdup(spec), *scan=copy, *start=scan;
    while (*scan != 0) 
      if (*scan == '&') {
	*scan=NUL;
	if (answer) fd_open_index(start);
	else answer=fd_open_index(start);
	start=scan+1; scan++;}
      else scan++;
    fd_open_index(start); fd_xfree(copy);
    return answer;}
  if (at=strchr(spec,'@')) {
    fd_network_index nindex; int portno;
    fd_u8char *name, *port;
    fd_lisp xname;
    if (strchr(at+1,'@')) {
      port=at+1; at=strchr(port,'@');
      xname=get_xname(spec,port-1);}
    else {port=spec; xname=FD_VOID;}
    if (strcmp(at+1,"local") == 0) {
      fd_u8char *tmp=fd_xmalloc(at-port+1);
      strncpy(tmp,spec,at-spec); tmp[at-spec]=NUL;
      name=fd_make_os_string(tmp); fd_xfree(tmp);
      portno=-1;}
    else {
      char *copy=fd_strdup(port); copy[at-port]='\0';
      portno=fd_get_portno(copy); fd_xfree(copy);
      name=fd_strdup(at+1);}
    nindex=fd_open_network_index(name,portno,xname,spec); fd_xfree(name);
    return (fd_index) nindex;}
  else return fd_open_file_index(spec);
}

static fd_index interpret_index(lisp spec)
{
  if (PRIM_TYPEP(spec,index_type))
    return (fd_index) CPTR_DATA(spec);
  else if (STRINGP(spec)) {
    fd_index exists=fd_find_index(STRING_DATA(spec));
    if (exists) return exists;
    else return fd_open_index(STRING_DATA(spec));}
  else if ((SYMBOLP(spec)) &&
	   (PRIM_TYPEP(SYMBOL_VALUE(spec),index_type)))
    return (fd_index) CPTR_DATA(SYMBOL_VALUE(spec));
  else if (PAIRP(spec)) {
    lisp host=CAR(spec), port=CDR(spec);
    if ((STRINGP(host)) && (FIXNUMP(port))) {
      char buf[128];
      sprintf(buf,"%d@%s",FIXLISP(port),STRING_DATA(host));
      return (fd_index)
	fd_open_network_index(STRING_DATA(host),FIXLISP(port),FD_VOID,buf);}
    else fd_raise_lisp_exception(fd_BadServerSpec,_("for index"),spec);}
  else if (SYMBOLP(spec)) {
    char *value=fd_string_getenv(SYMBOL_NAME(spec));
    if (value == NULL)
      fd_raise_detailed_exception(fd_GetEnvFailed,SYMBOL_NAME(spec));
    else {
      fd_index rx=fd_open_index(value); fd_xfree(value);
      return rx;}}
  else fd_raise_lisp_exception(fd_BadIndexSpec,"",spec);
}

FRAMERD_EXPORT
/* fd_interpret_index:
     Arguments: an expression and an environment
     Returns: a pointer to an index
 Evaluates the expression in the environment and tries to produce an
index object from the result. */
fd_index fd_interpret_index(lisp spec)
{
  return interpret_index(spec);
}

/** Compound indices **/

static fd_lisp compound_index_fetch(fd_index ix,fd_lisp key)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices;
  fd_lisp results=FD_EMPTY_CHOICE;
  while (i < n) {
    fd_lisp r=cix->indices[i]->handler->ix_fetch(cix->indices[i],key);
    if (!(FD_VOIDP(r))) {FD_ADD_TO_CHOICE(results,r);}
    i++;}
  return results;
}

static int compound_index_fetch_size(fd_index ix,fd_lisp key)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices, count=0;
  fd_lisp results=FD_EMPTY_CHOICE;
  while (i < n) {
    int nvals;
    if (cix->indices[i]->handler->ix_fetch_size)
      nvals=cix->indices[i]->handler->ix_fetch_size(cix->indices[i],key);
    else {
      fd_lisp v=fd_index_get(cix->indices[i],key,FD_EMPTY_CHOICE);
      nvals=FD_CHOICE_SIZE(v); fd_decref(v);}
    count=count+nvals;
    i++;}
  return count;
}

static fd_lisp compound_index_dir(fd_index ix)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices;
  fd_lisp results=FD_EMPTY_CHOICE;
  while (i < n) {
    fd_lisp r=cix->indices[i]->handler->ix_dir(cix->indices[i]);
    FD_ADD_TO_CHOICE(results,r);
    i++;}
  return results;
}

static void compound_index_prefetch(fd_index ix,fd_lisp keys)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices;
  while (i < n) {
    if (cix->indices[i]->handler->ix_prefetch)
      cix->indices[i]->handler->ix_prefetch(cix->indices[i],keys);
    else {
      DO_CHOICES(key,keys) {
	fd_lisp v=fd_index_get(ix,key,FD_VOID); fd_decref(v);}
      END_DO_CHOICES;}
    i++;}
}

static void compound_index_cache_index(fd_index ix)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices;
  while (i < n) {
    if (cix->indices[i]->handler->ix_spend_memory)
      cix->indices[i]->handler->ix_spend_memory(cix->indices[i]);
    i++;}
}

static void compound_index_close(fd_index ix)
{
  struct FD_COMPOUND_INDEX *cix=(struct FD_COMPOUND_INDEX *)ix;
  int i=0, n=cix->n_indices;
  while (i < n) {
    if (cix->indices[i]->handler->ix_spend_memory)
      cix->indices[i]->handler->ix_spend_memory(cix->indices[i]);
    i++;}
}

static struct FD_INDEX_HANDLER compound_index_handler={
  NULL,NULL,NULL,NULL, /* get, get_size, add , drop are all by default */
  compound_index_fetch,
  compound_index_fetch_size,
  NULL,
  compound_index_prefetch,
  compound_index_dir,
  compound_index_cache_index,
  compound_index_close,
  NULL,
  NULL};


static void update_compound_index_id(struct FD_COMPOUND_INDEX *ix)
{
  struct FD_STRING_STREAM out; int i=0, n=ix->n_indices; fd_u8char *oldid;
  FD_INITIALIZE_STRING_STREAM(&out,128);
  while (i < n) {
    fd_sputs(&out,ix->indices[i]->id); i++;
    if (i<n) fd_sputc(&out,'|');}
  oldid=ix->id; ix->id=out.ptr; fd_xfree(oldid);
}

FRAMERD_EXPORT
struct FD_COMPOUND_INDEX *fd_make_compound_index(int n,fd_index *indices)
{
  struct FD_COMPOUND_INDEX *cix=fd_malloc(sizeof(struct FD_COMPOUND_INDEX));
  cix->id=fd_strdup("compound"); cix->read_only=1; cix->zipf_threshold=0;
  cix->handler=&compound_index_handler; cix->type=compound_index;
  fd_init_hashtable(&(cix->cache),1024);
  fd_init_hashtable(&(cix->adds),0);
  fd_init_hashtable(&(cix->drops),0);
  fd_init_hashtable(&(cix->sizes),17);
  fd_init_hashset(&(cix->interned_values),0);
  cix->for_slotid=FD_VOID; cix->cache_size=1024; cix->sizes_size=256;
  cix->adds_size=0; cix->drops_size=0;
  cix->indices=indices; cix->n_indices=n;
#if FD_THREADS_ENABLED
  fd_init_mutex(&(cix->lock));
#endif
  update_compound_index_id(cix);
  fd_register_index((fd_index)cix);
  return cix;
}

FRAMERD_EXPORT
void fd_add_to_compound_index(struct FD_COMPOUND_INDEX *cix,fd_index ix)
{
  int i=0, n=cix->n_indices; fd_index *indices=cix->indices;
  while (i < n) if (indices[i]==ix) return; else i++;
  cix->indices=indices=fd_xrealloc(indices,sizeof(fd_index)*(n+1));
  indices[n]=ix; cix->n_indices++;
  fd_swap_out_index((fd_index)cix);
  update_compound_index_id(cix);
}

FRAMERD_EXPORT
void fd_trim_compound_index(struct FD_COMPOUND_INDEX *cix)
{
  int i=0, n=cix->n_indices; fd_index *indices=cix->indices;
  while (i < n) {fd_swap_out_index(indices[i]); i++;}
}

/** Initialization **/

static void commit_indices_atexit()
{
  if (fd_normal_exit) fd_commit_indices();
}

static int indices_initialized=0;

void fd_initialize_index_c()
{
  if (indices_initialized) return;

#if FD_THREADS_ENABLED
  fd_init_mutex(&all_indices_write_lock);
#endif

  atexit((fd_exit_proc_type)commit_indices_atexit);

  fd_register_source_file("index",__DATE__,vcid);

  indices_initialized=1;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: index.c,v $
   Revision 1.39  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.38  2004/10/31 22:27:33  haase
   Handle prefetches for compound components that don't have prefetch handlers

   Revision 1.37  2004/10/19 22:13:54  haase
   Use xalloc for compound indices

   Revision 1.36  2004/10/19 00:22:46  haase
   Fix to ID updating

   Revision 1.35  2004/10/19 00:17:19  haase
   Made the background into a compound index

   Revision 1.34  2004/10/18 15:25:08  haase
   Added compound indices

   Revision 1.33  2004/10/14 13:33:31  haase
   Cache failed fetches

   Revision 1.32  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.31  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.30  2004/05/14 14:41:57  haase
   Made preloading be an option for all kinds of indices

   Revision 1.29  2004/02/18 16:21:53  haase
   Don't bother prefetching if you don't need to

   Revision 1.28  2004/02/17 02:51:39  haase
   Index commits no longer swap out

   Revision 1.27  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.26  2003/11/26 16:51:43  haase
   Fixed bug in network subindices

   Revision 1.25  2003/11/26 13:51:54  haase
   Added index subservers

   Revision 1.24  2003/10/20 09:23:27  haase
   Fixed some erroneous assumptions about the initializion of index sizes table

   Revision 1.23  2003/10/05 06:41:33  haase
   Added a lock to index swap outs

   Revision 1.22  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.21.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.21.2.2  2003/08/02 13:44:34  haase
   Reorganized pool and index initialization, added serial numbers

   Revision 1.21.2.1  2003/01/26 20:43:15  haase
   Misc. fixes especially some GC

   Revision 1.21  2002/07/05 21:20:08  uid59704
   Fixed GC contract of fd_hashset_intern and added fd_hashset_intern string

   Revision 1.20  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.19  2002/06/01 21:08:17  haase
   Indentation changes

   Revision 1.18  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.17  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.16  2002/04/24 23:50:48  haase
   interpret_pool/index now leaves error signalling to fd_use_pool/index

   Revision 1.15  2002/04/24 20:06:18  haase
   Made index specs able to specify multiple indices with &

   Revision 1.14  2002/04/10 18:58:15  haase
       Made canonicalization of filenames and server specs use
   fd_get_real_pathname and fd_get_real_hostname, rather than
   trying special kludges.

   Revision 1.13  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
