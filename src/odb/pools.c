/* C Mode */

/* pools.c

   Implements functions for pool access and managment
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

static char vcid[] =
  "$Id: pools.c,v 1.26 2005/01/14 16:48:48 haase Exp $";

/** Declarations and Utilities **/
/** Registering pools **/
/** Naming pools **/
/** Looking up OIDs **/
/** External pool lookup functions **/
/** Using pools **/
/** Committing Pools **/
/** Initialization routine **/

#define FD_INLINE_OIDS 1

#include "framerd.h"
#include "framerd/dtcodes.h"
#include <limits.h>

/* Exceptions */

fd_exception
   fd_PoolOverlap=_("overlap between coverage of registered pools"),
   fd_FilePoolExhausted=_("file pool is exhausted (out of OIDs)"),
   fd_UnlockablePool=_("the pool cannot be locked"),
   fd_ReadOnlyPool=_("the pool is read-only"),
   fd_NotAFilePool=_("file does not describe a file pool"),
   fd_NotASuperPool=_("file does not describe a super pool"),
   fd_UnknownPool=_("the specified pool cannot be found"),
   fd_UnregisteredPool=_("the pool has not been registered"),
   fd_BadPoolSpec=_("the pool specification is invalid"),
   fd_UnWritablePool=_("the pool cannot be written"),
   fd_UnallocatedOID=_("the OID has not been allocated");

/** Declarations and Utilities **/

#if FD_THREADS_ENABLED
static fd_mutex _fd_pool_table_lock;
#endif

#if FD_THREADS_ENABLED
static fd_mutex dirsrv_lock;
#endif

/* Utility function */
static int in_poolp(FD_OID id,fd_pool p)
{
  return FD_OID_IN_RANGE(id, p->base, p->capacity);
}

/** Naming pools **/

#if FD_THREADS_ENABLED
static fd_mutex pool_names_lock;
#endif

static struct FD_POOL_NAME {
  fd_u8char *name; fd_pool pool;} *pool_names=NULL;
static int n_names=0;

/* Finds the pool with a particular name */
static fd_pool find_pool_named(fd_u8char *name)
{
  int i=0;
  fd_lock_mutex(&pool_names_lock);
  while (i < n_names) 
    if ((strcasecmp(name,pool_names[i].name)) == 0) {
      fd_unlock_mutex(&pool_names_lock);
      return pool_names[i].pool;}
    else i++;
  fd_unlock_mutex(&pool_names_lock);
  return NULL;
}

static fd_pool add_pool_name(fd_pool p,fd_u8char *name)
{
  int i=0;
  fd_lock_mutex(&pool_names_lock);
  while (i < n_names) 
    if ((strcasecmp(name,pool_names[i].name)) == 0) {
      fd_unlock_mutex(&pool_names_lock);
      return pool_names[i].pool;}
    else i++;
  if (pool_names)
    pool_names=fd_realloc(pool_names,sizeof(struct FD_POOL_NAME)*(n_names+1),
			  sizeof(struct FD_POOL_NAME)*n_names);
  else pool_names=fd_malloc(sizeof(struct FD_POOL_NAME)*1);
  /* Assign the shortest possible prefix id */
  if (p->prefix_id == NULL)
    p->prefix_id=pool_names[n_names].name=fd_strdup(name);
  else if (strlen(name) < strlen(p->prefix_id))
    p->prefix_id=pool_names[n_names].name=fd_strdup(name);
  else pool_names[n_names].name=fd_strdup(name);
  pool_names[n_names].pool=p;
  n_names++;
  fd_unlock_mutex(&pool_names_lock);
  return p;
}

void process_pool_label(fd_pool p,fd_lisp label)
{
  if (FD_STRINGP(label)) {
    fd_u8char *data=FD_STRING_DATA(label);
    fd_u8char *buf=fd_xmalloc(1+FD_STRING_LENGTH(label));
    fd_u8char *dot=strchr(data,'.');
    fd_pool other=add_pool_name(p,data);
    if (p != other) {
      fd_warn("Conflicting pools labeled %s: %s and %s",data,p->id,other->id);
      fd_xfree(buf); return;}
    /* Now record shorter versions */
    while (dot) {
      strncpy(buf,data,dot-data); buf[dot-data]=NUL; add_pool_name(p,buf);
      dot=strchr(dot+1,'.');}
    fd_xfree(buf);}
  else if (FD_CHOICEP(label)) {
    FD_DO_CHOICES(elt,label) {
      process_pool_label(p,elt);}
    END_FD_DO_CHOICES;}
}

FRAMERD_EXPORT
/* fd_find_pool_named:
     Arguments: a utf-8 string
     Returns: a pointer to a pool or NULL
  Returns the pool which has been assigned the designated name. */
fd_pool fd_find_pool_named(fd_u8char *name)
{
  fd_pool p=NULL;
  WITH_MUTEX_LOCKED(&_fd_pool_table_lock) {
    p=find_pool_named(name);}
  END_WITH_MUTEX_LOCKED(&_fd_pool_table_lock);
  return p;
}

#if (FD_LIGHTWEIGHT_OIDS)
/* Registering pools */

DTYPES_EXPORT int _fd_n_pools;
#if FD_THREADS_ENABLED
DTYPES_EXPORT fd_mutex _fd_pool_buckets_lock;
#endif

typedef struct FD_POOL_STUB *pstub;

/* Here are the cases for pool overlap:

p1:     [      ] |  [    ]     |    [     ]  |     [  ]
p2:       [  ]   |     [   ]   |  [  ]       |  [        ]

*/
static int check_overlap(fd_pool p1,fd_pool p2)
{
  FD_OID base1=p1->base;
  FD_OID base2=p2->base;
  int min1=FD_OID_LOW(base1), max1=FD_OID_LOW(base1)+p1->capacity-1;
  int min2=FD_OID_LOW(base2), max2=FD_OID_LOW(base2)+p2->capacity-1;
  if ((FD_OID_HIGH(base1) == (FD_OID_HIGH(base2))) &&
      (((min1 >= min2) && (max1 <= max2)) || /* p1 inside p2 */
       ((min1 <= min2) && (max1 >= min2) && (max1 <= max2)) || /* p1 front overlaps p2 */
       ((min2 <= min1) && (max2 >= min1) && (max2 <= max1)) || /* p1 rear overlaps p2 */
       ((min2 >= min1) && (max2 <= max1))))  /* p2 inside p1 */
    return 1;
  else return 0;
}

static int register_pool(fd_pool p)
{
  FD_OID base=p->base; int b, zero_based;
  b=_fd_get_pool_bucket(base);
  /* If the pool being added is at the base of the bucket, 
     check it agains the special case base pool */
  if (((OID_LOW(p->base)) && (0xFFFFFF)) == 0) {
    if (_fd_pool_buckets[b].pool)
      if (p == (fd_pool)_fd_pool_buckets[b].pool) return 0;
      else fd_raise_exception("Pool overlap");}
  if (_fd_pool_buckets[b].n_pools == 0) {
    _fd_pool_buckets[b].n_pools=1;
    _fd_pool_buckets[b].pools=fd_malloc(sizeof(struct FD_POOL_STUB *));
    _fd_pool_buckets[b].pools[0]=(pstub)p;}
  else {
    struct FD_POOL_STUB **pools=_fd_pool_buckets[b].pools, **old=NULL;
    int n_pools=_fd_pool_buckets[b].n_pools;
    int i=0; while (i < n_pools)
      if (p == (fd_pool)pools[i]) return 1;
      else if (check_overlap((fd_pool)pools[i],p))
	fd_raise_exception("Pool overlap");
      else i++;
    pools=fd_realloc(pools,sizeof(struct FD_POOL_STUB *)*(n_pools+1),
		     sizeof(struct FD_POOL_STUB *)*n_pools);
    pools[n_pools]=(pstub)p;
    _fd_pool_buckets[b].pools=pools;
    _fd_pool_buckets[b].n_pools++;}
  /* If the pool is at the base of the bucket,
     set the ->pool and ->capacity fields */
  if (((OID_LOW(p->base)) && (0xFFFFFF)) == 0) {
    _fd_pool_buckets[b].pool=(pstub)p;
    _fd_pool_buckets[b].capacity=p->capacity;}
  p->serial=_fd_n_pools++;
  return 1;
}

FRAMERD_EXPORT
/* fd_register_pool:
     Arguments: a pointer to a pool
     Returns: void
  Adds an entry for the pool to the pool table, doing nothing if
it is already there and signalling an error if the pool overlaps
with a currently registered pool. */
int fd_register_pool(fd_pool p)
{
  int new_pool=0;
  WITH_MUTEX_LOCKED(&_fd_pool_buckets_lock) {
    new_pool=register_pool(p);}
  END_WITH_MUTEX_LOCKED(&_fd_pool_buckets_lock);
  process_pool_label(p,p->label);
  return new_pool;
}

#else /* FD_LIGHTWEIGHT_OIDS */

/** Registering pools **/

/* The pool database uses a `pool table' to lookup up pools.  This is
    a single array with all of the information needed to find the pool for 
    an OID which will hopefully fit in the cache, so that it can be searched
    really fast.  It is sorted to allow a binary search to locate the pool
    for a particular OID. */
struct FD_POOL_TABLE_ENTRY *_fd_pool_table=NULL;
int fd_n_pools=0;

/* Finds the pool table entry for the pool p. */
static struct FD_POOL_TABLE_ENTRY *find_pool_table_entry(fd_pool p)
{
  int i=0; while (i < fd_n_pools)
    if (p == _fd_pool_table[i].p) return &_fd_pool_table[i];
    else i++;
  return NULL;
}

/* Used to sort the pool table */
static int compare_pool_table_entries(const void *e1,const void *e2)
{
  struct FD_POOL_TABLE_ENTRY *pe1=(struct FD_POOL_TABLE_ENTRY *)e1;
  struct FD_POOL_TABLE_ENTRY *pe2=(struct FD_POOL_TABLE_ENTRY *)e2;
  return (FD_COMPARE_OIDS(pe1->base,pe2->base));
}

/* Used to find overlaps.  This could use the fact that the
   pool table is sorted, but it's not that important to be fast
   since it is only called at pool registration time. */
static int find_overlap(FD_OID base,int cap)
{
  FD_OID top=base; int i=0, limit=fd_n_pools;
  unsigned int nmin=FD_OID_LOW(base);
  unsigned int nmax=nmin+cap-1;
  FD_SET_OID_LOW(top,FD_OID_LOW(top)+cap);
  while (i < limit) {
    FD_OID pbase=_fd_pool_table[i].base;
    unsigned int pmin=FD_OID_LOW(pbase);
    unsigned int pmax=pmin+_fd_pool_table[i].capacity-1;
    if ((FD_OID_HIGH(base) == (FD_OID_HIGH(pbase))) &&
	(((nmin >= pmin) && (nmin <= pmax)) ||
	 ((nmax >= pmin) && (nmax <= pmax)) ||
	 ((pmin >= nmin) && (pmin <= nmax)) ||
	 ((pmax >= nmin) && (pmax <= nmax))))
      return i;
    else i++;}
  return -1;
}

/* Actually adds the new entry and resorts the array */
static void add_pool_table_entry(fd_pool p)
{
  if (_fd_pool_table)
    _fd_pool_table=fd_xrealloc
      (_fd_pool_table,sizeof(struct FD_POOL_TABLE_ENTRY)*(fd_n_pools+1));
  else _fd_pool_table=malloc(sizeof(struct FD_POOL_TABLE_ENTRY));
  _fd_pool_table[fd_n_pools].p=p;
  _fd_pool_table[fd_n_pools].base=p->base;
  _fd_pool_table[fd_n_pools].capacity=p->capacity;
  p->serial=fd_n_pools++;
  qsort(_fd_pool_table,fd_n_pools,sizeof(struct FD_POOL_TABLE_ENTRY),
	compare_pool_table_entries);
}

FRAMERD_EXPORT
/* fd_register_pool:
     Arguments: a pointer to a pool
     Returns: void
  Adds an entry for the pool to the pool table, doing nothing if
it is already there and signalling an error if the pool overlaps
with a currently registered pool. */
int fd_register_pool(fd_pool p)
{
  int new_pool=0;
  WITH_MUTEX_LOCKED(&_fd_pool_table_lock) {
    struct FD_POOL_TABLE_ENTRY *found=find_pool_table_entry(p);
    if (found == NULL) {
      int overlap=find_overlap(p->base,p->capacity);
      if (overlap >= 0) fd_raise_exception(fd_PoolOverlap);
      add_pool_table_entry(p); new_pool=1;}}
  END_WITH_MUTEX_LOCKED(&_fd_pool_table_lock);
  process_pool_label(p,p->label);
  return new_pool;
}

#endif /* not FD_CONSED_OIDS */

/** Looking up OIDs **/

static fd_pool (*oid_locator)(FD_OID id)=NULL;
static lisp locate_oid_symbol;
static int directory_server_init=0;
static fd_server directory_server=NULL;

static fd_pool use_pooltab(lisp loid);
static fd_pool use_directory_server(lisp loid);

static fd_pool search_for_pool(lisp loid)
{
  FD_OID id=FD_OID_ADDR(loid);
  fd_pool pool=NULL;
  if (oid_locator) pool=oid_locator(id);
  if (pool) {
    if (in_poolp(id,pool)) return pool;
    else fd_warn(_("External OID locator returned wrong pool (%s) for %q"),
		 pool->id,loid);}
  if (pool == NULL) pool=use_pooltab(loid);
  if (pool) {
    if (in_poolp(id,pool)) return pool;
    else fd_warn(_("Pooltab returned wrong pool (%s) for %q"),
		 pool->id,loid);}
  pool=use_directory_server(loid);
  if (pool) {
    if (in_poolp(id,pool)) return pool;
    else fd_warn(_("Network OID locator returned wrong pool (%s) for %q"),
		 pool->id,loid);}
  return NULL;
}

static fd_pool use_pooltab(lisp loid)
{
  FD_OID id=FD_OID_ADDR(loid); fd_pool result=NULL;
  lisp pooltab=fd_getenv("%POOLTAB");
  DO_CHOICES(entry,pooltab)
    if ((VECTORP(entry)) && (VECTOR_LENGTH(entry) == 3) &&
	(OIDP(VECTOR_REF(entry,0))) && (FIXNUMP(VECTOR_REF(entry,1)))) {
      FD_OID base=OID_ADDR(VECTOR_REF(entry,0));
      int cap=FIXLISP(VECTOR_REF(entry,1));
      if (FD_OID_IN_RANGE(id,base,cap)) {
        fd_pool p=fd_interpret_pool(VECTOR_REF(entry,2));
	if (p == NULL) fd_warn(_("Bad pooltab entry: %q"),entry);
	else if (in_poolp(id,p)) {result=p; break;}
	else fd_warn(_("Bad pooltab entry: %q"),entry);}}
    else {
      fd_pool p=fd_interpret_pool(entry);
      if (p == NULL) fd_warn(_("Bad pooltab entry: %q"),entry);
      else if (in_poolp(id,p)) {result=p; break;}}
  END_DO_CHOICES;
  return result;
}

static fd_pool use_directory_server(lisp loid)
{
  if (directory_server_init == 0) {
    WITH_MUTEX_LOCKED(&dirsrv_lock) {
      if (directory_server_init == 0) { 
	char *source=fd_string_getenv("OID_LOCATOR");
	if (source) directory_server=fd_connect(source);
	else directory_server=NULL;
	if (directory_server) directory_server->ref_count++;
	if (source) fd_xfree(source);
	directory_server_init=1;}}
    END_WITH_MUTEX_LOCKED(&dirsrv_lock);}
  if (directory_server) {
    lisp expr=FD_MAKE_LIST(2,locate_oid_symbol,loid);
    lisp spec=fd_careful_dtype_eval(expr,directory_server);
    fd_pool p=fd_interpret_pool(spec);
    return p;}
  else return NULL;
}

FRAMERD_EXPORT
/* fd_set_oid_locator:
     Arguments: a function which maps OIDs to pools
     Returns: nothing

Sets the OID locator function for this session, which takes
an OID and returns its pool.  This function is only called if the
pool is not already known, so the purpose of the oid locator
is to find otherwise undeclared pools. */
void fd_set_oid_locator(fd_pool (*ol)(FD_OID id))
{
  oid_locator=ol;
}

/** External pool lookup functions **/

FRAMERD_EXPORT
/* fd_get_pool:
    Arguments: a lisp pointer to an OID
    Returns: a pointer to a pool

  Finds the pool containing an oid. */
fd_pool fd_get_pool(lisp id)
{
  return FD_GET_POOL(id);
}

FRAMERD_EXPORT
/* fd_locate_pool:
    Arguments: a lisp pointer (to an OID)
    Returns: a pointer to a pool or NULL

  This is just like fd_get_pool, but will try harder, calling
locator functions to try pools which haven't been explicitly
registered. */
fd_pool fd_locate_pool(lisp loid)
{
  fd_pool p=FD_GET_POOL(loid);
  if (p) return p;
  else return search_for_pool(loid);
}

/* External functions */

#if (FD_LIGHTWEIGHT_OIDS)
FRAMERD_EXPORT
/* fd_get_pool_count:
    Arguments: nothing
    Returns: an int
  Returns the number of current identified pools. */
int fd_get_pool_count() { return _fd_n_pools;}

FRAMERD_EXPORT
/* fd_for_pools:
    Arguments: a function on a pool pointer and a void pointer
               and a void data pointer
    Returns: void
    Applies the function to each known pool and the data pointer
      passed to the call (got that?). */
void fd_for_pools(void (*fcn)(fd_pool p,void *arg),void *arg)
{
  int i=0, n; fd_pool *pools;
  /* We do this in case `fcn' modifies the pool table */
  WITH_MUTEX_LOCKED(&_fd_pool_table_lock) {
    struct FD_POOL_BUCKET *scan=_fd_pool_buckets;
    struct FD_POOL_BUCKET *limit=scan+_fd_n_pool_buckets;
    n=_fd_n_pools; pools=fd_malloc(sizeof(fd_pool)*n);
    while (scan < limit)
      if (scan->pools) {
	int j=0, len=scan->n_pools;
	struct FD_POOL_STUB **bpools=scan->pools;
	while (j < len) {pools[i++]=(fd_pool)bpools[j++];}
	scan++;}
      else scan++;}
  END_WITH_MUTEX_LOCKED(&_fd_pool_table_lock);
  /* Now we do the actual work, feeling relatively safe that we won't
     step on our own toes.*/
  i=0; while (i < n) {fcn(pools[i],arg); i++;}
  fd_free(pools,sizeof(fd_pool)*n);
}

#else
/* When OIDS aren't consed, these functions are defined in src/cons/oids.c */
FRAMERD_EXPORT
/* fd_get_pool_count:
    Arguments: nothing
    Returns: an int
  Returns the number of current identified pools. */
int fd_get_pool_count() { return fd_n_pools;}

FRAMERD_EXPORT
/* fd_for_pools:
    Arguments: a function on a pool pointer and a void pointer
               and a void data pointer
    Returns: void
    Applies the function to each known pool and the data pointer
      passed to the call (got that?). */
void fd_for_pools(void (*fcn)(fd_pool p,void *arg),void *arg)
{
  int i=0, n=fd_n_pools; fd_pool *pools;
  /* We do this in case `fcn' modifies the pool table */
  WITH_MUTEX_LOCKED(&_fd_pool_table_lock) {
    n=fd_n_pools;
    pools=fd_malloc(sizeof(fd_pool)*n);
    while (i < n) {
      pools[i]=_fd_pool_table[i].p; i++;}}
  END_WITH_MUTEX_LOCKED(&_fd_pool_table_lock);
  /* Now we do the actual work, feeling relatively safe that we won't
     step on our own toes.*/
  i=0; while (i < n) {fcn(pools[i],arg); i++;}
  fd_free(pools,sizeof(fd_pool)*n);
}
#endif

FRAMERD_EXPORT
/* fd_pool_load:
     Arguments: a pointer to a pool
     Returns: an int
 Returns the number of OIDs allocated in pool. */
int fd_pool_load(fd_pool p)
{
  if (p->handler->get_load) return p->handler->get_load(p);
  else fd_raise_detailed_exception(_("Can't get load of pool"),p->id);
}

FRAMERD_EXPORT
/* fd_random_oid:
     Arguments: a pointer to a pool
     Returns: an lisp pointer to an oid (or the empty choice)
 Returns a random allocated OID in pool, or the empty
choice if the pool is empty. */
lisp fd_random_oid(fd_pool p)
{
  int size=fd_pool_load(p);
  int offset;
  FD_OID base=p->base, id; 
  if (size == 0) return FD_EMPTY_CHOICE;
  offset=fd_random()%size;
  FD_SET_OID_HIGH(id,FD_OID_HIGH(base));
  FD_SET_OID_LOW(id,FD_OID_LOW(base)+offset);
  return fd_make_oid(id);
}

/** Using pools **/

FRAMERD_EXPORT
/* fd_use_pool:
     Arguments: a pool specification
     Returns: a pool

   Returns a pool based on a specification.  The specification either has
    the form port@host indicating a server or a filename indicating
    a file pool.
*/
fd_pool fd_use_pool(fd_u8char *c)
{
  fd_u8char *at, *amp; 
  if (c == NULL) fd_raise_exception(_("NULL pool spec"));
  else if (amp=strchr(c,'&')) {
    fd_pool first_pool=NULL;
    char *copy=fd_strdup(c), *scan=copy, *start=scan;
    while (*scan != 0) 
      if (*scan == '&') {
	*scan=NUL;
	if (first_pool) fd_use_pool(start);
	else first_pool=fd_use_pool(start);
	start=scan+1; scan++;}
      else scan++;
    fd_use_pool(start); fd_xfree(copy);
    return first_pool;}
  else if (at=strchr(c,'@')) {
    fd_network_pool npool; int portno; fd_u8char *name;
    if (strcmp(at+1,"local") == 0) {
      fd_u8char *tmp=fd_xmalloc(at-c+1);
      strncpy(tmp,c,at-c); tmp[at-c]=NUL;
      name=fd_make_os_string(tmp); fd_xfree(tmp);
      portno=-1;}
    else {
      char *copy=fd_strdup(c); copy[at-c]='\0';
      portno=fd_get_portno(copy); fd_xfree(copy);
      name=fd_make_os_string(at+1);}
    npool=fd_use_network_pool(name,portno,c); fd_xfree(name);
    return (fd_pool) npool;}
  else if (fd_file_existsp(c))
    return (fd_pool) fd_use_file_pool(c);
  else {
    fd_u8char *probe=fd_malloc(strlen(c)+16);
    strcpy(probe,c); strcat(probe,".pool");
    if (fd_file_existsp(probe)) {
      fd_pool p=fd_use_file_pool(probe);
      fd_free(probe,strlen(c)+16);
      return p;}
    else {
      fd_free(probe,strlen(c)+16);
      fd_raise_detailed_exception(fd_BadPoolSpec,c);}}
}

static fd_pool interpret_pool(lisp x)
{
  if (PRIM_TYPEP(x,pool_type)) return (fd_pool)(CPTR_DATA(x));
  else if (STRINGP(x))
    if (STRING_LENGTH(x) != 0) {
      fd_pool exists=fd_find_pool_named(STRING_DATA(x));
      if (exists) return exists;
      else return fd_use_pool(STRING_DATA(x));}
    else return NULL;
  else if ((SYMBOLP(x)) &&
	   (PRIM_TYPEP(SYMBOL_VALUE(x),pool_type)))
    return (fd_pool) CPTR_DATA(SYMBOL_VALUE(x));
  else if (PAIRP(x)) {
    lisp host=CAR(x), port=CDR(x);
    if ((STRINGP(host)) && (FIXNUMP(port))) {
      char idbuf[128]; char port_buf[32];
      sprintf(port_buf,"%d",FIXLISP(port));
      sprintf(idbuf,"%s@%s",port_buf,STRING_DATA(host));
      return (fd_pool)
	fd_use_network_pool(STRING_DATA(host),FIXLISP(port),idbuf);}
    else return NULL;}
  else if (SYMBOLP(x)) {
    char *value=fd_string_getenv(SYMBOL_NAME(x));
    if (value == NULL)
      return NULL;
    else {
      fd_pool p=fd_use_pool(value);
      fd_xfree(value);
      return p;}}
  else return NULL;
}

FRAMERD_EXPORT
/* fd_interpret_pool:
     Arguments: an expression and an environment
     Returns: a pointer to a pool
 Evaluates the expression in the environment and tries to produce an
index object from the result. */
fd_pool fd_interpret_pool(lisp spec)
{
  return interpret_pool(spec);
}

FRAMERD_EXPORT
/* fd_init_pool:
     Arguments: a pointer to a pool and some initial fields
     Returns: void
 Sets up some of the pools initial fields. */
void fd_init_pool_data(fd_pool p,enum FD_POOL_TYPE type,
		       FD_OID base,unsigned int cap,int ro,
		       fd_u8char *id,fd_u8char *prefix,fd_lisp label)
{
  p->type=type; p->id=id; p->prefix_id=prefix; p->label=label; 
  p->capacity=cap; p->modifiedp=0; p->read_only=ro;
  p->base=base; fd_init_hashset(&(p->modified),64);
}

/** Committing Pools **/

FRAMERD_EXPORT
/* fd_commit_pool:
     Arguments: a pool
     Returns: nothing
*/
void fd_commit_pool(fd_pool p)
{
  if ((p->modifiedp == 0) || (fd_ephemeralp())) {
    p->handler->close_pool(p); return;}
  else if (p->handler->commit_pool) {
    fd_notify("Commiting pool %s (%d changes)",
	      p->id,p->modified.n_keys);
    p->handler->commit_pool(p);}
  else fd_raise_detailed_exception(fd_UnWritablePool,p->id);
}

/* This is called by fd_for_pools */
static void commit_pool_proc(fd_pool p,void *iter_arg);

FRAMERD_EXPORT
/* fd_commit_pools: 
     Arguments: none
     Returns: none

   Side effects:
    Saves the values of all modified oids, and clears the modified
oids hashset.
*/
void fd_commit_pools()
{
  int message_output=0;
  fd_for_pools(commit_pool_proc,&message_output);
}
static void commit_pool_proc(fd_pool p,void *iter_arg)
{
  int *message_output=(int *) iter_arg;
  if (p->modifiedp) {
    if (*message_output == 0) {
      fd_notify(_("Commiting all pools")); *message_output=1;}
    fd_commit_pool(p);}
}

/** Syncing Pools **/

FRAMERD_EXPORT
/* fd_sync_pool:
     Arguments: a pool
     Returns: nothing
*/
int fd_sync_pool(fd_pool p)
{
  if (p->handler->sync_pool)
    return p->handler->sync_pool(p);
  else return -1;
}

/* This is called by fd_for_pools */
static void sync_pool_proc(fd_pool p,void *iter_arg);

FRAMERD_EXPORT
/* fd_sync_pools: 
     Arguments: none
     Returns: none

   Side effects:
    Saves the values of all modified oids, and clears the modified
oids hashset.
*/
void fd_sync_pools()
{
  int message_output=0;
  fd_for_pools(sync_pool_proc,&message_output);
}
static void sync_pool_proc(fd_pool p,void *iter_arg)
{
  fd_sync_pool(p);
}

/** Initialization routine **/

static void commit_pools_atexit()
{
  if (fd_normal_exit) fd_commit_pools();
}

void fd_initialize_pools_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&pool_names_lock);
  fd_init_mutex(&_fd_pool_table_lock);
  fd_init_mutex(&dirsrv_lock);
#endif

  atexit((fd_exit_proc_type)commit_pools_atexit);

  locate_oid_symbol=fd_make_symbol("LOCATE-OID");

  fd_register_source_file("pools",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: pools.c,v $
   Revision 1.26  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.25  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.24  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.23  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.22  2003/11/03 00:11:43  haase
   Made interpret_pool not signal errors directly

   Revision 1.21  2003/09/30 11:16:15  haase
   Added extra locks to protect pool and index registries

   Revision 1.20  2003/09/13 21:57:56  haase
   Fixed automatic closing of unused network connections

   Revision 1.19  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.18.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.18.2.2  2003/08/02 13:44:34  haase
   Reorganized pool and index initialization, added serial numbers

   Revision 1.18.2.1  2003/01/26 20:45:15  haase
   Misc. fixes and general cleanup

   Revision 1.18  2002/05/19 13:34:16  haase
   Fix typo in last commit

   Revision 1.17  2002/05/19 13:23:22  haase
   Fix null pointer passed to realloc on first pool naming

   Revision 1.16  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.15  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.14  2002/04/24 23:50:48  haase
   interpret_pool/index now leaves error signalling to fd_use_pool/index

   Revision 1.13  2002/04/24 20:06:19  haase
   Made pool specs able to specify multiple pools with &

   Revision 1.12  2002/04/16 00:09:25  haase
   Made process_pool_label be non-static to be used by file-pool.c

   Revision 1.11  2002/04/11 00:23:45  haase
   Removed API for naming pools

   Revision 1.10  2002/04/10 18:52:40  haase
   include/framerd/odb.h

   Revision 1.9  2002/04/10 14:13:13  haase
   Fixed bug with assigning invalid poolids; more work needed

   Revision 1.8  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
