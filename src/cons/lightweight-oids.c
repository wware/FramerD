struct FD_HASHTABLE _fd_oid_buckets[FD_OID_BUCKETS];

#if FD_THREADS_ENABLED
fd_mutex _fd_pool_buckets_lock;
#endif

struct FD_POOL_BUCKET _fd_pool_buckets[256];
int _fd_n_pool_buckets;
int _fd_n_pools;

/* Can be used to sort a pool lookup table */
static int compare_pool_buckets(const void *e1,const void *e2)
{
  struct FD_POOL_BUCKET *pe1=*((struct FD_POOL_BUCKET **)e1);
  struct FD_POOL_BUCKET *pe2=*((struct FD_POOL_BUCKET **)e2);
  return (FD_COMPARE_OIDS(pe1->base,pe2->base));
}

DTYPES_EXPORT
int _fd_get_pool_bucket(FD_OID base)
{
  int i=0, pid=-1, off=FD_OID_LOW(base)&0xFFFFFF;
  FD_SET_OID_LOW(base,FD_OID_LOW(base)&0xFF000000);
  while (i < _fd_n_pool_buckets)
    if (FD_COMPARE_OIDS(_fd_pool_buckets[i].base,base) == 0) {
      pid=i; break;}
    else i++;
  if (pid >= 0) return pid;
  if (i >= 256) fd_raise_exception("Too many pool buckets");
  _fd_pool_buckets[i].base=base; _fd_pool_buckets[i].id=i;
  _fd_pool_buckets[i].n_pools=0;
  _fd_pool_buckets[i].pool=NULL;
  _fd_pool_buckets[i].pools=NULL;
  _fd_n_pool_buckets++;
  return i;
}

DTYPES_EXPORT struct FD_POOL_STUB *_fd_get_pool_from_bucket(int pid,int off)
{
  int i=0, n_pools=_fd_pool_buckets[pid].n_pools;
  struct FD_POOL_STUB **pools=_fd_pool_buckets[pid].pools;
  FD_OID oid=FD_OID_PLUS(_fd_pool_buckets[pid].base,off);
  while (i < n_pools)
    if ((COMPARE_OIDS(oid,pools[i]->base)>=0) &&
	(OID_DIFFERENCE(oid,pools[i]->base) < pools[i]->capacity))
      return pools[i];
    else i++;
  return NULL;
}

DTYPES_EXPORT FD_OID fd_oid_addr(fd_lisp x)
{
  return FD_OID_ADDR(x);
}
DTYPES_EXPORT fd_lisp fd_make_oid(FD_OID x)
{
  return FD_MAKE_OID(x);
}

/* External functions */

/* fd_initialize_oids_c
     Arguments: none
     Returns: nothing
  Initializes OID and OID value data structures
*/
void fd_initialize_oids_c ()
{
  int i=0; while (i < FD_OID_BUCKETS) {
    fd_init_hashtable(&_fd_oid_buckets[i],15); i++;}
  fd_register_source_file("oids",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: lightweight-oids.c,v $
   Revision 1.7  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.5  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.4  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.3  2002/04/28 21:56:26  haase
   Made WIN32 version use lightweight OIDs

   Revision 1.2  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
