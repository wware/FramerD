/** Special hash includes **/

#define MAGIC_MODULUS 16777213

FASTOP unsigned int get_chain(unsigned int hash,int size)
{
  int chain=(hash%(size-2));
  if (chain == 0) return size-2;
  else return chain;
}

/** Allocating new oids **/

static struct FD_CONSOID *next_oid=NULL, *last_oid=NULL;

static void allocate_next_oids()
{
  unsigned long asint;
  next_oid=fd_xmalloc(sizeof(struct FD_CONSOID)*OID_BLOCK_SIZE);
  last_oid=next_oid+OID_BLOCK_SIZE;
  asint=(unsigned long)next_oid;
  if (asint&0xF) next_oid=next_oid+(0xF-(asint&0xF));
}

static struct FD_CONSOID *get_an_object()
{
  if (next_oid >= last_oid) allocate_next_oids();
  fd_malloc_adjust(sizeof(struct FD_CONSOID));
  return next_oid++;
}

/** OID hash tables **/

static struct FD_HASHSET oid_table;
static void grow_oid_table();
static int oid_table_initialized=0;

/* We have a special hash function, which is actually the same as
   the one used by the FramerD index facility. It just does a 64 bit
   modulus.  */
FASTOP unsigned int oid_hash(FD_OID id)
{
#if FD_OIDS_ARE_SCALARS
  return id%(MAGIC_MODULUS);
#else
  unsigned int hi=FD_OID_HIGH(id), lo=FD_OID_LOW(id);
  int i=0; while (i++ < 4) {
    hi=((hi<<8)|(lo>>24))%(MAGIC_MODULUS); lo=lo<<8;}
  return hi;
#endif
}

DTYPES_EXPORT
/* fd_oid_table:
    Arguments: none
    Returns: a pointer to the hashset being used as a OID table. */
fd_hashset fd_oid_table() {return &oid_table;}

DTYPES_EXPORT
/* fd_make_oid:
    Arguments: an OID address structure
    Returns: a lisp pointer to a OID object with the corresponding address

  This is basically the same loop as above with a substitution of
   FD_COMPARE_OIDS for ==.
*/
lisp fd_make_oid(FD_OID id)
{
  int size; lisp *table;
  unsigned int hash, probe, chain;
  if (oid_table_initialized == 0) fd_initialize_oid_table();
  lock_mutex(&(oid_table.lock));
  size=oid_table.n_slots; table=oid_table.table;
  hash=oid_hash(id); probe=hash%size; chain=get_chain(hash,size);
  while (OIDP(table[probe]))
    if ((FD_COMPARE_OIDS(id,(FD_OID_ADDR(table[probe])))) == 0) {
      unlock_mutex(&(oid_table.lock)); return table[probe];}
    else {probe=(probe+chain)%size;}
  if (oid_table.n_keys > ((oid_table.n_slots)/2)) {
    grow_oid_table(oid_table.n_slots); unlock_mutex(&(oid_table.lock));
    return fd_make_oid(id);}
  else {
    FD_CONSOID new_oid=get_an_object();
    new_oid->id=id;
    new_oid->value=(FD_VOID);
    table[probe].type=object_type;
    table[probe].data.oid=new_oid;
    oid_table.n_keys++;
    unlock_mutex(&(oid_table.lock));
    return table[probe];}
}

DTYPES_EXPORT
/* fd_probe_oid:
    Arguments: an OID
    Returns: a lisp pointer to the OID object or the empty set

  This is like fd_make_oid but doesn't make the oid, only
returns it if it exists.
*/
lisp fd_probe_oid(FD_OID id)
{
  int size; lisp *table;
  unsigned int hash, probe, chain;
  if (oid_table_initialized == 0) fd_initialize_oid_table();
  lock_mutex(&(oid_table.lock));
  size=oid_table.n_slots; table=oid_table.table;
  hash=oid_hash(id); probe=hash%size; chain=get_chain(hash,size);
  while (OIDP(table[probe]))
    if ((FD_COMPARE_OIDS(id,(FD_OID_ADDR(table[probe])))) == 0)
      {unlock_mutex(&(oid_table.lock)); return table[probe];}
    else {probe=(probe+chain)%size;}
  unlock_mutex(&(oid_table.lock));
  return FD_EMPTY_CHOICE;
}

#define choosemax(x,y) ((x > y) ? x : y)

static void grow_oid_table(unsigned int size)
{
  unsigned int new_size=
    fd_select_table_size(choosemax(size,oid_table.n_slots));
  lisp *new_table=fd_malloc(sizeof(lisp)*new_size);
  lisp *scan=oid_table.table, *limit=scan+oid_table.n_slots;
  lisp *nscan=new_table, *nlimit=nscan+new_size;
  while (nscan < nlimit) *nscan++=(FD_VOID);
  fd_notify(_("Growing OID table from %d/%d"),
	    oid_table.n_keys,oid_table.n_slots);
  while (scan < limit)
    if (OIDP(*scan)) {
      lisp elt=*scan;
      unsigned int new_hash=oid_hash(FD_OID_ADDR(elt));
      unsigned int probe=new_hash%new_size, chain=get_chain(new_hash,new_size);
      while (OIDP(new_table[probe])) probe=(probe+chain)%new_size;
      new_table[probe]=elt; scan++;}
    else scan++;
  fd_free(oid_table.table,oid_table.n_slots*sizeof(lisp));
  oid_table.n_slots=new_size;
  oid_table.table=new_table;
  fd_notify(_("OID table grown to %d/%d"),oid_table.n_keys,oid_table.n_slots);
}

DTYPES_EXPORT
/* fd_grow_oid_table:
    Arguments: an unsigned int size
    Returns: nothing

  Grows the oid table to at least a specified size.  This is provided
   because if you know there will be a lot of object references, you can
   grow the oid table at first and avoid having to take the time to
   grow it along the way. */
void fd_grow_oid_table(unsigned int size)
{
  UNWIND_PROTECT {
    lock_mutex(&(oid_table.lock));
    grow_oid_table(size);}
  ON_UNWIND {
    unlock_mutex(&(oid_table.lock));}
  END_UNWIND
}

/* fd_initialize_oid_table:
    Arguments: None
    Returns: nothing
  Initializes the OID table.  */
void fd_initialize_oid_table()
{
  if (oid_table_initialized) return;
  else oid_table_initialized=1;
  oid_table.n_keys=0; oid_table.n_slots=523;
  oid_table.table=fd_malloc(523*sizeof(lisp));
  {lisp *scan=oid_table.table, *limit=scan+523;
   while (scan < limit) *scan++=(FD_VOID);}
  allocate_next_oids();
}

DTYPES_EXPORT
/* _fd_not_an_oid:
    Arguments: a lisp pointer
    Returns: never, but pretends to return an FD_OID pointer
*/
struct FD_CONSOID *_fd_not_an_oid(lisp x)
{
  fd_type_error(_("pointer is not an oid"),x);
  return NULL;
}

/** Threadsafe OID access **/

/* This functions access OID values in a threadsafe way.  */

DTYPES_EXPORT
/* _fd_oid_current_value:
     Arguments: a lisp pointer to an OID
     Value: a lisp pointer (the OID's value)

  This accesses the OID's value field.
  It refcounts its value.
*/
#if FD_THREADS_ENABLED
lisp _fd_oid_current_value(lisp x)
{
  lisp v;
  if (!(OIDP(x))) fd_type_error(_("not an OID"),x);
  FD_LOCK_OID(x);
  v=((FD_OID_PTR(x))->value);
  v=incref(v);
  FD_UNLOCK_OID(x);
  return v;
}
#else
lisp _fd_oid_current_value(lisp x)
{
  if (!(OIDP(x))) fd_type_error(_("not an OID"),x);
  return incref(((OID_PTR(x))->value));
}
#endif

DTYPES_EXPORT
/* fd_oid_loadedp:
     Arguments: a lisp pointer to an OID
     Value: 1 or 0 if the OIDs value is non-void (e.g. loaded)

  This returns 1 if an OIDs value is defined (VOID counts as undefined)
*/
#if FD_THREADS_ENABLED
int _fd_oid_loadedp(lisp x)
{
  lisp v; int loaded=1;
  if (!(OIDP(x))) fd_type_error(_("not an OID"),x);
  FD_LOCK_OID(x);
  v=((FD_OID_PTR(x))->value);
  if (FD_VOIDP(v)) loaded=0;
  FD_UNLOCK_OID(x);
  return loaded;
}
#else
int _fd_oid_loadedp(lisp x)
{
  if (!(OIDP(x))) fd_type_error(_("not an OID"),x);
  else if (FD_VOIDP((OID_PTR(x)->value))) return 0;
  else return 1;
}
#endif

DTYPES_EXPORT
/* _fd_store_oid_value:
     Arguments: a lisp pointer to an OID and a lisp value
     Value: none

  Modifies the value associated with an OID.
*/
#if FD_THREADS_ENABLED
void _fd_store_oid_value(lisp x,lisp v)
{
  FD_LOCK_OID(x);
  (FD_OID_PTR(x))->value=fd_incref(v);
  FD_UNLOCK_OID(x);
}
#else
void _fd_store_oid_value(lisp x,lisp v)
{
  (OID_PTR(x))->value=fd_incref(v);
}
#endif

/* fd_initialize_oids_c
     Arguments: none
     Returns: nothing
  Initializes OID and OID value data structures
*/
void fd_initialize_oids_c ()
{
  fd_initialize_oid_table();
  fd_register_source_file("oids",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: consed-oids.c,v $
   Revision 1.5  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.4  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.3  2004/07/16 16:43:41  haase
   Made OIDs be long longs if they're big enough

   Revision 1.2  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
