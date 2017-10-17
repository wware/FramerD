/* C Mode */

/* network-pool.c
   Implements remote servers which store OIDs
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
  "$Id: network-pool.c,v 1.23 2005/01/14 16:48:48 haase Exp $";

/** Initializations, Declarations and utilities **/
/** Finding or creating network pools **/
/** Network pool operations **/
/** Committing to network pools **/
/** Initialization **/

#include "framerd.h"

/** Initializations, Declarations and utilities **/

#if FD_THREADS_ENABLED
static fd_mutex _network_pool_lookup_lock;
#endif

static lisp pool_data_symbol, new_oid_symbol, oid_changes_symbol;
static lisp lock_oid_symbol, unlock_oid_symbol;
static lisp clear_oid_lock_symbol, oid_value_symbol, fetch_oids_symbol;

static fd_exception
  Confusing_Server_Response="Funny pool data from server",
  ErrorCommittingOID="Error saving oid to server";

static void search_for_current_pool(fd_pool p,void *ptr);

static struct FD_POOL_HANDLER network_pool_handler;

/* Utility function */
static int in_poolp(FD_OID id,fd_pool p)
{
  return FD_OID_IN_RANGE(id, p->base, p->capacity);
}

static lisp oid_value_symbol, new_oid_symbol;
static lisp unlock_oid_symbol, lock_oid_symbol, clear_oid_lock_symbol;

#include <assert.h>

/** Finding existing pools **/

struct NP_SEARCH {char *servername; int portno; fd_network_pool result;};

static void search_for_current_pool(fd_pool p,void *ptr)
{
  struct NP_SEARCH *nps=ptr;
  if (nps->result) return;
  if (p->type == network_pool) {
    fd_network_pool npool=(fd_network_pool) p;
    if ((npool->conn->port == nps->portno) &&
	(strcmp(npool->conn->servername,nps->servername) == 0))
      nps->result=npool;}
}

static fd_network_pool find_existing_network_pool(fd_u8char *host,int portno)
{
  struct NP_SEARCH nps;
  nps.portno=portno; nps.result=NULL;
  if (portno < 0)
    nps.servername=fd_get_real_pathname(host);
  else nps.servername=fd_get_real_hostname(host);
  if (nps.servername == NULL) /* Only possible for server lookups */
    fd_raise_detailed_exception(fd_UnknownHost,host);
  fd_for_pools(search_for_current_pool,(void *)&nps);
  fd_xfree(nps.servername);
  return nps.result;
}

/** Creating network pools **/

static
fd_network_pool init_network_pool(fd_server conn,lisp pool_data,char *id)
{
  /* Make a new one */
  fd_network_pool npool=fd_malloc(sizeof(struct FD_NETWORK_POOL));
  /* Determined from pool data */
  lisp base, capacity, read_only, label, syncstamp;
  int read_only_code=0, pool_data_length=fd_list_length(pool_data);

  TIDY_ERRNO("init_network_pool preamble");

  /* Get base, capacity and load from server */
  if ((pool_data_length < 3) || (pool_data_length > 4))
    fd_raise_detailed_exception
      (Confusing_Server_Response,fd_object_to_string(pool_data));
  base=CAR(pool_data); 
  capacity=CAR(CDR(pool_data));
  read_only=CAR(CDR(CDR(pool_data)));
  if (pool_data_length == 4) label=CAR(CDR(CDR(CDR(pool_data))));
  else label=FD_EMPTY_CHOICE;
  if (!(OIDP(base)))
    fd_raise_detailed_exception
      (Confusing_Server_Response,fd_object_to_string(pool_data));
  if (!(FIXNUMP(capacity)))
    fd_raise_detailed_exception
      (Confusing_Server_Response,fd_object_to_string(pool_data));
  
  if (FD_FALSEP(read_only)) read_only_code=FD_POOL_LOCKABLE;
  else if (FD_EMPTY_LISTP(read_only)) read_only_code=FD_POOL_LOCKABLE;
  else if (FD_TRUEP(read_only)) read_only_code=FD_POOL_READ_ONLY;
  else fd_raise_detailed_exception
    (Confusing_Server_Response,fd_object_to_string(pool_data));

  fd_init_pool_data((fd_pool)npool,network_pool,
		    (FD_OID_ADDR(base)),FD_FIXLISP(capacity),read_only_code,
		    fd_strdup(id),NULL,fd_incref(label));

  {
    fd_lisp syncstamp=fd_careful_dtcall(conn,"GET-SYNCSTAMP",FD_VOID);
    if (FD_PAIRP(syncstamp)) npool->syncstamp=syncstamp;
    else {
      npool->syncstamp=FD_VOID; fd_decref(syncstamp);}}

  npool->conn=conn;

  if ((npool->prefix_id == NULL) && (FD_STRINGP(label)))
    npool->prefix_id=fd_strdup(FD_STRING_DATA(label));
  
  npool->handler=&network_pool_handler;

  /* Register the pool */
  fd_register_pool((fd_pool)npool);

  TIDY_ERRNO("init_network_pool");

  return npool;
}

fd_network_pool open_network_pool(char *servername,int port,char *id)
{
  fd_server conn=fd_new_connection(servername,port,id);
  lisp id_arg=fd_make_string(fd_session_id());
  lisp data=fd_careful_dtcall(conn,"POOL-DATA",id_arg,FD_VOID);
  if (PAIRP(data)) {
    fd_network_pool np=init_network_pool(conn,data,id);
    decref(data);
    return np;}
  else if (VECTORP(data)) {
    int i=0, l=VECTOR_LENGTH(data); fd_network_pool primary;
    while (i < l) {
      lisp pdata=VECTOR_REF(data,i);
      fd_network_pool np=init_network_pool(conn,pdata,id);
      if (i == 0) primary=np; i++;}
    return primary;}
  else fd_raise_lisp_exception
	 (_("Strange network POOL-DATA response"),id,data);
}

FRAMERD_EXPORT
/* fd_use_network_pool:
     Arguments: a port number, a host name, and an id string
     Returns: a pointer to a networked pool structure

   Errors:
    Cannot connect to server

   Side effects:
    Creates a networked pool structure for the server
    Adds a pointer to the pool structure to fd_all_pools
*/
fd_network_pool fd_use_network_pool(char *servername,int port,fd_u8char *id)
{
  fd_network_pool p;
  FD_WITH_MUTEX_LOCKED(&_network_pool_lookup_lock) {
    p=find_existing_network_pool(servername,port);
    if (p==NULL) p=open_network_pool(servername,port,id);}
  FD_END_WITH_MUTEX_LOCKED(&_network_pool_lookup_lock);
  return p;
}

/** Network pool operations **/

static lisp network_pool_fetch(fd_pool p,lisp x)
{
  fd_network_pool np=(fd_network_pool)p;
  /* Access to networked servers as pools */
  lisp expr=FD_MAKE_LIST(2,oid_value_symbol,x), response;
  /* fd_careful_dtype_eval signals errors when errors are returned */
  response=fd_careful_dtype_eval(expr,np->conn);
  decref(expr);
  return response;
}

static unsigned int network_pool_get_load(fd_pool p)
{
  fd_network_pool np=(fd_network_pool) p;
  lisp base=fd_make_oid(p->base);
  lisp answer=fd_careful_dtcall(np->conn,"GET-LOAD",base,FD_VOID);
  if (!(FIXNUMP(answer))) { /* Might use old server protocol */
    decref(answer);
    answer=fd_careful_dtcall(np->conn,"GET-LOAD",FD_VOID);}
  if (FIXNUMP(answer)) return FIXLISP(answer);
  else fd_raise_exception
	 (_("Networked pool won't provide load data"));
}

static void network_pool_close(fd_pool p)
{
  fd_network_pool np=(fd_network_pool)p;
  lisp unlock_expr=
    FD_MAKE_LIST(2,fd_make_symbol("UNLOCK-ALL"),
		 fd_make_string(fd_session_id()));
  fd_careful_dtype_eval(unlock_expr,np->conn);
  decref(unlock_expr);
}

static lisp network_pool_new_oid(fd_pool p)
{
  lisp expr=FD_MAKE_LIST1(new_oid_symbol), response;
  response=fd_careful_dtype_eval(expr,((fd_network_pool)p)->conn);
  decref(expr);
  return response;
}

static void network_pool_cleanup(fd_server conn)
{
  lisp unlock_expr=
    FD_MAKE_LIST(2,fd_make_symbol("UNLOCK-ALL"),
		 fd_make_string(fd_session_id()));
  fd_careful_dtype_eval(unlock_expr,conn);
  decref(unlock_expr);
}

static void network_pool_locker(fd_pool p,lisp obj,int action)
{
  if (action == FD_POOL_LOCK_OID) {
    lisp lock_expr=FD_MAKE_LIST(3,lock_oid_symbol,obj,
				fd_make_string(fd_session_id()));
    fd_network_pool np=(fd_network_pool) p;
    lisp v=fd_careful_dtype_eval(lock_expr,np->conn);
    fd_decref(lock_expr);
    if (FD_VOIDP(v))
      fd_raise_lisp_exception(fd_Cant_Lock_Remote_OID,p->id,obj);
    else {
      lisp old_value=FD_EMPTY_CHOICE;
      if (np->conn->closefn == NULL)
	np->conn->closefn=network_pool_cleanup;
      _fd_store_oid_value_nolock(obj,v); fd_decref(v);
      fd_hashset_add(&(p->modified),obj);}}
  else if (action == FD_POOL_UNLOCK_OID) {
    lisp clear_expr=
      FD_MAKE_LIST(3,clear_oid_lock_symbol,obj,
		   fd_make_string(fd_session_id()));
    lisp answer=fd_careful_dtype_eval(clear_expr,((fd_network_pool)p)->conn);
    if ((FD_EXCEPTIONP(answer)) || (FD_ERRORP(answer)))
      fd_raise_lisp_exception(_("Can't revert from server"),p->id,answer);
    decref(answer); decref(clear_expr);}
}

/** Committing to network pools **/

static int handles_bulk_commitp(fd_network_pool np)
{
  lisp expr=FD_MAKE_LIST(2,fd_make_symbol("BOUND?"),
			 fd_make_symbol("BULK-COMMIT"));
  lisp value=fd_careful_dtype_eval(expr,np->conn);
  decref(expr);
  if (FD_FALSEP(value)) return 0; else return 1;
}

static void network_pool_bulk_commit(fd_network_pool p)
{
  int i=0, n_modified=0;
  fd_lisp *mods=fd_get_modified((fd_pool)p,&n_modified,64);
  fd_lisp *scan=mods, *limit=scan+n_modified;
  lisp vector=fd_make_vector(2*n_modified), commit_expr, result;
  fd_notify(_("Commiting %d OID changes to network server %s"),n_modified,p->id); 
  /* Construct the alternating OID/VALUE vector */
  while (scan < limit) {
    fd_lisp v=fd_oid_value(*scan);
    if (FD_SLOTMAPP(v)) {
      fd_lock_mutex(&(SLOTMAP_PTR(v)->lock));
      SLOTMAP_PTR(v)->modified=0;
      fd_unlock_mutex(&(SLOTMAP_PTR(v)->lock));}
    FD_VECTOR_SET(vector,i,*scan); i++;
    FD_VECTOR_SET(vector,i,fd_oid_value(*scan)); i++;
    scan++;}
  /* Build the bulk commit expression to pass to the server. */
  commit_expr=FD_MAKE_LIST
    (3,fd_make_symbol("BULK-COMMIT"),fd_make_string(fd_session_id()),vector);
  /* Do the remote commit */
  result=fd_careful_dtype_eval(commit_expr,p->conn);
  /* Reinit the modified hashtable.  This has to be done before we start unlocking
     slotmaps, so that any newly enabled modifications get into the table. */
  fd_reinit_hashset(&(p->modified),64,1);
  decref(commit_expr); decref(result);
  fd_notify(_("Committed OID changes to network server %s"),p->id); 
  fd_free(mods,sizeof(fd_lisp)*n_modified);
  p->modifiedp=0;
}

static void commit_oid_to_server(fd_network_pool np,lisp oid)
{
  lisp expr, response, value;
  value=fd_oid_current_value(oid); /* Not threadsafe unless oid is locked */
  expr=FD_MAKE_LIST(4,unlock_oid_symbol,oid,
		    fd_make_string(fd_session_id()),
		    value);
  if (SLOTMAPP(value)) {
    fd_slotmap sm=SLOTMAP_PTR(value);
    lock_mutex(&(sm->lock));}
  response=fd_careful_dtype_eval(expr,np->conn); decref(expr); 
  if (SLOTMAPP(value)) {
    fd_slotmap sm=SLOTMAP_PTR(value);
    sm->modified=0; unlock_mutex(&(sm->lock));}
  if (!(FD_TRUEP(response)))
    fd_raise_lisp_exception(ErrorCommittingOID,"@?/?",value);
}

static void network_pool_commit(fd_pool p)
{
  if (p->modifiedp) {
    if (handles_bulk_commitp((fd_network_pool) p))
      network_pool_bulk_commit((fd_network_pool) p);
    else {
      int i=0, n_modified=0;
      fd_lisp *mods=fd_get_modified(p,&n_modified,64);
      fd_lisp *scan=mods, *limit=scan+n_modified;
      fd_network_pool np=(fd_network_pool) p;
      fd_notify(_("Commiting %d OID changes to network server %s"),
		n_modified,np->id); 
      while (scan < limit) {
	  FD_WITH_OID_LOCKED(*scan) {
	    commit_oid_to_server(np,*scan);}
	  FD_END_WITH_OID_LOCKED(*scan);
	  scan++;}
      fd_notify(_("Committed OID changes to network server %s"),np->id); 
      fd_free(mods,sizeof(fd_lisp)*n_modified);
      np->modifiedp=0;}}
}

static void network_pool_commit_oid(fd_pool p,lisp oid)
{
  FD_WITH_OID_LOCKED(oid)
    commit_oid_to_server((fd_network_pool)p,oid);
  FD_END_WITH_OID_LOCKED(oid);
}

static int network_pool_sync(fd_pool p)
{
  fd_network_pool np=(fd_network_pool)p;
  if (FD_VOIDP(np->syncstamp)) return -1;
  else {
    lisp syncstamp=fd_incref(np->syncstamp);
    lisp expr=(FD_MAKE_PAIR(oid_changes_symbol,syncstamp));
    lisp result=fd_careful_dtype_eval(expr,np->conn);
    if (!(FD_PAIRP(result))) {
      fd_warn("Odd sync return result: %q",result);
      fd_decref(result); fd_decref(expr);
      return -1;}
    else if (FD_EMPTY_LISTP(FD_CDR(result))) {
      fd_swap_out_pool(p);
      np->syncstamp=fd_incref(FD_CAR(result)); fd_decref(syncstamp);
      fd_decref(expr); fd_decref(result);}
    else {
      lisp changes=FD_CDR(result), new_syncstamp=FD_CAR(result);
      int n_changes=FD_CHOICE_SIZE(changes);
      FD_DO_CHOICES(oid,changes) {
#if FD_TRACE_SYNCHRONIZATION
	fd_fprintf(stderr,"Swapping out %q to synchronize\n",oid);
#endif
	fd_swap_out(oid);}
      END_FD_DO_CHOICES;
      np->syncstamp=fd_incref(new_syncstamp); fd_decref(syncstamp);
      fd_decref(expr); fd_decref(result);
      return n_changes;}}
}

/* Network pool bulk fetch*/

/* network_pool_bulk_fetch:
      Arguments: a pointer to a file pool
                 a lisp pointer to a choice of OIDs
      Returns: void
   Loads a set of oids into memory at once.
*/
/* network_pool_bulk_fetch:
      Arguments: a pointer to a file pool
                 a lisp pointer to a choice of OIDs
      Returns: void
   Loads a set of oids into memory at once.
*/
static int network_pool_bulk_fetch
  (fd_pool p,fd_lisp *oids,fd_lisp *values,int n)
{
  fd_network_pool np=(fd_network_pool)p;
  fd_lisp vec=fd_make_vector(n), expr, result;
  int i=0, worth_doing=0; while (i < n) {
    FD_OID id=FD_OID_ADDR(oids[i]);
    if (!(in_poolp(id,p))) {
      FD_VECTOR_SET(vec,i,FD_VOID); i++;}
    else if (fd_oid_loadedp(oids[i])) {
      values[i]=fd_oid_value(oids[i]); i++;}
    else {
      FD_VECTOR_SET(vec,i,oids[i]); worth_doing=1; i++;}}
  assert(i == n);
  if (worth_doing) {
    expr=FD_MAKE_LIST(2,fetch_oids_symbol,vec);
    result=fd_dtype_eval(expr,np->conn); fd_decref(expr);
    if ((FD_VECTORP(result)) && ((FD_VECTOR_LENGTH(result)) == n)) {
      fd_lisp *data=FD_VECTOR_ELEMENTS(result);
      i=0; while (i < n) {
	if (!(FD_VOIDP(data[i]))) values[i]=fd_incref(data[i]); i++;}
      fd_decref(result);
      return 1;}
    else return 0;}
  else {
    fd_decref(vec);
    return 1;}
}

/** Initialization **/

static struct FD_POOL_HANDLER network_pool_handler={
  network_pool_new_oid,
  network_pool_fetch,
  network_pool_commit,
  network_pool_commit_oid,
  network_pool_bulk_fetch,
  network_pool_close,
  network_pool_locker,
  network_pool_get_load,
  NULL,
  NULL,
  network_pool_sync
};

void fd_initialize_network_pool_c()
{

  oid_changes_symbol=fd_make_symbol("OID-CHANGES");
  pool_data_symbol=fd_make_symbol("POOL-DATA");
  new_oid_symbol=fd_make_symbol("NEW-OID");
  oid_value_symbol=fd_make_symbol("OID-VALUE");
  lock_oid_symbol=fd_make_symbol("LOCK-OID");
  unlock_oid_symbol=fd_make_symbol("UNLOCK-OID");
  clear_oid_lock_symbol=fd_make_symbol("CLEAR-OID-LOCK");
  fetch_oids_symbol=fd_make_symbol("FETCH-OIDS");

#if FD_THREADS_ENABLED
  fd_init_mutex(&_network_pool_lookup_lock);
#endif

  fd_register_source_file("network-pool",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: network-pool.c,v $
   Revision 1.23  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.22  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.21  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.20  2004/03/25 18:21:50  haase
   Fixed but with network-pool bulk fetch which clobbered other pools bulk fetches

   Revision 1.19  2004/02/14 14:53:23  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.18  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.17  2003/12/01 16:18:41  haase
   Check return values of fd_get_real_hostname

   Revision 1.16  2003/11/26 14:52:51  haase
   Networked pools and indices to the same server now have their own connection

   Revision 1.15  2003/09/30 11:16:15  haase
   Added extra locks to protect pool and index registries

   Revision 1.14  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.13.2.3  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.13.2.2  2003/08/02 13:59:20  haase
   Reorganized pool initialization

   Revision 1.13.2.1  2003/01/26 20:45:13  haase
   Misc. fixes and general cleanup

   Revision 1.13  2002/06/23 11:51:02  haase
   Fixed some race conditions with OID saving and multi threaded processes (where one thread is saving an OID while another one is modifying it)

   Revision 1.12  2002/05/29 18:42:02  haase
   Made prefetching avoid redundant network traffic

   Revision 1.11  2002/04/10 18:52:40  haase
   include/framerd/odb.h

   Revision 1.10  2002/04/10 14:13:13  haase
   Fixed bug with assigning invalid poolids; more work needed

   Revision 1.9  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
