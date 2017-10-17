/* C Mode */

/* network-index.c
   Implements networked hash tables for DType objects.
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
 "$Id: network-index.c,v 1.33 2005/01/14 16:48:46 haase Exp $";

/* Includes and declarations */
/** Utility functions **/
/** Creating and finding network indices **/
/** Network index operations **/
/** Initialization **/

#include "framerd.h"

#if FD_THREADS_ENABLED
static fd_mutex _network_index_lookup_lock;
#endif

#define BULK_ADD_QUANTA 3000

static lisp quote_symbol;
/* Symbols used in constructing requests to networked indices. */
static lisp iserver_get, iserver_get_size, iserver_add, iserver_drop;
static lisp ixserver_get, ixserver_get_size, ixserver_add, ixserver_drop;
static lisp iserver_writable, iserver_keys, iserver_changes;
static lisp ixserver_writable, ixserver_keys, ixserver_changes;
static lisp iserver_bulk_get, iserver_bulk_add;
static lisp ixserver_bulk_get, ixserver_bulk_add;

static struct FD_INDEX_HANDLER network_index_handler;

static lisp network_index_fetch(fd_index ix,lisp key);
static int network_index_fetch_size(fd_index ix,lisp key);
static void network_index_commit(fd_index ix);
static lisp network_index_keys(fd_index ix);
static void network_index_close(fd_index ix);

/** Utility functions **/

FASTOP lisp quote_lisp(lisp x)
{
  if ((SYMBOLP(x)) || (PAIRP(x)))
    return FD_MAKE_PAIR(quote_symbol,FD_MAKE_PAIR(incref(x),FD_EMPTY_LIST));
  else return incref(x);
}

/** Creating and finding network indices **/

/* This does the work of creating the struct, initializing its fields,
    making the network connection, and assigning appropriate handlers. */
static fd_network_index open_network_index(char *host,int port,fd_lisp xname,char *id)
{
  fd_network_index ni=fd_xmalloc(sizeof(struct FD_NETWORK_INDEX));
  ni->id=fd_strdup(id); ni->type=network_index;
  ni->read_only=1; ni->zipf_threshold=0;
  ni->interned_values.n_slots=0; ni->xname=xname;
#if (FD_THREADS_ENABLED)
  fd_init_mutex(&(ni->lock));
#endif
  fd_init_hashtable(&(ni->cache),128);
  fd_init_hashtable(&(ni->sizes),128);
  ni->conn=fd_new_connection(host,port,id);
  
  {
    fd_lisp syncstamp=fd_careful_dtcall(ni->conn,"GET-SYNCSTAMP",FD_VOID);
    if (FD_PAIRP(syncstamp)) ni->syncstamp=syncstamp;
    else {
      ni->syncstamp=FD_VOID; fd_decref(syncstamp);}}
  
  ni->handler=&network_index_handler;
  
  {
    fd_lisp query=
      ((FD_VOIDP(xname)) ? (FD_MAKE_LIST1(iserver_writable)) :
       (FD_MAKE_LIST(2,ixserver_writable,xname)));
    fd_lisp writablep=fd_careful_dtype_eval(query,ni->conn);
    fd_decref(query);
    if (FD_FALSEP(writablep)) ni->read_only=1;
    else ni->read_only=0;}
  fd_init_hashtable(&(ni->adds),128);
  fd_init_hashtable(&(ni->drops),128);
  ni->cache_size=128; ni->sizes_size=128;
  ni->adds_size=128; ni->drops_size=128;
  
  fd_register_index((fd_index)ni);
  return ni;
}

/* This finds if the index has already been created. */
static fd_network_index find_network_index
    (char *servername,int portno,fd_lisp xname)
{
  char *realname=
    ((portno<0) ? (fd_get_real_pathname(servername)) :
     (fd_get_real_hostname(servername)));
  fd_index scan=fd_all_indices;
  if (realname == NULL)
    fd_raise_detailed_exception(fd_UnknownHost,servername);
  while (scan) 
    if (scan->type != network_index) scan=scan->next;
    else if ((portno == ((fd_network_index)scan)->conn->port) &&
	     (strcmp(((fd_network_index)scan)->conn->servername,realname) == 0) &&
	     (FD_LISP_EQ(xname,((fd_network_index)scan)->xname))) {
      fd_xfree(realname);
      return (fd_network_index)scan;}
    else scan=scan->next;
  fd_xfree(realname);
  return NULL;
}

FRAMERD_EXPORT
/* fd_open_network_index:
     Arguments: servername (a string), port_no (an int), and id (a utf8 string)
     Returns: a pointer to a network index struct

   Finds or creates (if neccessary) a network index served by the specified
    servername and port.
*/
fd_network_index fd_open_network_index(char *servname,int port_no,fd_lisp xname,fd_u8char *id)
{
  fd_network_index ix=NULL;
  FD_WITH_MUTEX_LOCKED(&_network_index_lookup_lock) {
    ix=find_network_index(servname,port_no,xname);
    if (ix == NULL) ix=open_network_index(servname,port_no,xname,id);}
  FD_END_WITH_MUTEX_LOCKED(&_network_index_lookup_lock);
  return ix;
}

/** Network index operations **/

static lisp network_index_fetch(fd_index ix,lisp key)
{
  fd_network_index nix=(fd_network_index)ix;
  lisp query, response;
  query=FD_MAKE_LIST1(quote_lisp(key));
  if (FD_VOIDP(nix->xname))
    query=FD_MAKE_PAIR(iserver_get,query);
  else {
    query=FD_MAKE_PAIR(nix->xname,query);
    query=FD_MAKE_PAIR(ixserver_get,query);}
  response=fd_careful_dtype_eval(query,nix->conn);
  decref(query); 
  return response;
}

static int network_index_fetch_size(fd_index ix,lisp key)
{
  fd_network_index nix=(fd_network_index)ix;
  lisp query, response;
  query=FD_MAKE_LIST1(quote_lisp(key));
  if (FD_VOIDP(nix->xname))
    query=FD_MAKE_PAIR(iserver_get_size,query);
  else {
    query=FD_MAKE_PAIR(nix->xname,query);
    query=FD_MAKE_PAIR(ixserver_get_size,query);}
  response=fd_careful_dtype_eval(query,nix->conn);
  decref(query); 
  if (FIXNUMP(response)) return FIXLISP(response);
  else fd_raise_detailed_exception
    ("Index server returned bad size",fd_object_to_string(response));
}

static void bulk_add(fd_hashtable adds,fd_server s,fd_lisp xname)
{
  int i=0; fd_pair *scan=adds->table, *limit=scan+adds->n_slots;
  fd_lisp vec=fd_make_vector(BULK_ADD_QUANTA*2), save_expr, result;
  fd_lisp *velts=FD_VECTOR_ELEMENTS(vec);
  /* Init the vector, which we will reuse */
  i=0; while (i < BULK_ADD_QUANTA*2) velts[i++]=FD_VOID; i=0;
  while (scan < limit) {
    /* If we have filled our working vector, we do a save */
    if (i >= BULK_ADD_QUANTA*2) {
      /* We make our save expr and send it off. */
      if (FD_VOIDP(xname))
	save_expr=FD_MAKE_LIST(2,iserver_bulk_add,fd_incref(vec));
      else save_expr=FD_MAKE_LIST(3,ixserver_bulk_add,xname,fd_incref(vec));
      result=fd_careful_dtype_eval(save_expr,s);
      fd_decref(save_expr); fd_decref(result);
      /* Now, we reset the elements of the vector because
	 we will use it again. */
      i=0; while (i < BULK_ADD_QUANTA*2) {
	fd_decref(velts[i]); velts[i]=FD_VOID; i++;}
      i=0;}
    if (*scan) {
      fd_pair elt=*scan++;
      velts[i++]=incref(elt->car);
      velts[i++]=incref(elt->cdr);}
    else scan++;}
  /* Now that we're done, we have the remaining keys to save,
     so we make our save expr, send it off and clean up. */
  save_expr=FD_MAKE_LIST(2,iserver_bulk_add,fd_incref(vec));
  result=fd_careful_dtype_eval(save_expr,s);
  fd_decref(save_expr); fd_decref(result);
  /* This should finally free the vector we have been reusing. */
  fd_decref(vec);
}

static void onebyone_add(fd_hashtable adds,fd_server s,fd_lisp xname)
{
  fd_pair *scan=adds->table, *limit=scan+adds->n_slots;
  while (scan < limit)
    if (*scan) {
      fd_pair elt=*scan++;
      fd_lisp key=elt->car, value=fd_incref(elt->cdr);
      fd_lisp add_expr, answer;
      if (CHOICEP(value)) value=fd_return_proper_choice(value);
      if (FD_VOIDP(xname))
	add_expr=FD_MAKE_LIST(3,iserver_add,quote_lisp(key),quote_lisp(value));
      else add_expr=FD_MAKE_LIST(4,ixserver_add,xname,quote_lisp(key),quote_lisp(value));
      decref(value);
      answer=fd_careful_dtype_eval(add_expr,s);
      decref(add_expr); decref(answer);}
    else scan++;
}

static void onebyone_drop(fd_hashtable drops,fd_server s,fd_lisp xname)
{
  fd_lisp drop_expr, answer;
  fd_pair *scan=drops->table, *limit=scan+drops->n_slots;
  while (scan < limit)
    if (*scan) {
      fd_pair elt=*scan++;
      fd_lisp key=elt->car, value=elt->cdr;
      fd_lisp drop_expr, answer;
      if (FD_VOIDP(xname))
	drop_expr=
	  FD_MAKE_LIST(3,iserver_drop,quote_lisp(key),quote_lisp(value));
      else drop_expr=
	     FD_MAKE_LIST(4,ixserver_drop,xname,quote_lisp(key),quote_lisp(value));
      answer=fd_careful_dtype_eval(drop_expr,s);
      decref(drop_expr); decref(answer);}
    else scan++;
}

static int server_supports_drops(fd_server s)
{
  lisp question=FD_MAKE_LIST(2,fd_make_symbol("BOUND?"),iserver_drop);
  lisp answer=fd_careful_dtype_eval(question,s);
  decref(question);
  if (FD_FALSEP(answer)) {
    fd_warn(_("Remote index does not support dropping"));
    return 0;}
  else {
    fd_decref(answer); return 1;}
}

static int server_supports_bulk_commit(fd_server s,fd_lisp xname)
{
  lisp question=
    ((FD_VOIDP(xname)) ? (FD_MAKE_LIST(2,fd_make_symbol("BOUND?"),iserver_bulk_add)) :
     (FD_MAKE_LIST(2,fd_make_symbol("BOUND?"),ixserver_bulk_add)));
  lisp answer=fd_careful_dtype_eval(question,s);
  fd_decref(question); 
  if (FD_FALSEP(answer)) return 0;
  else {fd_decref(answer); return 1;}
}

static void network_index_commit(fd_index ix)
{
  fd_network_index nix=(fd_network_index)ix;
  fd_server s=nix->conn; fd_lisp xname=nix->xname; int no_drops=1;
  fd_hashtable adds=&(nix->adds), drops=&(nix->drops);
  int do_bulk_add=0;
  if ((adds->n_keys == 0) && (drops->n_keys == 0)) return;
  if ((adds->n_keys > 16) && (server_supports_bulk_commit(s,xname))) do_bulk_add=1; 
  {
    UNWIND_PROTECT {
      fd_lock_mutex(&(adds->lock)); fd_lock_mutex(&(drops->lock));
      fd_notify(_("Committing changes to network index %s (%d+%d keys changed)"),
		ix->id,adds->n_keys,drops->n_keys);
      if (do_bulk_add) bulk_add(adds,s,xname); else onebyone_add(adds,s,xname);
      if (drops->n_keys)
	if (server_supports_drops(s)) onebyone_drop(drops,s,xname);
	else fd_warn("Server %s doesn't support drops!",ix->id);
      if (fd_normal_exit == 0) {
	fd_reinit_hashtable(adds,128,1);
	fd_reinit_hashtable(drops,128,1);}}
    ON_UNWIND {
      fd_unlock_mutex(&(adds->lock)); fd_unlock_mutex(&(drops->lock));
      fd_notify(_("Finished saving network index %s"),ix->id);}
    END_UNWIND;}
}

static void network_index_prefetch(fd_index ix,fd_lisp keys)
{
  int i=0, lim=FD_CHOICE_SIZE(keys);
  fd_network_index nix=(fd_network_index)ix;
  fd_hashtable cache=&(nix->cache), adds=&(nix->adds), drops=&(nix->drops);
  fd_lisp vec=fd_make_vector(FD_CHOICE_SIZE(keys));
  fd_lisp expr=((FD_VOIDP(nix->xname)) ? (FD_MAKE_LIST(2,iserver_bulk_get,vec)) :
		(FD_MAKE_LIST(3,ixserver_bulk_get,nix->xname,vec)));
  fd_lisp values;
  FD_WITH_MUTEX_LOCKED(&(nix->lock)) {
    FD_DO_CHOICES(key,keys) {
      if (fd_hashtable_probe(cache,key)) {}
      else {FD_VECTOR_SET(vec,i,fd_incref(key)); i++;}}
    END_FD_DO_CHOICES;
    /* Finish out vector */
    while (i < lim) {FD_VECTOR_SET(vec,i,FD_VOID); i++;}
    values=fd_dtype_eval(expr,nix->conn);
    if ((FD_VECTORP(values)) &&
	(FD_VECTOR_LENGTH(values) == FD_VECTOR_LENGTH(vec))) {
      int i=0, limit=FD_VECTOR_LENGTH(vec);
      while (i < limit) {
	fd_lisp plus=
	  fd_hashtable_get(adds,FD_VECTOR_REF(vec,i),(FD_EMPTY_CHOICE));
	fd_lisp minus=
	  fd_hashtable_get(drops,FD_VECTOR_REF(vec,i),(FD_EMPTY_CHOICE));
	fd_hashtable_set(&(ix->cache),
			 FD_VECTOR_REF(vec,i),FD_VECTOR_REF(values,i));
	if (!(FD_EMPTYP(plus)))
	  fd_hashtable_add(&(ix->cache),FD_VECTOR_REF(vec,i),plus);
	if (!(FD_EMPTYP(minus)))
	  fd_hashtable_drop(&(ix->cache),FD_VECTOR_REF(vec,i),minus);
	fd_decref(plus); fd_decref(minus);
	i++;}
      fd_decref(values); fd_decref(expr);}
    else {
      fd_decref(values); fd_decref(expr);}}
  FD_END_WITH_MUTEX_LOCKED(&(nix->lock));
}

static lisp network_index_keys(fd_index ix)
{
  fd_network_index nix=(fd_network_index)ix;
  lisp expr=((FD_VOIDP(nix->xname)) ? (FD_MAKE_LIST1(iserver_keys)) :
	     (FD_MAKE_LIST(2,ixserver_keys,nix->xname)));
  lisp keys=fd_careful_dtype_eval(expr,nix->conn);
  decref(expr);
  return keys;
}

static int network_index_sync(fd_index ix)
{
  fd_network_index nix=(fd_network_index)ix;
  if (FD_VOIDP(nix->syncstamp)) return -1;
  else {
    lisp syncstamp=fd_incref(nix->syncstamp);
    lisp expr=((FD_VOIDP(nix->xname)) ? (FD_MAKE_PAIR(iserver_changes,syncstamp)) :
	       (FD_MAKE_PAIR(ixserver_changes,FD_MAKE_PAIR(nix->xname,syncstamp))));
    lisp result=fd_careful_dtype_eval(expr,nix->conn);
    if (!(FD_PAIRP(result))) {
      fd_warn("Odd sync return result: %q",result);
      fd_decref(result); fd_decref(expr);
      return -1;}
    else if (FD_EMPTY_LISTP(FD_CDR(result))) {
      fd_decref(expr); fd_decref(result);
      fd_swap_out_index(ix);
      nix->syncstamp=fd_incref(FD_CAR(result)); fd_decref(syncstamp);}
    else {
      lisp changes=FD_CDR(result), new_syncstamp=FD_CAR(result);
      int n_changes=FD_CHOICE_SIZE(changes);
      FD_DO_CHOICES(key,changes) {
	if (fd_hashtable_probe(&(nix->cache),key)) {
#if FD_TRACE_SYNCHRONIZATION
	  fd_fprintf(stderr,"Clearing entry %q in %s for synchronization\n",
		     key,nix->id);
#endif
	  fd_hashtable_set(&(nix->cache),key,FD_VOID);}}
      END_FD_DO_CHOICES;
      nix->syncstamp=fd_incref(new_syncstamp); fd_decref(syncstamp);
      fd_decref(expr); fd_decref(result);
      return n_changes;}}
}

static void network_index_close(fd_index ix)
{
  fd_network_index nix=(fd_network_index)ix;
  fd_close_connection(nix->conn);
}

/** Initialization **/

static struct FD_INDEX_HANDLER network_index_handler={
  NULL,NULL,NULL,NULL, /* get, get_size, add, drop are all by default */
  network_index_fetch,
  network_index_fetch_size,
  network_index_commit,
  network_index_prefetch,
  network_index_keys,
  NULL, /* No spend memory */
  network_index_close,
  network_index_sync,
  NULL};

void fd_initialize_network_index_c()
{
  quote_symbol=fd_make_symbol("QUOTE");
  iserver_writable=fd_make_symbol("ISERVER-WRITABLE?");
  iserver_keys=fd_make_symbol("ISERVER-KEYS");
  iserver_changes=fd_make_symbol("ISERVER-CHANGES");
  iserver_get=fd_make_symbol("ISERVER-GET");
  iserver_get_size=fd_make_symbol("ISERVER-GET-SIZE");
  iserver_add=fd_make_symbol("ISERVER-ADD!");
  iserver_drop=fd_make_symbol("ISERVER-DROP!");
  iserver_bulk_get=fd_make_symbol("ISERVER-BULK-GET");
  iserver_bulk_add=fd_make_symbol("ISERVER-BULK-ADD!");

  ixserver_writable=fd_make_symbol("IXSERVER-WRITABLE?");
  ixserver_keys=fd_make_symbol("IXSERVER-KEYS");
  ixserver_changes=fd_make_symbol("IXSERVER-CHANGES");
  ixserver_get=fd_make_symbol("IXSERVER-GET");
  ixserver_get_size=fd_make_symbol("IXSERVER-GET-SIZE");
  ixserver_add=fd_make_symbol("IXSERVER-ADD!");
  ixserver_drop=fd_make_symbol("IXSERVER-DROP!");
  ixserver_bulk_get=fd_make_symbol("IXSERVER-BULK-GET");
  ixserver_bulk_add=fd_make_symbol("IXSERVER-BULK-ADD!");

#if FD_THREADS_ENABLED
  fd_init_mutex(&_network_index_lookup_lock);
#endif

  fd_register_source_file("network-index",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: network-index.c,v $
   Revision 1.33  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.32  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.31  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.30  2004/05/14 14:41:57  haase
   Made preloading be an option for all kinds of indices

   Revision 1.29  2004/02/14 14:53:22  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.28  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.27  2003/12/01 16:18:39  haase
   Check return values of fd_get_real_hostname

   Revision 1.26  2003/12/01 15:07:20  haase
   Added error case for network indices with bad hostnames

   Revision 1.25  2003/11/26 14:52:50  haase
   Networked pools and indices to the same server now have their own connection

   Revision 1.24  2003/11/26 13:51:54  haase
   Added index subservers

   Revision 1.23  2003/09/30 19:08:52  haase
   Wrapped locks around pool/index lookup/creation

   Revision 1.22  2003/09/30 11:16:15  haase
   Added extra locks to protect pool and index registries

   Revision 1.21  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.20.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.20.2.1  2003/01/26 20:43:15  haase
   Misc. fixes especially some GC

   Revision 1.20  2002/07/03 21:52:00  haase
   Made index saving decline to reinit hashsets when invoked while exiting

   Revision 1.19  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.18  2002/05/19 13:19:43  haase
   Fix string leaks

   Revision 1.17  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.16  2002/04/10 18:58:15  haase
       Made canonicalization of filenames and server specs use
   fd_get_real_pathname and fd_get_real_hostname, rather than
   trying special kludges.

   Revision 1.15  2002/04/09 21:12:41  haase
   Fix bulk add gc bug

   Revision 1.14  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
