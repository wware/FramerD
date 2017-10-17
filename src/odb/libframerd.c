/* C Mode */

/* libframerd.c
   Implements init function for the FramerD library, including
   both ODB and index functions.
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
  "$Id: libframerd.c,v 1.9 2005/01/14 16:48:48 haase Exp $";

#include "framerd.h"

static int framerd_initialized=0;

DTYPES_EXPORT void fd_initialize_dtypes(void);
extern void fd_initialize_pools_c(void);
extern void fd_initialize_file_pool_c(void);
extern void fd_initialize_network_pool_c(void);
extern void fd_initialize_super_pool_c(void);
extern void fd_initialize_odb_c(void);
extern void fd_initialize_frames_c(void);
extern void fd_initialize_index_c(void);
extern void fd_initialize_file_index_c(void);
extern void fd_initialize_network_index_c(void);
extern void fd_initialize_search_c(void);

FRAMERD_EXPORT
/* fd_report_framerd_stats:
    Arguments: none
    Returns: void
   Reports framerd stats to the standard error.
*/
void fd_report_framerd_stats(FILE *to)
{
  if (to == NULL) to=stderr;
  fprintf(to,
	  _(";; %d pools, %d indices, %d net connections\n"),
	  fd_get_pool_count(),fd_get_index_count(),fd_get_server_count());
#if (FD_LIGHTWEIGHT_OIDS)
  fprintf(to,
	  _(";; %d OIDs currently loaded, %d new OIDs, %d OID loads overall, %d symbols\n"),
	  fd_loaded_oids,
	  fd_new_oids,fd_oids_loaded,
	  fd_symbol_table()->n_keys);
#else
  fprintf(to,
	  _(";; %d/%d OIDs refd/loaded, %d new OIDs, %d OID loads, %d symbols\n"),
	  fd_oid_table()->n_keys,fd_loaded_oids,
	  fd_new_oids,fd_oids_loaded,
	  fd_symbol_table()->n_keys);
#endif
  fprintf(to,_(";; %ld+%ld=%ld bytes currently in use total\n"),
	  fd_cons_usage(),fd_malloc_usage(),fd_cons_usage()+fd_malloc_usage());
  fflush(to);
}

/* Auto syncing */


static fd_pool *autosync_pools=NULL;
static fd_index *autosync_indices=NULL;
static int n_autosync_pools=0, n_autosync_indices=0, autosync_all=0;
#if FD_THREADS_ENABLED
static fd_mutex autosync_tables_lock;
#endif

FRAMERD_EXPORT
/* fd_autosync_pool
     Arguments: a pointer to a pool struct
     Returns: void
     Records the pool for synchronization whenever fd_autosync() is called. */
void fd_autosync_pool(fd_pool p)
{
  fd_pool *scan, *limit; int found=0;
  fd_lock_mutex(&autosync_tables_lock);
  scan=autosync_pools; limit=scan+n_autosync_pools;
  while (scan < limit)
    if (*scan == p) {found=1; break;}
    else scan++;
  if (found==0) {
    if (n_autosync_pools)
      autosync_pools=
	fd_realloc(autosync_pools,sizeof(fd_pool)*(n_autosync_pools+1),
		   sizeof(fd_pool)*n_autosync_pools);
    else autosync_pools=fd_malloc(sizeof(fd_pool));
    autosync_pools[n_autosync_pools++]=p;}
  fd_unlock_mutex(&autosync_tables_lock);
}

FRAMERD_EXPORT
/* fd_autosync_index
     Arguments: a pointer to an index struct
     Returns: void
     Records the index for synchronization whenever fd_autosync() is called. */
void fd_autosync_index(fd_index ix)
{
  fd_index *scan, *limit; int found=0;
  fd_lock_mutex(&autosync_tables_lock);
  scan=autosync_indices; limit=scan+n_autosync_indices;
  while (scan < limit)
    if (*scan == ix) {found=1; break;}
    else scan++;
  if (found==0) {
    if (n_autosync_indices)
      autosync_indices=
	fd_realloc(autosync_indices,sizeof(fd_index)*(n_autosync_indices+1),
		   sizeof(fd_index)*n_autosync_indices);
    else autosync_indices=fd_malloc(sizeof(fd_pool));
    autosync_indices[n_autosync_indices++]=ix;}
  fd_unlock_mutex(&autosync_tables_lock);
}

FRAMERD_EXPORT
/* fd_autosync_all
     Arguments: boolean (1 or 0 for true or false)
     Returns: 1 if it did anything, zero otherwise
  Arranges for all pools and indices to be sync'd (or not) when
  fd_autosync() is called. */
int fd_autosync_all(int flag)
{
  if (autosync_all == flag) return 0;
  else {
    autosync_all=flag;
    return 1;}
}

FRAMERD_EXPORT
/* fd_autosync
     Arguments: none
     Returns: void
  Synchronizes all the flagged databases (pools and indices) with their
sources.  Databases are flagged with fd_autosync_pool and fd_autosync_index
while fd_autosync_all(flag) sets autosyncing for all pools and indices. */
void fd_autosync()
{
  if (autosync_all) {
    fd_sync_pools(); fd_sync_indices();}
  else {
    FD_WITH_MUTEX_LOCKED(&autosync_tables_lock) {
      fd_pool *pscan=autosync_pools,
	*plimit=autosync_pools+n_autosync_pools;
      fd_index *iscan=autosync_indices,
	*ilimit=autosync_indices+n_autosync_indices;
      while (pscan < plimit) fd_sync_pool(*pscan++);
      while (iscan < ilimit) fd_sync_index(*iscan++);}
    FD_END_WITH_MUTEX_LOCKED(&autosync_tables_lock);}
}


/* Initializing FramerD */

FRAMERD_EXPORT void fd_initialize_framerd()
{
  if (framerd_initialized) return; else framerd_initialized=1;
  fd_initialize_dtypes();
  fd_initialize_pools_c();
  fd_initialize_file_pool_c();
  fd_initialize_network_pool_c();
  fd_initialize_super_pool_c();
  fd_initialize_odb_c();
  fd_initialize_frames_c();
  fd_initialize_index_c();
  fd_initialize_file_index_c();
  fd_initialize_network_index_c();
  fd_initialize_search_c();
#if FD_THREADS_ENABLED
  fd_init_mutex(&autosync_tables_lock);
#endif
}

#if ((defined(WIN32)) && (!(defined(STATICLINK))))
BOOL APIENTRY DllMain( HANDLE hModule, 
                        DWORD ul_reason_for_call, 
                        LPVOID lpReserved )
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
      fd_initialize_framerd(); break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      {}
    }
    return TRUE;
}
#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: libframerd.c,v $
   Revision 1.9  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.8  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.7  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.6  2004/02/14 14:53:22  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
