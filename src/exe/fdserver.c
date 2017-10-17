/* C Mode */

/* fdserver.c
   The top level FramerD server executable
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

static char vcid[] = "$Id: fdserver.c,v 1.47 2005/01/14 16:48:45 haase Exp $";

#include "framerd/fdscript.h"
#include "framerd/server.h"

extern void fd_initialize_fdtext(void);

FRAMERD_EXPORT fd_lispenv fd_enabled_env;
FDSCRIPT_EXPORT fd_lispenv fd_osprims_env;
FDSCRIPT_EXPORT fd_lispenv fd_fdinternals_env;

IMPORTED fd_lispenv fd_texttools_env;

static fd_exception OIDNotLocked=_("OID Not Locked");
static int using_localhost=0;
static FILE *journal_file=NULL;
#if FD_THREADS_ENABLED
static fd_mutex journal_lock;
#endif

void fd_initialize_server_c(void);

#include "framerd/plugins.h"
#ifdef WIN32
#include <process.h>
#include <direct.h>
#endif
#include <time.h>
#include <signal.h>

#ifndef SPECIAL_INITS
#define SPECIAL_INITS
#endif

/* Levels of server security:
   lax: servers have full FDScript functionality
   standard: severs have safe FDScript functions and all
     init file definitions not declared risky
   safe: servers have only "safe" FDScript functions and
     all init file definitions explicitly declared safe
   strict: servers only provide declared functions
*/

static int port_no;
static char *port_id, *nid_file=NULL, *pid_file=NULL;
static int server_started=0;

static fd_pool primary_pool=NULL; /* Where new OIDs are created */
static fd_pool served_pools[32]; static int n_served_pools=0;
static unsigned int read_only=0;

static fd_index served_indices[64];
static int n_served_indices=0, index_read_only=1;

extern int fd_server_trace; extern FILE *fd_server_log;

static int server_start;

static int lax_security=1;
static int force_serialization=0;
static int total_transactions=0;
static fd_lispenv prims_env=NULL, server_env=NULL;
static int memory_base=0;
#if (FD_THREADS_ENABLED)
static fd_mutex eval_lock, oid_locks_lock;
#endif

static fd_lisp label_symbol, memory_headroom_symbol, quote_symbol;
static fd_lisp client_addr_symbol, sid_symbol, locks_symbol;

/* Change logs */

static int change_count=0;

#if FD_THREADS_ENABLED
static fd_mutex changelog_lock;
#endif

struct FD_CHANGELOG_ENTRY {
  int moment; fd_lisp keys;};

struct FD_CHANGELOG {
  int point, max, full;
  struct FD_CHANGELOG_ENTRY *entries;};

static struct FD_CHANGELOG oid_changelog, index_changelog;
static struct FD_SUBINDEX_CHANGELOG {
  fd_index ix; struct FD_CHANGELOG *clog;} *subindex_changelogs=NULL;
static int n_subindex_changelogs=0;

static void init_changelog(struct FD_CHANGELOG *clog,int size)
{
  clog->entries=fd_malloc(sizeof(struct FD_CHANGELOG_ENTRY)*size);
  clog->max=size; clog->point=0; clog->full=0;
}

static struct FD_CHANGELOG *get_subindex_changelog(fd_index ix,int make)
{
  struct FD_CHANGELOG *clog=NULL;
  FD_WITH_MUTEX_LOCKED(&changelog_lock) {
    int i=0; while (i < n_subindex_changelogs)
      if (subindex_changelogs[i].ix == ix) break; else i++;
    if (subindex_changelogs[i].ix == ix) clog=(subindex_changelogs[i].clog);
    else if (make == 0) clog=NULL;
    else {
      clog=fd_malloc(sizeof(struct FD_CHANGELOG));
      if (subindex_changelogs)
	subindex_changelogs=fd_realloc(subindex_changelogs,
				       sizeof(struct FD_SUBINDEX_CHANGELOG)*(n_subindex_changelogs+1),
				       sizeof(struct FD_SUBINDEX_CHANGELOG)*(n_subindex_changelogs));
      else subindex_changelogs=fd_malloc(sizeof(struct FD_SUBINDEX_CHANGELOG));
      subindex_changelogs[n_subindex_changelogs].ix=ix; subindex_changelogs[n_subindex_changelogs].clog=clog;
      init_changelog(clog,1024);
      n_subindex_changelogs++;}}
  FD_END_WITH_MUTEX_LOCKED(&changelog_lock);
  return clog;
}

static void add_to_changelog(struct FD_CHANGELOG *clog,fd_lisp keys)
{
  int changestamp;
  FD_WITH_MUTEX_LOCKED(&changelog_lock) {
    struct FD_CHANGELOG_ENTRY *entries=clog->entries;
    int point=clog->point;
    if (clog->full) {
      fd_decref(entries[point].keys);}
    entries[point].moment=change_count++;
    entries[point].keys=fd_incref(keys);
    if (clog->full)
      if (clog->point == clog->max) clog->point=0;
      else clog->point++;
    else if (clog->point == clog->max) {
      clog->point=0; clog->full=1;}
    else clog->point++;}
  FD_END_WITH_MUTEX_LOCKED(&changelog_lock);
}

static fd_lisp get_changes(struct FD_CHANGELOG *clog,int cstamp,int *new_cstamp)
{
  fd_lisp result;
  FD_WITH_MUTEX_LOCKED(&changelog_lock) {
    int bottom=((clog->full) ? (clog->point) : 0);
    int top=((clog->full) ? ((clog->point) ? (clog->point-1) : (clog->max)) : (clog->point-1));
    struct FD_CHANGELOG_ENTRY *entries=clog->entries;
    *new_cstamp=change_count;
    if ((clog->full == 0) && (clog->point == 0)) result=FD_EMPTY_CHOICE; /* Changelog is empty */
    else if (cstamp < entries[bottom].moment) result=FD_FALSE; /* Too far back. */
    else if (cstamp > entries[top].moment) result=FD_EMPTY_CHOICE; /* No changes. */
    else {
      fd_lisp changes=FD_EMPTY_CHOICE;
      int i=top, point=clog->point; while (i >= 0) 
	if (cstamp <= entries[i].moment) {
	  FD_ADD_TO_CHOICE(changes,fd_incref(entries[i].keys)); i--;}
	else break;
      if (cstamp > entries[i].moment) {
	i=clog->max; while (i >= point)
	  if (cstamp <= entries[i].moment) {
	    FD_ADD_TO_CHOICE(changes,fd_incref(entries[i].keys)); i--;}
	  else break;}
      result=changes;}}
  FD_END_WITH_MUTEX_LOCKED(&changelog_lock);
  return result;
}

static fd_lisp lisp_get_syncstamp_cproc()
{
  return FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(change_count));
}

/** Checking memory usage **/

FASTOP unsigned int memory_usage()
{
  return fd_malloc_usage()+fd_cons_usage();
}

#define SWAP_OUTP() \
   ((FD_FIXNUMP(FD_SYMBOL_VALUE(memory_headroom_symbol))) && \
    ((memory_usage()) > \
     ((unsigned int)\
      (memory_base+\
       (FD_FIXLISP(FD_SYMBOL_VALUE(memory_headroom_symbol)))))))


static void swap_out_index(fd_index ix,void *ignore)
{
  fd_swap_out_index(ix);
}

static void swap_everything_out()
{
  fd_report_framerd_stats(stderr);
  fd_swap_out_pools();
  fd_swap_out_indices();
  memory_base=memory_usage();
  fd_warn(_("Swapped out everything; memory usage = %d bytes, limit = %d bytes"),
     memory_base,memory_base+
     (FD_FIXLISP(FD_SYMBOL_VALUE(memory_headroom_symbol))));
  fd_report_framerd_stats(stderr);
}

/** Validating clients **/

static fd_lisp client_okp_symbol, maintainer_okp_symbol;

static int validate_client(char *hostname)
{
  fd_lisp method=fd_symeval(client_okp_symbol,server_env);
  if (!((FD_EMPTYP(method)) || (FD_VOIDP(method)))) {
    fd_lisp args=FD_MAKE_LIST1(fd_make_string(hostname));
    fd_lisp answer=fd_apply(method,args);
    fd_decref(method); fd_decref(args);
    if (FD_FALSEP(answer)) return 0; else {
      fd_decref(answer); return 1;}}
  else return 1;
}

static int validate_maintainer(char *hostname)
{
  fd_lisp method=fd_symeval(maintainer_okp_symbol,server_env);
  if (!((FD_EMPTYP(method)) || (FD_VOIDP(method)))) {
    fd_lisp args=FD_MAKE_LIST1(fd_make_string(hostname));
    fd_lisp answer=fd_apply(method,args);
    fd_decref(method); fd_decref(args);
    if (FD_FALSEP(answer)) return 0; else {
      fd_decref(answer); return 1;}}
  else return 0;
}

static fd_lisp maintainer_eval(fd_lisp expr)
{
  fd_client cl=fd_current_client();
  if ((cl) && (validate_maintainer(cl->id)))
    return fd_eval_in_env(expr,server_env);
  else return fd_make_error
	 (fd_make_string("Maintenance not allowed from client"));
}

static fd_lisp client_id_cproc()
{
  fd_client cl=fd_current_client();
  return fd_make_string(cl->id);
}

FASTOP fd_lisp arg_eval(fd_lisp expr,int i,fd_lispenv env)
{
  fd_lisp arg=fd_get_arg(expr,i,FD_VOID);
  if (FD_PAIRP(arg))
    if (FD_LISP_EQ(FD_CAR(arg),quote_symbol))
      if (FD_PAIRP(FD_CDR(arg)))
	return fd_incref(FD_CAR(FD_CDR(arg)));
      else fd_raise_exception(fd_SyntaxError);
    else return fd_eval_in_env(expr,env);
  else return fd_incref(arg);
}

static fd_lisp exit_server_cproc()
{
  fd_exit(0);
  return FD_VOID;
}

static fd_lisp abort_server_cproc()
{
  abort();
  return FD_VOID;
}

/** Serving super pools **/

static char *super_pool_name=NULL;

static fd_lisp allocate_pool_proc(fd_lisp size,fd_lisp id)
{
  int i=0, cap=1, sz=FD_FIXLISP(size);
  while ((i < 32) && (cap != sz)) {cap=cap*2; i++;}
  if (cap == sz) {
    fd_u8char *sid=((FD_STRINGP(id)) ? (FD_STRING_DATA(id)) :
		 (fd_object_to_string(id)));
    FD_OID x=fd_allocate_pool(super_pool_name,cap,sid);
    if (!FD_STRINGP(id)) fd_xfree(sid);
    fprintf(stderr,_("New pool with %d (0x%x) objects starting at @%x/%x\n"),
	    cap,cap,FD_OID_HIGH(x),FD_OID_LOW(x));
    fprintf(stderr,_("   allocated for %s\n"),FD_STRING_DATA(id));
    return fd_make_oid(x);}
  else return FD_EMPTY_CHOICE;
}

static fd_lisp recovered_pool_proc(fd_lisp base,fd_lisp size,fd_lisp id)
{
  if (!(FD_OIDP(base))) {
    FD_OID pbase=fd_recovered_pool
      (super_pool_name,FD_OID_ADDR(base),fd_lisp2int(size),id);
    return fd_make_oid(pbase);}
  else fd_type_error(_("base not an oid"),base);
}

static fd_lisp sp_base()
{
  FD_OID base=fd_super_pool_base(super_pool_name);
  return fd_make_oid(base);
}

static fd_lisp sp_top()
{
  FD_OID top=fd_super_pool_top(super_pool_name);
  return fd_make_oid(top);
}

static fd_lisp sp_loading()
{
  return FD_LISPFLOAT(fd_super_pool_loading(super_pool_name));
}

static fd_lisp serve_super_pool_proc(fd_lisp filename)
{
  fd_u8char *fname=fd_strdata(filename);
  FILE *f=fd_fopen(fname,"rb"); unsigned int code;
  if (f) code=fd_fread_4bytes(f);
  else fd_raise_detailed_exception(fd_FileOpenFailed,fname);
  if (code == FD_REGISTERED_SUPER_POOL_MAGIC_NUMBER) {
    fd_fclose(f); super_pool_name=fd_strdup(fname);}
  else if (code == FD_SUPER_POOL_MAGIC_NUMBER) {
    fd_fclose(f);
    fd_raise_detailed_exception
      ("Can't serve unregistered super pool",fname);}
  else {
    fd_fclose(f);
    fd_raise_detailed_exception(fd_NotASuperPool,fname);}
  fd_add_cproc(NULL,"ALLOCATE-POOL",2,allocate_pool_proc);
  fd_add_cproc(NULL,"RECOVERED-POOL",3,recovered_pool_proc);
  fd_add_cproc(NULL,"SP-LOADING",0,sp_loading);
  fd_add_cproc(NULL,"SP-BASE",0,sp_base);
  fd_add_cproc(NULL,"SP-TOP",0,sp_top);
  return FD_TRUE;
}

/** Oid locking **/

static struct FD_HASHTABLE oid_locks, oid_locks_inv;
static unsigned int n_locks=0;
static unsigned int locking=1; 
static FILE *locks_file=NULL;
static char *locks_filename=NULL;
static int update_oid_lock_file();

static int pool_writablep(fd_pool p)
{
  if (p->read_only == 0) return 1;
  else if (p->type == file_pool)
    if (fd_lock_file_pool((fd_file_pool)p))
      return 1;
    else return 0;
  else return 0;
}

static int lock_oid(fd_lisp oid,fd_lisp id)
{
  fd_lisp holder;
  if (locking == 0) return 1;
  fd_lock_mutex(&oid_locks_lock);
  holder=fd_hashtable_get(&oid_locks,oid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(holder)) {
    fd_pool p=fd_get_pool(oid);
    if ((pool_writablep(p)) == 0) {
      fd_unlock_mutex(&oid_locks_lock); return 0;}
    fd_hashtable_set(&oid_locks,oid,id); n_locks++;
    fd_hashtable_add(&oid_locks_inv,id,oid); 
    if (locks_file) {
      fd_fwrite_dtype(oid,locks_file);
      fd_fwrite_dtype(id,locks_file);
      fflush(locks_file);}
    fd_unlock_mutex(&oid_locks_lock);
    return 1;}
  else if (FD_LISP_EQUAL(id,holder)) {
    fd_unlock_mutex(&oid_locks_lock); fd_decref(holder);
    return 1;}
  else {
    fd_unlock_mutex(&oid_locks_lock);
    fd_decref(holder);
    return 0;}
}

static int check_oid_lock(fd_lisp oid,fd_lisp id)
{
  fd_lisp holder=fd_hashtable_get(&oid_locks,oid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(holder)) return 0;
  else if (FD_LISP_EQUAL(id,holder)) {
    fd_decref(holder); return 1;}
  else {fd_decref(holder); return 0;}
}

static int clear_oid_lock(fd_lisp oid,fd_lisp id)
{
  fd_lisp holder;
  if (locking == 0) return 1;
  fd_lock_mutex(&oid_locks_lock);
  holder=fd_hashtable_get(&oid_locks,oid,FD_EMPTY_CHOICE);
  if (FD_EMPTYP(holder)) {fd_unlock_mutex(&oid_locks_lock); return 0;}
  else if (FD_LISP_EQUAL(id,holder)) {
    fd_lisp all_locks=fd_hashtable_get(&oid_locks_inv,id,FD_EMPTY_CHOICE);
    int lock_count=FD_CHOICE_SIZE(all_locks);
    fd_decref(holder);
    fd_hashtable_set(&oid_locks,oid,FD_EMPTY_CHOICE);
    if (lock_count == 0) 
      fd_raise_lisp_exception(OIDNotLocked,fd_strdata(id),oid);
    else if (lock_count == 1)
      fd_hashtable_set(&oid_locks_inv,id,FD_EMPTY_CHOICE);
    else {
       fd_lisp v=fd_remove_from_choice(oid,all_locks);
       fd_hashtable_set(&oid_locks_inv,id,v);
       fd_decref(v);}
    fd_hashtable_zap(&oid_locks,oid); n_locks--;
    if (locks_file) {
      fd_fwrite_dtype(id,locks_file); fd_fwrite_dtype(oid,locks_file);
      fflush(locks_file);}
    fd_unlock_mutex(&oid_locks_lock);
    return 1;}
  else {
    fd_decref(holder);
    fd_unlock_mutex(&oid_locks_lock);
    return 0;}
}

static void remove_all_oid_locks(fd_lisp id)
{
  if (locking == 0) return;
  fd_lock_mutex(&oid_locks_lock);
  {
    fd_lisp locks=fd_hashtable_get(&oid_locks_inv,id,FD_EMPTY_CHOICE);
    FD_DO_CHOICES(oid,locks) {
      fd_hashtable_zap(&oid_locks,oid);}
    END_FD_DO_CHOICES;
    fd_decref(locks);
    fd_hashtable_set(&oid_locks_inv,id,FD_EMPTY_CHOICE);
    fd_unlock_mutex(&oid_locks_lock);
  }
}

static void open_oid_lock_stream(char *file)
{
  FILE *in=fd_fopen(file,"rb");
  fd_pair *scan, *limit;
  locks_filename=fd_strdup(file);
  if (in) {
    fd_lisp a, b; int c=getc(in);
    while (c != EOF) {
      ungetc(c,in); a=fd_fread_dtype(in), b=fd_fread_dtype(in);
      if (FD_OIDP(a)) lock_oid(a,b); else clear_oid_lock(b,a);
      c=getc(in);}
    fd_fclose(in); remove(file);}
  else {FD_CLEAR_ERR();}
  locks_file=fd_fopen(file,"wb");
  scan=oid_locks.table; limit=scan+oid_locks.n_slots;
  while (scan < limit)
    if (*scan) {
      fd_pair p=*scan++;
      if (!(FD_EMPTYP(p->cdr))) {
	fd_fwrite_dtype(p->car,locks_file);
	fd_fwrite_dtype(p->cdr,locks_file);}}
    else scan++;
  fflush(locks_file);
}

/* This writes out the current state of locks in memory to an external file.
   The locks file is appended to while the server is running; this means that
   it may contain OIDs which have been unlocked.  update_oid_lock_file updates
   the file from memory, making it only include the OIDs which are currently
   locked.  */
static int update_oid_lock_file()
{
  fd_pair *scan, *limit; int n_locks=0;
  if (locks_filename == NULL) return 0;
  fd_lock_mutex(&oid_locks_lock);
  if (locks_file) fclose(locks_file);
  remove(locks_filename);
  locks_file=fd_fopen(locks_filename,"wb");
  scan=oid_locks.table; limit=scan+oid_locks.n_slots;
  while (scan < limit)
    if (*scan) {
      fd_pair p=*scan++;
      if (!(FD_EMPTYP(p->cdr))) {
	n_locks++;
	fd_fwrite_dtype(p->car,locks_file);
	fd_fwrite_dtype(p->cdr,locks_file);}}
    else scan++;
  fflush(locks_file);
  fd_unlock_mutex(&oid_locks_lock);
  return n_locks;
}

/** OID Access API **/

static fd_lisp lock_oid_handler(fd_lisp expr,fd_lispenv env)
{
  fd_lisp oid=arg_eval(expr,1,env);
  fd_lisp id=arg_eval(expr,2,env);
  fd_lisp result=FD_VOID;
  if (locking == 0) result=fd_oid_value(oid);
  else if (lock_oid(oid,id)) {
    fd_lisp sid=fd_symeval(sid_symbol,env);
    if (FD_VOIDP(sid)) {
      fd_bind_value(sid_symbol,id,env);}
    else fd_decref(sid);
    result=fd_incref(fd_oid_value(oid));}
  fd_decref(oid); fd_decref(id);
  return result;
}

static fd_lisp unlock_oid_lexpr(fd_lisp args)
{
  if (locking == 0) {
    fd_lisp oid=fd_get_arg(args,0,FD_VOID);
    fd_lisp value=fd_get_arg(args,2,FD_VOID);
    fd_pool p=fd_get_pool(oid);
    fd_set_oid_value(oid,value);
    add_to_changelog(&oid_changelog,oid);
    fd_commit_pool(p);
    return FD_TRUE;}
  else {
    fd_lisp oid=fd_get_arg(args,0,FD_VOID);
    fd_lisp id=fd_get_arg(args,1,FD_VOID);
    fd_lisp value=fd_get_arg(args,2,FD_VOID);
    fd_pool p=fd_get_pool(oid);
    if (check_oid_lock(oid,id)) {
      fd_set_oid_value(oid,value);
      clear_oid_lock(oid,id);
      add_to_changelog(&oid_changelog,oid);
      fd_commit_pool(p);
      return FD_TRUE;}
    else return FD_FALSE;}
}

static fd_lisp clear_oid_lock_cproc(fd_lisp oid,fd_lisp id)
{
  if (locking == 0) return FD_TRUE;
  else if (clear_oid_lock(oid,id)) return FD_TRUE;
  else return FD_FALSE;
}

static fd_lisp break_oid_lock_cproc(fd_lisp oid)
{
  if (locking == 0) return FD_TRUE;
  else {
    fd_lisp id=fd_hashtable_get(&oid_locks,oid,FD_EMPTY_CHOICE);
    if (FD_EMPTYP(id)) return FD_FALSE;
    else {
      clear_oid_lock(oid,id);
      fd_decref(id);
      return FD_TRUE;}}
}

static fd_lisp unlock_all_cproc(fd_lisp id)
{
  remove_all_oid_locks(id);
  update_oid_lock_file();
  return FD_TRUE;
}

static fd_lisp update_locks_cproc()
{
  update_oid_lock_file();
  return FD_VOID;
}

static fd_lisp lisp_set_sloppy_locking()
{
  locking=0;
  return FD_TRUE;
}


/** Other API **/

static fd_lisp lisp_force_serialization()
{
  force_serialization=1;
  return FD_TRUE;
}

static fd_lisp lisp_set_read_only()
{
  read_only=1; locking=0;
  return FD_TRUE;
}

static fd_lisp lisp_set_port_id(fd_lisp arg)
{
  if (server_started)
    fd_raise_exception(_("Too late to change port id"));
  if (port_id) fd_xfree(port_id);
  port_id=fd_strdup(fd_strdata(arg));
  return FD_VOID;
}

static fd_lisp lisp_use_localhost()
{
  fd_use_localhost();
  using_localhost=1;
  return FD_TRUE;
}

static fd_lisp lisp_setuid_cproc(fd_lisp arg)
{
  fd_set_uid(fd_strdata(arg));
  return FD_TRUE;
}

static fd_lisp lisp_setlog_cproc(fd_lisp clog,fd_lisp elog)
{
  FILE *cfile=NULL, *efile=NULL;
  if (FD_STRINGP(clog)) cfile=fd_fopen(fd_strdata(clog),"a");
  else if (FD_TRUEP(clog)) cfile=stderr;
  if (FD_STRINGP(elog)) efile=fd_fopen(fd_strdata(elog),"a");
  else if (FD_TRUEP(elog)) efile=stderr;
  fd_set_server_log(cfile,efile);
  return FD_TRUE;
}

static fd_lisp get_total_transactions()
{
  int count;
  fd_lock_mutex(&eval_lock);
  count=total_transactions;
  fd_unlock_mutex(&eval_lock);
  return FD_LISPFIX(count);
}

/* Client data */

typedef struct CLIENT_DATA {
  fd_lispenv env;
  struct FD_STRING_STREAM xio;
  int jsync;} *client_data;

static void free_client_data(fd_client cl)
{
  struct CLIENT_DATA *cdata=(struct CLIENT_DATA *)cl->data;
  fd_lispenv env=cdata->env;
  if (locking) {
    fd_lisp sid=fd_symeval(sid_symbol,env);
    if (FD_STRINGP(sid)) remove_all_oid_locks(sid);
    fd_decref(sid);}
  fd_xfree(cdata->xio.ptr);
  fd_free_env(cdata->env);
  fd_free(cdata,sizeof(struct CLIENT_DATA));
  cl->data=NULL; cl->close_fcn=NULL;
}

static client_data init_client_data(fd_client cl)
{
  if (cl->data) return (client_data) cl->data;
  else {
    client_data cdata=fd_malloc(sizeof(struct CLIENT_DATA));
    fd_lispenv env=cdata->env=fd_make_module();
    fd_module_uses(env,prims_env);
    fd_module_uses(env,server_env);
    fd_module_uses(env,fd_texttools_env);
    FD_INITIALIZE_STRING_STREAM(&(cdata->xio),256);
    cdata->jsync=0; 
    cl->data=(void *)cdata;
    cl->close_fcn=free_client_data;
    return cdata;}
}

/* Journals and syncing */

#define JSYNC_TIME_INTERVAL 60 /* How often, in seconds, to sync the journal file */
#define JSYNC_TRANSACTION_INTERVAL 4096 /* How often, in transactions, to sync the journal file */

static time_t jsync_time;
static int transactions_since_jsync=0,
           jsync_transaction_interval=JSYNC_TRANSACTION_INTERVAL,
           jsync_time_interval=JSYNC_TIME_INTERVAL;

static void fdserver_jsync(int need_lock)
{
  if ((need_lock) && (journal_file)) fd_lock_mutex(&journal_lock);
  fd_commit_pools();
  fd_commit_indices();
  update_oid_lock_file();
#if (WIN32)
  if (journal_file) fseek(journal_file,0,SEEK_SET);
#else
  if (journal_file) ftruncate(fileno(journal_file),0);
#endif
  jsync_time=time(NULL)+jsync_time_interval;
  transactions_since_jsync=0;
  if ((need_lock) && (journal_file)) fd_unlock_mutex(&journal_lock);
}

static void journal_transaction(fd_lisp expr,fd_lisp result,fd_client cl)
{
  fd_lock_mutex(&journal_lock);
  fd_fwrite_dtype(cl->label,journal_file);
  fd_fwrite_dtype(expr,journal_file);
  fd_fwrite_dtype(result,journal_file);
  fflush(journal_file); transactions_since_jsync++;
  if (transactions_since_jsync)
    if ((transactions_since_jsync>jsync_transaction_interval) ||
	(time(NULL) > jsync_time)) 
      fdserver_jsync(0);
  fd_unlock_mutex(&journal_lock);
}

static void operation_needs_jsync()
{
  fd_client cl=(struct FD_CLIENT *)fd_current_client();
  client_data cdata=(struct CLIENT_DATA *)cl->data;
  cdata->jsync=1;
}

/** eval_server_fcn **/

static fd_lisp eval_server_fcn(fd_lisp expr,int *normal_return,fd_client cl)
{
  struct CLIENT_DATA *cdata;
  struct FD_STRING_STREAM *ss;
  struct FD_LISPENV *env;
  fd_lisp result=FD_VOID; *normal_return=1;
  fd_lock_mutex(&eval_lock);
  total_transactions++;
  if (SWAP_OUTP()) swap_everything_out();
  if (force_serialization == 0) fd_unlock_mutex(&eval_lock);
  {FD_WITH_HANDLING {
      if (cl->data) cdata=(client_data)cl->data;
      else cdata=init_client_data(cl);
      env=cdata->env; ss=&(cdata->xio); cdata->jsync=0;
      result=fd_eval_in_env(expr,env);
      if (cdata->jsync)
	if (journal_file) journal_transaction(expr,result,cl);
	else fdserver_jsync(1);}
    FD_ON_EXCEPTION {
      fd_lisp report;
      if (ss->size == 0)
	report=FD_MAKE_LIST
	  (3,fd_copy_string(fd_theException()),
	   fd_copy_string(fd_exception_details()),
	   fd_incref(fd_exception_object()));
      else 
	report=FD_MAKE_LIST
	  (4,fd_copy_string(fd_theException()),
	   fd_copy_string(fd_exception_details()),
	   fd_incref(fd_exception_object()),
	   fd_copy_string(ss->ptr));
      ss->size=0; ss->ptr[0]='\0';
      result=fd_make_error(report);
      fd_clear_exception();}
    FD_END_HANDLING;}
  fd_direct_xio(NULL,NULL,NULL);
  if (ss->size) {
    fprintf(stderr,"%s",ss->ptr); ss->size=0; ss->ptr[0]='\0';}
  if (force_serialization) fd_unlock_mutex(&eval_lock);
  return result;
}

/** Serving pools **/

static void serve_pool(fd_pool p)
{
  int i=0;
  if (n_served_pools == 0) primary_pool=p;
  i=0; while (i < n_served_pools) {
    if (served_pools[i] == p) {
      fd_warn(_("The pool %s is already being served"),p->id);
      return;}
    else i++;}
  if (n_served_pools >= 32)
    fd_raise_detailed_exception(_("Too many served pools"),p->id);
  fd_warn("Serving pool %s",p->id);
  served_pools[n_served_pools++]=p;
  if ((p->type == file_pool) &&
      (fd_file_writablep(((fd_file_pool)p)->filename)))
    fd_lock_file_pool((fd_file_pool)p);
}

static fd_lisp serve_pool_cproc(fd_lisp pl)
{
  fd_pool p=fd_interpret_pool(pl); serve_pool(p); 
  return fd_make_cptr(pool_type,p);
}

static void accumulate_pool(char *spec)
{
  fd_pool p=fd_use_pool(spec); serve_pool(p);
}

static fd_lisp make_pool_data(fd_pool p)
{
  fd_lisp expr=FD_EMPTY_LIST;
  expr=FD_MAKE_PAIR(fd_incref(p->label),expr);
  if (read_only) expr=FD_MAKE_PAIR(FD_TRUE, expr);
  else expr=FD_MAKE_PAIR(FD_FALSE, expr);
  expr=FD_MAKE_PAIR(FD_LISPFIX(p->capacity), expr);
  expr=FD_MAKE_PAIR(fd_make_oid(p->base),expr);
  return expr;
}

/** OID serving API **/

static fd_lisp new_oid_proc()
{
  fd_lisp obj=fd_new_oid(primary_pool);
  operation_needs_jsync();
  return obj;
}

static fd_lisp new_frame_proc()
{
  fd_lisp obj=fd_frame_create(primary_pool);
  operation_needs_jsync();
  /* fd_commit_pool(primary_pool); */
  return obj;
}

static fd_lisp server_oid_value(fd_lisp oid)
{
  fd_pool p=fd_get_pool(oid);
  int i=0; while (i < n_served_pools)
    if (served_pools[i] == p) return fd_oid_value(oid);
    else i++;
  return FD_VOID;
}

static fd_lisp fetch_oids_cproc(fd_lisp oidvec)
{
  fd_lisp answer; int i, size;
  if (FD_VECTORP(oidvec)) fd_prefetch_oids(oidvec);
  else fd_type_error(_("not a vector"),oidvec);
  size=FD_VECTOR_LENGTH(oidvec);
  answer=fd_make_vector(size);
  i=0; while (i < size) {
    fd_lisp oid=FD_VECTOR_REF(oidvec,i);
    if (FD_OIDP(oid)) {
      fd_lisp value=fd_oid_value(oid);
      FD_VECTOR_SET(answer,i,value);
      i++;}
    else {
      FD_VECTOR_SET(answer,i,FD_VOID);
      i++;}}
  return answer;
}

static fd_lisp store_oid_proc(fd_lisp oid,fd_lisp value)
{
  fd_pool p=fd_get_pool(oid);
  int i=0; while (i < n_served_pools)
    if (served_pools[i] == p) {
      fd_set_oid_value(oid,value);
      add_to_changelog(&oid_changelog,oid);
      return FD_TRUE;}
    else i++;
  operation_needs_jsync();
  return FD_FALSE;
}

static fd_lisp bulk_commit_cproc(fd_lisp id,fd_lisp vec)
{
  int i=0, l=FD_VECTOR_LENGTH(vec); fd_lisp changed_oids=FD_EMPTY_CHOICE;
  if (locking) {
    i=0; while (i < l) {
      fd_lisp oid=FD_VECTOR_REF(vec,i);
      if (!(FD_OIDP(oid))) i=i+2;
      else if (check_oid_lock(oid,id)) i=i+2;
      else fd_raise_exception(OIDNotLocked);}}
  i=0; while (i < l) {
    fd_lisp oid=FD_VECTOR_REF(vec,i);
    fd_lisp value=FD_VECTOR_REF(vec,i+1);
    if (FD_OIDP(oid)) {
      fd_set_oid_value(oid,value);
      FD_ADD_TO_CHOICE(changed_oids,oid);}
    i=i+2;}
  if (locking) {
    i=0; while (i < l) {
      fd_lisp oid=FD_VECTOR_REF(vec,i);
      if (FD_OIDP(oid)) clear_oid_lock(oid,id);
      i=i+2;}}
  add_to_changelog(&oid_changelog,changed_oids); fd_decref(changed_oids);
  operation_needs_jsync();
  return FD_TRUE;
}

static fd_lisp flip_oid(fd_lisp oid)
{
  FD_OID id;
  FD_SET_OID_HIGH(id,FD_OID_ADDR_HIGH(oid));
  FD_SET_OID_LOW(id,FD_OID_ADDR_LOW(oid));
  return fd_make_oid(id);
}

static fd_lisp pool_data_handler(fd_lisp req_expr,lispenv env)
{
  fd_lisp client_label=fd_get_arg(req_expr,1,FD_EMPTY_CHOICE);
  if (!(FD_EMPTYP(client_label))) fd_label_client(client_label);
  if (n_served_pools == 0) return FD_EMPTY_CHOICE;
  else if (n_served_pools == 1) return make_pool_data(served_pools[0]);
  else {
    fd_lisp vec=fd_make_vector(n_served_pools);
    int i=0; while (i < n_served_pools) {
      fd_lisp data=make_pool_data(served_pools[i]);
      FD_VECTOR_SET(vec,i,data); i++;}
    return vec;}
}

static fd_lisp oid_server_changes(fd_lisp sid,fd_lisp xid)
{
  if (FD_FIXLISP(sid) != server_start) return FD_FALSE;
  else {
    int new_syncstamp;
    fd_lisp changes=get_changes(&oid_changelog,FD_FIXLISP(xid),&new_syncstamp);
    if (FD_FALSEP(changes))
      return FD_MAKE_LIST1(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)));
    else return FD_MAKE_PAIR(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)),
			     changes);}
}

static fd_lisp get_load(fd_lisp args)
{
  fd_lisp base=fd_get_arg(args,0,FD_FALSE); fd_pool p=NULL;
  if (FD_FALSEP(base))
    if (primary_pool) p=primary_pool;
    else return FD_VOID;
  else {
    FD_OID id=FD_OID_ADDR(base);
    int i=0; while (i < n_served_pools)
      if ((FD_OID_HIGH(served_pools[i]->base) == FD_OID_HIGH(id)) &&
	  (FD_OID_LOW(served_pools[i]->base) == FD_OID_LOW(id)))
	{p=served_pools[i]; break;}
      else i++;}
  if (p == NULL) return FD_VOID;
  if (p->type == file_pool)
    return FD_LISPFIX(((fd_file_pool)p)->load);
  else if (p->type == network_pool)
    return fd_dtcall
      ((((fd_network_pool)p)->conn),"GET-LOAD",base);
  else fd_raise_exception("Corrupted pool structure");
}

/** Serving indices **/

static void serve_index(fd_index ix)
{
  int i=0;
  if ((n_served_indices == 0) && (read_only == 0))
    if (ix->read_only == 0) index_read_only=0;
  while (i < n_served_indices) {
    if (served_indices[i] == ix) {
      fd_warn(_("The index %s is already being served"),ix->id);
      return;}
    else i++;}
  if (n_served_indices >= 64)
    fd_raise_exception(_("Too many indices to serve"));
  fd_warn("Serving index %s",ix->id);
  served_indices[n_served_indices++]=ix;  
}

static fd_lisp serve_index_cproc(fd_lisp arg)
{
  fd_index new=fd_interpret_index(arg);
  serve_index(new);
  return fd_make_cptr(index_type,new);
}

static void accumulate_index(char *id)
{
  fd_index new=fd_open_index(id);
  serve_index(new);
}

/** Index serving API **/

static fd_index get_indexptr(fd_lisp x)
{
  if (FD_PRIM_TYPEP(x,index_type))
    return (fd_index) FD_CPTR_DATA(x);
  else fd_type_error("not an index",x);
}

static fd_lisp iserver_get(fd_lisp key)
{
  fd_lisp answer=FD_EMPTY_CHOICE; int i=0;
  while (i < n_served_indices) {
    fd_lisp v=fd_incref(fd_index_get(served_indices[i],key,FD_EMPTY_CHOICE));
    FD_ADD_TO_CHOICE(answer,v); i++;}
  return answer;
}
static fd_lisp ixserver_get(fd_lisp index,fd_lisp key)
{
  return fd_index_get(get_indexptr(index),key,FD_EMPTY_CHOICE);
}

static fd_lisp iserver_bulk_get(fd_lisp keys)
{
  int icount=0, ilimit=n_served_indices;
  fd_lisp keys_as_choice=(FD_EMPTY_CHOICE);
  fd_lisp answer=fd_make_vector(FD_VECTOR_LENGTH(keys));
  int i=0, limit=FD_VECTOR_LENGTH(keys); while (i < limit)
    if (FD_VOIDP(FD_VECTOR_REF(keys,i))) {
      FD_VECTOR_SET(answer,i,FD_EMPTY_CHOICE); i++;}
    else {
      fd_lisp key=FD_VECTOR_REF(keys,i);
      FD_ADD_TO_CHOICE(keys_as_choice,fd_incref(key));
      FD_VECTOR_SET(answer,i,FD_EMPTY_CHOICE); i++;}
  icount=0; while (icount < ilimit) {
    fd_index_prefetch(served_indices[icount],keys_as_choice); icount++;}
  icount=0; while (icount < ilimit) {
    fd_lisp *kelts=FD_VECTOR_ELEMENTS(keys);
    fd_lisp *velts=FD_VECTOR_ELEMENTS(answer);
    i=0; while (i < limit)
      if (FD_VOIDP(kelts[i])) i++;
      else {
	fd_lisp v=
	  fd_index_get(served_indices[icount],kelts[i],FD_EMPTY_CHOICE);
	FD_ADD_TO_CHOICE(velts[i],v); i++;}
    icount++;}
  fd_decref(keys_as_choice); 
  return answer;
}

static fd_lisp ixserver_bulk_get(fd_lisp index,fd_lisp keys)
{
  int icount=0, ilimit=n_served_indices;
  fd_lisp keys_as_choice=(FD_EMPTY_CHOICE);
  fd_lisp answer=fd_make_vector(FD_VECTOR_LENGTH(keys));
  fd_index ix=get_indexptr(index);
  int i=0, limit=FD_VECTOR_LENGTH(keys); while (i < limit)
    if (FD_VOIDP(FD_VECTOR_REF(keys,i))) {
      FD_VECTOR_SET(answer,i,FD_EMPTY_CHOICE); i++;}
    else {
      fd_lisp key=FD_VECTOR_REF(keys,i);
      FD_ADD_TO_CHOICE(keys_as_choice,fd_incref(key));
      FD_VECTOR_SET(answer,i,FD_EMPTY_CHOICE); i++;}
  icount=0; while (icount < ilimit) {
    fd_index_prefetch(ix,keys_as_choice); icount++;}
  icount=0; while (icount < ilimit) {
    fd_lisp *kelts=FD_VECTOR_ELEMENTS(keys);
    fd_lisp *velts=FD_VECTOR_ELEMENTS(answer);
    i=0; while (i < limit)
      if (FD_VOIDP(kelts[i])) i++;
      else {velts[i]=fd_index_get(ix,kelts[i],FD_EMPTY_CHOICE); i++;}
    icount++;}
  fd_decref(keys_as_choice); 
  return answer;
}

static fd_lisp iserver_keys()
{
  fd_lisp answer=FD_EMPTY_CHOICE; int i=0;
  while (i < n_served_indices) {
    fd_lisp v=fd_index_keys(served_indices[i]);
    FD_ADD_TO_CHOICE(answer,v); i++;}
  return answer;
}

static fd_lisp iserver_changes(fd_lisp sid,fd_lisp xid)
{
  if (FD_FIXLISP(sid) != server_start) return FD_FALSE;
  else {
    int new_syncstamp;
    fd_lisp changes=get_changes(&index_changelog,FD_FIXLISP(xid),&new_syncstamp);
    if (FD_FALSEP(changes))
      return FD_MAKE_LIST1(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)));
    else return FD_MAKE_PAIR(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)),
			     changes);}
}

static fd_lisp ixserver_keys(fd_lisp index)
{
  return fd_index_keys(get_indexptr(index));
}

static fd_lisp ixserver_changes(fd_lisp index,fd_lisp sid,fd_lisp xid)
{
  fd_index ix=get_indexptr(index);
  struct FD_CHANGELOG *clog=get_subindex_changelog(ix,0);
  if (clog == NULL) return FD_EMPTY_CHOICE;
  else if (FD_FIXLISP(sid) != server_start) return FD_FALSE;
  else {
    int new_syncstamp;
    fd_lisp changes=get_changes(clog,FD_FIXLISP(xid),&new_syncstamp);
    if (FD_FALSEP(changes))
      return FD_MAKE_LIST1(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)));
    else return FD_MAKE_PAIR(FD_MAKE_LIST(2,FD_LISPFIX(server_start),FD_LISPFIX(new_syncstamp)),
			     changes);}
}

static fd_lisp iserver_writablep()
{
  if ((read_only) || (index_read_only))
    return FD_FALSE;
  else return FD_TRUE;
}

static fd_lisp ixserver_writablep(fd_lisp index)
{
  if ((read_only) || (index_read_only))
    return FD_FALSE;
  else if (get_indexptr(index)->read_only) return FD_FALSE;
  else return FD_TRUE;
  
}

static fd_lisp iserver_get_size(fd_lisp key)
{
  int size=0, i=0;
  while (i < n_served_indices) {
    size=size+fd_index_get_size(served_indices[i],key); i++;}
  return FD_LISPFIX(size);
}

static fd_lisp ixserver_get_size(fd_lisp index,fd_lisp key)
{
  int size=fd_index_get_size(get_indexptr(index),key);
  return FD_LISPFIX(size);
}

static fd_lisp iserver_add_lexpr(fd_lisp args)
{
  fd_lisp key=fd_get_arg(args,0,FD_VOID);
  fd_lisp values=fd_get_arg(args,1,FD_VOID);
  fd_index_add(served_indices[0],key,values);
  add_to_changelog(&index_changelog,key);
  operation_needs_jsync();
  return FD_TRUE;
}

static fd_lisp ixserver_add_lexpr(fd_lisp args)
{
  fd_lisp index=fd_get_arg(args,0,FD_VOID);
  fd_lisp key=fd_get_arg(args,1,FD_VOID);
  fd_lisp values=fd_get_arg(args,2,FD_VOID);
  fd_index ix=get_indexptr(index);
  struct FD_CHANGELOG *clog=get_subindex_changelog(ix,1);
  fd_index_add(ix,key,values);
  add_to_changelog(clog,key);
  operation_needs_jsync();
  return FD_TRUE;
}

static fd_lisp iserver_bulk_add(fd_lisp vec)
{
  if (read_only || (n_served_indices == 0)) return FD_FALSE;
  else if (FD_VECTORP(vec)) {
    fd_lisp *data=FD_VECTOR_ELEMENTS(vec), keys=FD_EMPTY_CHOICE;
    int i=0, limit=FD_VECTOR_LENGTH(vec);
    while (i < limit) {
      if (FD_VOIDP(data[i])) break;
      else {
	fd_index_add(served_indices[0],data[i],data[i+1]);
	FD_ADD_TO_CHOICE(keys,fd_incref(data[i]));
	i=i+2;}}
    operation_needs_jsync();
    add_to_changelog(&index_changelog,keys); fd_decref(keys);
    return FD_TRUE;}
  else return FD_VOID;
}

static fd_lisp ixserver_bulk_add(fd_lisp index,fd_lisp vec)
{
  if (read_only || (n_served_indices == 0)) return FD_FALSE;
  else if (FD_VECTORP(vec)) {
    fd_index ix=get_indexptr(index);
    struct FD_CHANGELOG *clog=get_subindex_changelog(ix,1);
    fd_lisp *data=FD_VECTOR_ELEMENTS(vec), keys=FD_EMPTY_CHOICE;
    int i=0, limit=FD_VECTOR_LENGTH(vec);
    while (i < limit) {
      if (FD_VOIDP(data[i])) break;
      else {
	fd_index_add(ix,data[i],data[i+1]);
	FD_ADD_TO_CHOICE(keys,fd_incref(data[i]));
	i=i+2;}}
    add_to_changelog(clog,keys); fd_decref(keys);
    operation_needs_jsync();
    return FD_TRUE;}
  else return FD_VOID;
}

static fd_lisp iserver_drop_lexpr(fd_lisp args)
{
  fd_lisp key=fd_get_arg(args,0,FD_VOID);
  fd_lisp drop=fd_get_arg(args,1,FD_VOID);
  fd_index_drop(served_indices[0],key,drop);
  add_to_changelog(&index_changelog,fd_incref(key));
  operation_needs_jsync();
  return FD_TRUE;
}

static fd_lisp ixserver_drop_lexpr(fd_lisp args)
{
  fd_lisp index=fd_get_arg(args,0,FD_VOID);
  fd_lisp key=fd_get_arg(args,1,FD_VOID);
  fd_lisp drop=fd_get_arg(args,2,FD_VOID);
  fd_index ix=get_indexptr(index);
  struct FD_CHANGELOG *clog=get_subindex_changelog(ix,1);
  fd_index_drop(ix,key,drop);
  add_to_changelog(clog,fd_incref(key));
  operation_needs_jsync();
  return FD_TRUE;
}

/*** Status functions ***/

const char * usage_msg =
"Usage: fdserver [options*] (<service_name> | <port_id> | <server_init_file>)\n"
" where options may define configuration variables, e.g.\n"
"    FDSERVER_USER=hal9k\n"
" or may be direct options for fdserver:\n"
"    --local                Run the server locally (e.g. as localhost)\n"
"    --log <filename>       Log connections and statistics to <filename>\n"
"                           (use '-' for stdout, '--' for stderr)\n"
"    --trace <filename>     Log transaction details to <filename>\n"
"                           (use '-' for stdout, '--' for stderr)\n"
"                           Evaluate <fdscript_code> before starting\n"
"    --access (read-only | locking | run-with-scissors)\n"
"                           Limit OID and association access according\n"
"                           to specified policy\n"
"    -m | --module <module>\n"
"                           Load the module <module> and use it from\n"
"                            the default environment\n"
"    -c | --config <filename>\n"
"                           Load configuration information from <filename>\n"
"    -f | --file <filename>\n"
"                           Load <filename> into the default environment\n"
"                           before reading the control file\n"
"    -e | --eval <expr>\n"
"                           Evaluate <expr> in the default environment\n"
"                           before reading the control file\n"
"\n"
"\n";

static void show_usage()
{
  fprintf(stderr, usage_msg);
}

static void write_pid_file(char *base,long pid)
{
  FILE *pidf; char buf[128];
  sprintf(buf,"%s.pid",base);
  pidf=fopen(buf,"w");
  if (pidf) {
    pid_file=fd_strdup(buf);
    fprintf(pidf,"%ld\n",pid);
    fclose(pidf);}
  else fd_warn(_("Can't write PID file"));
}

static void write_nid_file(char *base,char *nid)
{
  char buf[512]; FILE *nidf;
  sprintf(buf,"%s.nid",base);
  nidf=fopen(buf,"w");
  if (nidf) {
    fprintf(nidf,"%s\n",nid);
    nid_file=fd_strdup(buf);
    fclose(nidf);}
  else fd_warn(_("Can't write NID file"));
}

static void terminate_handler(int signal)
{
  fd_warn("FDServer %s terminating due to signal %d",
	  port_id,signal);
  fd_commit_pools(); fd_commit_indices();
  if (update_oid_lock_file() == 0) {
    if (locks_file) fd_fclose(locks_file);
    remove(locks_filename);}
  if (pid_file) remove(pid_file);
  if (nid_file) remove(nid_file);
  fd_exit(0);
}

static void process_config_assignment(char *start)
{
  fd_lisp symbol, value; int add=0;
  char *equals=strchr(start,'=');
  char *buf=fd_xmalloc(equals-start+1);
  strncpy(buf,start,equals-start); buf[equals-start]=0;
  symbol=fd_make_symbol(buf);
  if (equals[1]=='+') {add=1; equals++;}
  if ((equals[1]==' ') || (equals[1]=='\0')) value=FD_VOID;
  else value=fd_parse_arg(equals+1);
  if (add) {
    fd_lisp val=fd_symbol_value(symbol);
    if (FD_VOIDP(val))
      fd_set_symbol_value(symbol,value);
    else {
      FD_ADD_TO_CHOICE(val,fd_incref(value));
      fd_set_symbol_value(symbol,val);
      fd_decref(val);}}
  else fd_set_symbol_value(symbol,value);      
  fd_decref(value);
}

static int socket_failurep(fd_set sockets)
{
  int i=0; while (i < 30)
    if (FD_ISSET(i,&sockets)) return 0;
    else i++;
  return 1;
}

/** Server sockets **/

static fd_set server_sockets;

static void close_server_sockets()
{
  int i=0; while (i < 256)
    if (FD_ISSET(i,&server_sockets)) {close(i); i++;} else i++;
}

/** The main() event **/

int main(int argc,char *argv[])
{
  int i=1; char buf[64], port_buf[16], *nid, nid_buf[128];
  int no_init=1, uid_set=0;
  char *filebase;

  server_start=(int)time(NULL);

  if (argc < 2) {show_usage(); return 1;}

  /* Set mnemonic from argument */
  fd_set_session_mnemonic(argv[0]);

  fd_cmd_args(&argc,&argv);

  fd_set_build_date(__DATE__);
  fd_initialize_fdscript();

  init_changelog(&oid_changelog,1024);
  init_changelog(&index_changelog,1024);

  prims_env=fd_make_module();
  server_env=fd_make_module();

  fd_module_uses(server_env,fd_enabled_env);
  fd_module_uses(server_env,fd_osprims_env);
  fd_module_uses(server_env,fd_fdinternals_env);
  fd_module_uses(server_env,fd_module_table);

  /* Plugin inits */
  fd_initialize_fdtext();
  fd_initialize_fdwww();
  fd_module_uses(server_env,fd_texttools_env);
  fd_module_uses(server_env,prims_env);

  fd_initialize_server_c();
  /* Special inits */
  SPECIAL_INITS;

#if FD_THREADS_ENABLED
  fd_init_mutex(&eval_lock);
  fd_init_mutex(&oid_locks_lock);
  fd_init_mutex(&journal_lock);
#endif

  client_addr_symbol=fd_make_symbol("CLIENT-ADDR");
  locks_symbol=fd_make_symbol("%LOCKED-OIDS");
  sid_symbol=fd_make_symbol("%CLIENT-ID");
  label_symbol=fd_make_symbol("LABEL");
  quote_symbol=fd_make_symbol("QUOTE");
  client_okp_symbol=fd_make_symbol("CLIENT-OK?");
  maintainer_okp_symbol=fd_make_symbol("MAINTAINER-OK?");
  memory_headroom_symbol=fd_make_symbol("MEMORY-HEADROOM");

  fd_add_special_form(prims_env,"POOL-DATA",pool_data_handler);
  fd_add_cproc(prims_env,"GET-SYNCSTAMP",0,lisp_get_syncstamp_cproc);
  fd_add_lexpr(prims_env,"GET-LOAD",FD_NORMAL_LEXPR,get_load);
  fd_add_cproc(prims_env,"NEW-OID",0,new_oid_proc);
  fd_add_cproc(prims_env,"NEW-FRAME",0,new_frame_proc);
  fd_add_cproc(prims_env,"OID-VALUE",1,server_oid_value);
  fd_add_cproc(prims_env,"STORE-OID-VALUE!",2,store_oid_proc);

  fd_add_cproc(prims_env,"FLIP-OID",1,flip_oid);
  fd_add_cproc(prims_env,"FETCH-OIDS",1,fetch_oids_cproc);
  fd_add_cproc(prims_env,"BULK-COMMIT",2,bulk_commit_cproc);

  fd_add_special_form(prims_env,"LOCK-OID",lock_oid_handler);
  fd_add_lexpr(prims_env,"UNLOCK-OID",FD_ND_LEXPR,unlock_oid_lexpr);
  fd_add_cproc(prims_env,"CLEAR-OID-LOCK",2,clear_oid_lock_cproc);
  fd_add_cproc(prims_env,"BREAK-OID-LOCK",1,break_oid_lock_cproc);
  fd_add_cproc(prims_env,"UNLOCK-ALL",1,unlock_all_cproc);
  fd_add_cproc(prims_env,"OID-CHANGES",2,oid_server_changes);

  fd_add_cproc(prims_env,"ISERVER-GET",1,iserver_get);
  fd_add_cproc(prims_env,"ISERVER-BULK-GET",1,iserver_bulk_get);
  fd_add_cproc(prims_env,"ISERVER-GET-SIZE",1,iserver_get_size);
  fd_add_lexpr(prims_env,"ISERVER-ADD!",
	       FD_ND_LEXPR,iserver_add_lexpr);
  fd_add_lexpr(prims_env,"ISERVER-DROP!",
	       FD_ND_LEXPR,iserver_drop_lexpr);
  fd_add_cproc(prims_env,"ISERVER-BULK-ADD!",1,iserver_bulk_add);
  fd_add_cproc(prims_env,"ISERVER-WRITABLE?",0,iserver_writablep);
  fd_add_cproc(prims_env,"ISERVER-KEYS",0,iserver_keys);
  fd_add_cproc(prims_env,"ISERVER-CHANGES",2,iserver_changes);


  fd_add_cproc(prims_env,"IXSERVER-GET",2,ixserver_get);
  fd_add_cproc(prims_env,"IXSERVER-BULK-GET",2,ixserver_bulk_get);
  fd_add_cproc(prims_env,"IXSERVER-GET-SIZE",2,ixserver_get_size);
  fd_add_lexpr(prims_env,"IXSERVER-ADD!",FD_ND_LEXPR,ixserver_add_lexpr);
  fd_add_lexpr(prims_env,"IXSERVER-DROP!",FD_ND_LEXPR,ixserver_drop_lexpr);
  fd_add_cproc(prims_env,"IXSERVER-BULK-ADD!",2,ixserver_bulk_add);
  fd_add_cproc(prims_env,"IXSERVER-WRITABLE?",1,ixserver_writablep);
  fd_add_cproc(prims_env,"IXSERVER-KEYS",1,ixserver_keys);
  fd_add_cproc(prims_env,"IXSERVER-CHANGES",3,ixserver_changes);

  fd_add_cproc(prims_env,"LIST-CLIENTS",0,fd_list_clients);
  fd_add_cproc(prims_env,"TOTAL-TRANSACTIONS",0,get_total_transactions);

  fd_add_cproc(prims_env,"CLIENT-ID",0,client_id_cproc);
  fd_add_cproc(prims_env,"MAINTAINER",1,maintainer_eval);
  fd_add_restricted_cproc("SET-PORT-ID!",1,lisp_set_port_id);
  fd_add_restricted_cproc("FORCE-SERIALIZATION!",0,lisp_force_serialization);
  fd_add_restricted_cproc("SET-READ-ONLY!",0,lisp_set_read_only);
  fd_add_restricted_cproc("SET-SLOPPY!",0,lisp_set_sloppy_locking);
  fd_add_restricted_cproc("SET-UID!",1,lisp_setuid_cproc);
  fd_add_restricted_cproc("SET-LOG!",2,lisp_setlog_cproc);

  fd_add_restricted_cproc("SERVE-POOL",1,serve_pool_cproc);
  fd_add_restricted_cproc("SERVE-INDEX",1,serve_index_cproc);
  fd_add_restricted_cproc("USE-LOCALHOST!",0,lisp_use_localhost);

  fd_add_restricted_cproc("SERVE-SUPER-POOL",1,serve_super_pool_proc);
  fd_add_restricted_cproc("EXIT-SERVER",0,exit_server_cproc);
  fd_add_restricted_cproc("ABORT-SERVER",0,abort_server_cproc);

  fd_add_cproc(prims_env,"%UPDATE-LOCKFILE",0,update_locks_cproc);

  fd_set_client_validator(validate_client);
  
  fd_init_hashtable(&oid_locks,256);
  fd_init_hashtable(&oid_locks_inv,256);

#if (!(WIN32))
  if ((getuid() == 0)  && (uid_set == 0)) {
    char *user=fd_string_getenv("FDSERVER_USER");
    char *group=fd_string_getenv("FDSERVER_GROUP");
    if (user == NULL)
      if (((int)fd_get_uid("fdaemon"))>=0) user="fdaemon";
      else user="nobody";
    else if ((fd_get_uid(user))<0) user="nobody";
    if (group == NULL)
      if (((int)fd_get_gid("fdaemon"))>=0) group="fdaemon";
      else group="nogroup";
    else if ((fd_get_uid(group))<0) user="nogroup";
    fd_warn(_("Running as root, changing uid to %s, gid to %s"),user,group);
    fd_set_gid(group); fd_set_uid(user);}
#endif

  {
    /* There are three cases:
       a start (.fdz) file, a simple portid, and a port@host id. */
    char *s=argv[argc-1];
    char *suffix=strstr(s,".fdz"), *at=strchr(s,'@'); 
    if ((suffix) && (suffix[4]==NUL)) {
      char *scan; int len;
      filebase=fd_absolute_pathname(s); len=strlen(filebase);
      port_id=fd_basename(s,0);
      fd_load_file(filebase,NULL,server_env); no_init=0;
      scan=filebase+len-1;
      while ((scan>filebase) &&
	     (*scan != '.') && (*scan != '/') && (*scan != '\\'))
	scan--;
      if (*scan == '.') *scan=NUL;}
    else if (at) {
      char *tmp=fd_malloc(at-s+1), *dir;
      port_id=fd_strdup(s); strncpy(tmp,s,at-s); tmp[at-s]=NUL;
      filebase=tmp; no_init=1;}
    else {
      port_id=fd_strdup(s); filebase=fd_strdup(s); no_init=1;}}

  if (no_init) {
    sprintf(buf,"%s.pool",filebase);
    if (fd_file_existsp(buf)) {
      accumulate_pool(buf);}
    sprintf(buf,"%s.index",filebase);
    if (fd_file_existsp(buf)) {
      accumulate_index(buf);}
    sprintf(buf,"%s.fdx",filebase);
    if (fd_file_existsp(buf)) {
      fd_warn(_("Loading init file %s"),buf);
      fd_load_file(buf,NULL,server_env);}}
    
  if (argc == 1) {}
  else while (i < argc-1)
    if ((strchr(argv[i],'=')) && (argv[i][0] != '-'))
      process_config_assignment(argv[i++]);
    else if (strcmp(argv[i],"--local") == 0) {
      using_localhost=1; i++;
      fd_use_localhost();
      fd_warn(_("Using localhost"));}
    else {
      char *argname=argv[i++], *argvalue=argv[i++];
      if (strncmp(argname,"--log",strlen("--log")) == 0)
	if ((strcmp(argvalue,"-")) == 0)
	  fd_set_server_log(stdout,NULL);
	else if ((strcmp(argvalue,"--")) == 0)
	  fd_set_server_log(stderr,NULL);
	else fd_set_server_log(fopen(argvalue,"w+"),NULL);
      else if (strncmp(argname,"--trace",strlen("--trace")) == 0)
	if ((strcmp(argvalue,"-")) == 0)
	  fd_set_server_log(NULL,stdout);
	else if ((strcmp(argvalue,"--")) == 0)
	  fd_set_server_log(NULL,stderr);
	else fd_set_server_log(NULL,fopen(argvalue,"w+"));
      else if (strcmp(argname,"--access") == 0) {
	if (strcmp(argvalue,"read-only") == 0) {
	  read_only=1; locking=0;}
	else if (strcmp(argvalue,"locking") == 0) {
	  read_only=1; locking=1;}
	else if (strcmp(argvalue,"run-with-scissors") == 0) {
	  read_only=0; locking=0;}
	else {
	  fprintf(stderr,_("Unknown access type `%s'"),argvalue);
	  exit(1);}}
      else if ((strcmp(argname,"--module") == 0) ||
	       (strcmp(argname,"-m") == 0)) {
	fd_lispenv menv=fd_get_module(argvalue);
	if (menv) {
	  fd_warn(_("Using module %s"),argvalue);
	  fd_module_uses(server_env,menv);}
	else fd_warn(_("Couldn't find module %s"),argvalue);}
      else if ((strcmp(argname, "--config") == 0) ||
	       (strcmp(argname, "-c") == 0)) {
	fd_load_config(argvalue);
	fd_do_preloads();}
      else if ((strcmp(argname,"--file") == 0) ||
	       (strcmp(argname,"-f") == 0)) {
	fd_warn(_("Loading init file %s"),argvalue);
	fd_load_file(argvalue,NULL,server_env);}
      else {
	show_usage();
	fd_raise_detailed_exception(fd_UnknownCmdLineArg,argname);}
    }

  {
    fd_u8char *journal_filename=fd_string_getenv("JOURNAL");
    if (journal_filename) {
      jsync_time_interval=fd_int_getenv("JSYNC_SECONDS",jsync_time_interval);
      jsync_transaction_interval=fd_int_getenv("JSYNC_TRANSACTIONS",jsync_transaction_interval);
      journal_file=fopen(journal_filename,"wb+");
      fd_xfree(journal_filename);}}

#if defined(SIGHUP)
  signal(SIGHUP,SIG_IGN);
#endif

#if defined(SIGPIPE)
  signal(SIGPIPE,SIG_IGN);
#endif

  signal(SIGTERM,terminate_handler);

  if ((lax_security) && (FD_VOIDP(FD_SYMBOL_VALUE(client_okp_symbol)))) {
    fd_warn(_("No CLIENT-OK? defined, going from lax to standard security"));
    lax_security=0;}
  if ((locking) && (!(read_only))) {
    char buf[1024]; sprintf(buf,"%s.locks",filebase);
    open_oid_lock_stream(buf);}
  fd_warn(_("Starting server %s"),port_id);
  {
    fd_lisp sym=fd_make_symbol("HELP");
    if (FD_VOIDP(FD_SYMBOL_VALUE(sym))) 
      fd_warn(_("The server %s does not provide a HELP function"),port_id);
  }
  memory_base=memory_usage();
  
  server_sockets=fd_open_server_socket(port_id);
  atexit(close_server_sockets);
  
  if (socket_failurep(server_sockets)) {
    fprintf(stderr,_("Couldn't open socket for server -- aborting\n")); exit(1);}

  /* Compute the `network id' for reaching this server. */
  if (using_localhost) {
    nid=nid_buf;
    sprintf(nid,"%s@localhost",port_id);}
  else if (strchr(port_id,'@') == NULL) {
    char hostname[64]; nid=nid_buf;
    gethostname(hostname,64);
    sprintf(nid,"%s@%s",port_id,hostname);}
  else nid=port_id;

  write_nid_file(filebase,nid);
  write_pid_file(filebase,(long)getpid());

  server_started=1;
  
  fd_disable_notifications();

  if (server_env->module->exports.n_keys == 0) {
    fd_hashtable h=&(server_env->module->bindings);
    fd_hashset ex=&(server_env->module->exports);
    fd_pair *scan=h->table, *limit=scan+h->n_slots;
    while (scan < limit)
      if (*scan) {
	fd_hashset_add(ex,((*scan)->car));
	scan++;}
      else scan++;}

  fflush(stdout); fflush(stderr);
  fd_start_server(server_sockets,eval_server_fcn);
  
#ifdef MINGW
  fd_commit_pools();
  fd_commit_indices();
#endif
  fd_exit(0);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: fdserver.c,v $
   Revision 1.47  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.46  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.45  2004/07/31 14:08:04  haase
   Added fd_do_preloads after fd_load_configs

   Revision 1.44  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.43  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.42  2004/04/23 12:48:17  haase
   Fixed bug with error returns in fdserver

   Revision 1.41  2004/02/09 12:47:34  haase
   Added implementation of database syncing for pools and indices

   Revision 1.40  2004/01/09 22:16:04  haase
   Experimental journalling for fdserver

   Revision 1.38  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.37  2003/11/30 21:23:43  haase
   Made fdserver close its server sockets on exit

   Revision 1.36  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.35  2003/11/29 12:24:12  haase
   Made fdserver swap out include pools and report before and afters

   Revision 1.34  2003/11/26 13:51:17  haase
   Made fdserver support index subservers

   Revision 1.33  2003/11/21 15:31:07  haase
   Made fdserver exit when it can't bind its socket

   Revision 1.32  2003/10/28 21:27:11  haase
   Fixed possible close of NULL file handle

   Revision 1.31  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.30  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.29.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.29.2.1  2003/01/26 20:40:27  haase
   Misc. fixes

   Revision 1.29  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.28  2002/06/01 21:11:50  haase
   Add handling of command line var configs which reset vars

   Revision 1.27  2002/05/15 08:45:18  haase
   Added pool/index service warnings to startup

   Revision 1.26  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.25  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.24  2002/04/03 19:51:28  haase
   Made fdserver ignore SIGPIPE

   Revision 1.23  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
