/* C Mode */

/* modules.c
   Support for organizing code into modules
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

static char vcid[] = "$Id: modules.c,v 1.13 2005/05/12 03:31:34 haase Exp $";

#include "fdeval.h"

#if FD_USING_DLLS && HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#if (defined(WIN32))
static char *dlerror()
{
  return "no dlerror support";
}
#endif

fd_exception fd_ModuleNotFound=_("Module file not found");
fd_exception fd_InvalidModule=_("Invalid module specifier");
static fd_exception ModuleAlreadyBound=_("Module name already bound");

static fd_lispenv get_loaded_module(fd_lisp module_spec);

static lisp make_upper_symbol(fd_u8char *s)
{
  fd_u8char *ustring=fd_upcase_string(s,-1);
  fd_lisp sym=fd_make_symbol(ustring);
  fd_xfree(ustring);
  return sym;
}

/* Checks if a files suffix makes it look like a DLL (shared object).  */
static int is_dllp(fd_u8char *file)
{
  if (strstr(file,FD_SHARED_LIB_SUFFIX)) return 1;
  else if (strstr(file,".dll")) return 1;
  else return 0;
}

/* Locking modules while loading */

#if FD_THREADS_ENABLED
static fd_mutex module_load_lock;
static fd_mutex module_access_lock;
static fd_mutex module_register_lock;
static fd_condvar module_load_condition;
#endif

static lisp module_lookup(lisp symbol,fd_lispenv env)
{
  struct FD_MODULE *m=env->module;
  if (m == NULL) return FD_VOID;
  else if (fd_hashset_get(&(m->exports),symbol))
    return fd_hashtable_get(&(m->bindings),symbol,FD_VOID);
  else return FD_VOID;
}

/* register_module:
    Arguments: a string and an environment
    Returns: void
 Binds the named symbol to the environment in the global module table.
 This will signal an error if the global binding is not VOID. */
static void register_module(fd_u8char *name,fd_lispenv menv,int access_mask,int needs_loading)
{
  lisp sym=fd_make_symbol(name);
  lisp val=module_lookup(sym,fd_module_table);
  if (FD_VOIDP(val)) {
    val=fd_make_cptr(env_type,fd_mallocd_env(menv));
    fd_hashtable_set(&(fd_module_table->module->bindings),sym,val);
    menv->module->access_bits=access_mask;
    if (needs_loading)  menv->module->load_status=unloaded;
    else menv->module->load_status=loaded;
    fd_decref(val);
    fd_module_export(fd_module_table,sym);}
  else {
    fd_decref(val);
    fd_raise_detailed_exception
      (_("REGISTER-MODULE: module name already used"),name);}
}

static fd_lispenv get_unregistered_module(fd_lisp module_name,fd_lispenv env)
{
  fd_lisp module;
  if (FD_SYMBOLP(module_name))
    module=fd_symeval(module_name,env);
  else fd_type_error("not a module name",module_name);
  if (FD_VOIDP(module)) {
    fd_lispenv fresh=fd_make_module();
    module=fd_make_cptr(env_type,fresh);
    fd_bind_value(module_name,module,env);
    fd_bind_value(module_name,module,fresh);
    fd_decref(module);
    return fresh;}
  else if (FD_VOIDP(module)) return NULL;
  else if (FD_PRIM_TYPEP(module,env_type)) {
    fd_decref(module);
    return FD_CPTR_DATA(module);}
  else fd_raise_lisp_exception
	 (ModuleAlreadyBound,FD_SYMBOL_NAME(module_name),module);  
}

static fd_lispenv get_registered_module(fd_lisp arg)
{
  if (FD_PRIM_TYPEP(arg,env_type)) return (fd_lispenv) FD_CPTR_DATA(arg);
  else if (FD_SYMBOLP(arg)) {
    fd_lisp regmod=module_lookup(arg,fd_module_table);
    if (FD_VOIDP(regmod)) {
      fd_lock_mutex(&module_register_lock);
      regmod=module_lookup(arg,fd_module_table);
      if (FD_VOIDP(regmod)) {
	fd_lispenv fresh=fd_make_module();
	fd_lisp module=fd_make_cptr(env_type,fresh);
	register_module(FD_SYMBOL_NAME(arg),fresh,0,1);
	fd_decref(module);
	fd_unlock_mutex(&module_register_lock);
	return fresh;}
      else if (FD_PRIM_TYPEP(regmod,env_type)) {
	fd_unlock_mutex(&module_register_lock);
	return (fd_lispenv) FD_CPTR_DATA(regmod);}
      else {
	fd_unlock_mutex(&module_register_lock);
	fd_raise_lisp_exception
	  (fd_Type_Error,_("corrupted module table"),regmod);}}
    else return (fd_lispenv) FD_CPTR_DATA(regmod);}
  else fd_type_error(fd_InvalidModule,arg);
}

FRAMERD_EXPORT
/* fd_register_module:
    Arguments: a string and an environment
    Returns: void
 Binds the named symbol to the environment in the global module table.
 This will signal an error if the global binding is not VOID. */
void fd_register_module(fd_u8char *name,fd_lispenv menv,int access_mask,int needs_loading)
{
  fd_lock_mutex(&module_register_lock);
  register_module(name,menv,access_mask,needs_loading);
  fd_unlock_mutex(&module_register_lock);
}

FRAMERD_EXPORT
/* fd_registered_module
    Arguments: a string
    Returns: a lisp environment
Returns a module environment, registered
*/
fd_lispenv fd_registered_module(fd_u8char *name,int access_mask)
{
  fd_lisp symbol=fd_make_symbol(name);
  fd_lisp module=module_lookup(symbol,fd_module_table);
  if (PRIM_TYPEP(module,env_type)) {
    fd_lispenv menv=FD_CPTR_DATA(module); int combine=0;
    fd_set_module_access(menv,access_mask,combine);
    fd_decref(module);
    return menv;}
  else {
    fd_lispenv menv; int replace=1;
    fd_lock_mutex(&module_register_lock);
    menv=fd_make_module();
    register_module(name,menv,access_mask,0);
    fd_unlock_mutex(&module_register_lock);
    return menv;}
}

FRAMERD_EXPORT
/* fd_set_module_access
    Arguments: a module (environment), a bit mask, and an operation code
    Returns: nothing

Modifies the access bits on the module.  Access bits are used to
determine which contexts are allowed to use the module.  Currently the
only access bit assigned is the `safe' bit which counts for
questionable environments (like remove servers or calls to SAFE-EVAL).
*/
void fd_set_module_access(fd_lispenv menv,int bits,int replace)
{
  if (menv->module == NULL)
    fd_raise_exception(_("Environment is not a module"));
  fd_lock_mutex(&module_access_lock);
  if (replace) menv->module->access_bits=bits;
  else menv->module->access_bits=(menv->module->access_bits)|bits;
  fd_unlock_mutex(&module_access_lock);
}

/* Loading modules */

enum MODULE_ACTION { load, loadwait, done, error };

/* This is more complicated than you would expect for two reasons:
    * we need to handle the case where two threads are loading the same module and 
      make sure that the module is loaded exactly once.
    * we need to make sure that a failed module load doesn't inhibit a later reload.
*/
static int load_file_for_module(fd_u8char *file,fd_lispenv menv)
{
  struct FD_MODULE *module=menv->module;
  enum MODULE_ACTION action=0; enum MODULE_LOAD_STATUS status;
  /* All of the logic below handles multiple thread interactions. */
  fd_lock_mutex(&module_load_lock);
  if (module->load_status == unloaded) action=load;
  else if (module->load_status == loading) action=loadwait;
  else if (module->load_status == loaded) action=done;
  status=module->load_status;
  if (action == done) {
    fd_unlock_mutex(&module_load_lock);
    return 1;}
  else if (action == loadwait) {
#if FD_THREADS_ENABLED
    pthread_cond_wait(&module_load_condition,&module_load_lock);
    while (module->load_status == loading)
      pthread_cond_wait(&module_load_condition,&module_load_lock);
#endif
    if (module->load_status == loaded) {
      fd_unlock_mutex(&module_load_lock);
      return 1;}
    else {
      fd_unlock_mutex(&module_load_lock);
      return 0;}}
  else if (action == load) { /* This is where the actual loading happens */
    WITH_HANDLING {
      module->load_status=loading;
      fd_unlock_mutex(&module_load_lock);
      if (is_dllp(file)) 
#if (FD_USING_DLLS)
	{if (fd_load_dll(file) < 0) {
	  fd_raise_detailed_exception(_("DLL error"),(char *)dlerror());}}
#else
      fd_raise_detailed_exception(_("No DLL loading"),file);
#endif
      else fd_process_file(file,NULL,menv);}
    ON_EXCEPTION {
      fd_warn("Exception %s (%s) while loading module file \"%s\": %q",
	      fd_theException(),fd_exception_details(),file,fd_exception_object());
      fd_lock_mutex(&module_load_lock);
      module->load_status=unloaded;
#if FD_THREADS_ENABLED
      pthread_cond_broadcast(&module_load_condition);
      fd_unlock_mutex(&module_load_lock);
#endif
      fd_reraise();}
    END_HANDLING;
    fd_lock_mutex(&module_load_lock);
    module->load_status=loaded;
#if FD_THREADS_ENABLED
    pthread_cond_broadcast(&module_load_condition);
    fd_unlock_mutex(&module_load_lock);
#endif
    return 1;}
}

FRAMERD_EXPORT
/* fd_reload_file_for_module:
     Arguments: a filename (a string), a module environment
     Returns: 1 if successful, 0 otherwise
     Reloads the file for a particular module. */
int fd_reload_file_for_module(fd_u8char *file,fd_lispenv menv)
{
  struct FD_MODULE *module=menv->module;
  enum MODULE_ACTION action=0; enum MODULE_LOAD_STATUS status, ostatus;
  /* All of the logic below handles multiple thread interactions. */
  fd_lock_mutex(&module_load_lock);
  ostatus=module->load_status;
  if (module->load_status == unloaded) action=load;
  else if (module->load_status == loading) action=loadwait;
  else if (module->load_status == loaded) {
    module->load_status=unloaded; action=load;}
  status=module->load_status;
  if (action == done) {
    fd_unlock_mutex(&module_load_lock);
    return 1;}
  else if (action == loadwait) {
#if FD_THREADS_ENABLED
    pthread_cond_wait(&module_load_condition,&module_load_lock);
    while (module->load_status == loading)
      pthread_cond_wait(&module_load_condition,&module_load_lock);
#endif
    if (module->load_status == loaded) {
      fd_unlock_mutex(&module_load_lock);
      return 1;}
    else {
      fd_unlock_mutex(&module_load_lock);
      return 0;}}
  else if (action == load) { /* This is where the actual loading happens */
    WITH_HANDLING {
      module->load_status=loading;
      fd_unlock_mutex(&module_load_lock);
      if (is_dllp(file)) 
#if (FD_USING_DLLS)
	{if (fd_load_dll(file) < 0) {
	  fd_raise_detailed_exception(_("DLL error"),(char *)dlerror());}}
#else
      fd_raise_detailed_exception(_("No DLL loading"),file);
#endif
      else fd_process_file(file,NULL,menv);}
    ON_EXCEPTION {
      fd_warn("Exception %s (%s) while loading module file \"%s\": %q",
	      fd_theException(),fd_exception_details(),file,fd_exception_object());
      fd_lock_mutex(&module_load_lock);
      module->load_status=ostatus;
#if FD_THREADS_ENABLED
      pthread_cond_signal(&module_load_condition);
      fd_unlock_mutex(&module_load_lock);
#endif
      fd_reraise();}
    END_HANDLING;
    fd_lock_mutex(&module_load_lock);
    module->load_status=loaded;
#if FD_THREADS_ENABLED
    pthread_cond_signal(&module_load_condition);
    fd_unlock_mutex(&module_load_lock);
#endif
    return 1;}
}

/* This initializes a module, particularly specifying the modules which it uses.
   It signals an error if the module has already been loaded.
 */
static void initialize_module(fd_lispenv env,fd_lisp use_spec)
{
  int already_loaded=0;
  fd_lock_mutex(&(module_load_lock));
  if (env->module->load_status == loaded) already_loaded=1;
  fd_unlock_mutex(&(module_load_lock));
  if (already_loaded)
    fd_raise_exception("Can't initialize a module which has already been loaded");
  {DO_CHOICES(to_use,use_spec) {
    fd_lispenv use_env=NULL;
    if (FD_PRIM_TYPEP(to_use,env_type))
      use_env=FD_CPTR_DATA(to_use);
    else if (FD_SYMBOLP(to_use)) {
      use_env=get_loaded_module(to_use);}
    if (use_env) fd_module_uses(env,use_env);}
  END_DO_CHOICES;}
}

/* Finding module files */

/* This looks in <dir> for a module named <name> and returns its
   pathname if it exists. */
static fd_u8char *check_module_name(fd_u8char *dir,fd_u8char *name)
{
  int dirlen=strlen(dir), namelen=strlen(name), need_slash=1;
  /* Allocate a buffer we know will be more than big enough */
  fd_u8char *file=fd_xmalloc(dirlen+namelen+24), *suffix_pos;
  if (dir[dirlen-1] == '/') need_slash=0;
  strcpy(file,dir); if (need_slash) strcat(file,"/"); strcat(file,name);
  suffix_pos=file+(dirlen+namelen+need_slash);
  strcpy(suffix_pos,".fdx");
  if (fd_file_existsp(file)) return file;
  strcpy(suffix_pos,FD_SHARED_LIB_SUFFIX);
  if (fd_file_existsp(file)) return file;
  strcpy(suffix_pos,".dll");
  if (fd_file_existsp(file)) return file;
  strcpy(suffix_pos,"/module.fdx");
  if (fd_file_existsp(file)) return file;
  fd_xfree(file);
  return NULL;
}

static fd_u8char *find_module_file(fd_u8char *name)
{
  fd_u8char *lname=fd_downcase_string(name,-1), *fname=NULL;
  lisp path=fd_getpath("MYFDPATH");
  DOLIST(dir,path) {
    fname=check_module_name(fd_strdata(dir),lname);
    if (fname) break;}
  fd_decref(path);
  if (fname == NULL) {
    path=fd_getpath("%FDPATH");
    {DOLIST(dir,path) {
      fname=check_module_name(fd_strdata(dir),lname);
      if (fname) break;}}
    fd_decref(path);}
  fd_xfree(lname);
  return fname;
}

/* Actually getting (and loading if neccessary) a module */

static fd_lispenv get_loaded_module(fd_lisp module_spec)
{
  if (FD_PRIM_TYPEP(module_spec,env_type))
    return (fd_lispenv) FD_CPTR_DATA(module_spec);
  else if (FD_SYMBOLP(module_spec)) {
    int need_to_load=0, wait_to_load=0; fd_u8char *modname=FD_SYMBOL_NAME(module_spec);
    fd_lispenv module=NULL;
    lisp val=module_lookup(module_spec,fd_module_table);
    if (FD_VOIDP(val)) need_to_load=1;
    else if (FD_PRIM_TYPEP(val,env_type)) {
      module=FD_CPTR_DATA(val);
      fd_lock_mutex(&module_load_lock);
      if (module->module->load_status == unloaded) need_to_load=1;
      else if (module->module->load_status == loading) {
#if FD_THREADS_ENABLED
	pthread_cond_wait(&module_load_condition,&module_load_lock);
	while (module->module->load_status == loading)
	  pthread_cond_wait(&module_load_condition,&module_load_lock);
	if (module->module->load_status == unloaded) need_to_load=1;
#endif
      }
      fd_unlock_mutex(&module_load_lock);
      fd_decref(val);}
    else {
      fd_decref(val);
      fd_raise_lisp_exception(fd_Type_Error,_("corrupted module table"),val);}
    if (need_to_load) {
      fd_u8char *loadfile=find_module_file(modname);
      if (loadfile == NULL)
	fd_raise_detailed_exception(fd_ModuleNotFound,modname);
      else {
	if (module == NULL) module=get_registered_module(module_spec);
	fd_module_uses(module,fd_enabled_env);
	load_file_for_module(loadfile,module);}
      return module;}
    else return module;}
  else fd_type_error(fd_InvalidModule,module_spec);
}

FRAMERD_EXPORT
/* fd_get_module:
     Arguments: a module name, a filename, and an int flag
     Returns: a pointer to a lispenv or NULL

  Returns the module with the given name, finding a file to load
it from if needed. */
fd_lispenv fd_get_module(fd_u8char *name)
{
  lisp sym=make_upper_symbol(name);
  return get_loaded_module(sym);
}

FRAMERD_EXPORT
/* fd_load_module:
     Arguments: a module specifier
     Returns: a pointer to a lispenv or NULL

  Returns the module with the given name, finding a file to load
it from if needed. */
fd_lispenv fd_load_module(fd_lisp spec)
{
  return get_loaded_module(spec);
}

FRAMERD_EXPORT
fd_lispenv fd_reload_module(fd_lisp module_spec)
{
  if (FD_SYMBOLP(module_spec)) {
    fd_u8char *modname=FD_SYMBOL_NAME(module_spec);
    lisp val=module_lookup(module_spec,fd_module_table);
    if (FD_VOIDP(val)) return get_loaded_module(module_spec);
    else if ((FD_PRIM_TYPEP(val,env_type)) &&
	     (((fd_lispenv)FD_CPTR_DATA(val))->module)) {
      fd_lispenv module=FD_CPTR_DATA(val);
      fd_u8char *loadfile=find_module_file(modname);
      if (loadfile == NULL)
	fd_raise_detailed_exception(fd_ModuleNotFound,modname);
      else if (fd_reload_file_for_module(loadfile,module))
	return module;
      else return NULL;}
    else {
      fd_decref(val);
      fd_raise_lisp_exception(fd_Type_Error,_("corrupted module table"),val);}}
  else fd_type_error(_("invalid module spec"),module_spec);
}

static fd_lisp lisp_reload_module(fd_lisp arg)
{
  if (fd_reload_module(arg)) return FD_TRUE; else return FD_FALSE;
}

/** New IN-MODULE */

/* This is a version of in-module which only works with unregistered modules.
   Unregisterd modules are just variables bound in the environment. */
static fd_lisp lisp_unregistered_in_module_handler(lisp expr,fd_lispenv env)
{
  fd_lisp module_name=fd_get_arg(expr,1,FD_VOID); fd_lispenv module;
  fd_lisp module_uses=fd_eval_in_env(fd_get_arg(expr,2,FD_TRUE),env);
  if (SYMBOLP(module_name)) {
    module=get_unregistered_module(module_name,env);}
  else {
    fd_lisp module_value=fd_eval_in_env(expr,env);
    if (PRIM_TYPEP(module_value,env_type)) {
      module=FD_CPTR_DATA(module_value);}
    else {
      fd_decref(module_value);
      fd_type_error
	("Access restriction: IN-MODULE only works with unregistered modules",
	 module_name);}}
  initialize_module(module,module_uses);
  fd_decref(module_uses); fd_set_load_env(module);
  return FD_VOID;
}
  
static fd_lisp lisp_in_module_handler(lisp expr,fd_lispenv env)
{
  fd_lisp module_name=fd_get_arg(expr,1,FD_VOID); fd_lispenv module;
  fd_lisp module_uses=fd_eval_in_env(fd_get_arg(expr,2,FD_TRUE),env);
  if (FD_SYMBOLP(module_name)) 
    module=get_unregistered_module(module_name,env);
  else {
    fd_lisp module_val=fd_eval_in_env(module_name,env);
    if (FD_PRIM_TYPEP(module_val,env_type)) {
      module=FD_CPTR_DATA(module_val);}
    else if (FD_SYMBOLP(module_val)) {
      module=get_registered_module(module_val);}
    else {
      fd_decref(module_uses); fd_decref(module_val);
      fd_type_error(fd_InvalidModule,module_name);}}
  initialize_module(module,module_uses);
  fd_decref(module_uses); fd_set_load_env(module);      
  return FD_VOID;
}

static fd_lisp lisp_in_safe_module_handler(lisp expr,fd_lispenv env)
{
  int combine=0;
  fd_lisp module_name=fd_get_arg(expr,1,FD_VOID); fd_lispenv module;
  fd_lisp module_uses=fd_eval_in_env(fd_get_arg(expr,2,FD_TRUE),env);
  if (FD_SYMBOLP(module_name)) 
    module=get_unregistered_module(module_name,env);
  else {
    fd_lisp module_val=fd_eval_in_env(module_name,env);
    if (FD_PRIM_TYPEP(module_val,env_type)) {
      module=FD_CPTR_DATA(module_val);}
    else if (FD_SYMBOLP(module_val)) {
      module=get_registered_module(module_val);}
    else {
      fd_decref(module_uses); fd_decref(module_val);
      fd_type_error(fd_InvalidModule,module_name);}}
  initialize_module(module,module_uses);
  fd_set_module_access(module,1,combine);
  fd_decref(module_uses); fd_set_load_env(module);      
  return FD_VOID;
}

/** WITHIN-MODULE **/

static lisp lisp_within_module_handler(lisp expr,fd_lispenv env)
{
  lisp module_spec=fd_get_arg(expr,1,FD_FALSE);
  lisp module_id=fd_eval_in_env(module_spec,env);
  lisp body=fd_get_body(expr,2), value=FD_VOID;
  fd_lispenv module=NULL;
  if (FD_SYMBOLP(module_id)) {
    fd_lisp v=module_lookup(module_id,fd_module_table);
    if (FD_PRIM_TYPEP(v,env_type)) module=CPTR_DATA(v);
    else if (FD_VOIDP(v)) module=NULL;
    else fd_type_error(fd_InvalidModule,module_id);
    fd_decref(v);}
  else if (FD_PRIM_TYPEP(module_id,env_type))
    module=CPTR_DATA(module_id);
  else fd_type_error(fd_InvalidModule,module_id);
  if (module == NULL)
    fd_raise_lisp_exception
      ("The module is not defined","WITHIN-MODULE",module_id);
  else {
    fd_decref(module_id);
    {DOLIST(expr,body) {
      fd_decref(value); value=fd_eval_in_env(expr,module);}}
    return value;}
}

static lisp use_module_handler(fd_lisp expr,fd_lispenv env)
{
  lisp modules=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  DO_CHOICES(module,modules) {
    fd_u8char *mname, *fname;
    fd_lispenv menv=NULL;
    if (STRINGP(module)) mname=FD_STRING_DATA(module);
    else if (SYMBOLP(module)) mname=FD_SYMBOL_NAME(module);
    else if (FD_PRIM_TYPEP(module,env_type)) menv=CPTR_DATA(module);
    else fd_type_error("can't be a module specifier",module);
    if (menv == NULL) menv=fd_get_module(mname);
    if (menv) fd_module_uses(env,menv);
    else fd_type_error("not a module",module);}
  END_DO_CHOICES;
  fd_decref(modules);
  return FD_VOID;
} 

/** Initialization **/

void fd_initialize_modules_c()
{
#if FD_THREADS_ENABLED
  fd_init_mutex(&module_load_lock);
  fd_init_mutex(&module_access_lock);
  fd_init_mutex(&module_register_lock);
  pthread_cond_init(&module_load_condition,NULL);
#endif
  
  fd_add_special_form
    (fd_global_env,"IN-MODULE",lisp_unregistered_in_module_handler);
  fd_add_restricted_special_form("IN-MODULE",lisp_in_module_handler);
  fd_add_restricted_special_form("IN-SAFE-MODULE",lisp_in_safe_module_handler);

  fd_add_restricted_cproc("RELOAD-MODULE",1,lisp_reload_module);

  fd_add_restricted_special_form("USE-MODULE",use_module_handler);
  fd_add_alias(fd_enabled_env,"USE-MODULE!","USE-MODULE");

  fd_add_special_form
    (fd_enabled_env,"WITHIN-MODULE",lisp_within_module_handler);

  fd_register_source_file("modules",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: modules.c,v $
   Revision 1.13  2005/05/12 03:31:34  haase
   Renamed wait to loadwait

   Revision 1.12  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.11  2004/07/31 14:04:46  haase
   Added fd_load_module

   Revision 1.10  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.9  2004/07/19 16:57:12  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.8  2004/06/25 16:02:18  haase
   Moved more module functions into modules.c and added locks for module registration race conditions

   Revision 1.7  2004/04/12 13:42:49  haase
   Made get-module wait for modules to finish loading

   Revision 1.6  2004/03/31 21:04:11  haase
   Various unthreaded compile fixes

   Revision 1.5  2004/03/22 15:08:07  haase
   Made cond_signal into cond_broadcast for module loads

   Revision 1.4  2004/03/10 14:37:11  haase
   Added module reloading

   Revision 1.3  2003/12/18 03:39:42  haase
   Cleaned up module loading and made it use the shared library suffix

   Revision 1.2  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.1  2003/12/06 16:43:52  haase
   Added modules.c for new module functionality

*/
