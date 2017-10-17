/* C Mode */

/* maint.c
   FramerD maintenance primitives for FDScript
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

static char vcid[] = "$Id: maint.c,v 1.25 2005/01/14 16:48:46 haase Exp $";

/** FramerD maintenance functions **/
/** Primitives for maintaining indices **/
/** Labelling file pools **/
/** Snapshots **/
/** Analyzing file indices **/
/** Super Pool Functions **/
/** Initializing **/

#include "fdscript.h"

FDSCRIPT_EXPORT fd_lispenv fd_fdmaint_env;
fd_lispenv fd_fdmaint_env;

/** FramerD maintenance functions **/

static lisp make_file_index_lexpr(fd_lisp args)
{
  time_t now=time(NULL);
  int i=0, size, major_version; FILE *data;
  fd_lisp filename, minsize, version, metadata;
  fd_get_args("MAKE-FILE-INDEX",args,
	      &filename,FD_VOID,&minsize,FD_LISPFIX(500),
	      &version,FD_LISPFIX(1),
	      &metadata,FD_EMPTY_CHOICE,
	      NULL);
  if (!(FD_STRINGP(filename)))
    fd_type_error(_("not a filestring"),filename);
  if ((!(FD_FIXNUMP(minsize))) && (FIXLISP(minsize) > 0))
    fd_type_error(_("index file size not a positive fixnum"),minsize);
  if (!(FD_FIXNUMP(version)))
    fd_type_error(_("version not a fixnum"),version);
  if (FD_FIXNUMP(version))
    major_version=FD_FIXLISP(version);
  else major_version=0;
  /* Initialize variables, since args are okay */
  size=fd_select_table_size(FIXLISP(minsize));
  data=fd_fopen_locked(STRING_DATA(filename),"wb",0);
  if (data == NULL)
    fd_raise_detailed_exception
      (fd_FileLockFailed,STRING_DATA(filename));
  if (size > 1000000) {
    fwrite_4bytes(FD_MULT_FILE_INDEX_MAGIC_NUMBER,data);}
  else {fwrite_4bytes(FD_FILE_INDEX_MAGIC_NUMBER,data);}
  fwrite_4bytes(size,data);
  /* Write the slots of the hashtable */
  while (i < size) {fwrite_4bytes(0,data); i++;}
  /* Write metadata */
  fwrite_4bytes(0xFFFFFFFE,data);
  fwrite_4bytes(40,data);
  fwrite_4bytes(major_version,data);
  fwrite_4bytes(0,data); fwrite_4bytes((int)now,data);
  fwrite_4bytes(0,data); fwrite_4bytes((int)0,data);
  fwrite_4bytes(0,data); fwrite_4bytes((int)0,data);
  if (FD_EMPTYP(metadata)) {
    fwrite_4bytes(0x0,data);}
  else {
    fd_fwrite_4bytes(8+size*4+40,data);
    fd_fwrite_dtype(metadata,data);}
  /* Close the stream */
  fclose(data);
  return fd_make_cptr(index_type,fd_open_index(STRING_DATA(filename)));
}

static lisp default_super_pool()
{
  lisp from_env=fd_getenv("SUPER_POOL");
  if (FD_EMPTYP(from_env))
    return fd_make_string("super@framerd.beingmeta.com");
  else return from_env;
}

static lisp make_file_pool_lexpr(lisp args)
{
  fd_lisp filename, capacity, base_spec, label, version, metadata;
  int cap=1;
  fd_get_args("MAKE-FILE-POOL",args,
	      &filename,FD_VOID,&capacity,FD_VOID,
	      &base_spec,FD_FALSE,&label,FD_FALSE,
	      &version,FD_LISPFIX(1),&metadata,FD_EMPTY_CHOICE,
	      NULL);
  if (!(STRINGP(filename)))
    fd_type_error(_("not a filestring"),filename);
  else if (!(FIXNUMP(capacity)))
    fd_type_error(_("pool capacity is not a fixnum"),capacity);
  else {
    int mincap=FIXLISP(capacity); while (cap < (1<<28))
      if (cap >= mincap) break; else cap=cap*2;}
  if (FD_FALSEP(base_spec)) base_spec=default_super_pool();
  if (OIDP(base_spec)) {
    FD_OID base=OID_ADDR(base_spec);
    fd_make_file_pool(STRING_DATA(filename),base,cap,
		      FD_FIXLISP(version),metadata);
    if (!(FD_FALSEP(label)))
      fd_label_file_pool(STRING_DATA(filename),label);
    return fd_make_cptr(pool_type,fd_use_pool(STRING_DATA(filename)));}
  else if (STRINGP(base_spec)) {
    FD_OID base=fd_allocate_pool
      (STRING_DATA(base_spec),cap,STRING_DATA(filename));
    fd_make_file_pool(STRING_DATA(filename),base,cap,
		      FD_FIXLISP(version),metadata);
    if (!(FD_FALSEP(label)))
      fd_label_file_pool(STRING_DATA(filename),label);
    return fd_make_cptr(pool_type,fd_use_pool(STRING_DATA(filename)));}
  else fd_type_error(_("base spec is not an OID or super pool"),base_spec);
  return fd_make_cptr(pool_type,fd_use_pool(STRING_DATA(filename)));
}

static lisp reset_pool(lisp fname)
{
  char *filename, tmp[1024]; FILE *f;
  if (!(STRINGP(fname))) fd_type_error(_("not a filestring"),fname);
  else filename=STRING_DATA(fname);
  f=fd_fopen_locked(filename,"r+b",0);
  if (f == NULL)
    fd_raise_lisp_exception(fd_FileLockFailed,"RESET-POOL",fname);
  else if (fread_4bytes(f) != FD_FILE_POOL_MAGIC_NUMBER)
    fd_raise_lisp_exception(fd_NotAFilePool,"RESET-POOL",fname);
  else {
    FD_OID base; lisp label, metadata;
    unsigned int oid_high, oid_low, capacity; off_t label_pos;
    int major_version;
    oid_high=fread_4bytes(f); FD_SET_OID_HIGH(base,oid_high);
    oid_low=fread_4bytes(f); FD_SET_OID_LOW(base,oid_low);
    capacity=fread_4bytes(f);
    fread_4bytes(f); /* Ignore load */
    label_pos=(off_t)(fread_4bytes(f));
    if (label_pos) {
      fseeko(f,label_pos,SEEK_SET);
      label=fd_fread_dtype(f);}
    else label=FD_VOID;
    metadata=fd_read_file_pool_metadata(f,&major_version,NULL,NULL,NULL,NULL);
    fclose(f);
    strcpy(tmp,filename); strcat(tmp,".tmp"); rename(filename,tmp);
    fd_make_file_pool(filename,base,capacity,major_version,metadata);
    if (!(FD_VOIDP(label))) fd_label_file_pool(filename,label);}
  return FD_TRUE;
}

static lisp lisp_make_super_pool_lexpr(lisp args)
{
  FD_OID base;
  fd_lisp filename, base_spec, initial_load;
  fd_get_args("MAKE-SUPER-POOL",args,
	      &filename,FD_VOID,
	      &base_spec,FD_FALSE,
	      &initial_load,FD_LISPFIX(0),
	      NULL);
  if (!(STRINGP(filename)))
    fd_type_error(_("not a filestring"),filename);
  else if (FD_FALSEP(base_spec)) {
    FD_OID base;
    unsigned int hi=fd_make_new_super_pool(STRING_DATA(filename));
    FD_SET_OID_HIGH(base,hi); FD_SET_OID_LOW(base,0);
    return fd_make_oid(base);}
  else if (!(OIDP(base_spec)))
    fd_type_error(_("pool base is not an OID"),base_spec);
  else if ((!(FIXNUMP(initial_load))) && (!(FD_FALSEP(initial_load))))
    fd_type_error(_("super pool capacity is not a fixnum"),initial_load);
  base=OID_ADDR(base_spec);
  fd_make_super_pool
    (STRING_DATA(filename),FD_OID_HIGH(base),FIXLISP(initial_load));
  return base_spec;
}

static lisp recover_pool(lisp filename,lisp base_spec,lisp capacity,lisp tag)
{
  if (!(STRINGP(filename)))
    fd_type_error(_("super pool filename is not a string"),
		  filename);
  else if (!(OIDP(base_spec)))
    fd_type_error(_("base spec is not an OID"),base_spec);
  else if (!(FIXNUMP(capacity)))
    fd_type_error(_("capacity is not a fixnum"),base_spec);
  else fd_recovered_pool
	 (STRING_DATA(filename),OID_ADDR(base_spec),FIXLISP(capacity),tag);
  return base_spec;
}

/** Labelling file pools **/

static lisp set_file_pool_label(lisp poolspec,lisp label)
{
  if (STRINGP(poolspec)) {
    fd_label_file_pool(STRING_DATA(poolspec),label);
    return FD_VOID;}
  else {
    fd_pool p=fd_interpret_pool(poolspec);
    if (p->type == file_pool) {
      fd_label_file_pool(((fd_file_pool)p)->filename,label);
      return FD_VOID;}
    else fd_raise_exception(_("Can't label network pools"));}
}


/** Metadata access **/

fd_lisp lisp_get_file_pool_metadata(fd_lisp arg)
{
  fd_lisp metadata;
  unsigned int major_version; fd_u8char *minor_version;
  time_t creation_time, repack_time, change_time;
  if (FD_STRINGP(arg)) {
    FILE *f=fd_fopen(FD_STRING_DATA(arg),"rb");
    if (f == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,FD_STRING_DATA(arg));
    else metadata=
	   fd_read_file_pool_metadata
	   (f,&major_version,&minor_version,
	    &creation_time,&repack_time,&change_time);
    fclose(f);}
  else if ((PRIM_TYPEP(arg,pool_type)) &&
	   (((fd_pool)(CPTR_DATA(arg)))->type == file_pool)) {
    fd_file_pool p=(fd_file_pool)(CPTR_DATA(arg));
    FILE *f=fd_fopen(p->filename,"rb");
    metadata=fd_read_file_pool_metadata
      (f,&major_version,&minor_version,
       &creation_time,&repack_time,&change_time);
    fclose(f);}
  else fd_type_error(_("not a file pool"),arg);
  /* Init the metadata if neccessary */
  if (FD_VOIDP(metadata))
    fd_type_error(_("not a file pool"),arg);
  else metadata=fd_make_slotmap(8);
  /* Set extra slots */
  fd_prim_add(metadata,fd_make_symbol("MAJOR-VERSION"),FD_LISPFIX(major_version));
  fd_prim_add(metadata,fd_make_symbol("MINOR-VERSION"),fd_init_string(minor_version,-1));
  if (creation_time > 0)
    fd_prim_add(metadata,fd_make_symbol("CREATION-TIMESTAMP"),
		fd_make_timestamp(creation_time));
  if (repack_time > 0)
    fd_prim_add(metadata,fd_make_symbol("REPACK-TIMESTAMP"),
		fd_make_timestamp(repack_time));
  if (change_time > 0)
    fd_prim_add(metadata,fd_make_symbol("CHANGE-TIMESTAMP"),
		fd_make_timestamp(change_time));
  return metadata;
}

fd_lisp lisp_get_file_index_metadata(fd_lisp arg)
{
  fd_lisp metadata; int major_version; off_t minor_version;
  time_t creation_time, repack_time, change_time;
if (FD_STRINGP(arg)) {
    FILE *f=fd_fopen(FD_STRING_DATA(arg),"rb");
    if (f == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,FD_STRING_DATA(arg));
    else metadata=fd_read_file_index_metadata
	   (f,&major_version,&minor_version,&creation_time,&repack_time,&change_time);
    fclose(f);}
  else if ((PRIM_TYPEP(arg,index_type)) &&
	   (((fd_pool)(CPTR_DATA(arg)))->type == file_index)) {
    fd_file_index ix=(fd_file_index)(CPTR_DATA(arg));
    FILE *f=fd_fopen(ix->filename,"rb");
    metadata=fd_read_file_index_metadata
      (f,&major_version,&minor_version,&creation_time,&repack_time,&change_time);
    fclose(f);}
  else fd_type_error(_("not a file index"),arg);
  /* Init the metadata if neccessary */
  if (FD_VOIDP(metadata))
    fd_type_error(_("not a file index"),arg);
  else metadata=fd_make_slotmap(8);
  /* Set extra slots */
  fd_prim_add(metadata,fd_make_symbol("MAJOR-VERSION"),
	      FD_LISPFIX(major_version));
  fd_prim_add(metadata,fd_make_symbol("MINOR-VERSION"),
	      FD_LISPFIX(minor_version));
  if (creation_time > 0)
    fd_prim_add(metadata,fd_make_symbol("CREATION-TIMESTAMP"),
		fd_make_timestamp(creation_time));
  if (repack_time > 0)
    fd_prim_add(metadata,fd_make_symbol("REPACK-TIMESTAMP"),
		fd_make_timestamp(repack_time));
  if (change_time > 0)
    fd_prim_add(metadata,fd_make_symbol("CHANGE-TIMESTAMP"),
		fd_make_timestamp(change_time));
  return metadata;
}

/** Snapshots **/

lisp lisp_make_pool_snapshot(lisp pool,lisp snapshot)
{
  char *filename;
  if (!(STRINGP(snapshot)))
    fd_type_error(_("not a filestring"),snapshot);
  else if (STRINGP(pool))
    filename=STRING_DATA(pool);
  else if ((PRIM_TYPEP(pool,pool_type)) &&
	   (((fd_pool)(CPTR_DATA(pool)))->type == file_pool)) {
    fd_file_pool p=(fd_file_pool)(CPTR_DATA(pool));
    fd_commit_pool((fd_pool)p);
    filename=p->filename;}
  else fd_type_error(_("not a pool"),pool);
  fd_make_pool_snapshot(filename,STRING_DATA(snapshot));
  return incref(snapshot);
}

lisp lisp_restore_pool_snapshot(lisp pool,lisp snapshot)
{
  char *filename;
  fd_file_pool p=NULL;
  if (!(STRINGP(snapshot)))
    fd_type_error(_("snapshot filename is not a string"),snapshot);
  else if (STRINGP(pool))
    filename=STRING_DATA(pool);
  else if ((PRIM_TYPEP(pool,pool_type)) &&
	   (((fd_pool)(CPTR_DATA(pool)))->type == file_pool)) {
    p=(fd_file_pool)(CPTR_DATA(pool));
    if (p->store) fclose(p->store); p->store=NULL;
    filename=p->filename;}
  else fd_type_error(_("not a pool"),pool);
  fd_restore_pool_snapshot(filename,STRING_DATA(snapshot));
  if (p) fd_revert_pool((fd_pool)p);
  return incref(pool);
}

/** Analyzing file indices **/

static lisp lisp_index_count(lisp index)
{
  fd_index ix=fd_interpret_index(index);
  if (ix->type == file_index) {
    fd_file_index indx=(fd_file_index)ix;
    int i=0, n_keys=0, size=indx->size;
    FILE *f=fd_fopen(indx->filename,"rb");
    fseek(f,8,SEEK_SET);
    while (i < size) {
      if (fread_4bytes(f)) n_keys++; i++;}
    fclose(f);
    return LISPFIX(n_keys);}
  else {
    lisp all_keys=fd_index_keys(ix);
    int sz=CHOICE_SIZE(all_keys); decref(all_keys);
    return LISPFIX(sz);}
}

static lisp lisp_index_size(lisp index)
{
  fd_index ix=fd_interpret_index(index);
  if (ix == NULL)
    fd_type_error(_("not an index"),index);
  else if (ix->type == file_index) {
    fd_file_index indx=(fd_file_index)ix;
    return LISPFIX(indx->size);}
  else fd_type_error(_("not a file index"),index);
}

static lisp lisp_index_load(lisp index)
{
  fd_index ix=fd_interpret_index(index);
  if (ix->type == file_index) {
    fd_file_index indx=(fd_file_index)ix;
    int i=0, n_keys=0, size=indx->size;
    FILE *f=fd_fopen(indx->filename,"rb");
    fseek(f,8,SEEK_SET);
    while (i < size) {
      if (fread_4bytes(f)) n_keys++; i++;}
    fclose(f);
    return LISPFLOAT((n_keys*1000.0)/(size*1000.0));}
  else fd_type_error(_("not a file index"),index);
}

/** Super Pool Functions **/

static lisp lisp_allocate_from_super_pool_cproc(lisp super_pool,lisp capacity)
{
  FD_OID id;
  if (!(STRINGP(super_pool)))
    fd_type_error(_("super pool id is not a string"),super_pool);
  else if (!(FIXNUMP(capacity)))
    fd_type_error(_("new pool capacity is not a fixnum"),capacity);
  else id=fd_allocate_pool
	 (STRING_DATA(super_pool),FIXLISP(capacity),"FDScript");
  return fd_make_oid(id);
}

static lisp lisp_super_pool_base_cproc(lisp super_pool_id)
{
  FD_OID id;
  if (!(STRINGP(super_pool_id)))
    fd_type_error(_("super pool id is not a string"),super_pool_id);
  else id=fd_super_pool_base(STRING_DATA(super_pool_id));
  return fd_make_oid(id);
}

static lisp lisp_super_pool_top_cproc(lisp super_pool_id)
{
  if (!(STRINGP(super_pool_id)))
    fd_type_error(_("super pool id is not a string"),super_pool_id);
  else {FD_OID id=fd_super_pool_top(STRING_DATA(super_pool_id));
	return fd_make_oid(id);}
}

static lisp lisp_super_pool_registeredp_cproc(lisp super_pool_id)
{
  if (!(STRINGP(super_pool_id)))
    fd_type_error(_("super pool id is not a string"),super_pool_id);
  else {
    fd_u8char *id=FD_STRING_DATA(super_pool_id);
    if (strchr(id,'@')) return FD_TRUE;
    else {
      FILE *f=fd_fopen(id,"rb");
      if (f) {
	int code=fread_4bytes(f); fd_fclose(f);
	if (code == FD_REGISTERED_SUPER_POOL_MAGIC_NUMBER)
	  return FD_TRUE;
	else if (code == FD_SUPER_POOL_MAGIC_NUMBER) return FD_FALSE;
	else fd_raise_detailed_exception("not a super pool",id);}
      else fd_raise_detailed_exception("can't open super pool",id);}}
}

static lisp lisp_super_pool_loading_cproc(lisp super_pool_id)
{
  float fullness;
  if (!(STRINGP(super_pool_id)))
    fd_type_error(_("super pool id is not a string"),super_pool_id);
  else fullness=fd_super_pool_loading(STRING_DATA(super_pool_id));
  return LISPFLOAT(fullness);
}

static lisp set_super_pool_aliasing_cproc(lisp arg1,lisp arg2)
{
  if ((OIDP(arg1)) && (OIDP(arg2))) {
    FD_OID from=OID_ADDR(arg1), to=OID_ADDR(arg2);
    fd_set_super_pool_aliasing(from,to);}
  else if (!(OIDP(arg1)))
    fd_type_error(_("not an OID"),arg1);
  else fd_type_error(_("not an OID"),arg2);
  return FD_VOID;
}


/** Initializing **/

FDSCRIPT_EXPORT
void fd_initialize_maint_c()
{
  fd_lispenv mm=fd_make_module();
  fd_register_module("FDMAINT",mm,FD_UNSAFE_ENV,0);
  fd_fdmaint_env=mm;

  /* FramerD maintenance primitives */
  fd_add_lexpr(mm,"MAKE-FILE-INDEX",FD_NORMAL_LEXPR,make_file_index_lexpr);
  fd_add_lexpr(mm,"MAKE-FILE-POOL",FD_NORMAL_LEXPR,make_file_pool_lexpr);
  fd_add_cproc(mm,"RESET-FILE-POOL",1,reset_pool);
  fd_add_cproc(mm,"RECOVER-POOL",4,recover_pool);
  fd_add_cproc(mm,"FILE-POOL-METADATA",1,lisp_get_file_pool_metadata);

  fd_add_cproc(mm,"INDEX-SIZE",1,lisp_index_size);
  fd_add_cproc(mm,"INDEX-COUNT",1,lisp_index_count);
  fd_add_cproc(mm,"INDEX-LOAD",1,lisp_index_load);
  fd_add_cproc(mm,"FILE-INDEX-METADATA",1,lisp_get_file_index_metadata);

  fd_add_cproc(mm,"MAKE-POOL-SNAPSHOT",2,lisp_make_pool_snapshot);
  fd_add_cproc(mm,"RESTORE-POOL-SNAPSHOT",2,lisp_restore_pool_snapshot);

  fd_add_cproc(mm,"LABEL-POOL!",2,set_file_pool_label);

  /* Super pool interrogation */
  fd_add_restricted_cproc
    ("SUPER-POOL-BASE",1,lisp_super_pool_base_cproc);
  fd_add_restricted_cproc
    ("SUPER-POOL-TOP",1,lisp_super_pool_top_cproc);
  fd_add_restricted_cproc
    ("SUPER-POOL-LOADING",1,lisp_super_pool_loading_cproc);
  fd_add_restricted_cproc
    ("REGISTERED-SUPER-POOL?",1,lisp_super_pool_registeredp_cproc);
  fd_add_restricted_cproc
    ("ALLOCATE-FROM-SUPER-POOL",2,lisp_allocate_from_super_pool_cproc);
  fd_add_restricted_lexpr
    ("MAKE-SUPER-POOL",FD_NORMAL_LEXPR,lisp_make_super_pool_lexpr);

  fd_add_restricted_cproc
    ("SET-SUPER-POOL-ALIASING!",2,set_super_pool_aliasing_cproc);

  fd_register_source_file("maint",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: maint.c,v $
   Revision 1.25  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.24  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.23  2004/07/16 14:09:26  haase
   more off_t fixes

   Revision 1.22  2004/03/12 20:30:26  haase
   Added new kind of index with multiplicative hash function more appropriate to very large indices

   Revision 1.21  2003/12/05 14:58:46  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.20  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.19.2.3  2003/08/18 10:43:12  haase
   Various file pool and file index changes, including fd_maybe_cache functions to replace offset retrieval.

   Revision 1.19.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.19.2.1  2003/01/26 20:41:47  haase
   Misc. fixes especially some GC

   Revision 1.19  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.18  2002/06/15 16:59:54  haase
   Restored super pool aliasing

   Revision 1.17  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.16  2002/04/25 15:06:08  haase
   Fixed bug in arg extraction for make-super-pool

   Revision 1.15  2002/04/24 20:26:18  haase
   Fixed changelog entries for various files

   Revision 1.14  2002/04/24 20:06:18  haase
   Fixed metadata writing for make_file_index lexpr

   Revision 1.13  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.12  2002/04/10 12:28:23  haase
   Fixed handling of NULL size pointer to metadata retrieval functions

   Revision 1.11  2002/04/10 03:02:10  haase
   Added version information to file pools and indices

   Revision 1.10  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
