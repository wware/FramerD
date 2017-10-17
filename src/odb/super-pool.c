/* C Mode */

/* super-pool.c

   Implements super pool manipulation functions
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

/** Initial definitions **/

static char vcid[] = "$Id: super-pool.c,v 1.8 2005/01/14 16:48:48 haase Exp $";

#include "framerd.h"
#include "framerd/dtcodes.h"
#include <limits.h>

#define SUPER_POOL_OK(code) \
   ((code == FD_SUPER_POOL_MAGIC_NUMBER) || \
    (code == FD_REGISTERED_SUPER_POOL_MAGIC_NUMBER))

/** Internal super pool functions **/

static lisp get_stamp_for_super_pool(char *label);

static FILE *open_superpool
 (char *superpool,FD_OID *base,unsigned int *current_load)
{
  unsigned int magic_no; FILE *s;
  s=fd_fopen_locked(superpool,"r+b",0);
  if (s == NULL) {
    fd_raise_detailed_exception
      ("Couldn't locate open super pool file",superpool);}
  magic_no=fread_4bytes(s);
  if (magic_no == FD_REGISTERED_SUPER_POOL_MAGIC_NUMBER) {}
  else if (magic_no == FD_SUPER_POOL_MAGIC_NUMBER)
    fd_warn(_("Warning: Super pool %s isn't registered"),superpool);
  else {
    fd_raise_detailed_exception(fd_NotASuperPool,superpool);}
  FD_SET_OID_HIGH(*base,fread_4bytes(s));
  FD_SET_OID_LOW(*base,fread_4bytes(s));
  *current_load=fread_4bytes(s);
  return s;
}

static void record_pool
  (char *superpool,FILE *s,FD_OID spbase,unsigned int current_load,
   FD_OID base,int capacity,lisp label)
{
  if ((FD_OID_HIGH(spbase) != (FD_OID_HIGH(base)))) {
    fclose(s);
    fd_raise_detailed_exception(_("Pool base not in super pool"),superpool);}
  else if ((0xFFFFFFFF - capacity) < FD_OID_LOW(base)) {
    fclose(s);
    fd_raise_detailed_exception
      ("Pool allocation overflows super pool",superpool);}
  else {
    FD_OID top=base; int c; lisp label;
    FD_SET_OID_LOW(top,FD_OID_LOW(base)+capacity-1);
    /* Start scanning the pool entries */
    fseek(s,16,SEEK_SET);
    /* Skip the label */
    label=fd_fread_dtype(s); decref(label);
    /* Read each entry */
    while ((c=getc(s)) != EOF) {
      FD_OID pbase; unsigned int pcap; lisp tag;
      FD_SET_OID_HIGH(pbase,fread_4bytes(s));
      FD_SET_OID_LOW(pbase,fread_4bytes(s));
      getc(s); /* Skip dt_fixnum code */
      pcap=fread_4bytes(s);
      tag=fd_fread_dtype(s);
      if (FD_OID_IN_RANGE(base,pbase,pcap)) {
	fclose(s);
	fd_raise_lisp_exception(_("Pool overlap at base"),superpool,tag);}
      else if (FD_OID_IN_RANGE(top,pbase,pcap)) {
	fclose(s);
	fd_raise_lisp_exception(_("Pool overlap at top"),superpool,label);}
      else decref(tag);}
    /* Reached the end, write your own entry */
    fwrite_byte(dt_oid,s);
    fwrite_4bytes(FD_OID_HIGH(base),s);
    fwrite_4bytes(FD_OID_LOW(base),s);
    fwrite_byte(dt_fixnum,s);
    fwrite_4bytes(capacity,s);
    fd_fwrite_dtype(label,s);
    /* Write the new load for the super pool */
    if ((FD_OID_LOW(base)+capacity) > current_load) {
      fseek(s,12,SEEK_SET); fwrite_4bytes(FD_OID_LOW(base)+capacity,s);}
    fclose(s);}
}

static lisp get_stamp_for_super_pool(char *label)
{
  lisp identity;
  identity=fd_make_string(fd_session_id());
  return fd_quote_lisp(FD_MAKE_LIST(2,fd_make_string(label),identity));
}

/** Top level super pool functions **/

FRAMERD_EXPORT
/* fd_allocate_pool:
    Arguments: a super pool id, a capacity, and a string label
    Returns: an FD_OID
 Allocates a new pool from a designated super pool with a particular
 capacity, returning the base of the new pool. */
FD_OID fd_allocate_pool
  (char *super_pool,unsigned int capacity,char *label)
{
  lisp sp_label=get_stamp_for_super_pool(label);
  if (strchr(super_pool,'@')) {
    fd_server s=fd_connect(super_pool); 
    lisp value, expr=FD_MAKE_LIST
      (3,fd_make_symbol("ALLOCATE-POOL"),LISPFIX(capacity),sp_label);
    value=fd_careful_dtype_eval(expr,s); decref(expr);
    fd_close_connection(s);
    if (OIDP(value)) return FD_OID_ADDR(value);
    else fd_raise_lisp_exception
	   ("Allocation from super pool failed",super_pool,value);}
  else {
    FILE *s; FD_OID spbase; 
    unsigned int current_load;
    s=open_superpool(super_pool,&spbase,&current_load);
    if ((0xFFFFFFFF - capacity) < current_load) {
      fclose(s);
      fd_raise_detailed_exception
	("Not enough space in super pool",super_pool);}
    else {
      FD_OID base=spbase; FD_SET_OID_LOW(base,current_load);
      record_pool(super_pool,s,spbase,current_load,base,capacity,sp_label);
      return base;}}
}

FRAMERD_EXPORT
/* fd_recovered_pool:
    Arguments: a super pool id, a base, a capacity, and a lisp pointer label
    Returns: an FD_OID
 Asserts lost information about a pool in a super pool. */
FD_OID fd_recovered_pool
  (char *super_pool,FD_OID base,unsigned int capacity,lisp sp_label)
{
  if (strchr(super_pool,'@')) {
    fd_server s=fd_connect(super_pool); 
    lisp value, expr=FD_MAKE_LIST
      (4,fd_make_symbol("RECOVERED-POOL"),
       fd_make_oid(base),LISPFIX(capacity),sp_label);
    value=fd_careful_dtype_eval(expr,s); decref(expr);
    fd_close_connection(s);
    if (OIDP(value)) return FD_OID_ADDR(value);
    else fd_raise_lisp_exception
	   ("Allocation from super pool failed",super_pool,value);}
  else {
    FILE *s; FD_OID spbase; unsigned int current_load;
    s=open_superpool(super_pool,&spbase,&current_load);
    record_pool(super_pool,s,spbase,current_load,base,capacity,sp_label);
    return base;}
}

FRAMERD_EXPORT
/* fd_super_pool_top:
    Arguments: a super pool id
    Returns: an FD_OID
 Returns the base of a super pool (e.g. @sp_id/0) */
FD_OID fd_super_pool_base(char *id)
{
  if (strchr(id,'@')) {
    fd_server s=fd_connect(id);
    lisp expr=FD_MAKE_LIST1(fd_make_symbol("SP-BASE"));
    lisp value=fd_careful_dtype_eval(expr,s);
    decref(expr); fd_close_connection(s);
    if (OIDP(value)) return FD_OID_ADDR(value);
    else fd_raise_lisp_exception
	   ("Strange return value","SP-BASE",value);}
  else {
    FD_OID bid; FILE *s=fd_fopen(id,"rb"); int code;
    FD_SET_OID_HIGH(bid,0); FD_SET_OID_LOW(bid,0);
    if (s == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,id);
    code=fread_4bytes(s);
    if (!(SUPER_POOL_OK(code)))
      fd_raise_detailed_exception(fd_NotASuperPool,id);
    FD_SET_OID_HIGH(bid,fread_4bytes(s));
    FD_SET_OID_LOW(bid,fread_4bytes(s));
    fd_fclose(s);
    return bid;}
}

FRAMERD_EXPORT
/* fd_super_pool_top:
    Arguments: a super pool id
    Returns: an FD_OID
 Returns the highest allocated FD_OID in a super pool */
FD_OID fd_super_pool_top(char *id)
{
  if (strchr(id,'@')) {
    fd_server s=fd_connect(id);
    lisp expr=FD_MAKE_LIST1(fd_make_symbol("SP-TOP"));
    lisp value=fd_careful_dtype_eval(expr,s);
    decref(expr); 
    fd_close_connection(s);
    if (OIDP(value)) return FD_OID_ADDR(value);
    else fd_raise_lisp_exception(_("Strange return value"),"SP-TOP",value);}
  else {
    FD_OID tid; unsigned int load;
    FILE *s=fd_fopen(id,"rb"); int code;
    FD_SET_OID_HIGH(tid,0); FD_SET_OID_LOW(tid,0);
    if (s == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,id);
    code=fread_4bytes(s);
    if (!(SUPER_POOL_OK(code)))
      fd_raise_detailed_exception(fd_NotASuperPool,id);
    FD_SET_OID_HIGH(tid,fread_4bytes(s));
    FD_SET_OID_LOW(tid,fread_4bytes(s));
    load=fread_4bytes(s); FD_SET_OID_LOW(tid,FD_OID_LOW(tid)+load);
    fd_fclose(s);
    return tid;}
}

FRAMERD_EXPORT
/* fd_super_pool_loading:
    Arguments: a super pool id
    Returns: a float
 Returns the fraction of the super pool which is allocated */
float fd_super_pool_loading(char *id)
{
  if (strchr(id,'@')) {
    fd_server s=fd_connect(id);
    lisp expr=FD_MAKE_LIST1(fd_make_symbol("SP-LOADING"));
    lisp value=fd_careful_dtype_eval(expr,s);
    float loading;
    decref(expr);
    fd_close_connection(s);
    if (FLONUMP(value)) loading=FLOATLISP(value);
    else fd_raise_lisp_exception(_("Strange return value"),"SP-LOADING",value);
    decref(value); return loading;}
  else {
    FILE *s=fd_fopen(id,"rb");
    unsigned int load, code;
    if (s == NULL)
      fd_raise_detailed_exception(fd_FileOpenFailed,id);
    code=fread_4bytes(s);
    if (!(SUPER_POOL_OK(code)))
      fd_raise_detailed_exception(fd_NotASuperPool,id);
    fread_4bytes(s); fread_4bytes(s);
    load=fread_4bytes(s);
    fd_fclose(s);
    return ((float)load)/((float)UINT_MAX);}
}


void fd_initialize_super_pool_c()
{
  fd_register_source_file("super-pool",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: super-pool.c,v $
   Revision 1.8  2005/01/14 16:48:48  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/09/13 21:57:56  haase
   Fixed automatic closing of unused network connections

   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4.2.1  2003/01/26 20:45:15  haase
   Misc. fixes and general cleanup

   Revision 1.4  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
