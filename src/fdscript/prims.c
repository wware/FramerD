/* C Mode */

/* prims.c
   Primitives for FDScript
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

static char vcid[] = "$Id: prims.c,v 1.16 2006/06/27 13:11:39 haase Exp $";

/** Includes and declarations **/
/** Predicates **/
/** Generic functions on objects **/
/** Symbol operations **/
/** Initializing the primitives **/

/** Includes and declarations **/

#include "fdscript.h"
#include <time.h>
#include <limits.h>
#ifdef WIN32
#include <direct.h>
#endif
#ifndef __alpha__ /* Handles bug in local GCC installation (I guess) */
#include <math.h>
#else
extern  double sqrt();
#endif

#ifdef HAVE_RUSAGE
#include <sys/resource.h>
#endif

/** Predicates **/

static lisp lisp_eq(lisp a1,lisp a2)
{
  if (LISP_EQ(a1,a2)) return FD_TRUE;
  else if ((FD_XPROCP(a1)) && (FD_XPROCP(a2))) {
    /* Kludge for circular GC fix */
    fd_sproc s1=FD_GET_SPROC(a1), s2=FD_GET_SPROC(a2);
    if (!(LISP_EQ(s1->lambda,s2->lambda)) ) return FD_FALSE;
    else if (s1->env != s2->env) return FD_FALSE;
    else return FD_TRUE;}
  else return FD_FALSE;
}

static lisp lisp_equal(lisp a1,lisp a2)
{
  if (LISP_EQ(a1,a2)) return FD_TRUE;
  else if (ATOMICP(a1)) return FD_FALSE;
  else if (fd_lisp_equal(a1,a2)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp is_literal_handler(lisp expr,lispenv env)
{
  lisp arg_expr=fd_get_arg(expr,1,FD_VOID);
  lisp arg=fd_eval_in_env(arg_expr,env);
  lisp options=fd_get_body(expr,2);
  DOLIST(elt,options)
    if (LISP_EQUAL(arg,elt)) {decref(arg); return FD_TRUE;}
  decref(arg); return FD_FALSE;
}

static lisp lisp_pairp(lisp x)
{
  if (PRIM_TYPEP(x,pair_type)) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_sequencep(lisp x)
{
  if ((PRIM_TYPEP(x,pair_type)) || (PRIM_TYPEP(x,string_type)) ||
      (PRIM_TYPEP(x,qstring_type)) || (PRIM_TYPEP(x,zstring_type)) ||
      (PRIM_TYPEP(x,vector_type)) || (PRIM_TYPEP(x,packet_type)) ||
      (FD_HVECTORP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_listp(lisp x)
{
  if ((FD_EMPTY_LISTP(x))) return FD_TRUE;
  else if (PAIRP(x)) {
    lisp scan1=x, scan2;
    if (PAIRP(CDR(x))) scan2=CDR(CDR(x));
    else scan2=CDR(x);
    while ((PAIRP(scan2)) && (PAIRP(scan2)) &&
	   (!(LISP_EQ(scan1,scan2))))
      {if (PAIRP(CDR(scan2))) scan2=CDR(CDR(scan2));
       else scan2=CDR(scan2);
       scan1=CDR(scan1);}
    if ((FD_EMPTY_LISTP(scan1)) || (FD_EMPTY_LISTP(scan2)))
      return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp lisp_stringp(lisp x)
{
  if (STRINGP(x))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_oidp(lisp x)
{
  if (OIDP(x)) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_slotmapp(lisp x)
{
  if (SLOTMAPP(x)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_framep(lisp x)
{
  if (SLOTMAPP(x)) return FD_TRUE;
  else if (OIDP(x)) {
    lisp v=fd_oid_value(x); 
    if (SLOTMAPP(v)) {
      fd_decref(v); return FD_TRUE;}
    else {fd_decref(v); return FD_FALSE;}}
  else return FD_FALSE;
}

static lisp lisp_packetp(lisp x)
{
  if (FD_PACKETP(x)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_symbolp(lisp x)
{
  if (SYMBOLP(x)) return FD_TRUE; else return FD_FALSE;
}

static lisp lisp_booleanp(lisp x)
{
  if ((FD_FALSEP(x)) || (FD_TRUEP(x)))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_characterp(lisp x)
{
  if (CHARACTERP(x))
    return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_numberp(lisp x)
{
  if (FIXNUMP(x)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_vectorp(lisp x) {
  if (VECTORP(x)) return FD_TRUE;
  else return FD_FALSE;}

static lisp lisp_voidp(lisp x) {
  if (FD_VOIDP(x)) return FD_TRUE;
  else return FD_FALSE;}


/** Generic functions on objects **/

static lisp hash_dtype(lisp x)
{
  int code=fd_hash_dtype(x);
  return LISPFIX(code);
}

static lisp hash_dtype2(lisp x)
{
  int code=fd_hash_dtype2(x);
  return LISPFIX(code);
}

static lisp hash_dtype3(lisp x)
{
  int code=fd_hash_dtype3(x);
  return LISPFIX(code);
}

static lisp hash_lisp(lisp x)
{
  unsigned int code=fd_hash_lisp(x);
  return LISPFIX(code);
}

static lisp dtype_size(lisp x)
{
  int code=fd_dtype_size(x);
  return LISPFIX(code);
}

static lisp lisp_ptr_type_handler(lisp expr,fd_lispenv env)
{
  lisp val=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  int code=PTR_TYPE(val);
  decref(val); return LISPFIX(code);
}

/** Symbol operations **/

static lisp lisp_symbol_to_string(lisp sym)
{
  if (SYMBOLP(sym)) return fd_copy_string(SYMBOL_NAME(sym));
  else fd_type_error(_("not a symbol"),sym);
}

static lisp lisp_probe_symbol(lisp str)
{
  if (STRINGP(str)) return fd_probe_symbol(STRING_DATA(str));
  else fd_type_error(_("not a string"),str);
}

static lisp lisp_string_to_symbol(lisp str)
{
  if (STRINGP(str)) return fd_make_symbol(STRING_DATA(str));
  else fd_type_error(_("not a string"),str);
}

static lisp lisp_intern(lisp str)
{
  if (STRINGP(str)) return fd_intern(STRING_DATA(str),STRING_LENGTH(str));
  else fd_type_error(_("not a string"),str);
}

static lisp collector;
static void collect_proc(lisp symbol)
{
  ADD_TO_CHOICE(collector,symbol);
}
static lisp lisp_all_symbols()
{
  lisp answer;
  collector=FD_EMPTY_CHOICE;
  fd_for_all_symbols(collect_proc);
  answer=collector; collector=FD_EMPTY_CHOICE;
  return answer;
}


static lisp lisp_make_packet_cproc(lisp arg)
{
  return fd_parse_packet(fd_strdata(arg));
}

/** Initializing the primitives **/

void fd_initialize_prims_c()
{
  fd_add_cproc(NULL,"EQ?",2,lisp_eq);
  fd_add_cproc(NULL,"OID?",1,lisp_oidp);
  fd_add_cproc(NULL,"FRAME?",1,lisp_framep);
  fd_add_cproc(NULL,"SLOTMAP?",1,lisp_slotmapp);
  fd_add_cproc(NULL,"PACKET?",1,lisp_packetp);
  fd_add_cproc(NULL,"SEQUENCE?",1,lisp_sequencep);
  fd_add_cproc(NULL,"PAIR?",1,lisp_pairp);
  fd_add_cproc(NULL,"LIST?",1,lisp_listp);
  /* Defined in scheme/arith.c */
  /* fd_add_cproc(NULL,"NUMBER?",1,lisp_numberp); */
  fd_add_cproc(NULL,"SYMBOL?",1,lisp_symbolp);
  fd_add_cproc(NULL,"BOOLEAN?",1,lisp_booleanp);
  fd_add_cproc(NULL,"CHARACTER?",1,lisp_characterp);
  fd_add_cproc(NULL,"CHAR?",1,lisp_characterp);
  fd_add_cproc(NULL,"STRING?",1,lisp_stringp);
  fd_add_cproc(NULL,"VECTOR?",1,lisp_vectorp);
  fd_add_cproc(NULL,"VOID?",1,lisp_voidp);

  fd_add_special_form(NULL,"IS-LITERAL?",is_literal_handler);

  fd_add_cproc(NULL,"PROBE-SYMBOL",1,lisp_probe_symbol);
  fd_add_cproc(NULL,"SYMBOL->STRING",1,lisp_symbol_to_string);
  fd_add_cproc(NULL,"STRING->SYMBOL",1,lisp_string_to_symbol);
  fd_add_cproc(NULL,"INTERN",1,lisp_intern);
  fd_add_cproc(NULL,"ALL-SYMBOLS",0,lisp_all_symbols);

  fd_add_special_form(NULL,"%PTR-TYPE",lisp_ptr_type_handler);
  fd_add_cproc(NULL,"HASH-LISP",1,hash_lisp);
  fd_add_cproc(NULL,"HASH-DTYPE",1,hash_dtype);
  fd_add_cproc(NULL,"HASH-DTYPE2",1,hash_dtype2);
  fd_add_cproc(NULL,"HASH-DTYPE3",1,hash_dtype3);
  fd_add_cproc(NULL,"DTYPE-SIZE",1,dtype_size);
  fd_add_cproc(NULL,"COPY-LISP",1,_fd_copy_lisp_proc);
  
  fd_add_cproc(NULL,"PACKET",1,lisp_make_packet_cproc);
  fd_add_alias(NULL,"##","PACKET");

  fd_register_source_file("prims",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: prims.c,v $
   Revision 1.16  2006/06/27 13:11:39  haase
   Fixed bug introduced in adding the new experimental hash function

   Revision 1.15  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.14  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.13  2004/03/30 11:32:15  haase
   Renamed mult_hash functions

   Revision 1.12  2004/03/13 00:08:53  haase
   New improved hashing algorithm for a new kind of index better in general and especially better suited to very large indices

   Revision 1.11  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.10.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.10.2.1  2003/01/26 20:41:48  haase
   Misc. fixes especially some GC

   Revision 1.10  2002/06/15 14:53:54  haase
   Renamed PTR-TYPE to %PTR-TYPE

   Revision 1.9  2002/05/19 10:12:55  haase
   Added fd_intern for making uppercase symbols

   Revision 1.8  2002/04/26 19:21:12  haase
   Added SLOTMAP? primitive

   Revision 1.7  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
