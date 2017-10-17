/* C Mode */

/* records.c
   Record primitives FDScript
   Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.

   Copyright (C) 2000 Massachusetts Institute of Technology

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

/* Contents */
/* Basic functions on lists (pair chains) */
/* Pair modification functions */
/* Member functions */
/* Lisp association list functions */

static char vcid[] = "$Id: records.c,v 1.7 2004/07/20 09:16:15 haase Exp $";

#include "fdscript.h"
#include <time.h>
#include <limits.h>

static lisp recordp_lexpr(lisp args)
{
  fd_lisp x, tag; fd_get_args("RECORD?",args,&x,FD_VOID,&tag,FD_FALSE,NULL);
  if (FD_FALSEP(tag))
    if ((RECORDP(x)) || (LRECORDP(x)))
      return FD_TRUE;
    else return FD_FALSE;
  else if ((RECORDP(x)) || (LRECORDP(x))) {
    if ((RECORDP(x)) ? (RECORD_TYPEP(x,tag)) : (LRECORD_TYPEP(x,tag)))
      return FD_TRUE;
    else return FD_FALSE;}
  else return FD_FALSE;
}

static lisp record_tag_cproc(lisp x)
{
  if (RECORDP(x)) return incref(RECORD_TAG(x));
  else if (LRECORDP(x)) return incref(LRECORD_TAG(x));
  else fd_type_error(_("not a record"),x);
}

static lisp record_data_lexpr(lisp args)
{
  fd_lisp x, tag;
  fd_get_args("RECORD-DATA",args,&x,FD_VOID,&tag,FD_FALSE,NULL);
  if (LRECORDP(x)) {
    if (FD_FALSEP(tag))
      return incref(LRECORD_DATA(x));
    else if (FD_LISP_EQUAL(LRECORD_TAG(x),tag))
      return incref(LRECORD_DATA(x));
    else fd_raise_lisp_exception
	   (fd_Type_Error,fd_object_to_string(tag),x);}
  else if (RECORDP(x))
    fd_type_error(_("not a record with lisp data"),x);
  else fd_type_error(_("not a record"),x);
}

static lisp make_record_cproc(lisp tag,lisp data)
{
  return fd_make_lrecord(incref(tag),incref(data));
}

void fd_initialize_records_c()
{
  fd_add_lexpr(NULL,"RECORD?",FD_NORMAL_LEXPR,recordp_lexpr);
  fd_add_cproc(NULL,"RECORD-TAG",1,record_tag_cproc);
  fd_add_lexpr(NULL,"RECORD-DATA",FD_NORMAL_LEXPR,record_data_lexpr);
  fd_add_cproc(NULL,"MAKE-RECORD",2,make_record_cproc);

  fd_register_source_file("pairs",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: records.c,v $
   Revision 1.7  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/01/26 20:48:13  haase
   Fix two-arg recordp

   Revision 1.5  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
