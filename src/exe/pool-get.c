/* C Mode */

/* pool-get.c
   Prints the value of an OID given a pool.
   This is the test executable for the object database functions of FramerD.
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

static char vcid[] = "$Id: pool-get.c,v 1.7 2005/01/14 16:48:45 haase Exp $";

#include "framerd/framerd.h"

char * lisp_typename [] = {
  "bad_type",
  "fixnum_type",
  "immediate_type",
  "character_type", 
  "symbol_type",
  "object_type",
  "qstring_type",
  "flonum_type",
  "string_type",
  "pair_type",
  "vector_type",
  "bigint_type",
  "rational_type",
  "complex_type",
  "record_type",
  "lrecord_type",
  "choice_type",
  "proper_choice_type",
  "quoted_choice_type",
  "slotmap_type",
  "tail_call_type",
  "cproc_type",
  "sproc_type",
  "ssproc_type", 
  "continuation_type",
  "gproc_type",
  "rproc_type",
  "dtype_server_type",
  "pool_type",
  "index_type",
  "hashtable_type",
  "hashset_type",
  "input_file_type",
  "output_file_type",
  "io_file_type",
  "input_string_type",
  "output_string_type",
  "multiple_value_type",
  "unknown_type: 38",
  "unknown_type: 39",
  "packet_type",
  "unknown_type: 41",
  "mystery_type",
  "segmented_stream_type",
  "record_stream_type",
  "mutex_type",
  "delay_type",
  "env_type",
  "tx_closure_type"
};

int main(int argc,char *argv[])
{
  lisp oid, value;

  fd_u8char *pool, *oid_str;
  
  pool = argv[1];
  oid_str = argv[2];
  
  if ((pool == NULL) || (oid_str == NULL) ||
      (strlen(pool) <= 0) || (strlen(oid_str) <= 0) ) {
    printf( "usage: pool-get pool oid\n" );
    printf( "  pool is a poolname, either file (foo.pool) or network (foo@host)\n" );
    printf( "  oid is an oid string, either literal (@0/0) or logical (@/pool/0)\n" );
     exit( 1 );
   }
 
  fd_initialize_framerd();
  fd_use_pool(pool);
  oid=fd_parse_arg(oid_str);

  value=fd_oid_value(oid);
  fd_fprintf(stdout,_("LISP Type (FD_PTR_TYPE(oid)): %x [%s]\n"),
	     FD_PTR_TYPE(oid), lisp_typename[FD_PTR_TYPE(oid)]);
  fd_fprintf(stdout,_("OID Type (FD_PTR_TYPE(FD_OID_VALUE(oid))): %x [%s]\n"),
	     FD_PTR_TYPE(FD_OID_VALUE(oid)),
	     lisp_typename[FD_PTR_TYPE(FD_OID_VALUE(oid))]);
  fd_fprintf(stdout,_("The OID %q in the pool %s has the value:\n"),
	     oid,argv[1]);
  fd_pprint_lisp(value,stdout,40);
  fd_fprintf(stdout,"\n");
  /* Just to be neat */
  fd_decref(value);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: pool-get.c,v $
   Revision 1.7  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
