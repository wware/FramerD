/* C Mode */

/* libfdscheme.c
   Implements init function for FDScript, including primitives
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

static char vcid[]
  = "$Id: libfdscheme.c,v 1.7 2005/01/14 16:48:49 haase Exp $";

#include "fdscript.h"

#ifndef OTHER_INITS
#define OTHER_INITS
#endif

static int fdscheme_initialized=0;

extern void fd_initialize_fdeval(void);
extern void fd_initialize_special_c(void);
extern void fd_initialize_reflect_c(void);
extern void fd_initialize_pairs_c(void);
extern void fd_initialize_cxr_c(void);
extern void fd_initialize_characters_c(void);
extern void fd_initialize_strings_c(void);
extern void fd_initialize_arith_c(void);
extern void fd_initialize_ioprims_c(void);
extern void fd_initialize_records_c(void);

FDSCRIPT_EXPORT void fd_initialize_fdscheme()
{
  if (fdscheme_initialized) return; else fdscheme_initialized=1;
  fd_initialize_fdeval();
  fd_initialize_special_c();
  fd_initialize_reflect_c();
  fd_initialize_characters_c();
  fd_initialize_pairs_c();
  fd_initialize_cxr_c();
  fd_initialize_strings_c();
  fd_initialize_arith_c();
  fd_initialize_ioprims_c();
  fd_initialize_records_c();
}


/* File specific stuff */

/* The CVS log for this file
   $Log: libfdscheme.c,v $
   Revision 1.7  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
