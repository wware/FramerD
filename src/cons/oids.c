/* Mode: C */

/* oids.c
   This file implements OIDs and OID references

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

static char vcid[] = "$Id: oids.c,v 1.7 2005/01/14 16:48:44 haase Exp $";

#define FD_INLINE_OIDS 1

#include "dtypes.h"

#if (FD_LIGHTWEIGHT_OIDS)
#include "lightweight-oids.c"
#else
#include "consed-oids.c"
#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: oids.c,v $
   Revision 1.7  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
