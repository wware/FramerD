/* C Mode */

/* index-get.c
   Prints the values associated with a key in the index.
   This is the test executable for the index functions of FramerD
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

static char vcid[] = "$Id: index-get.c,v 1.8 2005/01/14 16:48:45 haase Exp $";

#include "framerd/framerd.h"

int main(int argc,char *argv[])
{
  fd_index ix; lisp key, values;

  fd_initialize_framerd();
  ix=fd_open_index(argv[1]);
  key=fd_parse_arg(argv[2]);
  values=fd_index_get(ix,key,FD_EMPTY_CHOICE);
  fd_fprintf(stdout,_("The key %q in the index %s has %d values:\n"),
	     key,argv[1],FD_CHOICE_SIZE(values));
  {FD_DO_CHOICES(v,values) fd_fprintf(stdout,"  %q\n",v); END_FD_DO_CHOICES;}
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: index-get.c,v $
   Revision 1.8  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.5  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.4  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
