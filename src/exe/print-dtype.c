/* C Mode */

/* print-dtype.c
   Prints the DType representation found in a disk file
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

static char vcid[] = "$Id: print-dtype.c,v 1.10 2005/01/14 16:48:45 haase Exp $";

#include "framerd/dtypes.h"


int main(int argc,char *argv[])
{
  FILE *f;
  fd_lisp contents;

  if (argc != 2)
    {fprintf(stderr,"Usage: print-dtype <filename>\n"); return -1;}
  fd_inhibit_herald(1);
  fd_initialize_dtypes();
  f=fopen(argv[1],"rb");

  if (!(f)) {
    fprintf(stderr,"Usage: print-dtype <filename>\n");
    fprintf(stderr,"The file %s does not exist\n",argv[1]);
    return -1;}
  {FD_WITH_HANDLING
     while (1)
       {contents=fd_fread_dtype(f);
	{FD_DO_CHOICES(r,contents) {
	  char *string=fd_object_to_string(r);
	  printf("%s\n",string);
	  fd_xfree(string);}
	 END_FD_DO_CHOICES;}
	fd_decref(contents);}
  FD_ON_EXCEPTION
     fd_clear_exception();
  FD_END_HANDLING}
  fclose(f);
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: print-dtype.c,v $
   Revision 1.10  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.9  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.8  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.7  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.6.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.6  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
