/* C Mode */

/* make-dtype.c
   Writes a DType representation into a disk file
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

static char vcid[] = "$Id: make-dtype.c,v 1.9 2005/01/14 16:48:45 haase Exp $";

#include "framerd/dtypes.h"

int main(int argc,char *argv[])
{
  fd_lisp contents;

  if ((argc < 2) || (argc > 3)) {
    fprintf(stderr,"Usage: make-dtype <filename> <spec>\n"); 
    return -1;}
  fd_inhibit_herald(1);
  fd_initialize_dtypes();
  if (fd_file_writablep(argv[1]) == 0) {
    fd_warn("The file %s isn't writable",argv[1]);
    exit(1);}
  if ((argc == 3) && (strcmp(argv[2],"-") != 0)) {
    char *buf=argv[2];
    contents=fd_parse_arg(buf);
    fd_print_lisp(contents,stdout);
    fd_write_dtype_to_file(contents,argv[1]);}
  else {
    fd_lisp accumulator=FD_EMPTY_LIST, item, last;
    if (fd_file_existsp(argv[1])) remove(argv[1]);
    while (1) {
      item=fd_parse_lisp_from_stream(stdin);
      if (FD_EOF_OBJECTP(item)) {
	FD_DOLIST(contents,accumulator) {
	  fd_print_lisp(contents,stdout);
	  fd_add_dtype_to_file(contents,argv[1]);}
	break;}
      else {
	fd_lisp new=FD_MAKE_LIST1(item);
	if (FD_EMPTY_LISTP(accumulator)) last=accumulator=new;
	else {
	  FD_RPLACD(last,new); last=new;}}}}
  fprintf(stdout,"\n");
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: make-dtype.c,v $
   Revision 1.9  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.8  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

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
