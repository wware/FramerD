/* C Mode */

/* dtcall.c
   Command line program to evaluate an expression on a remote DType server.
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

static char vcid[] = "$Id: dtcall.c,v 1.8 2005/01/14 16:48:45 haase Exp $";

#include "framerd/dtypes.h"

static fd_u8char *read_stdin_as_string()
{
  struct FD_STRING_STREAM ss; int c;
  FD_INITIALIZE_STRING_STREAM(&ss,1024);
  while ((c=fd_fgetc(stdin)) >= 0) fd_sputc(&ss,c);
  return ss.ptr;
}

static fd_lisp read_stdin_as_lisp()
{
  fd_lisp answer=FD_EMPTY_CHOICE;
  while (1) {
    fd_lisp elt=fd_parse_lisp_from_stream(stdin);
    if (FD_EOF_OBJECTP(elt)) return answer;
    else {FD_ADD_TO_CHOICE(answer,elt);}}
}

static fd_lisp stdin_args;
static char *stdin_string;

int main(int argc,char *argv[])
{
  fd_server s; fd_lisp quote_symbol, first;

  if ((argc < 3)) {
    fprintf(stderr,_("Usage: dtcall <server> <operation> [args]*\n")); 
    return -1;}
  fd_inhibit_herald(1);
  fd_initialize_dtypes();
  stdin_args=FD_VOID;
  stdin_string=NULL;
  quote_symbol=fd_make_symbol("QUOTE");
  s=fd_connect(argv[1]);

  /* Parse args */
  {
    fd_lisp last; int i=3; fd_lisp arg, new; 
    first=FD_MAKE_LIST1(fd_parse_string(argv[2])); last=first;
    while (i < argc) {
      if (strcmp(argv[i],"-") == 0)
	if (FD_VOIDP(stdin_args))
	  if (stdin_string)
	    fd_warn(_("Can't use stdin as both lisp and string"));
	  else arg=stdin_args=read_stdin_as_lisp();
	else arg=fd_incref(stdin_args);
      else if (strcmp(argv[i],"$") == 0) {
	if (stdin_string == NULL)
	  if (FD_VOIDP(stdin_args)) {
	    stdin_string=read_stdin_as_string();
	    arg=fd_make_string(stdin_string);}
	  else fd_warn(_("Can't use stdin as both lisp and string"));
	else arg=fd_make_string(stdin_string);}
      else arg=fd_parse_arg(argv[i]);
      if ((FD_SYMBOLP(arg)) || (FD_PAIRP(arg))) new=fd_cons("('q)",arg);
      else new=fd_cons("(q)",arg);
      FD_RPLACD(last,new); last=new; i=i+1;}
  }

  /* Remote eval and pretty print values, one to a line */
  {
    fd_lisp result=fd_dtype_eval(first,s);
    FD_DO_CHOICES(r,result) {
      fd_pprint_lisp(r,stdout,70); printf("\n");}
    END_FD_DO_CHOICES;
    fd_decref(result);
  }
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: dtcall.c,v $
   Revision 1.8  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.5  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
