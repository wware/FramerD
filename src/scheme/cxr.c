/* C Mode */

/* cxr.c
   R4RS c[ad]+r primitives for FDScript
   Originally implemented by Ken Haase  and the Machine Understanding Group
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

static char vcid[] = "$Id: cxr.c,v 1.8 2005/01/14 16:48:49 haase Exp $";

#include "fdscript.h"

static lisp carx(lisp x) { return fd_car_noref(x); }
static lisp cdrx(lisp x) { return fd_cdr_noref(x); }

static lisp
caar(lisp c){
  return incref(carx(carx(c)));
}

static lisp
cdar(lisp c){
  return incref(cdrx(carx(c)));
}

static lisp
cadr(lisp c){
  return incref(carx(cdrx(c)));
}

static lisp
cddr(lisp c){
  return incref(cdrx(cdrx(c)));
}

static lisp
caaar(lisp c){
  return incref(carx(carx(carx(c))));
}

static lisp
cdaar(lisp c){
  return incref(cdrx(carx(carx(c))));
}

static lisp
cadar(lisp c){
  return incref(carx(cdrx(carx(c))));
}

static lisp
cddar(lisp c){
  return incref(cdrx(cdrx(carx(c))));
}

static lisp
caadr(lisp c){
  return incref(carx(carx(cdrx(c))));
}

static lisp
cdadr(lisp c){
  return incref(cdrx(carx(cdrx(c))));
}

static lisp
caddr(lisp c){
  return incref(carx(cdrx(cdrx(c))));
}

static lisp
cdddr(lisp c){
  return incref(cdrx(cdrx(cdrx(c))));
}

static lisp
caaaar(lisp c){
  return incref(carx(carx(carx(carx(c)))));
}

static lisp
cdaaar(lisp c){
  return incref(cdrx(carx(carx(carx(c)))));
}

static lisp
cadaar(lisp c){
  return incref(carx(cdrx(carx(carx(c)))));
}

static lisp
cddaar(lisp c){
  return incref(cdrx(cdrx(carx(carx(c)))));
}

static lisp
caadar(lisp c){
  return incref(carx(carx(cdrx(carx(c)))));
}

static lisp
cdadar(lisp c){
  return incref(cdrx(carx(cdrx(carx(c)))));
}

static lisp
caddar(lisp c){
  return incref(carx(cdrx(cdrx(carx(c)))));
}

static lisp
cdddar(lisp c){
  return incref(cdrx(cdrx(cdrx(carx(c)))));
}

static lisp
caaadr(lisp c){
  return incref(carx(carx(carx(cdrx(c)))));
}

static lisp
cdaadr(lisp c){
  return incref(cdrx(carx(carx(cdrx(c)))));
}

static lisp
cadadr(lisp c){
  return incref(carx(cdrx(carx(cdrx(c)))));
}

static lisp
cddadr(lisp c){
  return incref(cdrx(cdrx(carx(cdrx(c)))));
}

static lisp
caaddr(lisp c){
  return incref(carx(carx(cdrx(cdrx(c)))));
}

static lisp
cdaddr(lisp c){
  return incref(cdrx(carx(cdrx(cdrx(c)))));
}

static lisp
cadddr(lisp c){
  return incref(carx(cdrx(cdrx(cdrx(c)))));
}

static lisp
cddddr(lisp c){
  return incref(cdrx(cdrx(cdrx(cdrx(c)))));
}

void fd_initialize_cxr_c()
{
  fd_add_cproc(NULL,"CAAR",1,caar);
  fd_add_cproc(NULL,"CDAR",1,cdar);
  fd_add_cproc(NULL,"CADR",1,cadr);
  fd_add_cproc(NULL,"CDDR",1,cddr);
  fd_add_cproc(NULL,"CAAAR",1,caaar);
  fd_add_cproc(NULL,"CDAAR",1,cdaar);
  fd_add_cproc(NULL,"CADAR",1,cadar);
  fd_add_cproc(NULL,"CDDAR",1,cddar);
  fd_add_cproc(NULL,"CAADR",1,caadr);
  fd_add_cproc(NULL,"CDADR",1,cdadr);
  fd_add_cproc(NULL,"CADDR",1,caddr);
  fd_add_cproc(NULL,"CDDDR",1,cdddr);
  fd_add_cproc(NULL,"CAAAAR",1,caaaar);
  fd_add_cproc(NULL,"CDAAAR",1,cdaaar);
  fd_add_cproc(NULL,"CADAAR",1,cadaar);
  fd_add_cproc(NULL,"CDDAAR",1,cddaar);
  fd_add_cproc(NULL,"CAADAR",1,caadar);
  fd_add_cproc(NULL,"CDADAR",1,cdadar);
  fd_add_cproc(NULL,"CADDAR",1,caddar);
  fd_add_cproc(NULL,"CDDDAR",1,cdddar);
  fd_add_cproc(NULL,"CAAADR",1,caaadr);
  fd_add_cproc(NULL,"CDAADR",1,cdaadr);
  fd_add_cproc(NULL,"CADADR",1,cadadr);
  fd_add_cproc(NULL,"CDDADR",1,cddadr);
  fd_add_cproc(NULL,"CAADDR",1,caaddr);
  fd_add_cproc(NULL,"CDADDR",1,cdaddr);
  fd_add_cproc(NULL,"CADDDR",1,cadddr);
  fd_add_cproc(NULL,"CDDDDR",1,cddddr);
  fd_register_source_file("cxr",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: cxr.c,v $
   Revision 1.8  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.5  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
