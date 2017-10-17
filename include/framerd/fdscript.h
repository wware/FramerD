/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: fdscript.h,v 1.11 2005/01/14 16:48:44 haase Exp $

  This file is part of FramerD, a representation language and semantic
  database developed by Kenneth B. Haase and his students at the Media
  Laboratory at the Massachusetts Institute of Technology in Cambridge,
  Massachusetts.  Research at the Media Lab is supported by funds and
  equipment from a variety of corporations and government sponsors whose
  contributions are gratefully acknowledged.

    Use, modification, and redistribution of this program is permitted
    under the terms of either (at the developer's discretion) the GNU
    General Public License (GPL) Version 2, the GNU Lesser General Public
    License.

    This program is based on the FramerD library released in Fall 2001 by
    MIT under both the GPL and the LGPL licenses, both of which accompany
    this distribution.  Subsequent modifications by beingmeta, inc. are
    also released under both the GPL and LGPL licenses (at the developer's
    discretion).

*************************************************************************/

#ifndef FRAMERD_FDSCRIPT_H /* If defined, skip the file */
#define FRAMERD_FDSCRIPT_H

#include "framerd/common.h"
#include "framerd/cons.h"
#include "framerd/os.h"

#include "framerd/index.h"
#include "framerd/odb.h"
#include "framerd/search.h"

#include "framerd/eval.h"

/* A few external procedures */

FDSCRIPT_EXPORT fd_lispenv fd_osprims_env;
FDSCRIPT_EXPORT fd_lispenv fd_fdinternals_env;
FDSCRIPT_EXPORT fd_exception fd_Cant_Write_File, fd_Cant_Read_File;
FDSCRIPT_EXPORT fd_exception fd_DefframeExpectationFailure;

FDSCRIPT_EXPORT double fd_to_float(fd_lisp x);
FDSCRIPT_EXPORT fd_lisp fd_default_input_port();
FDSCRIPT_EXPORT fd_lisp fd_default_output_port();

FDSCRIPT_EXPORT void fd_display_string(char *string,fd_lisp ports);
FDSCRIPT_EXPORT void fd_display(fd_lisp x,fd_lisp ports);
FDSCRIPT_EXPORT void fd_display_strings(fd_lisp ports,...);
FDSCRIPT_EXPORT void fd_printout(fd_lisp port,fd_lisp body,fd_lispenv env);
FDSCRIPT_EXPORT fd_lisp fd_stringout(fd_lisp body,fd_lispenv env);

FDSCRIPT_EXPORT void fd_initialize_fdscript(void);

/* Math functions */

FDSCRIPT_EXPORT fd_lisp fd_plus(fd_lisp x,fd_lisp y);
FDSCRIPT_EXPORT fd_lisp fd_minus(fd_lisp x,fd_lisp y);
FDSCRIPT_EXPORT fd_lisp fd_times(fd_lisp x,fd_lisp y);
FDSCRIPT_EXPORT fd_lisp fd_div(fd_lisp x,fd_lisp y);
FDSCRIPT_EXPORT int fd_compare(fd_lisp x,fd_lisp y);

/* URL access functions */

FDSCRIPT_EXPORT void fd_register_url_protocol(char *,fd_lisp (*getter)(char *));
FDSCRIPT_EXPORT fd_lisp fd_urlget(char *);

/* Sequence functions */

FDSCRIPT_EXPORT int fd_seq_length(fd_lisp);
FDSCRIPT_EXPORT fd_lisp fd_seq_elt(fd_lisp,int);

#endif /* FRAMERD_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: fdscript.h,v $
   Revision 1.11  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.10  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.9  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.8  2003/09/09 00:33:17  haase
   Whitespace changes

   Revision 1.7  2003/09/07 18:25:48  haase
   Added API access to sequence length and element operations

   Revision 1.6  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.5  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
