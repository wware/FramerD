/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: search.h,v 1.8 2005/01/14 16:48:44 haase Exp $

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

#ifndef FRAMERD_SEARCH_H
#define FRAMERD_SEARCH_H

#include "framerd/common.h"
#include "framerd/cons.h"


/* Frame searching functions */

FRAMERD_EXPORT fd_lisp fd_get_frame_features(fd_lisp frames);
FRAMERD_EXPORT fd_lisp fd_get_slot_features(fd_lisp frames,fd_lisp slotids);
FRAMERD_EXPORT fd_lisp fd_get_slot_value_features(fd_lisp slotids,fd_lisp values);
FRAMERD_EXPORT void fd_index_frame(fd_index idx,fd_lisp frame);
FRAMERD_EXPORT void fd_index_slots(fd_index idx,fd_lisp frames,fd_lisp slots);
FRAMERD_EXPORT void fd_index_slot_values
  (fd_index idx,fd_lisp frames,fd_lisp slots,fd_lisp values);
FRAMERD_EXPORT
void fd_index_notice_slot_values
  (fd_index idx,fd_lisp frames,fd_lisp slots,fd_lisp values);
FRAMERD_EXPORT fd_lisp fd_find_frames(fd_lisp indices,...);
FRAMERD_EXPORT fd_lisp fd_find_similar(fd_lisp indices,fd_lisp frames,fd_lisp slotids);
FRAMERD_EXPORT fd_lisp fd_strict_search(fd_lisp indices,fd_lisp spec);

FRAMERD_EXPORT
fd_hashtable fd_score_from_spec(fd_lisp indices,fd_lisp frames,fd_lisp spec);
FRAMERD_EXPORT
fd_hashtable fd_score_from_samples
  (fd_lisp indices,fd_lisp frames,fd_lisp samples,fd_lisp slots);

FRAMERD_EXPORT fd_lisp fd_search_get(fd_lisp indices,fd_lisp keys);
FRAMERD_EXPORT int fd_stop_featurep(fd_lisp feature);

/* CNF Tables */

typedef struct FD_CNF_DISJOIN {
  int n_choices, max_choices, total, closed;
  fd_lisp *choices; fd_hashset lookup;} *fd_cnf_disjoin;
struct FD_CNF_TABLE {
  int n_disjoins; struct FD_CNF_DISJOIN **disjoins;};

#endif /* ndef FRAMERD_SEARCH_H */



/* File specific stuff */

/* The CVS log for this file
   $Log: search.h,v $
   Revision 1.8  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/11/21 17:48:21  haase
   Made indexing with explicit values not do automatic expansion

   Revision 1.5  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
