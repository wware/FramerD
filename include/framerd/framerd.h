/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: framerd.h,v 1.7 2005/01/14 16:48:44 haase Exp $

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

#ifndef FRAMERD_FRAMERD_H /* If defined, skip the file */
#define FRAMERD_FRAMERD_H

#include "framerd/common.h"
#include "framerd/cons.h"
#include "framerd/os.h"

#include "framerd/index.h"
#include "framerd/odb.h"
#include "framerd/search.h"

#ifndef FD_TRACE_SYNCHRONIZATION
#define FD_TRACE_SYNCHRONIZATION 0
#endif

FRAMERD_EXPORT void fd_initialize_framerd(void);

FRAMERD_EXPORT void fd_autosync_pool(fd_pool p);
FRAMERD_EXPORT void fd_autosync_index(fd_index ix);
FRAMERD_EXPORT int fd_autosync_all(int flag);
FRAMERD_EXPORT void fd_autosync(void);

#endif /* FRAMERD_FRAMERD_H */




/* File specific stuff */

/* The CVS log for this file
   $Log: framerd.h,v $
   Revision 1.7  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2004/02/14 14:53:18  haase
   Added autosync functions for declaring pools and indices to be automatically synchronized when fd_autosync() is called

   Revision 1.4  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.3.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.3  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
