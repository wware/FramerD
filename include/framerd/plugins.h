/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: plugins.h,v 1.4 2003/08/27 10:53:28 haase Exp $

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

#if INIT_FDTEXT
IMPORTED void fd_initialize_fdtext(void);
#define MAYBE_INIT_FDTEXT() fd_initialize_fdtext()
#else
#define MAYBE_INIT_FDTEXT() 
#endif

#if INIT_FDWWW
IMPORTED void fd_initialize_fdwww(void);
#define MAYBE_INIT_FDWWW() fd_initialize_fdwww()
#else
#define MAYBE_INIT_FDWWW() 
#endif

#if INIT_FDANALOGY
IMPORTED void fd_initialize_fdanalogy(void);
#define MAYBE_INIT_FDANALOGY() fd_initialize_fdanalogy()
#else
#define MAYBE_INIT_FDANALOGY() 
#endif







/* File specific stuff */

/* The CVS log for this file
   $Log: plugins.h,v $
   Revision 1.4  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.3.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.3  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
