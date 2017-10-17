/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: indextools.h,v 1.12 2006/07/08 23:24:33 haase Exp $

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

  This is the file indextools.h supporting access to file index structures
   for maintenance and analysis programs.

*************************************************************************/

#ifndef FRAMERD_INDEXTOOLS_H
#define FRAMERD_INDEXTOOLS_H 1
#include <framerd/framerd.h>
#include <limits.h>

#define IDX_MAGIC_NUMBER 151913496

struct FD_ASSOC {
  fd_lisp key; int hash, index, n_values;
  unsigned int pos;
  fd_lisp values;};

extern
void fd_write_assocs_to_index
  (FILE *to,struct FD_ASSOC *assocs,int n_keys,int mult);

extern
struct FD_ASSOC *fd_read_assocs_from_index
(FILE *from,int *size,int *read_off,int *multp,
   int with_values,int value_min,int value_max,
   char *tag,int verbose);

extern int fd_copy_assoc_values
  (struct FD_ASSOC *assocs,int n_keys,FILE *from,FILE *to,int pos,int r_off,int verbose);
extern int fd_start_file_index(FILE *to,int n_slots,int mult,fd_lisp,int,time_t,time_t);
extern void fd_write_keys_to_index
   (FILE *to,struct FD_ASSOC *assocs,
    int mult,int n_keys,int new_size,int pos,char *tag);

#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: indextools.h,v $
   Revision 1.12  2006/07/08 23:24:33  haase
   Verbosity controls for index maintenance functions

   Revision 1.11  2004/04/27 17:36:28  haase
   Added verbosity control to file index repacking

   Revision 1.10  2004/03/12 21:18:36  haase
   Extend new indices to indextools etc.

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.2  2003/08/18 12:55:18  haase
   Added conditional includes for indextools.h

   Revision 1.8.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8  2002/06/03 21:51:21  haase
   Progress reports now provide more context

   Revision 1.7  2002/04/22 14:23:07  haase
   Added extended metadata to file pools and indices

   Revision 1.6  2002/04/10 03:01:50  haase
   Added version information to file pools and indices

   Revision 1.5  2002/04/03 01:33:09  haase
   Moved indextools out of FD_SOURCE core

   Revision 1.4  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
