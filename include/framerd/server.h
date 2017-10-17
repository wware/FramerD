/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: server.h,v 1.7 2004/01/09 17:42:47 haase Exp $

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

extern fd_set fd_open_tcp_server_socket(char *spec,int port);
extern fd_set fd_open_local_server_socket(char *fname);
extern fd_set fd_open_server_socket(fd_u8char *spec);
extern void fd_start_server
 (fd_set server_sockets,fd_lisp (*server_eval)(fd_lisp,int *,fd_client));
extern void fd_set_server_log(FILE *slog,FILE *tlog);
extern void fd_use_localhost(void);
extern fd_client fd_current_client(void);

extern void fd_set_client_validator(int (*vfcn)(char *hostname));
extern int fd_label_client(fd_lisp label);
extern fd_lisp fd_list_clients();



/* File specific stuff */

/* The CVS log for this file
   $Log: server.h,v $
   Revision 1.7  2004/01/09 17:42:47  haase
   Rename client to fd_client for server code

   Revision 1.6  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.5  2002/04/02 21:41:09  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
