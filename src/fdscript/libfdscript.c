/* C Mode */

/* libfdscript.c
   Implements init function for FDScript, including primitives
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

static char vcid[]
  = "$Id: libfdscript.c,v 1.7 2005/01/14 16:48:46 haase Exp $";

#include "fdscript.h"

#ifndef OTHER_INITS
#define OTHER_INITS
#endif

static int fdscript_initialized=0;

extern void fd_initialize_fdscheme(void);
extern void fd_initialize_prims_c(void);
extern void fd_initialize_seq_c(void);
extern void fd_initialize_osprims_c(void);
extern void fd_initialize_printout_c(void);
extern void fd_initialize_hashprims_c(void);
extern void fd_initialize_maint_c(void);
extern void fd_initialize_framerd_c(void);

FDSCRIPT_EXPORT void fd_initialize_fdscript()
{
  if (fdscript_initialized) return; else fdscript_initialized=1;
  fd_initialize_fdscheme();
  fd_initialize_prims_c();
  fd_initialize_seq_c();
  fd_initialize_printout_c();
  fd_initialize_osprims_c();
  fd_initialize_hashprims_c();
  fd_initialize_framerd_c();
  fd_initialize_maint_c();
}

#if ((defined(WIN32)) && (!(defined(STATICLINK))))
BOOL APIENTRY DllMain( HANDLE hModule, 
                        DWORD ul_reason_for_call, 
                        LPVOID lpReserved )
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
      fd_initialize_fdscript(); break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      {}
    }
    return TRUE;
}
#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: libfdscript.c,v $
   Revision 1.7  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
