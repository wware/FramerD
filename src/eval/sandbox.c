/* C Mode */

/* sandbox.c
   FDScript security functions
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

static char vcid[] = "$Id: sandbox.c,v 1.8 2005/01/14 16:48:45 haase Exp $";

/** Safety/security definitions **/
/** Safety Model **/
/** Safe binding and modifying constructs **/
/** Initialization **/

#include "fdeval.h"

/** Safety definitions **/

static lisp lambda_symbol;
fd_lispenv fd_enabled_env;

FRAMERD_EXPORT
/* fd_add_restricted_cproc:
     Arguments: a string, an int, and a function pointer
     Returns: void
  Defines a primitive procedure in the standard restricted environment. */
void fd_add_restricted_cproc(char *name,int n_args,lisp (*proc)())
{
  fd_add_cproc(fd_enabled_env,name,n_args,proc);
}
FRAMERD_EXPORT
/* fd_add_restricted_special_form:
     Arguments: a string, an int, and a function pointer
     Returns: void
  Defines a primitive special form in the standard restricted environment. */
void fd_add_restricted_special_form
  (char *name,lisp (*proc)(lisp expr,lispenv env))
{
  fd_add_special_form(fd_enabled_env,name,proc);
}
FRAMERD_EXPORT
/* fd_add_restricted_lexpr:
     Arguments: a string, an int, and a function pointer
     Returns: void
  Defines a primitive lexpr in the standard restricted environment. */
void fd_add_restricted_lexpr(char *name,int argcode,lisp (*proc)(lisp args))
{
  fd_add_lexpr(fd_enabled_env,name,argcode,proc);
}

/** Safe lambdas **/

static lisp sandbox_lambda_handler(lisp expr,fd_lispenv env)
{
  lisp lambda=FD_MAKE_PAIR(lambda_symbol,incref(CDR(expr)));
  return fd_make_sproc(lambda,NULL);
}

static lisp dump_sandbox_lambda(lisp arg)
{
  if (PRIM_TYPEP(arg,sproc_type)) {
    fd_sproc sproc=PTR_DATA(arg,sproc);
    if (sproc->env == NULL)
      return incref(sproc->lambda);
    else fd_raise_lisp_exception(fd_NoDTypeRep,"sandbox",arg);}
  else fd_raise_lisp_exception(fd_NoDTypeRep,"sandbox",arg);
}
static lisp restore_sandbox_lambda(lisp arg)
{
  return sandbox_lambda_handler(arg,NULL);
}

/** Initialization **/

void fd_initialize_sandbox_c()
{
  struct FD_TYPE_REGISTRY *r=fd_register_typecode(sproc_type);
  fd_lisp lispenv_ptr;

  fd_enabled_env=fd_make_module();
  lispenv_ptr=fd_make_cptr(env_type,fd_mallocd_env(fd_enabled_env));

  /* For some reason, just calling fd_register_restricted_module
     (as listed below in a comment) causes real problems when
     compiling with optimization.  So we do the installation of
     the module by hand, into both names FDSCRIPT and FDSCRIPT-MODULE */
  /* fd_register_restricted_module("FDSCRIPT-MODULE",fd_enabled_env); */
  fd_set_value(fd_make_symbol("FDSCRIPT-MODULE"),
	       fd_make_cptr(env_type,fd_mallocd_env(fd_enabled_env)),
	       fd_enabled_env);
  fd_module_export(fd_enabled_env,fd_make_symbol("FDSCRIPT-MODULE"));
  fd_set_value(fd_make_symbol("FDSCRIPT"),
	       fd_make_cptr(env_type,fd_mallocd_env(fd_enabled_env)),
	       fd_enabled_env);
  fd_module_export(fd_enabled_env,fd_make_symbol("FDSCRIPT"));

  lambda_symbol=fd_make_symbol("LAMBDA");
  r->compound_tag=lambda_symbol;
  r->compound_dump_fcn=dump_sandbox_lambda;
  r->compound_restore_fcn=restore_sandbox_lambda;

  fd_add_special_form(NULL,"SANDBOX",sandbox_lambda_handler);

  fd_register_source_file("sandbox",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: sandbox.c,v $
   Revision 1.8  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.7  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.6  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.5  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.4.2.1  2003/01/26 20:56:05  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.4  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
