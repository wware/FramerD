/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: eval.h,v 1.18 2004/10/18 23:04:22 haase Exp $

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

#ifndef FRAMERD_EVAL_H
#define FRAMERD_EVAL_H

#include "framerd/common.h"
#include "framerd/cons.h"
#include "framerd/os.h"

#ifndef FRAMERD_EXPORT
#define FRAMERD_EXPORT extern
#endif

#define MAX_CPROC_ARGS 8
#define MAX_LET_BINDINGS 32
#ifndef USE_BIGNUMS
#define USE_BIGNUMS 1
#endif


/* FDScript Exceptions */

FRAMERD_EXPORT fd_exception fd_SyntaxShortExpr, fd_SyntaxError,
             fd_TooManyArgs, fd_TooFewArgs, fd_WeirdArgs,
             fd_NotAFunction, fd_FilenameMustBeString, fd_NoSuchFile,
             fd_SetRequiresSymbol, fd_SchemeError;
FRAMERD_EXPORT fd_exception
  fd_StackOverflow,
  fd_UnboundVariable,
  fd_UnboundFunction,
  fd_BadArgList,
  fd_NoSpecialFormApply,
  fd_DLLError,
  fd_BadLambda,
  fd_IntOverflow,
  fd_FlonumOverflow,
  fd_UnknownMethod,
  fd_ModuleNotFound;



/* The Evaluator */

enum MODULE_LOAD_STATUS { unloaded, loading, loaded };

struct FD_MODULE {
  struct FD_HASHTABLE bindings; 
  struct FD_HASHSET exports;
  struct FD_LISPENV *uses[40];
  struct FD_LISPENV *wrapper;
  enum MODULE_LOAD_STATUS load_status;
  fd_lisp changes; int access_bits;
  int n_uses;};

#define FD_SAFE_ENV 1
#define FD_UNSAFE_ENV 0

struct FD_LISPENV {
  int n_refs, n_bindings, max_bindings, mallocd_rib;
  struct FD_LISPENV *parent, *mallocd;
  struct FD_BINDING {fd_lisp var, val;} *rib;
  struct FD_MODULE *module;};

typedef struct FD_LISPENV *fd_lispenv;
typedef fd_lispenv lispenv;

FRAMERD_EXPORT fd_lisp _fd_finalize_static_tail_call(fd_lisp tc,fd_lispenv env);

#define FD_WITH_LEXICAL_ENV(e,pt,init_bindings) \
   {struct FD_LISPENV _env, *e; struct FD_BINDING _rib[init_bindings]; \
    FD_UNWIND_PROTECT { \
      _env.n_refs=-1; _env.n_bindings=0; _env.max_bindings=init_bindings; \
      _env.parent=pt; _env.mallocd=NULL; _env.rib=_rib; _env.module=NULL; \
      _env.mallocd_rib=0; e=&_env;
#define FD_END_WITH_LEXICAL_ENV(result) \
  if (FD_PRIM_TYPEP(result,tail_call_type)) \
    result=_fd_finalize_static_tail_call(result,&_env); \
  } FD_ON_UNWIND { \
      _fd_finish_stack_env(&_env);} \
     FD_END_UNWIND;}
#define FD_END_WITH_LEXICAL_ENV_NOVALUE() \
  } FD_ON_UNWIND { \
      _fd_finish_stack_env(&_env);} \
     FD_END_UNWIND;}

#define FD_ENVIRONMENTP(x) (PRIM_TYPEP(x,env_type))
#define FD_ENVIRONMENT_PTR(x) ((fd_lispenv)FD_CPTR_DATA(x))
#define fd_environment_ptr(x) \
  ((FD_ENVIRONMENTP(x)) ? ((fd_lispenv)FD_CPTR_DATA(x)) : \
   ((fd_type_error(_("not an environment"),x)),(fd_lispenv)NULL))

#define LISP_TEST_FALSEP(x) \
  ((FD_FALSEP(x)) || \
   ((FD_PRIM_TYPEP(x,multiple_value_type)) && \
    ((FD_PTR_DATA(x,vector))->length > 0) && \
    (FD_FALSEP((FD_PTR_DATA((x),vector)->elements)[0]))))

FRAMERD_EXPORT void _fd_finish_stack_env(fd_lispenv env);
FRAMERD_EXPORT fd_lispenv fd_make_env(fd_lispenv parent);
FRAMERD_EXPORT fd_lispenv fd_mallocd_env(fd_lispenv env);
FRAMERD_EXPORT void fd_free_env(fd_lispenv env);

FRAMERD_EXPORT fd_lispenv fd_make_module(void);
FRAMERD_EXPORT void fd_register_module(fd_u8char *name,fd_lispenv m,int access_bits,int needs_loading);
FRAMERD_EXPORT fd_lispenv fd_registered_module(fd_u8char *name,int risky);
FRAMERD_EXPORT void fd_set_module_access(fd_lispenv menv,int bits,int replace);

FRAMERD_EXPORT fd_lispenv fd_get_module(fd_u8char *name);
FRAMERD_EXPORT fd_lispenv fd_load_module(fd_lisp spec);
FRAMERD_EXPORT fd_lispenv fd_reload_module(fd_lisp spec);
FRAMERD_EXPORT void fd_module_uses(fd_lispenv m,fd_lispenv i);
FRAMERD_EXPORT void fd_module_export(fd_lispenv m,fd_lisp sym);

FRAMERD_EXPORT void fd_do_preloads(void);


/* Writing new primitives */

FRAMERD_EXPORT void fd_add_cproc
   (fd_lispenv env,char *name,int n_args,fd_lisp (*proc)());
FRAMERD_EXPORT void fd_add_restricted_cproc
   (char *name,int n_args,fd_lisp (*proc)());
FRAMERD_EXPORT void fd_add_special_form
   (fd_lispenv env,char *name,fd_lisp (*proc)(fd_lisp expr,fd_lispenv env));
FRAMERD_EXPORT void fd_add_restricted_special_form
   (char *name,fd_lisp (*proc)(fd_lisp expr,fd_lispenv env));
FRAMERD_EXPORT void fd_add_lexpr
   (fd_lispenv env,char *name,int argcode,fd_lisp (*proc)(fd_lisp args));
FRAMERD_EXPORT void fd_add_restricted_lexpr
   (char *name,int argcode,fd_lisp (*proc)(fd_lisp args));
FRAMERD_EXPORT void fd_add_alias(fd_lispenv env,char *alias,char *original);

FRAMERD_EXPORT fd_lisp fd_get_arg(fd_lisp expr,int n,fd_lisp dflt);
FRAMERD_EXPORT fd_lisp fd_get_body(fd_lisp expr,int n);
FRAMERD_EXPORT void fd_get_args(fd_u8char *,fd_lisp,...);
FRAMERD_EXPORT fd_lisp fd_get_extended_arg(char *argname,fd_lisp xarg,fd_lisp dflt);

#define FD_SPECIAL_FORM (-17)
#define FD_NORMAL_LEXPR (-37)
#define FD_ND_LEXPR (-42)

#define FD_GET_SPROC(x) \
   ((FD_PRIM_TYPEP(x,ssproc_type)) ? ((fd_sproc)(FD_PTR_DATA(x,ssproc))) : \
    (((FD_PRIM_TYPEP(x,sproc_type)) || (FD_PRIM_TYPEP(x,gproc_type))) ? \
     (FD_PTR_DATA(x,sproc)) : ((fd_sproc)NULL)))

#define FD_SPECIAL_FORMP(x) \
  ((FD_PRIM_TYPEP(x,cproc_type)) && \
   ((FD_PTR_DATA(x,cproc))->n_args == FD_SPECIAL_FORM))
#define FD_NORMAL_LEXPRP(x) \
  ((FD_PRIM_TYPEP(x,cproc_type)) && \
   ((FD_PTR_DATA(x,cproc))->n_args == FD_NORMAL_LEXPR))
#define FD_ND_LEXPRP(x) \
  ((FD_PRIM_TYPEP(x,cproc_type)) && \
   ((FD_PTR_DATA(x,cproc))->n_args == FD_ND_LEXPR))

/* Interaction functions */

FRAMERD_EXPORT void fd_debug_fdscript(int flag);


/* Evaluator functions */

FRAMERD_EXPORT fd_lisp fd_symeval(fd_lisp sym,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_lexical_symeval(fd_lisp sym,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_required_symeval(fd_lisp sym,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_start_eval(fd_lisp expr,lispenv env);
FRAMERD_EXPORT fd_lisp fd_eval_in_env(fd_lisp expr,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_do_application(fd_lisp func,fd_lisp args);
FRAMERD_EXPORT fd_lisp fd_apply(fd_lisp func,fd_lisp args);
FRAMERD_EXPORT fd_lisp fd_lisp_call(fd_lisp func,fd_lisp arg);
FRAMERD_EXPORT fd_lisp fd_finish_value(fd_lisp value);

FRAMERD_EXPORT void fd_set_value(fd_lisp var,fd_lisp val,fd_lispenv env);
FRAMERD_EXPORT void fd_safe_set_value(fd_lisp var,fd_lisp val,fd_lispenv env);
FRAMERD_EXPORT void fd_bind_value(fd_lisp var,fd_lisp val,fd_lispenv env);

FRAMERD_EXPORT fd_lisp fd_mv_ref(fd_lisp val,unsigned int i);
FRAMERD_EXPORT int fd_mv_arity(fd_lisp val);
FRAMERD_EXPORT fd_lisp fd_mv_return(fd_lisp *vals,int len);

FRAMERD_EXPORT fd_hashtable fd_threadenv();
FRAMERD_EXPORT fd_lisp fd_thread_symeval(fd_lisp sym);
FRAMERD_EXPORT void fd_thread_symbind(fd_lisp sym,fd_lisp val);

FRAMERD_EXPORT fd_lisp fd_make_sproc(fd_lisp lambda,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_make_ssproc(fd_lisp lambda,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_make_rproc(fd_server s,fd_lisp op);

FRAMERD_EXPORT void fd_autoload(char *pname,char *file);
FRAMERD_EXPORT void fd_autolink(char *pname,char *file);

FRAMERD_EXPORT int fd_load_dll(char *mname);
FRAMERD_EXPORT char *fd_get_component_file(char *filename);
FRAMERD_EXPORT fd_lisp fd_load_file
  (char *filename,char *enc,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_load_library
  (char *filename,char *enc,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_file_reloader
  (char *filename,char *enc,fd_lispenv env);
FRAMERD_EXPORT void fd_reloader(void);

FRAMERD_EXPORT fd_lisp fd_process_file(char *fname,char *enc,fd_lispenv env);

FRAMERD_EXPORT void fd_set_load_env(fd_lispenv env);
FRAMERD_EXPORT fd_lispenv fd_get_load_env(void);
FRAMERD_EXPORT void fd_set_load_file(FILE *file);
FRAMERD_EXPORT FILE *fd_get_load_file(void);

FRAMERD_EXPORT char *fd_evalstring(char *input,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_eval_elts(fd_lisp lst,fd_lispenv env);
FRAMERD_EXPORT fd_lisp fd_eval(fd_lisp expr);

FRAMERD_EXPORT void fd_set_script_source_file(char *filename);

FRAMERD_EXPORT fd_lisp fd_dtapply
  (fd_lisp rproc,fd_lisp args,fd_lispenv env,int eval_args);

/* Evaluator security functions */

/* This is where symbols which have alternative module
   bindings are defined. */
FRAMERD_EXPORT fd_lispenv fd_global_env;

FRAMERD_EXPORT fd_lispenv fd_enabled_env;
FRAMERD_EXPORT fd_lispenv fd_module_table;
FRAMERD_EXPORT fd_lisp fd_safe_symbol;
FRAMERD_EXPORT void fd_set_stack_limit(unsigned int limit);

/* Record type functions */

FRAMERD_EXPORT int fd_record_typep(fd_lisp x,fd_lisp tag);
FRAMERD_EXPORT void fd_set_supertype(fd_lisp tag,fd_lisp super_tag);


/* String Operations */

FDSCRIPT_EXPORT void fd_string_set(fd_lisp str,int index,fd_unichar_t ch);
FDSCRIPT_EXPORT fd_unichar_t fd_string_ref(fd_lisp string,int index);
FDSCRIPT_EXPORT int fd_string_length(fd_lisp str);


/* Using new type definitions */

FDSCRIPT_EXPORT fd_lisp fd_long2lisp(long n);
FDSCRIPT_EXPORT fd_lisp fd_ulong2lisp(unsigned long n);


/* Utility FASTOPs */

#if (FDEVAL_CORE)
/* Gets the element of expr at start (where 0 gets the first element) */
static lisp get_arg(fd_lisp expr,int start,fd_lisp dflt)
{
  lisp body=expr;
  while ((start > 0) && (PAIRP(body))) {start--; body=CDR(body);}
  if (start == 0) {
    if (PAIRP(body)) {
      fd_lisp v=CAR(body);
      if (FD_QUOTED_EMPTY_CHOICEP(v)) return (FD_EMPTY_CHOICE);
      else if (FD_PRIM_TYPEP(v,quoted_choice_type)) {
        RETURN_LISP(choice_type,choice,PTR_DATA(v,choice));}
      else if (PRIM_TYPEP(v,multiple_value_type)) {
        lisp v1=fd_mv_ref(v,0); decref(v1);
        return v1;}
      else return v;}
    else if (!(FD_VOIDP(dflt))) return dflt;}
  if (!(FD_VOIDP(dflt))) return dflt;
  else if (!(PAIRP(expr)))
    fd_raise_exception(fd_SyntaxShortExpr);
  else fd_raise_detailed_exception
  (fd_SyntaxShortExpr,fd_object_to_string(expr));
}
#else
#define get_arg fd_get_arg
#endif

#if (FDEVAL_CORE)
/* Gets the tail of expr starting at start (where 0 gets the whole list) */
static lisp get_body(fd_lisp expr,int start)
{
  lisp body=expr;
  while ((start > 0) && (PAIRP(body))) {start--; body=CDR(body);}
  if (start == 0) return body;
  else if (!(PAIRP(expr)))
    fd_raise_lisp_exception(fd_SyntaxError,"not a list",expr);
  else {
    lisp head=CAR(expr);
    if (SYMBOLP(head))
      fd_raise_detailed_exception
	(fd_SyntaxShortExpr,fd_object_to_string(expr));
    else fd_raise_detailed_exception
	   (fd_SyntaxShortExpr,fd_object_to_string(expr));}
}
#else
#define get_body fd_get_body
#endif

#if (FDEVAL_CORE)
FASTOP lisp fd_evaluate(fd_lisp expr,fd_lispenv env)
{
  if (PAIRP(expr)) return fd_eval_in_env(expr,env);
  else if (SYMBOLP(expr)) return fd_required_symeval(expr,env);
  else return incref(expr);
}
FASTOP lisp fd_partial_eval(fd_lisp expr,fd_lispenv env)
{
  if (PAIRP(expr)) return fd_start_eval(expr,env);
  else if (SYMBOLP(expr)) return fd_required_symeval(expr,env);
  else return incref(expr);
}
#else
#define fd_evaluate fd_eval_in_env
#define fd_partial_eval fd_start_eval
#endif

#endif /* ndef FRAMERD_EVAL_H */


/* Declarations for accessing foreground, background, etc. */
FRAMERD_EXPORT fd_lisp fd_get_background(void);
FRAMERD_EXPORT fd_lisp fd_get_foreground(void);
FRAMERD_EXPORT fd_lisp fd_get_conspool(void);
FRAMERD_EXPORT fd_lisp fd_lookup_frame(fd_u8char *string);


/* File specific stuff */

/* The CVS log for this file
   $Log: eval.h,v $
   Revision 1.18  2004/10/18 23:04:22  haase
   Made repacking remove its temp files

   Revision 1.17  2004/07/31 14:02:33  haase
   Added fd_do_preloads, fd_load_module, and fixed signature of fd_get/set_load_file to be FILE *

   Revision 1.16  2004/03/10 14:37:56  haase
   Added a generic reloading facility

   Revision 1.15  2003/12/05 14:58:47  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.14  2003/09/07 18:25:18  haase
   Added prototpye for fd_lisp_call

   Revision 1.13  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.12.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.12.2.1  2002/09/26 02:03:27  haase
   Fixed collision between textlet bindings and global bindings by making textlet bindings always check and bind locally

   Revision 1.12  2002/07/18 19:15:17  haase
   Upped max use count for modules

   Revision 1.11  2002/06/24 18:07:54  haase
   Added C interface to foreground, background, and oid name lookup

   Revision 1.10  2002/05/11 13:28:01  haase
   Added library functions for dealing with multiple values

   Revision 1.9  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.8  2002/04/03 18:16:39  haase
   Added READ-4BYTES primitive

   Revision 1.7  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/

