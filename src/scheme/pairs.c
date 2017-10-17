/* C Mode */

/* pairs.c
   Pairs primitives for FDScript
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

static char vcid[] = "$Id: pairs.c,v 1.9 2005/01/14 16:48:49 haase Exp $";

/* Contents */
/* Basic functions on lists (pair chains) */
/* Pair modification functions */
/* Member functions */
/* Lisp association list functions */

#include "fdscript.h"
#include <time.h>
#include <limits.h>


/* Basic functions on lists (pair chains) */

static lisp make_pair(lisp car,lisp cdr)
{ return FD_MAKE_PAIR(incref(car),incref(cdr)); }

static lisp list_star_handler(lisp args)
{
  lisp answer=FD_EMPTY_LIST, *tail=&answer, scan=args;
  while ((PAIRP(CDR(scan)))) {
    lisp new=make_pair(CAR(scan),FD_EMPTY_LIST);
    *tail=new; tail=&(CDR(new)); scan=CDR(scan);}
  *tail=incref(CAR(scan));
  return answer;
}

static lisp car_cproc(lisp x)
{
  if (PRIM_TYPEP(x,pair_type))
    return incref(CAR(x));
  else fd_type_error(_("not a pair"),x);
}

static lisp cdr_cproc(lisp x)
{
  if (PRIM_TYPEP(x,pair_type))
    return incref(CDR(x));
  else fd_type_error(_("not a pair"),x);
}

static lisp lisp_listref(lisp lst,lisp index)
{
  int i=fd_lisp2int(index);
  DOLIST(elt,lst)
    if (i == 0) return incref(elt);
    else i--;
  fd_raise_lisp_exception
    (fd_Out_Of_Bounds,"LIST-REF: the list didn't have enough elements",lst);
}

static lisp lisp_listtail(lisp lst,lisp index)
{
  int i=fd_lisp2int(index); lisp scan=lst;
  while (PAIRP(scan))
    if (i == 0) return incref(scan);
    else {i--; scan=CDR(scan);}
  fd_raise_lisp_exception
    (fd_Out_Of_Bounds,"List index out of bounds",lst);
}


/* Pair modification functions */

/* Warning: not threadsafe */
static lisp setcar_cproc(lisp x,lisp y)
{
  if (PRIM_TYPEP(x,pair_type)) {
    fd_lisp v=CAR(x);
    RPLACA(x,incref(y));
    fd_decref(v);
    return FD_VOID;}
  else fd_type_error(_("not a pair"),x);
}

/* Warning: not threadsafe */
static lisp setcdr_cproc(lisp x,lisp y)
{
  if (PRIM_TYPEP(x,pair_type)) {
    fd_lisp v=CDR(x);
    RPLACD(x,incref(y));
    fd_decref(v);
    return FD_VOID;}
  else fd_type_error(_("not a pair"),x);
}

typedef struct SORT_ENTRY {int score; lisp obj;} *sort_entry;

static int compare_sort_entries(sort_entry a,sort_entry b)
{
  if (a->score > b->score) return -1;
  else if (a->score == b->score) return 0;
  else return 1;
}

static lisp append(lisp lists)
{
  lisp scan=lists, answer=FD_EMPTY_LIST, *tail=&answer;
  while (PAIRP(scan))
    if (FD_EMPTY_LISTP(CDR(scan)))
      {*tail=incref(CAR(scan)); scan=CDR(scan);}
    else {lisp new=_fd_copy_lisp_proc(CAR(scan)); 
	  *tail=new; while (PAIRP(new))
	    {if (FD_EMPTY_LISTP(CDR(new))) tail=&(CDR(new));
	     new=CDR(new);}
	  if (!(FD_EMPTY_LISTP(new)))
	    fd_type_error(_("not a proper list"),CAR(scan));
	  else scan=CDR(scan);}
  return answer;
}

static lisp sortcar(lisp expr,lispenv env)
{
  lisp set_expr=fd_get_arg(expr,1,FD_VOID);
  lisp the_set=fd_eval_in_env(set_expr,env);
  int i=0, j=0;
  struct SORT_ENTRY *entries=
    fd_malloc(sizeof(struct SORT_ENTRY)*CHOICE_SIZE(the_set));
  lisp answer=FD_EMPTY_LIST;
  {DO_CHOICES(elt,the_set) {
    lisp score=CAR(elt), obj=CDR(elt);
    entries[i].score=FIXLISP(score); entries[i].obj=obj;
    if (!(PRIM_TYPEP(score,fixnum_type))) 
      fd_raise_exception(_("rank is not a fixnum"));
    i++;}
   END_DO_CHOICES;}
  qsort((void *)entries,i,sizeof(struct SORT_ENTRY),
	(int (*)(const void *,const void *))compare_sort_entries);
  j=0; while (j < i) {
    lisp entry=entries[j++].obj;
    lisp pair=FD_MAKE_PAIR(LISPFIX(entries[j].score),incref(entry));
    answer=FD_MAKE_PAIR(pair,answer); j++;}
  fd_free(entries,sizeof(struct SORT_ENTRY)*CHOICE_SIZE(the_set));
  decref(the_set);
  return answer;
}


/* Member functions */

static lisp lisp_isoneof(lisp expr,lispenv env)
{
  lisp elt=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp candidates=fd_get_body(expr,2);
  if (ATOMICP(elt)) {
    DOLIST(poss,candidates) if (LISP_EQ(elt,poss))
      {decref(elt); return FD_TRUE;}
    decref(elt);
    return FD_FALSE;}
  else {
    DOLIST(poss,candidates) if (LISP_EQUAL(elt,poss))
      {decref(elt); return FD_TRUE;}
    decref(elt);
    return FD_FALSE;}
}

static lisp lisp_memberp(lisp elt,lisp lst)
{
  if (ATOMICP(elt))
    {DOLIST(each,lst) if (LISP_EQ(elt,each)) return FD_TRUE;
     return FD_FALSE;}
  else {DOLIST(each,lst) if (LISP_EQUAL(elt,each)) return FD_TRUE;
	return FD_FALSE;}
}

static lisp lisp_member(lisp elt,lisp lst)
{
  lisp scan=lst;
  while (PAIRP(scan))
    if (LISP_EQUAL(CAR(scan),elt)) return incref(scan);
    else scan=CDR(scan);
  return FD_FALSE;
}

static lisp lisp_memq(lisp elt,lisp lst)
{
  lisp scan=lst;
  while (PAIRP(scan))
    if (LISP_EQ(CAR(scan),elt)) return incref(scan);
    else scan=CDR(scan);
  return FD_FALSE;
}

static lisp lisp_memv(lisp elt,lisp lst)
{
  lisp scan=lst;
  if (NUMBERP(elt))
    while (PAIRP(scan))
      if (LISP_EQUAL(CAR(scan),elt)) return incref(scan);
      else scan=CDR(scan);
  else while (PAIRP(scan))
    if (LISP_EQ(CAR(scan),elt)) return incref(scan);
    else scan=CDR(scan);
  return FD_FALSE;
}


/* Lisp association list functions */

static lisp lisp_assoc(lisp elt,lisp lst)
{
  DOLIST(each,lst)
    if (PAIRP(each)) {
      lisp key=fd_car_noref(each);
      if (LISP_EQUAL(key,elt)) return incref(each);}
    else fd_type_error(_("not a well-formed alist"),lst);
  return FD_FALSE;
}

static lisp lisp_assq(lisp elt,lisp lst)
{
  DOLIST(each,lst)
    if (PAIRP(each)) {
      lisp key=CAR(each);
      if (LISP_EQ(key,elt)) return incref(each);}
    else fd_type_error(_("not a well formed alist"),lst);
  return FD_FALSE;
}

static lisp lisp_assv(lisp elt,lisp lst)
{
  if (NUMBERP(elt)) {
    DOLIST(each,lst)
      if (PAIRP(each)) {
	lisp key=CAR(each);
	if (LISP_EQUAL(key,elt)) return incref(each);}
      else fd_type_error(_("not a well-formed alist"),lst);}
  else {
    DOLIST(each,lst)
      if (PAIRP(each)) {
	lisp key=CAR(each);
	if (LISP_EQ(key,elt)) return incref(each);}
      else fd_type_error(_("not a well-formed alist"),lst);}
  return FD_FALSE;
}

void fd_initialize_pairs_c()
{
  fd_add_cproc(NULL,"CONS",2,make_pair);
  fd_add_lexpr(NULL,"LIST*",FD_NORMAL_LEXPR,list_star_handler);
  fd_add_cproc(NULL,"CAR",1,car_cproc);
  fd_add_cproc(NULL,"CDR",1,cdr_cproc);
  fd_add_cproc(NULL,"REST",1,cdr_cproc);
  fd_add_cproc(NULL,"LIST-REF",2,lisp_listref);
  fd_add_cproc(NULL,"LIST-TAIL",2,lisp_listtail);

  fd_add_cproc(NULL,"SET-CAR!",2,setcar_cproc);
  fd_add_cproc(NULL,"SET-CDR!",2,setcdr_cproc);

  fd_add_lexpr(NULL,"APPEND",FD_NORMAL_LEXPR,append);
  fd_add_special_form(NULL,"SORTCAR",sortcar);
  fd_add_cproc(NULL,"LIST->CHOICES",1,fd_list_to_choice);
  fd_add_cproc(NULL,"LIST->SET",1,fd_list_to_choice);

  fd_add_special_form(NULL,"IS-ONE-OF?",lisp_isoneof);
  fd_add_cproc(NULL,"MEMBER?",2,lisp_memberp);
  fd_add_cproc(NULL,"MEMBER",2,lisp_member);
  fd_add_cproc(NULL,"MEMQ",2,lisp_memq);
  fd_add_cproc(NULL,"MEMV",2,lisp_memv);

  fd_add_cproc(NULL,"ASSOC",2,lisp_assoc);
  fd_add_cproc(NULL,"ASSQ",2,lisp_assq);
  fd_add_cproc(NULL,"ASSV",2,lisp_assv);

  fd_register_source_file("pairs",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: pairs.c,v $
   Revision 1.9  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.8  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.7  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.6.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.6  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
