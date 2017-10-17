/* C Mode */

/* xmleval.c
   Implements an XML interpreter using callouts to FDScript.

   Copyright (C) 2003 beingmeta, inc. (A Delaware Corporation)

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

static char vcid[] = "$Id: xmleval.c,v 1.16 2005/03/20 14:24:41 haase Exp $";

#define FD_INLINE_STRING_OPS 1
#define FD_INLINE_CHARACTER_OPS 1
#define FD_SOURCE 1
#define HTMLGEN_EXPORT EXPORTED
#include "fdwww.h"
#include "framerd/fdtext.h"

static fd_lisp id_symbol, bind_symbol, counter_symbol, expander_symbol;
static fd_lisp seq_symbol, choice_symbol, sep_symbol, env_symbol;
static fd_lisp test_symbol, else_symbol, elif_symbol, expr_symbol;
static fd_lisp key_symbol, case_symbol, value_symbol;
fd_lispenv fd_xmleval_env;

#define finish_value(x) \
  while (PRIM_TYPEP(x,tail_call_type)) x=fd_finish_value(x);

/* Handler lookup */

/* We're breaking an abstraction barrier here by
   accessing the module slot of the env structure.
   It may mean more work if we change how environments are
   implemented. */
static fd_lisp get_exported(fd_lisp sym,fd_lispenv env)
{
  fd_lispenv scan=env; struct FD_MODULE *m=scan->module;
  while ((m == NULL) && (scan != NULL)) {
    if (scan=scan->parent) m=scan->module;}
  if ((m) && (fd_hashset_get(&(m->exports),sym)))
    return fd_symeval(sym,env);
  else if (m) {
    int i=0, lim=m->n_uses; while (i < lim)
      if (fd_hashset_get(&(m->uses[i]->module->exports),sym))
	return fd_symeval(sym,env);
      else i++;
    return FD_VOID;}
  else return FD_VOID;
}

HTMLGEN_EXPORT
/* fd_get_xml_handler:
    Arguments: an XML tag, a LISP environment, and a C pointer to a lisp pointer
    Returns: 1 or 0 if a handler is found
    A handler found is stored in the lisp pointer, and it has been incref'd */
int fd_get_xml_handler(fd_lisp tag,fd_lispenv env,fd_lisp *handler)
{
  fd_lisp ns=fd_xmltag_namespace(tag);
  fd_lisp name=fd_xmltag_name(tag), namesym, v;
  fd_lispenv lookup_env;
  if (FD_FALSEP(ns)) lookup_env=env;
  else if (FD_SYMBOLP(ns)) {
    lookup_env=fd_get_module(FD_SYMBOL_NAME(ns));}
  else lookup_env=NULL;
  if (lookup_env == NULL) return 0;
  if (FD_SYMBOLP(name)) namesym=name;
  else if (FD_STRINGP(name))
    namesym=fd_intern(FD_STRING_DATA(name),FD_STRING_LENGTH(name));
  else return 0;
  /* Now do the lookup */
  v=get_exported(namesym,lookup_env);
  if (FD_VOIDP(v)) {
    fd_lisp dflt=fd_symeval(namesym,fd_xmleval_env);
    if (FD_VOIDP(dflt)) return 0;
    else {*handler=dflt; return 1;}}
  else {*handler=v; return 1;}
}


/* Interpreting XML data */

static fd_lisp process_xmlattrib(fd_lisp arg,fd_lispenv env)
{
  if ((FD_STRINGP(arg)) && (FD_STRING_DATA(arg)[0] == '$')) {
    fd_u8char *s=FD_STRING_DATA(arg)+1;
    fd_lisp expr=fd_parse_lisp_from_string(&s);
    fd_lisp result=fd_eval_in_env(expr,env);
    fd_decref(expr); return result;}
  else return fd_incref(arg);
}

static int tag_matchp(fd_lisp key,fd_lisp y)
{
  if (FD_LISP_EQ(key,y)) return 1;
  else {
    fd_lisp tagname=fd_xmltag_name(y);
    if ((FD_SYMBOLP(key)) && (FD_SYMBOLP(tagname)))
      return (FD_LISP_EQ(key,tagname));
    else {
      fd_u8char *kstring, *tstring; int result;
      if (FD_SYMBOLP(key)) kstring=FD_SYMBOL_NAME(key);
      else if (FD_STRINGP(key)) kstring=FD_STRING_DATA(key);
      else fd_type_error("XML tag key",key);
      if (FD_SYMBOLP(tagname)) tstring=FD_SYMBOL_NAME(tagname);
      else if (FD_STRINGP(tagname)) tstring=FD_STRING_DATA(tagname);
      else fd_type_error("XML tag key",tagname);
      result=((strcasecmp(tstring,kstring) == 0));
      fd_decref(tagname);
      return result;}}
}

static fd_lisp get_bind_attrib(fd_lisp xmlpr,fd_lisp sym)
{
  fd_lisp attrib_list=fd_get_arg(xmlpr,1,FD_VOID);
  while (FD_PAIRP(attrib_list))
    if (FD_PAIRP(FD_CAR(attrib_list))) {
      fd_lisp name=(FD_CAR(FD_CAR(attrib_list)));
      if (tag_matchp(sym,name)) {
	fd_lisp aval=FD_CAR(FD_CDR(FD_CAR(attrib_list)));
	return fd_intern(fd_strdata(aval),fd_strlen(aval));}
      else attrib_list=FD_CDR(attrib_list);}
    else attrib_list=FD_CDR(attrib_list);
  return FD_VOID;
}

static fd_lisp xml_getarg(fd_lisp arg,fd_lisp xmlpr,fd_lispenv env)
{
  fd_lisp results=FD_EMPTY_CHOICE;
  fd_lisp attribs=fd_xml_attributes(xmlpr);
  fd_lisp content=fd_xml_content(xmlpr);
  fd_lisp argname=((FD_PAIRP(arg)) ? (FD_CAR(arg)) : arg);
  { /* First check for attrib matchings */
    FD_DOLIST(pair,attribs) {
      if (FD_PAIRP(pair)) 
	if (tag_matchp(argname,FD_CAR(pair))) {
	  FD_ADD_TO_CHOICE(results,process_xmlattrib(FD_CAR(FD_CDR(pair)),env));}}}
  { /* Then look for content */
      fd_lisp answer=FD_EMPTY_CHOICE;
      FD_DOLIST(item,content)
	if (FD_PAIRP(item)) {
	  if (tag_matchp(argname,FD_CAR(item))) {
	    fd_lisp content=fd_xml_content(item);
	    if ((FD_PAIRP(content)) && (FD_EMPTY_LISTP(FD_CDR(content))) &&
		(FD_STRINGP(FD_CAR(content)))) {
	      FD_ADD_TO_CHOICE(results,process_xmlattrib(FD_CAR(content),env));}
	    else {
	      FD_ADD_TO_CHOICE(results,fd_incref(content));}
	    fd_decref(content);}}
	else {}}
  fd_decref(attribs); fd_decref(content);
  if (FD_EMPTYP(results))
    if (FD_PAIRP(arg)) {
      fd_lisp dflt=fd_get_arg(arg,1,FD_VOID);
      return fd_incref(dflt);}
    else return results;
  else return results;
}

static fd_lisp xml_getarg_cproc(fd_lisp xpr,fd_lisp elt_name,fd_lisp env)
{
  if (FD_FALSEP(env)) return xml_getarg(xpr,elt_name,NULL);
  else if (FD_PRIM_TYPEP(env,env_type))
    return xml_getarg(elt_name,xpr,(fd_lispenv)FD_CPTR_DATA(env));
  else return xml_getarg(elt_name,xpr,NULL);
}

static void execute_content(fd_lisp xml_elt,fd_lispenv env,fd_htstream *hs)
{
  fd_lisp content=fd_xml_content(xml_elt);
  FD_DOLIST(elt,content)
    if (FD_PAIRP(elt))
      fd_unparse_xml(elt,env,hs);
    else if (FD_STRINGP(elt)) fd_http_puts(FD_STRING_DATA(elt),hs);
    else fd_html_write(elt);
}

HTMLGEN_EXPORT
/* fd_xml_callout:
    Arguments: a handler procedure (a LISP pointer), an XML expression, and a LISP environment
    Returns: a LISP value
 Uses handler to process the XML expression within the specified environment.
 Evaluation rules proceed as follows:
   Special forms are applied directly to the XML expression and the environment
   Pairs of the form (EXPANDER . proc) apply proc to the XML expression and evaluate
    the result in the environment
   Compound procedures are called by extracting each argument to the procedure from
    the attributes or elements of the XML expression; if there is a dotted (&rest) argument,
    it is bound to the entire expression.
*/
fd_lisp fd_xml_callout (fd_lisp handler,fd_lisp expr,fd_lispenv env)
{
  if (PRIM_TYPEP(handler,cproc_type)) 
    if (FD_SPECIAL_FORMP(handler)) {
      fd_cproc c=FD_PTR_DATA(handler,cproc);
      return ((lisp (*)(lisp, lispenv)) c->func)(expr,env);}
    else fd_type_error("XML callout",handler);
  else if ((FD_PAIRP(handler)) && (FD_LISP_EQ(FD_CAR(handler),expander_symbol))) {
    fd_lisp expander=FD_CDR(handler);
    fd_lisp expansion=fd_lisp_call(expander,expr);
    fd_lisp result=fd_eval_in_env(expansion,env);
    fd_decref(expansion);
    return result;}
  else if (FD_XPROCP(handler)) {
    fd_sproc s=FD_GET_SPROC(handler);
    fd_lisp lambda=s->lambda, formals=CAR(CDR(lambda));
    fd_lisp args=formals, tail_call, bind=get_bind_attrib(expr,id_symbol), result;
    int i, call_length=1; 
    struct FD_VECTOR *cvec;
    while (FD_PAIRP(args)) {call_length++; args=FD_CDR(args);}
    if (FD_SYMBOLP(args)) call_length++;
    cvec=fd_malloca(struct FD_VECTOR);
    cvec->elements=fd_malloc(sizeof(lisp)*call_length);
    cvec->length=call_length;
    cvec->elements[0]=fd_incref(handler);
    i=1; args=formals;
    while (FD_PAIRP(args))
      if (FD_LISP_EQ(FD_CAR(args),env_symbol)) {
	cvec->elements[i]=fd_make_cptr(env_type,fd_mallocd_env(env));
	i++; args=FD_CDR(args);}
      else {
	cvec->elements[i]=xml_getarg(FD_CAR(args),expr,env);
	if (FD_CHOICEP(cvec->elements[i]))
	  FD_PTR_TYPE(cvec->elements[i])=quoted_choice_type;
	i++; args=FD_CDR(args);}
    if (FD_SYMBOLP(args)) cvec->elements[i]=fd_incref(expr);
    FD_SET_PRIM_TYPE(tail_call,tail_call_type);
    FD_SET_PTR_DATA(tail_call,vector,cvec);
    result=tail_call; finish_value(result);
    if (FD_SYMBOLP(bind)) {
      fd_bind_value(bind,result,env);
      fd_decref(result);
      return FD_VOID;}
    else return result;}
}

/* Core functions */

static fd_lisp parse_arg(fd_u8char *string)
{
  if (*string == '$') return fd_parse_string(string+1);
  else return fd_parse_string(string);
}

static fd_lisp xml_eval_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_htstream *hs=fd_get_http_output();
  fd_lisp id_arg=xml_getarg(id_symbol,xpr,env);
  fd_lisp expr_args=xml_getarg(expr_symbol,xpr,env);
  if (FD_EMPTYP(expr_args)) {
    fd_lisp content=fd_xml_content(xpr);
    FD_DOLIST(expr_string,content) {
      if (FD_STRINGP(expr_string)) {
	fd_lisp expr=parse_arg(fd_strdata(expr_string));
	fd_lisp val=fd_eval_in_env(expr,env);
	fd_html_write(val);
	fd_decref(val); fd_decref(expr);}
      else {}}
    fd_decref(content);}
  else {
    FD_DO_CHOICES(expr_arg,expr_args)
      if (FD_STRINGP(expr_arg)) {
	fd_lisp expr=parse_arg(fd_strdata(expr_arg));
	fd_lisp val=fd_eval_in_env(expr,env);
	fd_html_write(val);
	fd_decref(val); fd_decref(expr);}
      else {}
    FD_END_DO_CHOICES;
    fd_decref(expr_arg);}
    return FD_VOID;
}
static fd_lisp xml_seval_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_htstream *hs=fd_get_http_output();
  fd_lisp id_arg=xml_getarg(id_symbol,xpr,env);
  fd_lisp expr_args=xml_getarg(expr_symbol,xpr,env);
  if (FD_EMPTYP(expr_args)) {
    fd_lisp content=fd_xml_content(xpr);
    FD_DOLIST(expr_string,content) {
      if (FD_STRINGP(expr_string)) {
	fd_lisp expr=parse_arg(fd_strdata(expr_string));
	fd_lisp val=fd_eval_in_env(expr,env);
	if (FD_STRINGP(val)) fd_http_puts(FD_STRING_DATA(val),hs);
	else fd_html_write(val);
	fd_decref(val); fd_decref(expr);}
      else {}}
    fd_decref(content);}
  else {
    FD_DO_CHOICES(expr_arg,expr_args)
      if (FD_STRINGP(expr_arg)) {
	fd_lisp expr=parse_arg(fd_strdata(expr_arg));
	fd_lisp val=fd_eval_in_env(expr,env);
	if (FD_STRINGP(val)) fd_http_puts(FD_STRING_DATA(val),hs);
	else fd_html_write(val);
	fd_decref(val); fd_decref(expr);}
      else {}
    FD_END_DO_CHOICES;
    fd_decref(expr_arg);}
    return FD_VOID;
}

/* Iteration */

static void xmleval_iter_step
  (fd_lisp content,fd_lispenv env,int i,fd_htstream *hs)
{
  FD_DOLIST(elt,content)
    if (FD_PAIRP(elt))
      if (tag_matchp(sep_symbol,fd_xml_tag(elt)))
	if (i == 0) {} else fd_unparse_xml(elt,env,hs);
      else fd_unparse_xml(elt,env,hs);
    else if (FD_STRINGP(elt)) fd_http_puts(FD_STRING_DATA(elt),hs);
    else fd_html_write(elt);
}

static fd_lisp xml_forseq_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp elt_var=get_bind_attrib(xpr,bind_symbol);
  fd_lisp ivar=get_bind_attrib(xpr,counter_symbol);
  fd_lisp sequence=xml_getarg(seq_symbol,xpr,env);
  fd_lisp content=fd_xml_content(xpr);
  fd_htstream *hs=fd_get_http_output();
  int i=0, len=fd_seq_length(sequence); int bind_i=1;
  FD_WITH_LEXICAL_ENV(doseq_env,env,2) {
    if (!(FD_SYMBOLP(ivar))) bind_i=0;
    fd_bind_value(elt_var,FD_VOID,doseq_env);
    if (bind_i)
      fd_bind_value(ivar,FD_VOID,doseq_env);
    while (i < len) {
      fd_lisp selt=fd_seq_elt(sequence,i);
      fd_set_value(elt_var,selt,doseq_env);
      fd_decref(selt);
      if (bind_i)
	fd_set_value(ivar,FD_LISPFIX(i),doseq_env);
      xmleval_iter_step(content,doseq_env,i,hs);
      i++;}}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
  fd_decref(sequence);
  return FD_VOID;
}

static fd_lisp xml_forchoices_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp elt_var=get_bind_attrib(xpr,bind_symbol);
  fd_lisp ivar=get_bind_attrib(xpr,counter_symbol);
  fd_lisp choice=xml_getarg(seq_symbol,xpr,env);
  fd_lisp content=fd_xml_content(xpr);
  fd_htstream *hs=fd_get_http_output();
  int i=0; int bind_i=1;
  FD_WITH_LEXICAL_ENV(dochoice_env,env,2) {
    if (!(FD_SYMBOLP(ivar))) bind_i=0;
    fd_bind_value(elt_var,FD_VOID,dochoice_env);
    if (bind_i)
      fd_bind_value(ivar,FD_VOID,dochoice_env);
    {FD_DO_CHOICES(val,choice) {
      fd_set_value(elt_var,val,dochoice_env);
      if (bind_i)
	fd_set_value(ivar,FD_LISPFIX(i),dochoice_env);
      xmleval_iter_step(content,dochoice_env,i,hs);
      i++;}
    FD_END_DO_CHOICES;}}
  FD_END_WITH_LEXICAL_ENV_NOVALUE();
  fd_decref(choice);
  return FD_VOID;
}

static fd_lisp xml_iter_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp elt_var=get_bind_attrib(xpr,bind_symbol);
  fd_lisp ivar=get_bind_attrib(xpr,counter_symbol);
  fd_lisp content=fd_xml_content(xpr);
  fd_htstream *hs=fd_get_http_output();
  fd_lisp sequence=xml_getarg(seq_symbol,xpr,env);
  if (FD_EMPTYP(sequence)) {
    fd_lisp choice=xml_getarg(choice_symbol,xpr,env);
    int i=0; int bind_i=1;
    FD_WITH_LEXICAL_ENV(dochoice_env,env,2) {
      if (!(FD_SYMBOLP(ivar))) bind_i=0;
      fd_bind_value(elt_var,FD_VOID,dochoice_env);
      if (bind_i)
	fd_bind_value(ivar,FD_VOID,dochoice_env);
      {FD_DO_CHOICES(val,choice) {
	fd_set_value(elt_var,val,dochoice_env);
	if (bind_i)
	  fd_set_value(ivar,FD_LISPFIX(i),dochoice_env);
	xmleval_iter_step(content,dochoice_env,i,hs);
	i++;}
      FD_END_DO_CHOICES;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    fd_decref(choice);
    return FD_VOID;}
  else {
    int i=0, len=fd_seq_length(sequence); int bind_i=1;
    FD_WITH_LEXICAL_ENV(doseq_env,env,2) {
      if (!(FD_SYMBOLP(ivar))) bind_i=0;
      fd_bind_value(elt_var,FD_VOID,doseq_env);
      if (bind_i)
	fd_bind_value(ivar,FD_VOID,doseq_env);
      while (i < len) {
	fd_lisp selt=fd_seq_elt(sequence,i);
	fd_set_value(elt_var,selt,doseq_env);
	fd_decref(selt);
	if (bind_i)
	  fd_set_value(ivar,FD_LISPFIX(i),doseq_env);
	xmleval_iter_step(content,doseq_env,i,hs);
	i++;}}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();
    fd_decref(sequence);
    return FD_VOID;}
}

/* Conditional XML expressions */

static int execute_test(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp method_string=fd_xml_get(xpr,test_symbol);
  fd_lisp method_expr=fd_parse_string(fd_strdata(method_string));
  fd_lisp test_result=fd_eval_in_env(method_expr,env);
  if ((FD_FALSEP(test_result)) || (FD_EMPTYP(test_result))) {
    fd_decref(method_string); fd_decref(method_expr);
    return 0;}
  else {
    fd_decref(method_string); fd_decref(method_expr);
    fd_decref(test_result);
    return 1;}
}

static fd_lisp xml_if_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp content=fd_xml_content(xpr);
  if (execute_test(xpr,env)) {
    FD_DOLIST(item,content)
      if (FD_PAIRP(item)) {
	fd_lisp tag=fd_xml_tag(item);
	if ((tag_matchp(else_symbol,tag)) ||
	    (tag_matchp(elif_symbol,tag))) {}
	else fd_unparse_xml(item,env,fd_get_http_output());
	fd_decref(tag);}
    else fd_unparse_xml(item,env,fd_get_http_output());}
  else {
    FD_DOLIST(item,content)
      if (!(FD_PAIRP(item))) {}
      else {
	fd_lisp tag=fd_xml_tag(item);
	if (tag_matchp(else_symbol,tag)) {
	  fd_lisp else_contents=fd_xml_content(item);
	  fd_htstream *hs=fd_get_http_output();
	  FD_DOLIST(e,else_contents) fd_unparse_xml(e,env,hs);
	  fd_decref(else_contents); fd_decref(tag);}
	else if (tag_matchp(elif_symbol,tag)) {
	  xml_if_handler(item,env);
	  fd_decref(tag);}
	else {}}}
  fd_decref(content);
  return FD_VOID;
}

static int execute_deftest(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp var_string=fd_xml_get(xpr,test_symbol);
  fd_lisp var_name=fd_parse_string(fd_strdata(var_string));
  fd_lisp test_result=fd_symeval(var_name,env);
  if ((FD_FALSEP(test_result)) ||
      (FD_EMPTYP(test_result)) ||
      (FD_VOIDP(test_result))) {
    fd_decref(var_string); fd_decref(var_name);
    return 0;}
  else {
    fd_decref(var_string); fd_decref(var_name);
    fd_decref(test_result);
    return 1;}
}

static fd_lisp xml_ifdef_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp content=fd_xml_content(xpr);
  if (execute_deftest(xpr,env)) {
    FD_DOLIST(item,content)
      if (FD_PAIRP(item)) {
	fd_lisp tag=fd_xml_tag(item);
	if ((tag_matchp(else_symbol,tag)) ||
	    (tag_matchp(elif_symbol,tag))) {}
	else fd_unparse_xml(item,env,fd_get_http_output());
	fd_decref(tag);}
    else fd_unparse_xml(item,env,fd_get_http_output());}
  else {
    FD_DOLIST(item,content)
      if (!(FD_PAIRP(item))) {}
      else {
	fd_lisp tag=fd_xml_tag(item);
	if (tag_matchp(else_symbol,tag)) {
	  fd_lisp else_contents=fd_xml_content(item);
	  fd_htstream *hs=fd_get_http_output();
	  FD_DOLIST(e,else_contents) fd_unparse_xml(e,env,hs);
	  fd_decref(else_contents); fd_decref(tag);}
	else if (tag_matchp(elif_symbol,tag)) {
	  xml_if_handler(item,env);
	  fd_decref(tag);}
	else {}}}
  fd_decref(content);
  return FD_VOID;
}

/* XML SWITCH */

static int xml_case_compare(fd_lisp key,fd_lisp value)
{
  if (FD_LISP_EQUAL(key,value)) return 1;
  else if ((FD_STRINGP(key)) && (FD_SYMBOLP(value)))
    return (strcmp(FD_STRING_DATA(key),FD_SYMBOL_NAME(value)) == 0);
  else if ((FD_STRINGP(value)) && (FD_SYMBOLP(key)))
    return (strcmp(FD_STRING_DATA(value),FD_SYMBOL_NAME(key)) == 0);
  else if ((FD_PRIM_TYPEP(key,pool_type)) && (FD_STRINGP(value))) {
    fd_pool p=(fd_pool)FD_CPTR_DATA(key);
    if ((FD_LISP_EQUAL(p->label,value)) ||
	(strcmp(FD_STRING_DATA(value),p->id) == 0))
      return 1;
    else return 0;}
  else return 0;
}

static fd_lisp xmlswitch_handler(fd_lisp xpr,fd_lispenv env)
{
  fd_lisp key=xml_getarg(key_symbol,xpr,env);
  fd_lisp content=fd_xml_content(xpr);
  fd_htstream *hs=fd_get_http_output();
  FD_DOLIST(elt,content)
    if (FD_PAIRP(elt)) {
      fd_lisp tag=fd_xml_tag(elt);
      if (tag_matchp(case_symbol,tag)) {
	fd_lisp val=xml_getarg(value_symbol,elt,env);
	if (xml_case_compare(key,val)) {
	  execute_content(elt,env,hs); break;}}
      else if (tag_matchp(else_symbol,tag)) {
	execute_content(elt,env,hs); break;}}
  return FD_VOID;
}

/* XMLEVAL primitive */

static fd_lisp xmleval_lexpr(fd_lisp args)
{
  fd_lisp content=fd_get_arg(args,0,FD_VOID);
  fd_lisp base_env_arg=fd_get_arg(args,1,FD_FALSE);
  fd_lisp other_bindings=fd_get_body(args,2);
  fd_lispenv base_env=NULL;
  if (FD_FALSEP(base_env_arg)) base_env=NULL;
  else if (FD_PRIM_TYPEP(base_env_arg,env_type))
    base_env=FD_CPTR_DATA(base_env_arg);
  else if (FD_SYMBOLP(base_env_arg)) {
    fd_lispenv mod_env=
      fd_get_module(FD_SYMBOL_NAME(base_env_arg));
    if (mod_env) base_env=mod_env;
    else fd_type_error(_("Not an environment"),base_env_arg);}
  else fd_type_error(_("Not an environment"),base_env_arg);

  if (FD_EMPTY_LISTP(other_bindings)) 
    fd_unparse_xml(content,base_env,fd_get_http_output());
  else {
    FD_WITH_LEXICAL_ENV(extended_env,base_env,fd_list_length(other_bindings)/2) {
      fd_lisp scan=other_bindings;
      while ((FD_PAIRP(scan)) &&  (FD_PAIRP(FD_CDR(scan)))) {
	fd_bind_value(FD_CAR(scan),FD_CAR(FD_CDR(scan)),extended_env);
	scan=FD_CDR(scan); scan=FD_CDR(scan);}
      fd_unparse_xml(content,extended_env,fd_get_http_output());}
    FD_END_WITH_LEXICAL_ENV_NOVALUE();}
  return FD_VOID;
}

static fd_lisp xmlcall_lexpr(fd_lisp args)
{
  fd_lisp method=fd_get_arg(args,0,FD_VOID);
  fd_lisp xmldata=fd_get_arg(args,1,FD_VOID);
  fd_lisp xmlenv=fd_get_arg(args,2,FD_FALSE);
  fd_lispenv env=NULL;
  if (FD_FALSEP(xmlenv)) env=NULL;
  else if (FD_PRIM_TYPEP(xmlenv,env_type))
    env=FD_CPTR_DATA(xmlenv);
  else if (FD_SYMBOLP(xmlenv)) {
    fd_lispenv mod_env=
      fd_get_module(FD_SYMBOL_NAME(xmlenv));
    if (mod_env) env=mod_env;
    else fd_type_error(_("Not an environment"),xmlenv);}
  else fd_type_error(_("Not an environment"),xmlenv);
  fd_xml_callout(method,xmldata,env);
  return FD_VOID;
}

void initialize_xmleval_c()
{
  fd_xmleval_env=fd_make_module();

  id_symbol=fd_make_symbol("ID");
  bind_symbol=fd_make_symbol("BIND");
  counter_symbol=fd_make_symbol("COUNTER");
  seq_symbol=fd_make_symbol("SEQ");
  choice_symbol=fd_make_symbol("CHOICE");
  sep_symbol=fd_make_symbol("SEP");
  expander_symbol=fd_make_symbol("EXPANDER");
  env_symbol=fd_make_symbol("%ENV");
  key_symbol=fd_make_symbol("KEY");
  case_symbol=fd_make_symbol("CASE");
  value_symbol=fd_make_symbol("VALUE");

  test_symbol=fd_make_symbol("TEST");
  else_symbol=fd_make_symbol("ELSE");
  elif_symbol=fd_make_symbol("ELIF");
  expr_symbol=fd_make_symbol("EXPR");

  fd_add_special_form(fd_xmleval_env,"EVAL",xml_eval_handler);
  fd_add_special_form(fd_xmleval_env,"X",xml_eval_handler);
  fd_add_special_form(fd_xmleval_env,"XS",xml_seval_handler);

  fd_add_special_form(fd_xmleval_env,"FORSEQ",xml_forseq_handler);
  fd_add_special_form(fd_xmleval_env,"FORCHOICES",xml_forchoices_handler);
  fd_add_special_form(fd_xmleval_env,"ITER",xml_iter_handler);

  fd_add_special_form(fd_xmleval_env,"IF",xml_if_handler);
  fd_add_special_form(fd_xmleval_env,"IFDEF",xml_ifdef_handler);

  fd_add_special_form(fd_xmleval_env,"SWITCH",xmlswitch_handler);

  fd_add_lexpr(fd_xml_env,"XMLEVAL",FD_NORMAL_LEXPR,xmleval_lexpr);
  fd_add_lexpr(fd_xml_env,"XMLCALL",FD_NORMAL_LEXPR,xmlcall_lexpr);
  fd_add_cproc(fd_xml_env,"XMLEVAL-ARG",3,xml_getarg_cproc);

  fd_register_source_file("xmleval",__DATE__,vcid);
}
