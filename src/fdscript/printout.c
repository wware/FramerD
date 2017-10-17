/* C Mode */

/* printout.c
   Formatted output for FDScript
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

static char vcid[] = "$Id: printout.c,v 1.12 2005/01/14 16:48:46 haase Exp $";

#include "stdarg.h"
#include "fdscript.h"

static lisp standard_output_symbol;

fd_exception fd_SchemeError="Error from Scheme code";

/** PRINTOUT: Formatted Output **/

static void process_printout_args(lisp port,lispenv env,lisp args,int eval)
{
  DOLIST(arg,args) {
    lisp value=((eval) ? (fd_eval_in_env(arg,env)) : incref(arg));
    if (FD_VOIDP(value)) {}
    else if (STRINGP(value))
      if (FD_OUTPUT_FILEP(port))
	fd_fputs_encoded
	  (STRING_DATA(value),STRING_LENGTH(value),(FILE *)CPTR_DATA(port));
      else if (FD_OUTPUT_STRINGP(port))
	fd_sputs((fd_string_stream)CPTR_DATA(port),STRING_DATA(value));
      else fd_type_error(_("not an output port"),port);
    else if (FD_OUTPUT_FILEP(port))
      fd_print_lisp(value,(FILE *)CPTR_DATA(port));
    else if (FD_OUTPUT_STRINGP(port))
      fd_print_lisp_to_string(value,(fd_string_stream)CPTR_DATA(port));
    else fd_type_error(_("not an output port"),port);
    decref(value);}
}

static lisp lisp_printout_apply(lisp args)
{
  lisp spec=FD_EMPTY_LIST, *tail=&spec, scan=args;
  lisp port=fd_default_output_port();
  while ((PAIRP(CDR(scan)))) {
    lisp new=FD_MAKE_PAIR(incref(CAR(scan)),FD_EMPTY_LIST);
    *tail=new; tail=&(CDR(new)); scan=CDR(scan);}
  *tail=incref(CAR(scan));
  process_printout_args(port,NULL,spec,0);
  decref(spec); decref(port);
  return FD_VOID;
}

static lisp printout_handler(lisp expr,lispenv env)
{
  lisp oport=fd_default_output_port();
  process_printout_args(oport,env,fd_get_body(expr,1),1);
  decref(oport);
  return FD_VOID;
}

static lisp printout_to_handler(lisp expr,lispenv env)
{
  lisp ports=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp oport=fd_default_output_port();
  UNWIND_PROTECT {
    fd_thread_symbind(standard_output_symbol,ports);
    fd_decref(ports);
    process_printout_args(ports,env,fd_get_body(expr,2),1);}
  ON_UNWIND {
    fd_thread_symbind(standard_output_symbol,oport);
    fd_decref(oport);}
  END_UNWIND;
  return FD_VOID;
}

static lisp lineout_handler(lisp expr,lispenv env)
{
  lisp port=fd_default_output_port();
  process_printout_args(port,env,fd_get_body(expr,1),1);
  if (FD_OUTPUT_FILEP(port)) {
    fputc('\n',(FILE *)CPTR_DATA(port));
    fflush((FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream xs=(fd_string_stream)CPTR_DATA(port);
    fd_sputs(xs,"\n");}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_VOID;
}

static lisp logger_handler(lisp expr,lispenv env)
{
  lisp oport=fd_default_output_port(); char buf[64];
  time_t tick=time(NULL); struct tm now;
  fd_localtime(&now,tick);
  sprintf(buf,"[%02d-%02dT%02d:%02d:%02d ",
	  now.tm_mon+1,now.tm_mday,now.tm_hour,now.tm_min,now.tm_sec);
  fd_display_string(buf,oport);
  process_printout_args(oport,env,fd_get_body(expr,1),1);
  fd_display_string("]\n",oport);
  decref(oport);
  return FD_VOID;
}

static lisp notify_handler(lisp expr,lispenv env)
{
  fd_lisp message=fd_stringout(fd_get_body(expr,1),env);
  fd_notify("%s: %s",fd_get_session_mnemonic(),FD_STRING_DATA(message));
  decref(message);
  return FD_VOID;
}

static lisp notify_star_handler(lisp expr,lispenv env)
{
  fd_lisp message=fd_stringout(fd_get_body(expr,2),env);
  fd_lisp tag=fd_get_arg(expr,1,FD_VOID);
  if (FD_SYMBOLP(tag))
    fd_notify("%s: %s",FD_SYMBOL_NAME(tag),FD_STRING_DATA(message));
  else if (FD_STRINGP(tag))
    fd_notify("%s: %s",FD_STRING_DATA(tag),FD_STRING_DATA(message));
  else fd_notify("%q: %s",tag,FD_STRING_DATA(message));
  decref(message);
  return FD_VOID;
}

static lisp warn_handler(lisp expr,lispenv env)
{
  fd_lisp message=fd_stringout(fd_get_body(expr,1),env);
  fd_warn("fdscript: %s",FD_STRING_DATA(message));
  decref(message);
  return FD_VOID;
}

static lisp warn_star_handler(lisp expr,lispenv env)
{
  fd_lisp message=fd_stringout(fd_get_body(expr,2),env);
  fd_lisp tag=fd_get_arg(expr,1,FD_VOID);
  if (FD_SYMBOLP(tag))
    fd_warn("%s: %s",FD_SYMBOL_NAME(tag),FD_STRING_DATA(message));
  else if (FD_STRINGP(tag))
    fd_warn("%s: %s",FD_STRING_DATA(tag),FD_STRING_DATA(message));
  else fd_warn("%q: %s",tag,FD_STRING_DATA(message));
  decref(message);
  return FD_VOID;
}

static lisp trouble_handler(lisp expr,lispenv env)
{
  fd_lisp message=fd_stringout(fd_get_body(expr,1),env);
  fd_u8char *strmessage=fd_strdup(FD_STRING_DATA(message));
  decref(message);
  fd_raise_detailed_exception(fd_SchemeError,strmessage);
  return FD_VOID;
}

static lisp trouble_star_handler(lisp expr,lispenv env)
{
  fd_lisp irritant=FD_VOID;
  fd_lisp tag=fd_get_arg(expr,1,FD_VOID);
  fd_lisp message=fd_stringout(fd_get_body(expr,2),env);
  fd_u8char *strmessage=fd_strdup(FD_STRING_DATA(message)), *exname;
  if (FD_PAIRP(tag)) {
    int i=0, len=fd_list_length(tag)-1;
    fd_lisp scan=FD_CDR(tag);
    irritant=fd_make_vector(len); tag=FD_CAR(tag);
    while (i < len) {
      FD_VECTOR_SET(irritant,i,fd_eval_in_env(FD_CAR(scan),env));
      scan=FD_CDR(tag); i++;}}
  if (FD_STRINGP(tag)) {
    fd_warn("Tag of TROUBLE statement is not a symbol");
    exname=fd_strdup(FD_STRING_DATA(message));}
  else if (FD_SYMBOLP(tag)) exname=FD_SYMBOL_NAME(tag);
  else {
    fd_warn("Tag of TROUBLE statement is not a symbol");
    exname=fd_SchemeError;}
  decref(message);
  if (FD_VOIDP(irritant))
    fd_raise_detailed_exception(exname,strmessage);
  else fd_raise_lisp_exception(exname,strmessage,irritant);
  return FD_VOID;
}

static void comma_list_helper(lisp v,lisp format,lisp ports)
{
  if (FD_FALSEP(format)) fd_display(v,ports);
  else {
    lisp args=FD_MAKE_LIST1(v);
    lisp value=fd_apply(format,args);
    if (!((FD_VOIDP(value)) || (FD_EMPTYP(value)))) {
      fd_display(value,ports); decref(value);}
    decref(args);}
}

static lisp comma_list_handler(lisp expr,lispenv env)
{
  lisp ports=fd_default_output_port();
  lisp items=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp conjunction=fd_eval_in_env(fd_get_arg(expr,2,FD_FALSE),env);
  lisp format=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  if (CHOICE_SIZE(items) == 1) 
    comma_list_helper(items,format,ports);
  else {
    int count=CHOICE_SIZE(items);
    DO_CHOICES(v,items) {
      comma_list_helper(v,format,ports); --count;
      if (count == 0) {}
      else if (count == 1)
	if (FD_FALSEP(conjunction))
	  fd_display_string(" and/or ",ports);
	else if (STRINGP(conjunction))
	  fd_display_strings(ports," ",STRING_DATA(conjunction)," ",NULL);
	else {
	  fd_display_string(" ",ports);
	  fd_display(conjunction,ports);
	  fd_display_string(" ",ports);}
      else fd_display_string(", ",ports);}
    END_DO_CHOICES;}
  decref(ports); decref(items);
  decref(conjunction); decref(format);
  return FD_VOID;
}  

FDSCRIPT_EXPORT void fd_display_string(char *string,lisp port)
{
  if (FD_OUTPUT_FILEP(port)) {
    fputs(string,(FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_sputs((fd_string_stream)CPTR_DATA(port),string);}
  else fd_type_error(_("not an output port"),port);
}

FDSCRIPT_EXPORT void fd_display_strings(lisp port,...)
{
  va_list args; char *string;
  va_start(args,port);
  while ((string=va_arg(args,char *))) fd_display_string(string,port);
  return;
}

FDSCRIPT_EXPORT void fd_display(lisp x,lisp port)
{
  if (FD_OUTPUT_FILEP(port)) {
    fd_print_lisp(x,(FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_print_lisp_to_string(x,(fd_string_stream)CPTR_DATA(port));}
  else fd_type_error(_("not an output port"),port);
}

FDSCRIPT_EXPORT void fd_printout(lisp port,lisp body,lispenv env)
{
  process_printout_args(port,env,body,1);
}

FDSCRIPT_EXPORT lisp fd_stringout(lisp body,lispenv env)
{
  lisp stream, value;
  fd_string_stream s=fd_malloc(sizeof(struct FD_STRING_STREAM));
  lisp oport=fd_default_output_port();
  UNWIND_PROTECT {
    FD_INITIALIZE_STRING_STREAM(s,1024);
    stream=fd_make_cptr(output_string_type,(void *)s);
    fd_thread_symbind(standard_output_symbol,stream); 
    fd_decref(stream);
    process_printout_args(stream,env,body,1);}
  ON_UNWIND {
    if (fd_theException() == NULL)
      value=fd_copy_string(s->ptr);
    fd_thread_symbind(standard_output_symbol,oport);
    fd_decref(oport);}
  END_UNWIND;
  return value;
}

static lisp stringout_handler(lisp expr,lispenv env) {
  return fd_stringout(fd_get_body(expr,1),env);}

static lisp lisp_write_to_string_lexpr(lisp args)
{
  if (FD_EMPTY_LISTP(FD_CDR(args))) {
    fd_lisp object=fd_get_arg(args,0,FD_VOID);
    struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,128);
    fd_print_lisp_to_string(object,&ss);
    return fd_init_string(ss.ptr,ss.size);}
  else fd_raise_lisp_exception(fd_TooManyArgs,"READABLY",args);
}

FDSCRIPT_EXPORT
void fd_initialize_printout_c()
{
  standard_output_symbol=fd_make_symbol("*STANDARD-OUTPUT*");

  fd_add_lexpr(NULL,"PRINTOUT-APPLY",FD_ND_LEXPR,lisp_printout_apply);
  fd_add_special_form(NULL,"PRINTOUT",printout_handler);
  fd_add_special_form(NULL,"PRINTOUT-TO",printout_to_handler);
  fd_add_special_form(NULL,"LINEOUT",lineout_handler);
  fd_add_special_form(NULL,"STRINGOUT",stringout_handler);
  fd_add_special_form(NULL,"BUILD-STRING",stringout_handler);
  fd_add_special_form(NULL,"COMMA-LIST",comma_list_handler);
  fd_add_lexpr(NULL,"READABLY",FD_ND_LEXPR,lisp_write_to_string_lexpr);

  fd_add_special_form(NULL,"LOGGER",logger_handler);
  fd_add_special_form(NULL,"NOTIFY",notify_handler);
  fd_add_special_form(NULL,"NOTIFY*",notify_star_handler);
  fd_add_special_form(NULL,"WARN",warn_handler);
  fd_add_special_form(NULL,"WARN*",warn_star_handler);
  fd_add_special_form(NULL,"TROUBLE",trouble_handler);
  fd_add_special_form(NULL,"TROUBLE*",trouble_star_handler);


  fd_register_source_file("printout",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: printout.c,v $
   Revision 1.12  2005/01/14 16:48:46  haase
   Updated copyrights to 2005

   Revision 1.11  2004/07/20 09:16:13  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.10  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.9.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.9.2.1  2003/01/26 20:41:48  haase
   Misc. fixes especially some GC

   Revision 1.9  2002/04/29 13:48:57  haase
   Fixed leak in port argument to PRINTOUT-TO

   Revision 1.8  2002/04/17 00:48:46  haase
   Made default notifier include session mnemoic

   Revision 1.7  2002/04/17 00:30:21  haase
   src/text/text.c

   Revision 1.6  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
