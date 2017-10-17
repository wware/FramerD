/* C Mode */

/* ioprims.c
   I/O primitives for FDScript
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

static char vcid[] = "$Id: ioprims.c,v 1.37 2005/01/14 16:48:49 haase Exp $";

/** Declarations **/
/** Input and Output ports **/
/** Dynamically binding default I/O **/
/** Print/GC functions for ports **/
/** File Input **/
/** International I/O **/
/** File Output **/
/** String Input Ports **/
/** String Output Ports **/
/** STDIO functions **/
/** Input primitives **/
/** Output primitives **/
/** Dtype I/O **/
/** PRINTOUT: Formatted Output **/
/** Bulk frame import and export **/
/** Reading STDIN as string **/
/** Initializing **/

#if HAVE_TERMIOS_H
#include "termios.h"
#endif
#include "stdarg.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "fdscript.h"

/** Declarations **/

fd_exception fd_Cant_Open_File=_("Can't open file"),
	     fd_Cant_Write_File=_("Can't write to file"), 
             fd_Cant_Read_File=_("Can't read from file");
/** Input and Output ports **/

static lisp current_file_symbol;
static lisp standard_input_symbol, standard_output_symbol;
static lisp default_input_port, default_output_port;
FDSCRIPT_EXPORT
lisp fd_default_input_port()
{
  lisp bound=fd_thread_symeval(standard_input_symbol);
  if (FD_VOIDP(bound)) return incref(default_input_port);
  else return bound;
}
static lisp lisp_current_input_port()
{
  return fd_default_input_port();
}
FDSCRIPT_EXPORT
lisp fd_default_output_port() 
{
  lisp bound=fd_thread_symeval(standard_output_symbol);
  if (FD_VOIDP(bound)) return incref(default_output_port);
  else return bound;
}
static lisp lisp_current_output_port()
{
  return fd_default_output_port();
}

static lisp get_input_port(lisp args,int pos)
{
  if (FD_EMPTY_LISTP(args)) return fd_default_input_port();
  else if ((FD_PAIRP(args)) && (FD_PAIRP(FD_CDR(args))))
    fd_raise_lisp_exception(fd_TooManyArgs,"input op",args);
  else {
    lisp port=fd_get_arg(args,pos,FD_FALSE);
    if (FD_FALSEP(port)) return fd_default_input_port();
    return incref(port);}
}

static lisp get_output_port(lisp args,int pos)
{
  int len=0;
  DOLIST(elt,args) len++;
  if (len <= pos) return fd_default_output_port();
  else if (len > pos+1)
    fd_raise_lisp_exception(fd_TooManyArgs,"output op",args);
  else {
    lisp port=fd_get_arg(args,pos,FD_FALSE);
    if (FD_FALSEP(port)) return fd_default_output_port();
    return incref(port);}
}

static lisp lisp_close_input_port(lisp port)
{
  if (FD_INPUT_FILEP(port)) {
    if (CPTR_DATA(port)) {
      fd_fclose((FILE *)CPTR_DATA(port));
      (CPTR_DATA(port))=NULL;
      return FD_TRUE;}
    else return FD_FALSE;}
  else if (FD_INPUT_STRINGP(port)) {
    return FD_TRUE;}
  else fd_type_error(_("not a port"),port);
  
}

static lisp lisp_input_portp(lisp port)
{
  if (FD_INPUT_FILEP(port)) return FD_TRUE;
  else if (FD_INPUT_STRINGP(port)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_output_portp(lisp port)
{
  if (FD_OUTPUT_FILEP(port)) return FD_TRUE;
  else if (FD_OUTPUT_STRINGP(port)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp lisp_close_output_port(lisp port)
{
  if (FD_OUTPUT_FILEP(port))
    if (CPTR_DATA(port)) {
      fd_fclose((FILE *)CPTR_DATA(port));
      (CPTR_DATA(port))=NULL;
      return FD_TRUE;}
    else return FD_FALSE;
  else if (FD_OUTPUT_STRINGP(port)) {
    return FD_TRUE;}
  else fd_type_error(_("not a port"),port);
  
}

static lisp eof_objectp(lisp x)
{
  if (FD_EOF_OBJECTP(x)) return FD_TRUE;
  else return FD_FALSE;
}

static lisp get_port_arg
  (lisp args,int pos,lispenv env,lisp_default_fn default_fcn)
{
  lisp port_arg=fd_get_arg(args,pos,FD_FALSE);
  lisp port;
  if (env) port=fd_eval_in_env(port_arg,env);
  else port=incref(port_arg);
  if (FD_FALSEP(port)) return default_fcn();
  else return port;
}

static lisp flet_eval(lisp sym,lisp tmp_val,lispenv env,lisp body)
{
  lisp cur_val=fd_thread_symeval(sym), value=FD_VOID;
  UNWIND_PROTECT {
    fd_thread_symbind(sym,tmp_val);
    {DOLIST(expr,body) {decref(value); value=fd_eval_in_env(expr,env);}}}
  ON_UNWIND {
    fd_thread_symbind(sym,cur_val);
    fd_decref(cur_val);}
  END_UNWIND;
  return value;
}

static lisp flet_apply(lisp sym,lisp tmp_val,lisp fcn,lisp args)
{
  lisp cur_val=fd_thread_symeval(sym), value=FD_VOID;
  UNWIND_PROTECT {
    fd_thread_symbind(sym,tmp_val);
    value=fd_apply(fcn,args);}
  ON_UNWIND {
    fd_thread_symbind(standard_output_symbol,cur_val);
    fd_decref(cur_val);}
  END_UNWIND;
  return value;
}

/** Dynamically binding default I/O **/

static lisp lisp_with_input_handler(lisp expr,lispenv env)
{
  fd_lisp value=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp retval=flet_eval
    (standard_input_symbol,value,env,fd_get_body(expr,2));
  fd_decref(value);
  return retval;
}
     
static lisp lisp_with_output_handler(lisp expr,lispenv env)
{
  fd_lisp value=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp retval=flet_eval
    (standard_output_symbol,value,env,fd_get_body(expr,2));
  fd_decref(value);
  return retval;
}
     
/** Print/GC functions for ports **/

static void print_stdfile(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#FILE 0x%lx]",CPTR_DATA(x));
}

static void print_stdifile(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#IFILE 0x%lx]",CPTR_DATA(x));
}

static void print_stdofile(lisp x,fd_string_stream s)
{
  fd_printf(s,"[#OFILE 0x%lx]",CPTR_DATA(x));
}

static void free_stdfile(lisp x)
{
  FILE *f=(FILE *)CPTR_DATA(x);
  if ((f) && (f != stdin) && (f != stdout) && (f != stderr))
    fd_fclose(f);
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

static void print_fd_string_stream(lisp x,fd_string_stream s)
{
  fd_string_stream data=(fd_string_stream)CPTR_DATA(x);
  fd_printf(s,"[#STRING-STREAM 0x%lx(%d/%d)]",
	    CPTR_DATA(x),data->size,data->limit);
}

static void free_string_stream(lisp x)
{
  fd_string_stream s=(fd_string_stream)CPTR_DATA(x);
  fd_xfree(s->ptr); fd_free(s,sizeof(struct FD_STRING_STREAM));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

static void print_string_istream(lisp x,fd_string_stream s)
{
  struct FD_STRING_ISTREAM *data=(struct FD_STRING_ISTREAM *)CPTR_DATA(x);
  fd_printf(s,"[#STRING-ISTREAM 0x%lx(%d/%d)]",
	    CPTR_DATA(x),
	    strlen(data->original),data->ptr-data->original);
}

static void free_string_istream(lisp x)
{
  struct FD_STRING_ISTREAM *data=(struct FD_STRING_ISTREAM *)CPTR_DATA(x);
  free(data->original);
  fd_free(data,sizeof(struct FD_STRING_ISTREAM));
  fd_qfree(PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

/** File Input **/

static lisp lisp_open_input_file(lisp filename)
{
  if (STRINGP(filename)) {
    FILE *f=fd_fopen(STRING_DATA(filename),"r");
    if (f == NULL)
      fd_raise_detailed_exception(fd_Cant_Read_File,STRING_DATA(filename));
    else return fd_make_cptr(input_file_type,f);}
  else fd_type_error(_("not a filestring"),filename);
}

static lisp lisp_set_file_encoding(lisp fileptr,lisp encoding_name)
{
  if (!(STRINGP(encoding_name)))
    fd_type_error(_("not an encoding name (string or symbol)"),
		  encoding_name);
  if (FD_FILEP(fileptr)) {
    FILE *f=(FILE *)CPTR_DATA(fileptr);
    fd_set_file_encoding(f,STRING_DATA(encoding_name));
    return FD_VOID;}
  else fd_type_error(_("not a file port"),encoding_name);
}

static lisp lisp_with_input_from_file(lisp filename,lisp thunk)
{
  lisp input_stream=lisp_open_input_file(filename);
  lisp result=
    flet_apply(standard_input_symbol,input_stream,thunk,FD_EMPTY_LIST);
  fd_decref(input_stream);
  return result;
}
     
static lisp lisp_call_with_input_file(lisp filename,lisp proc)
{
  if (STRINGP(filename)) {
    FILE *f=fd_fopen(STRING_DATA(filename),"r"); 
    if (f) {
      lisp args=FD_MAKE_LIST1(fd_make_cptr(input_file_type,f));
      lisp value=fd_apply(proc,args); decref(args);
      return value;}
    else fd_raise_detailed_exception
	   (fd_Cant_Read_File,STRING_DATA(filename));}
  else fd_type_error(_("Not a filestring"),filename);
}

static lisp lisp_with_file_input_handler(lisp expr,lispenv env)
{
  fd_lisp value=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  fd_lisp retval=flet_eval
    (standard_input_symbol,value,env,fd_get_body(expr,2));
  fd_decref(value);
  return retval;
}

/* Reads a LISP object from a file. */
static lisp lisp_read_from_file(lisp filename)
{
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  else {
    char *name=STRING_DATA(filename); FILE *f=fd_fopen(name,"r");
    lisp value;
    if (f == NULL)
      fd_raise_detailed_exception(fd_Cant_Read_File,STRING_DATA(filename));
    else value=fd_parse_lisp_from_stream(f);
    fd_fclose(f);
    return value;}
}

/* Returns the contents of a file as a string */
static lisp lisp_filestring_lexpr(lisp args)
{
  fd_lisp filename, encoding_name;
  fd_get_args("FILESTRING",args,&filename,FD_VOID,&encoding_name,FD_FALSE,NULL);
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  else if (FD_FALSEP(encoding_name)) {
    char *string=fd_filestring(STRING_DATA(filename));
    lisp result=fd_make_string(string); fd_xfree(string);
    return result;}
  else {
    struct FD_TEXT_ENCODING *e=
      fd_get_encoding(fd_strdata(encoding_name));
    if (e == NULL)
      fd_raise_detailed_exception(fd_UnknownEncoding,fd_strdata(encoding_name));
    else return fd_foreign_filestring(STRING_DATA(filename),e);}
}

/* Returns the contents of a file as a packet */
static lisp lisp_filedata_cproc(lisp filename)
{
  FILE *in; char *buf; int size=0, limit=4096, delta;
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  in=fd_fopen(STRING_DATA(filename),"rb");
  if (in == NULL)
    fd_raise_detailed_exception(fd_FileOpenFailed,STRING_DATA(filename));
  buf=fd_xmalloc(4096); limit=4096;
  while ((delta=fread(buf+size,sizeof(char),limit-size,in)) == limit-size) {
    buf=fd_xrealloc(buf,limit+limit/2); limit=limit+limit/2;
    size=size+delta;}
  size=size+delta; fclose(in);
  return fd_make_packet(size,fd_mallocize(buf,size));
}

/** File Output **/

static lisp lisp_open_output_file(lisp filename)
{
  if (STRINGP(filename)) {
    FILE *f=fd_fopen(STRING_DATA(filename),"w");
    if (f) return fd_make_cptr(output_file_type,f);
    else fd_raise_detailed_exception
      (fd_Cant_Write_File,STRING_DATA(filename));}
  else fd_raise_detailed_exception
    ("Filename arg must be string",fd_object_to_string(filename));
}

static lisp lisp_with_file_output_handler(lisp expr,lispenv env)
{
  lisp filename=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp output_stream=lisp_open_output_file(filename);
  lisp result=
    flet_eval
    (standard_output_symbol,output_stream,env,fd_get_body(expr,2));
  fd_decref(filename); fd_decref(output_stream);
  return result;
}

static lisp lisp_with_output_to_file(lisp filename,lisp thunk)
{
  lisp output_stream=lisp_open_output_file(filename);
  lisp result=
    flet_apply(standard_output_symbol,output_stream,thunk,FD_EMPTY_LIST);
  fd_decref(output_stream);
  return result;
}

static lisp lisp_call_with_output_file(lisp filename,lisp proc)
{
  if (STRINGP(filename)) {
    FILE *f=fd_fopen(STRING_DATA(filename),"w");
    if (f) {
      lisp args=FD_MAKE_LIST1(fd_make_cptr(output_file_type,f));
      lisp value=fd_apply(proc,args); decref(args);
      return value;}
    else fd_raise_detailed_exception
      (fd_Cant_Write_File,STRING_DATA(filename));}
  else fd_raise_detailed_exception
    ("Filename arg must be string",fd_object_to_string(filename));
}

static lisp lisp_write_to_file(lisp obj,lisp filename)
{
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  else {
    char *name=STRING_DATA(filename); FILE *f=fd_fopen(name,"w");
    if (f == NULL)
      fd_raise_detailed_exception(fd_Cant_Write_File,STRING_DATA(filename));
    fd_print_lisp(obj,f); fd_fclose(f);
    return FD_TRUE;}
}

static lisp lisp_write_data_cproc(lisp data,lisp dest)
{
  int binary=0; FILE *f; int need_to_close=0;
  if (FD_PACKETP(data)) binary=1;
  if (STRINGP(dest)) {
    if (binary) f=fd_fopen(STRING_DATA(dest),"wb");
    else f=fd_fopen(STRING_DATA(dest),"w");
    need_to_close=1;}
  else if (FD_OUTPUT_FILEP(dest)) f=(FILE *)CPTR_DATA(dest);
  else fd_type_error(_("not a port"), dest);
  if (FD_PACKETP(data)) {
    fwrite(FD_PACKET_DATA(data),sizeof(char),FD_PACKET_LENGTH(data),f);}
  else if (STRINGP(data)) {
    fd_fputs_raw(STRING_DATA(data),STRING_LENGTH(data),f);}
  else fd_type_error(_("not a string or packet"),data);
  if (need_to_close) fd_fclose(f);
  return FD_VOID;
}

static lisp lisp_read_data_cproc(lisp source,lisp n_bytes)
{
  int bytes_read=0, n=FD_FIXLISP(n_bytes), bytes_to_read=n, off=0;
  char *bytes; FILE *f; 
  if (FD_INPUT_FILEP(source)) f=(FILE *)CPTR_DATA(source);
  else fd_type_error(_("Not an input file"),source);
  bytes=fd_malloc(n);
  while ((bytes_to_read>0) &&
	 (bytes_read=fread(bytes+off,1,bytes_to_read,f))) {
    off=off+bytes_read; bytes_to_read=bytes_to_read-bytes_read;}
  if (bytes_to_read) {
    char *shorter=fd_malloc(off); memcpy(shorter,bytes,off);
    fd_free(bytes,n);
    return fd_make_packet(off,shorter);}
  else return fd_make_packet(n,bytes);
}

/** String Input Ports **/

static lisp lisp_open_input_string(lisp string)
{
  if (STRINGP(string)) {
    struct FD_STRING_ISTREAM *s;
    s=fd_malloc(sizeof(struct FD_STRING_ISTREAM));
    s->original=fd_strdup(STRING_DATA(string)); s->ptr=s->original;
    return fd_make_cptr(input_string_type,s);}
  else fd_type_error(_("not a string"),string);
}

static lisp lisp_read_from_string(lisp string)
{
  char *str;
  if (!(STRINGP(string)))
    fd_type_error(_("not a string"),string);
  else str=STRING_DATA(string);
  return fd_parse_string(str);
}

/* Reads a LISP object from a string */
static lisp lisp_parse_arg(lisp string)
{
  if (STRINGP(string)) 
    return fd_parse_arg(STRING_DATA(string));
  else return incref(string);
}

static lisp lisp_parse_args(lisp expr,lispenv env)
{
  DOLIST(var,CDR(expr))
    if (SYMBOLP(var)) {
      lisp value=fd_symeval(var,env);
      if (STRINGP(value)) {
	lisp parsed=fd_parse_string(STRING_DATA(value));
	fd_set_value(var,parsed,env); fd_decref(parsed);}
      else if (CHOICEP(value)) {
	lisp result=FD_EMPTY_CHOICE;
	DO_CHOICES(each,value) {
	  if (STRINGP(each)) {
	    lisp parsed=fd_parse_string(STRING_DATA(each));
	    ADD_TO_CHOICE(result,parsed);}
	  else {ADD_TO_CHOICE(result,incref(each));}}
	END_DO_CHOICES;
	fd_set_value(var,result,env); fd_decref(result);}
      decref(value);}
    else fd_raise_exception("PARSE-ARGS: non-symbol");
  return FD_VOID;
}

static lisp lisp_with_string_input_handler(lisp expr,lispenv env)
{
  lisp string_arg=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  lisp string_stream=lisp_open_input_string(string_arg);
  lisp result=flet_eval
    (standard_input_symbol,string_stream,env,fd_get_body(expr,2));
  decref(string_arg); decref(string_stream);
  return result;
}

/** String Output Ports **/

static lisp lisp_open_output_string()
{
  fd_string_stream ss=fd_malloc(sizeof(struct FD_STRING_STREAM));
  FD_INITIALIZE_STRING_STREAM(ss,1024);
  return fd_make_cptr(output_string_type,(void *) ss);
}

static lisp fd_string_stream_contents(lisp ss)
{
  if (FD_OUTPUT_STRINGP(ss)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(ss);
    return fd_copy_string(s->ptr);}
  else fd_type_error(_("not a string stream"),ss);
}

static lisp lisp_with_string_output_handler(lisp expr,lispenv env)
{
  lisp result=FD_EMPTY_CHOICE;
  lisp string_port=lisp_open_output_string();
  struct FD_STRING_STREAM *s=
    (struct FD_STRING_STREAM *)CPTR_DATA(string_port);
  lisp inner_value=flet_eval
    (standard_output_symbol,string_port,env,fd_get_body(expr,1));
  result=fd_make_substring(s->ptr,s->ptr+s->size);
  /* Zero out the string port, since you've used the value. */
  decref(string_port); decref(inner_value);
  return result;
}


static lisp lisp_write_to_string(lisp object)
{
  struct FD_STRING_STREAM ss; FD_INITIALIZE_STRING_STREAM(&ss,128);
  fd_print_lisp_to_string(object,&ss);
  return fd_init_string(ss.ptr,ss.size);
}


/** STDIO functions **/

static lisp lisp_fopen(lisp fname,lisp mode)
{
  if ((STRINGP(fname)) && (STRINGP(mode))) {
    FILE *f=fd_fopen(STRING_DATA(fname),STRING_DATA(mode));
    char *reading=strchr(STRING_DATA(mode),'r');
    char *writing=strchr(STRING_DATA(mode),'w');
    char *appending=strchr(STRING_DATA(mode),'a');
    char *changing=strchr(STRING_DATA(mode),'+');
    if (f)
      if ((reading) && ((writing) || (appending) || (changing)))
	return fd_make_cptr(io_file_type,(void *)f);
      else if (reading)
	return fd_make_cptr(input_file_type,(void *)f);
      else return fd_make_cptr(output_file_type,(void *)f);
    else fd_raise_detailed_exception(fd_FileOpenFailed,STRING_DATA(fname));}
  else if (!(STRINGP(mode)))
    fd_type_error(_("not a string"),mode);
  else fd_type_error(_("not a filestring"),mode);
}

static lisp lisp_fopen_locked(lisp fname,lisp mode)
{
  if ((STRINGP(fname)) && (STRINGP(mode))) {
    FILE *f=fd_fopen_locked(STRING_DATA(fname),STRING_DATA(mode),1);
    char *reading=strchr(STRING_DATA(mode),'r');
    char *writing=strchr(STRING_DATA(mode),'w');
    char *appending=strchr(STRING_DATA(mode),'a');
    char *changing=strchr(STRING_DATA(mode),'+');
    if (f)
      if (changing)
	return fd_make_cptr(io_file_type,(void *)f);
      else if (reading)
	return fd_make_cptr(input_file_type,(void *)f);
      else return fd_make_cptr(output_file_type,(void *)f);
    else fd_raise_detailed_exception(fd_FileLockFailed,STRING_DATA(fname));}
  else if (!(STRINGP(mode)))
    fd_type_error(_("not a string"),mode);
  else fd_type_error(_("not a filestring"),mode);
}

static lisp lisp_fopen_encoded(lisp fname,lisp mode,lisp encoding_name)
{
  if ((STRINGP(fname)) && (STRINGP(mode))) {
    FILE *f=fd_fopen(STRING_DATA(fname),STRING_DATA(mode));
    char *reading=strchr(STRING_DATA(mode),'r');
    char *writing=strchr(STRING_DATA(mode),'w');
    char *appending=strchr(STRING_DATA(mode),'a');
    char *changing=strchr(STRING_DATA(mode),'+');
    if (f) {
      if (SYMBOLP(encoding_name))
	fd_set_file_encoding(f,FD_SYMBOL_NAME(encoding_name));
      else if (STRINGP(encoding_name))
	fd_set_file_encoding(f,FD_STRING_DATA(encoding_name));
      else fd_type_error(_("Not an encoding"),encoding_name);
      if (changing)
	return fd_make_cptr(io_file_type,(void *)f);
      else if (reading)
	return fd_make_cptr(input_file_type,(void *)f);
      else return fd_make_cptr(output_file_type,(void *)f);}
    else fd_raise_detailed_exception(fd_Cant_Write_File,STRING_DATA(fname));}
  else if (!(STRINGP(mode)))
    fd_type_error(_("not a string"),mode);
  else fd_type_error(_("not a filestring"),fname);
}

#if HAVE_MKSTEMP
static lisp lisp_mkstemp_lexpr(lisp args)
{
  fd_lisp template, mode;
  fd_get_args("MKSTEMP",args,&template,FD_VOID,&mode,FD_FALSE,NULL);
  if ((STRINGP(template)) && ((STRINGP(mode)) || (FD_FALSEP(mode)))) {
    fd_u8char *tmpl=fd_filename(fd_strdata(template));
    fd_u8char *mode_arg=
      ((FD_FALSEP(mode)) ? ((fd_u8char *) "r+b") : (fd_strdata(mode)));
    char *reading=strchr(mode_arg,'r');
    char *writing=strchr(mode_arg,'w');
    char *appending=strchr(mode_arg,'a');
    char *changing=strchr(mode_arg,'+');
    int fileno=mkstemp(tmpl);
    lisp filename=fd_copy_string(tmpl), fileptr;
    FILE *f=fdopen(fileno,mode_arg);
    if (f) {
      lisp values=fd_make_vector(2);
      if (changing) fileptr=fd_make_cptr(io_file_type,(void *)f);
      else if (reading)
	fileptr=fd_make_cptr(input_file_type,(void *)f);
      else fileptr=fd_make_cptr(output_file_type,(void *)f);
      FD_VECTOR_SET(values,0,fileptr);
      FD_VECTOR_SET(values,1,filename);
      {FD_RETURN_LISP(multiple_value_type,vector,FD_PTR_DATA(values,vector));}}
    else fd_raise_detailed_exception(fd_Cant_Write_File,"temp file");}
  else if (!(STRINGP(mode)))
    fd_type_error(_("mode is not a string"),mode);
  else fd_type_error(_("template is not a string"),template);
}
#endif

static lisp lisp_get_stdin()
{
  return fd_make_cptr(input_file_type,stdin);
}

static lisp lisp_get_stdout()
{
  return fd_make_cptr(output_file_type,stdout);
}

static lisp lisp_get_stderr()
{
  return fd_make_cptr(output_file_type,stderr);
}

#if HAVE_TERMIOS
static fd_lisp lisp_open_serial_port_cproc(fd_lisp filename,fd_lisp baud_rate)
{
  FILE *f;
  int fd=open(fd_strdata(filename),O_RDWR|O_NONBLOCK,666); struct termios info;
  if (fd < 0) fd_raise_detailed_exception("open failed",fd_strdata(filename));
  if (tcgetattr(fd,&info)<0)
    fd_raise_detailed_exception("tcgetattr failed",fd_strdata(filename));
  cfsetspeed(&info,FD_FIXLISP(baud_rate));
  tcsetattr(fd,TCSANOW,&info);
  f=fdopen(fd,"rw");
  fd_set_file_encoding(f,"latin1");
  return fd_make_cptr(io_file_type,(void *)f);
}
#endif

static lisp lisp_set_file_position(lisp lfile,lisp offset)
{
  if (FD_FILEP(lfile)) {
    if (fseek((FILE *)CPTR_DATA(lfile),fd_lisp2int(offset),SEEK_SET))
      fd_raise_exception("Seek failed");
    else return FD_TRUE;}
  else fd_type_error(_("not a file stream"),lfile);
}

static lisp lisp_get_file_position(lisp lfile)
{
  if (FD_FILEP(lfile)) {
    int pos=ftell((FILE *)CPTR_DATA(lfile));
    return FD_LISPFIX(pos);}
  else fd_type_error(_("not a file stream"),lfile);
}

static lisp lisp_get_file_size(lisp lfile)
{
  if (FD_FILEP(lfile)) {
    FILE *f=(FILE *)CPTR_DATA(lfile);
    struct stat status;
    int stat_result=fstat(fileno(f),&status);
    if (stat_result < 0) return (FD_EMPTY_CHOICE);
    else return FD_LISPFIX(status.st_size);}
  else if (FD_STRINGP(lfile)) {
    struct stat status;
    char *fname=fd_filename(FD_STRING_DATA(lfile));
    int stat_result=stat(fname,&status);
    fd_xfree(fname);
    if (stat_result < 0) return (FD_EMPTY_CHOICE);
    else return FD_LISPFIX(status.st_size);}
  else fd_type_error(_("not a file stream"),lfile);
}

static lisp lisp_fclose(lisp lfile)
{
  if (FD_FILEP(lfile)) {
    fd_fclose((FILE *)CPTR_DATA(lfile));
    CPTR_DATA(lfile)=NULL;
    return FD_TRUE;}
  else fd_type_error(_("not a file stream"),lfile);
}
     
static lisp lisp_ftruncate_lexpr(lisp args)
{
  fd_lisp lfile, fpos;
  fd_get_args("FTRUNCATE",args,&lfile,FD_VOID,&fpos,FD_FALSE,NULL);
  if (FD_FILEP(lfile)) {
#if WIN32
    fd_raise_exception("WIN32 doesn't support ftruncate");
#else
    FILE *f=(FILE *)CPTR_DATA(lfile);
    int fnum=fileno(f);
    off_t pos=((FD_FIXNUMP(fpos)) ? (FD_FIXLISP(fpos)) : (ftell(f)));
    ftruncate(fnum,pos);
#endif
    return FD_TRUE;}
  else fd_type_error(_("not a file stream"),lfile);
}
     
/** Input primitives **/

static lisp lisp_read_char_lexpr(lisp args)
{
  lisp result=FD_VOID;
  lisp port=get_input_port(args,0);
  if (FD_INPUT_FILEP(port)) {
    int c=fd_fgetc((FILE *)CPTR_DATA(port)); 
    if (c < 0) result=FD_EOF_OBJECT;
    else result=fd_make_character((char)c);}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) result=fd_make_character(*(s->ptr++));
    else result=FD_EOF_OBJECT;}
  else fd_type_error(_("not an input port"),port);
  decref(port);
  return result;
}
     
static lisp lisp_peek_char_lexpr(lisp args)
{
  lisp result=FD_VOID;
  lisp port=get_input_port(args,0);
  if (FD_INPUT_FILEP(port)) {
    int c=fd_fgetc((FILE *)CPTR_DATA(port)); 
    if (c < 0) result=FD_EOF_OBJECT;
    else {
      fd_ungetc(c,(FILE *)CPTR_DATA(port));
      result=fd_make_character((char)c);}}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) result=fd_make_character(*(s->ptr));
    else result=FD_EOF_OBJECT;}
  else fd_type_error(_("not an input port"),port);
  decref(port); return result;
}

static lisp lisp_char_ready_lexpr(lisp args)
{
  lisp result=FD_VOID;
  lisp port=get_input_port(args,0);
  if (FD_INPUT_FILEP(port))
    if (feof((FILE *)CPTR_DATA(port))) result=FD_FALSE;
    else result=FD_TRUE;
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) result=FD_TRUE;
    else result=FD_FALSE;}
  else fd_type_error(_("not an input port"),port);
  decref(port);
  return result;
}

static lisp lisp_read_line_lexpr(lisp args)
{
  lisp result=FD_VOID; int reading=1;
  lisp port=get_input_port(args,0);
  if (FD_INPUT_FILEP(port)) {
    FILE *f=(FILE *) (CPTR_DATA(port));
    struct FD_XFILE *xf=fd_get_xfile(f);
    struct FD_TEXT_ENCODING *enc=
      ((xf) ? (xf->encoding) : (fd_get_default_encoding()));
    struct FD_STRING_STREAM out; char buf[2048], *result; int bytes_read;
    FD_INITIALIZE_STRING_STREAM(&out,128);
    while (result=fgets(buf,2048,f)) {
      int len=strlen(buf); 
      if (buf[len-1]=='\n') {
	fd_write_utf8(&out,buf,buf+len-1,enc); break;}
      else fd_write_utf8(&out,buf,buf+len-1,enc);}
    if (out.size) return fd_stream_string(&out);
    else if (result) return fd_stream_string(&out);
    FD_CLEAR_ERR(); fd_xfree(out.ptr);
    return FD_EOF_OBJECT;}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) {
      char *new=strchr((s->ptr),'\n');
      if (new) {
	lisp answer; *new='\0'; answer=fd_copy_string(s->ptr);
      *new='\n'; s->ptr=new+1; result=answer;}
      else {
	lisp answer=fd_copy_string(s->ptr);
	s->ptr=s->ptr+strlen(s->ptr); result=answer;}}
    else result=FD_EOF_OBJECT;}
  else fd_type_error(_("not an input port"),port);
  decref(port); return result;
}

static lisp lisp_read_lexpr(lisp args)
{
  lisp result=FD_VOID;
  lisp port=get_input_port(args,0);
  if (FD_INPUT_FILEP(port)) {
    int c=fd_fgetc((FILE *)CPTR_DATA(port));
    if ((c == EOF) && (feof((FILE *)CPTR_DATA(port))))
      result=FD_EOF_OBJECT;
    else {
      fd_ungetc(c,(FILE *)CPTR_DATA(port));
      result=fd_parse_lisp_from_stream((FILE *)CPTR_DATA(port));}}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    result=fd_parse_lisp_from_string(&(s->ptr));}
  else fd_type_error(_("not an input port"),port);
  decref(port); return result;
}

static lisp lisp_read_byte_cproc(lisp port)
{
  lisp result=FD_VOID;
  if (FD_INPUT_FILEP(port)) {
    int c=fgetc((FILE *)CPTR_DATA(port));
    if (c < 0) result=FD_EOF_OBJECT;
    else result=LISPFIX(c);}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) result=LISPFIX(*(s->ptr++));
    else result=FD_EOF_OBJECT;}
  else fd_type_error(_("not an input port"),port);
  return result;
}

static lisp lisp_read_4bytes_cproc(lisp port)
{
  lisp result=FD_VOID;
  if (FD_INPUT_FILEP(port)) {
    unsigned int word=0, byte;
    byte=fd_fread_byte((FILE *)CPTR_DATA(port)); word=(word<<8)|byte;
    if (byte < 0) return FD_EOF_OBJECT;
    byte=fd_fread_byte((FILE *)CPTR_DATA(port)); word=(word<<8)|byte;
    if (byte < 0) return FD_EOF_OBJECT;
    byte=fd_fread_byte((FILE *)CPTR_DATA(port)); word=(word<<8)|byte;
    if (byte < 0) return FD_EOF_OBJECT;
    byte=fd_fread_byte((FILE *)CPTR_DATA(port)); word=(word<<8)|byte;
    if (byte < 0) return FD_EOF_OBJECT;
    if (word < 0x80000000) return FD_LISPFIX(word);
    else return fd_ulong2lisp(word);}
  else if (FD_INPUT_STRINGP(port)) {
    struct FD_STRING_ISTREAM *s=(struct FD_STRING_ISTREAM *)CPTR_DATA(port);
    if (*(s->ptr)) result=LISPFIX(*(s->ptr++));
    else result=FD_EOF_OBJECT;}
  else fd_type_error(_("not an input port"),port);
  return result;
}

/** Output primitives **/

static lisp lisp_write_lexpr(lisp args)
{
  lisp values=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  if (FD_OUTPUT_FILEP(port))
    fd_print_lisp(values,(FILE *)CPTR_DATA(port));
  else if (FD_OUTPUT_STRINGP(port))
    fd_print_lisp_to_string(values,(fd_string_stream)CPTR_DATA(port));
  else fd_type_error(_("not an output port"),port);
  decref(port); return FD_TRUE;
}

static lisp lisp_print_lexpr(lisp args)
{
  lisp values=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  if (FD_OUTPUT_FILEP(port)) {
    fd_print_lisp(values,(FILE *)CPTR_DATA(port));
    fputc('\n',(FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_print_lisp_to_string(values,(fd_string_stream)CPTR_DATA(port));
    fd_sputc((fd_string_stream)CPTR_DATA(port),'\n');}
  else fd_type_error(_("not an output port"),port);
  decref(port); return FD_TRUE;
}

static lisp lisp_display_lexpr(lisp args)
{
  lisp values=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream ss=(fd_string_stream)(CPTR_DATA(port));
    int escaped=ss->escape; ss->escape=0;
    fd_print_lisp_to_string(values,(fd_string_stream)CPTR_DATA(port));
    ss->escape=escaped;}
  else if (FD_OUTPUT_FILEP(port)) {
    struct FD_STRING_STREAM ss;
    FD_INITIALIZE_STRING_STREAM(&ss,1024); ss.escape=0;
    fd_print_lisp_to_string(values,&ss);
    fd_fputs_raw(ss.ptr,ss.size,(FILE *)CPTR_DATA(port));
    fd_xfree(ss.ptr);}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_pprint_lexpr(lisp args)
{
  int width;
  fd_lisp expr, lwidth, port;
  fd_get_args("PPRINT",args,&expr,FD_VOID,&port,FD_FALSE,&lwidth,FD_LISPFIX(80),NULL);
  if (FD_FALSEP(port)) port=fd_default_output_port();
  FD_CHECK_TYPE(lwidth,FD_FIXNUMP,"an integer"); width=FD_FIXLISP(lwidth);
  if (FD_OUTPUT_FILEP(port))
    fd_pprint_lisp(expr,(FILE *)CPTR_DATA(port),width);
  else if (FD_OUTPUT_STRINGP(port))
    fd_pprint_lisp_to_string(expr,(fd_string_stream)CPTR_DATA(port),
			     0,0,width);
  else fd_type_error(_("not an output port"),port);
  return FD_TRUE;
}

static lisp lisp_write_line_lexpr(lisp args)
{
  lisp line=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  if (!(STRINGP(line)))
    fd_type_error(_("not a string"),line);
  else if (FD_OUTPUT_FILEP(port))
    fd_fputs_encoded
      (STRING_DATA(line),STRING_LENGTH(line),(FILE *)CPTR_DATA(port));
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(port);
    if (FD_USE_SPACE(s,STRING_LENGTH(line)))
      strcat(s->ptr,STRING_DATA(line));}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_write_char_lexpr(lisp args)
{
  lisp chr=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  if (FD_OUTPUT_FILEP(port))
    fd_fputc(((unichar_t)CHAR_CODE(chr)),(FILE *)CPTR_DATA(port));
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(port);
    fd_sputc(s,CHAR_CODE(chr));}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_write_byte_lexpr(lisp args)
{
  lisp byte=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  int bcode=fd_lisp2int(byte);
  if ((bcode > 255) || (bcode < 0)) fd_type_error(_("not a valid byte"),byte);
  else if (FD_OUTPUT_FILEP(port))
    fputc(bcode,(FILE *)CPTR_DATA(port));
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(port);
    char buf[2]; buf[0]=bcode; buf[1]='\0';
    if (FD_USE_SPACE(s,1)) strcat(s->ptr,buf);}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_write_int_lexpr(lisp args)
{
  lisp byte=fd_get_arg(args,0,FD_VOID);
  lisp port=get_output_port(args,1);
  int bcode=fd_lisp2int(byte);
  if (FD_OUTPUT_FILEP(port)) {
    fwrite_4bytes(bcode,(FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(port);
    char buf[5];
    buf[0]=(bcode>>24); buf[1]=((bcode>>16)&0xFF);
    buf[2]=((bcode>>8)&0xFF); buf[3]=((bcode)&0xFF);
    buf[4]='\0';
    if (FD_USE_SPACE(s,4)) strcat(s->ptr,buf);}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_newline_lexpr(lisp args)
{
  lisp port=get_output_port(args,0);
  if (FD_OUTPUT_FILEP(port)) {
    fd_fputc('\n',(FILE *)CPTR_DATA(port));
    fflush((FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {
    fd_string_stream s=(fd_string_stream)CPTR_DATA(port);
    char buf[2]; buf[0]='\n'; buf[1]='\0';
    if (FD_USE_SPACE(s,1)) strcat(s->ptr,buf);}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

static lisp lisp_flush_output_lexpr(lisp args)
{
  lisp port=get_output_port(args,0);
  if (FD_OUTPUT_FILEP(port)) {
    fflush((FILE *)CPTR_DATA(port));}
  else if (FD_OUTPUT_STRINGP(port)) {}
  else fd_type_error(_("not an output port"),port);
  decref(port);
  return FD_TRUE;
}

/** Dtype I/O **/

static lisp lisp_read_dtype(lisp lfile)
{
  if (FD_INPUT_FILEP(lfile)) {
    int c=fgetc((FILE *)CPTR_DATA(lfile));
    if ((c == EOF) && (feof((FILE *)CPTR_DATA(lfile))))
      return FD_EOF_OBJECT;
    else {
      ungetc(c,(FILE *)CPTR_DATA(lfile));
      return fd_fread_dtype((FILE *)CPTR_DATA(lfile));}}
  else fd_type_error(_("not a file port"),lfile);
}
   
/* Reads all the dtypes in a file and returns them non-deterministically */
static lisp read_dtype_from_file(lisp filename)
{
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  else {
    char *name=STRING_DATA(filename); FILE *f=fd_fopen(name,"rb");
    lisp value=FD_EMPTY_CHOICE; int c;
    if (f == NULL) fd_raise_detailed_exception
		     (fd_Cant_Read_File,STRING_DATA(filename));
    while ((c=getc(f)) != EOF) {
      lisp v; ungetc(c,f); v=fd_fread_dtype(f);
      ADD_TO_CHOICE(value,v);}
    fclose(f);
    return value;}
}

static lisp write_dtype_to_file(lisp expr,lispenv env)
{
  lisp obj, filename, caparg; int size=0, capabilities=0;
  obj=fd_eval_in_env(fd_get_arg(expr,1,FD_VOID),env);
  filename=fd_eval_in_env(fd_get_arg(expr,2,FD_VOID),env);
  caparg=fd_eval_in_env(fd_get_arg(expr,3,FD_FALSE),env);
  if (FD_FALSEP(caparg)) {}
  else if (FD_FIXNUMP(caparg)) capabilities=FD_FIXLISP(caparg);
  else fd_decref(caparg);
  if (!(STRINGP(filename)))
    fd_raise_detailed_exception
      (fd_FilenameMustBeString,fd_object_to_string(filename));
  else {
    char *name=STRING_DATA(filename);
    FILE *f=fd_fopen_locked(name,"wb",0);
    if (f == NULL) fd_raise_detailed_exception
		     (fd_Cant_Write_File,STRING_DATA(filename));
    if (capabilities) {
      DO_CHOICES(elt,obj) size=size+fd_fwrite_dtype_x(elt,f,capabilities); END_DO_CHOICES;}
    else {
      DO_CHOICES(elt,obj) size=size+fd_fwrite_dtype(elt,f); END_DO_CHOICES;}
    decref(obj); decref(filename);
    fclose(f); return LISPFIX(size);}
}

static lisp add_dtype_to_file_lexpr(lisp args)
{
  fd_lisp obj, files, caparg; int capabilities=0;
  fd_get_args("ADD-DTYPE-TO-FILE",args,&obj,FD_VOID,&files,FD_VOID,&caparg,FD_FALSE,NULL);
  if (FD_FALSEP(caparg)) {}
  else if (FD_FIXNUMP(caparg)) capabilities=FD_FIXLISP(caparg);
  {FD_DO_CHOICES(filename,files) {
    if (!(STRINGP(filename)))
      fd_raise_detailed_exception
	(fd_FilenameMustBeString,fd_object_to_string(filename));
    else {
      char *name=STRING_DATA(filename);
      FILE *f=fd_fopen_locked(name,"ab",0);
      if (f == NULL) f=fd_fopen_locked(name,"wb",0);
      if (f == NULL) fd_raise_detailed_exception
		       (fd_Cant_Write_File,STRING_DATA(filename));
      if (capabilities) {
	FD_DO_CHOICES(elt,obj) fd_fwrite_dtype_x(elt,f,capabilities); END_FD_DO_CHOICES;}
      else {
	FD_DO_CHOICES(elt,obj) fd_fwrite_dtype(elt,f); END_FD_DO_CHOICES;}
      fclose(f);}}
  END_FD_DO_CHOICES;}
  return FD_TRUE;
}

static lisp lisp_write_dtype_lexpr(fd_lisp args)
{
  fd_lisp x, lfile, caparg; int capabilities=0;
  fd_get_args("WRITE-DTYPE",args,&x,FD_VOID,&lfile,FD_VOID,&caparg,FD_FALSE,NULL);  
  /* Handle the optional capabilities arg */
  if (FD_FALSEP(caparg)) {}
  else if (FD_FIXNUMP(caparg)) capabilities=FD_FIXLISP(caparg);
  /* Do the actual writing */
  if (FD_OUTPUT_FILEP(lfile)) {
    int size=0; FILE *f=(FILE *)CPTR_DATA(lfile);
    if (capabilities) {
      FD_DO_CHOICES(elt,x) size=size+fd_fwrite_dtype_x(elt,f,capabilities); END_FD_DO_CHOICES;}
    else {
      FD_DO_CHOICES(elt,x) size=size+fd_fwrite_dtype(elt,f); END_FD_DO_CHOICES;}
    return LISPFIX(size);}
  else fd_type_error(_("not an output file port"),lfile);
}
   
/* Reads a dtype from a packet */
static lisp lisp_read_dtype_from_packet(lisp packet)
{
  if (!(PRIM_TYPEP(packet,packet_type)))
    fd_type_error(_("not a packet"),packet);
  else {
    struct FD_DBUF b; fd_lisp_string  s=PTR_DATA(packet,string);
    lisp value; b.ptr=b.start=s->data; b.end=b.start+s->length;
    value=fd_dread_dtype(&b);
    return value;}
}

/* Writes a dtype to a packet */
static lisp lisp_write_dtype_to_packet_lexpr(fd_lisp args)
{
  fd_lisp object, caparg; int capabilities=0;
  struct FD_DBUF b; int len;
  fd_get_args("WRITE-DTYPE-TO-PACKET",args,&object,FD_VOID,&caparg,FD_FALSE,NULL);    
  /* Handle the optional capabilities arg */
  if (FD_FALSEP(caparg)) {}
  else if (FD_FIXNUMP(caparg)) capabilities=FD_FIXLISP(caparg);
  /* Initialize the DBUF */
  b.ptr=b.start=fd_xmalloc(1024); b.end=b.start+1024;
  /* Do the actual writing */
  if (capabilities) {
    FD_DO_CHOICES(elt,object) fd_dwrite_dtype_x(object,&b,capabilities); FD_END_DO_CHOICES;}
  else {
    FD_DO_CHOICES(elt,object) fd_dwrite_dtype(object,&b); FD_END_DO_CHOICES;}
  len=b.ptr-b.start;
  /* Make the packet to return. */
  return fd_make_packet(len,fd_mallocize(b.start,len));
}

/** Bulk frame import and export **/

static lisp lisp_export_frames_lexpr(lisp args)
{
  fd_lisp frames, port;
  fd_get_args("EXPORT-FRAMES",args,&frames,FD_VOID,&port,FD_FALSE,NULL);
  if (FD_FALSEP(port)) port=lisp_get_stdout();
  else if (STRINGP(port)) port=lisp_open_output_file(port);
  else port=incref(port);
  {DO_CHOICES(f,frames) {
    lisp export=fd_export_frame(f,FD_EMPTY_CHOICE,0);
    if (FD_OUTPUT_FILEP(port)) {
      fd_pprint_lisp(export,(FILE *)CPTR_DATA(port),80);
      putc('\n',(FILE *)CPTR_DATA(port));}
    else if (FD_OUTPUT_STRINGP(port)) {
      fd_pprint_lisp_to_string
	(export,(fd_string_stream)CPTR_DATA(port), 0,0,80);
      fd_sputs((fd_string_stream)CPTR_DATA(port),"\n");}
    else fd_type_error(_("not an output port"),port);
    decref(export);}
  END_DO_CHOICES;}
  decref(port);
  return FD_VOID;
}

/** Reading from the console **/

static lisp stdin_string()
{
  char *buf=fd_xmalloc(1024); int size=0, limit=1024;
  int c; 
  while ((c=getc(stdin)) != EOF) {
    if (size == limit) {buf=fd_xrealloc(buf,limit*2); limit=limit*2;}
    buf[size++]=(char) c;}
  if (size == limit) {buf=fd_xrealloc(buf,limit*2); limit=limit*2;}
  buf[size++]='\0';
  {lisp answer=fd_make_string(buf); fd_xfree(buf);
   return answer;}
}

/** Initializing **/

static void initialize_ioprims_types()
{
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(io_file_type);
   r->gc_fcn=free_stdfile;
   r->print_fcn=print_stdfile;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(input_file_type);
   r->gc_fcn=free_stdfile;
   r->print_fcn=print_stdifile;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(output_file_type);
   r->gc_fcn=free_stdfile;
   r->print_fcn=print_stdofile;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(output_string_type);
   r->gc_fcn=free_string_stream;
   r->print_fcn=print_fd_string_stream;}
  {struct FD_TYPE_REGISTRY *r=fd_register_typecode(input_string_type);
   r->gc_fcn=free_string_istream;
   r->print_fcn=print_string_istream;}
}

FDSCRIPT_EXPORT
void fd_initialize_ioprims_c()
{
  initialize_ioprims_types();

  default_input_port=lisp_get_stdin();
  default_output_port=lisp_get_stdout();

  current_file_symbol=fd_make_symbol("*CURRENT-FILE*");
  standard_input_symbol=fd_make_symbol("*STANDARD-INPUT*");
  standard_output_symbol=fd_make_symbol("*STANDARD-OUTPUT*");

  fd_add_special_form(NULL,"WITH-OUTPUT",lisp_with_output_handler);
  fd_add_special_form(NULL,"WITH-INPUT",lisp_with_input_handler);

  fd_add_special_form(NULL,"WITH-STRING-INPUT",lisp_with_string_input_handler);
  fd_add_special_form(NULL,"WITH-STRING-OUTPUT",lisp_with_string_output_handler);
  fd_add_restricted_special_form("WITH-FILE-INPUT",lisp_with_file_input_handler);
  fd_add_restricted_special_form("WITH-FILE-OUTPUT",lisp_with_file_output_handler);

  fd_add_restricted_cproc("OPEN-INPUT-FILE",1,lisp_open_input_file);
  fd_add_cproc(NULL,"OPEN-INPUT-STRING",1,lisp_open_input_string);
  fd_add_alias(NULL,"OPEN-STRING-INPUT-STREAM","OPEN-INPUT-STRING");
  fd_add_cproc(NULL,"CLOSE-INPUT-PORT",1,lisp_close_input_port);
  fd_add_cproc(NULL,"CURRENT-INPUT-PORT",0,lisp_current_input_port);
  fd_add_cproc(NULL,"INPUT-PORT?",1,lisp_input_portp);
  fd_add_restricted_cproc("OPEN-OUTPUT-FILE",1,lisp_open_output_file);
  fd_add_cproc(NULL,"OPEN-OUTPUT-STRING",0,lisp_open_output_string);
  fd_add_cproc(NULL,"CLOSE-OUTPUT-PORT",1,lisp_close_output_port);
  fd_add_restricted_cproc("CURRENT-OUTPUT-PORT",0,lisp_current_output_port);
  fd_add_cproc(NULL,"OUTPUT-PORT?",1,lisp_output_portp);
  fd_add_restricted_cproc("CALL-WITH-OUTPUT-FILE",2,lisp_call_with_output_file);
  fd_add_restricted_cproc("CALL-WITH-INPUT-FILE",2,lisp_call_with_input_file);
  fd_add_restricted_cproc("WITH-OUTPUT-TO-FILE",2,lisp_with_output_to_file);
  fd_add_restricted_cproc("WITH-INPUT-FROM-FILE",2,lisp_with_input_from_file);

  fd_add_restricted_cproc("SET-FILE-ENCODING!",2,lisp_set_file_encoding);
  
  fd_add_restricted_cproc("READ-FROM-FILE",1,lisp_read_from_file);
  fd_add_cproc(NULL,"READ-FROM-STRING",1,lisp_read_from_string);
  fd_add_cproc(NULL,"PARSE-ARG",1,lisp_parse_arg);
  fd_add_special_form(NULL,"PARSE-ARGS",lisp_parse_args);

  fd_add_restricted_cproc("FOPEN",2,lisp_fopen);
  fd_add_restricted_cproc("FOPEN-LOCKED",2,lisp_fopen_locked);
  fd_add_restricted_cproc("FOPEN-ENCODED",3,lisp_fopen_encoded);
#if HAVE_MKSTEMP
  fd_add_restricted_lexpr("MKSTEMP",FD_NORMAL_LEXPR,lisp_mkstemp_lexpr);
#endif
#if HAVE_TERMIOS
  fd_add_cproc(NULL,"OPEN-SERIAL-PORT",2,lisp_open_serial_port_cproc);
#endif

  fd_add_restricted_cproc("STDIN",0,lisp_get_stdin);
  fd_add_restricted_cproc("STDOUT",0,lisp_get_stdout);
  fd_add_restricted_cproc("STDERR",0,lisp_get_stderr);
  fd_add_restricted_cproc("FCLOSE",1,lisp_fclose);
  fd_add_restricted_lexpr("FTRUNCATE",FD_NORMAL_LEXPR,lisp_ftruncate_lexpr);
  fd_add_restricted_cproc("SET-FILE-POSITION!",2,lisp_set_file_position);
  fd_add_restricted_cproc("GET-FILE-POSITION",1,lisp_get_file_position);
  fd_add_restricted_cproc("GET-FILE-SIZE",1,lisp_get_file_size);

  fd_add_lexpr(NULL,"READ",FD_NORMAL_LEXPR,lisp_read_lexpr);
  fd_add_lexpr(NULL,"READLINE",FD_NORMAL_LEXPR,lisp_read_line_lexpr);
  fd_add_lexpr(NULL,"READ-CHAR",FD_NORMAL_LEXPR,lisp_read_char_lexpr);
  fd_add_lexpr(NULL,"PEEK-CHAR",FD_NORMAL_LEXPR,lisp_peek_char_lexpr);
  fd_add_lexpr(NULL,"CHAR-READY?",FD_NORMAL_LEXPR,lisp_char_ready_lexpr);
  fd_add_cproc(NULL,"READ-BYTE",1,lisp_read_byte_cproc);
  fd_add_cproc(NULL,"READ-4BYTES",1,lisp_read_4bytes_cproc);

  fd_add_cproc(NULL,"EOF-OBJECT?",1,eof_objectp);
  fd_add_lexpr(NULL,"WRITE",FD_NORMAL_LEXPR,lisp_write_lexpr);
  fd_add_lexpr(NULL,"DISPLAY",FD_NORMAL_LEXPR,lisp_display_lexpr);
  fd_add_lexpr(NULL,"WRITE-CHAR",FD_NORMAL_LEXPR,lisp_write_char_lexpr);
  fd_add_lexpr(NULL,"WRITE-BYTE",FD_NORMAL_LEXPR,lisp_write_byte_lexpr);
  fd_add_lexpr(NULL,"WRITE-INT",FD_NORMAL_LEXPR,lisp_write_int_lexpr);
  fd_add_lexpr(NULL,"NEWLINE",FD_NORMAL_LEXPR,lisp_newline_lexpr);
  fd_add_lexpr(NULL,"WRITE-LINE",FD_NORMAL_LEXPR,lisp_write_line_lexpr);
  fd_add_lexpr(NULL,"FLUSH-OUTPUT",FD_NORMAL_LEXPR,lisp_flush_output_lexpr);

  fd_add_cproc(NULL,"PRIN1",2,lisp_write_lexpr);
  fd_add_lexpr(NULL,"PPRINT",FD_ND_LEXPR,lisp_pprint_lexpr);

  fd_add_cproc(NULL,"READ-DTYPE",1,lisp_read_dtype);
  fd_add_lexpr(NULL,"WRITE-DTYPE",FD_ND_LEXPR,lisp_write_dtype_lexpr);

  fd_add_cproc(NULL,"OPEN-STRING-STREAM",0,lisp_open_output_string);
  fd_add_cproc(NULL,"STRING-STREAM-CONTENTS",1,fd_string_stream_contents);

  fd_add_restricted_lexpr("FILESTRING",FD_NORMAL_LEXPR,lisp_filestring_lexpr);
  fd_add_restricted_cproc("FILEDATA",1,lisp_filedata_cproc);
  fd_add_restricted_cproc("WRITE-DATA",2,lisp_write_data_cproc);
  fd_add_restricted_cproc("READ-DATA",2,lisp_read_data_cproc);
  
  fd_add_restricted_cproc("WRITE-TO-FILE",2,lisp_write_to_file);
  fd_add_cproc(NULL,"WRITE-TO-STRING",1,lisp_write_to_string);
  fd_add_restricted_special_form("WRITE-DTYPE-TO-FILE",write_dtype_to_file);
  fd_add_restricted_lexpr
    ("ADD-DTYPE-TO-FILE",FD_ND_LEXPR,add_dtype_to_file_lexpr);
  fd_add_restricted_cproc("READ-DTYPE-FROM-FILE",1,read_dtype_from_file);

  fd_add_lexpr(NULL,"WRITE-DTYPE-TO-PACKET",FD_ND_LEXPR,lisp_write_dtype_to_packet_lexpr);
  fd_add_cproc(NULL,"READ-DTYPE-FROM-PACKET",1,lisp_read_dtype_from_packet);

  fd_add_lexpr(NULL,"PRINT",FD_NORMAL_LEXPR,lisp_print_lexpr);

  fd_add_restricted_lexpr
    ("EXPORT-FRAMES",FD_NORMAL_LEXPR,lisp_export_frames_lexpr);

  fd_add_restricted_cproc("STDIN-STRING",0,stdin_string);

  fd_register_source_file("ioprims",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: ioprims.c,v $
   Revision 1.37  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.36  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.35  2004/09/13 08:54:53  haase
   Fixed bug in write-char which didn't handle unicode chars correctly

   Revision 1.34  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.33  2004/06/15 17:38:06  haase
   Fixed strange compilation problem

   Revision 1.32  2004/06/13 13:11:24  haase
   Make serial ports default to latin1 encoding

   Revision 1.31  2004/06/12 10:28:16  haase
   readline clears errno when returning EOF

   Revision 1.30  2004/06/10 15:34:31  haase
   Added OPEN-SERIAL-PORT

   Revision 1.29  2004/04/02 10:10:58  haase
   Made WITH-STRING-OUTPUT remalloc its result to the right size.

   Revision 1.28  2004/03/30 11:21:43  haase
   Extended FDSCript DTYPE writing functions to include optional capabilities args

   Revision 1.27  2004/03/11 08:36:53  haase
   Added READ-DATA for pulling packets from files

   Revision 1.26  2004/02/18 20:43:48  haase
   Made new READLINE implementation return EOF

   Revision 1.25  2004/02/16 21:54:43  haase
   Added optimizations to READLINE for utf8

   Revision 1.24  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.23  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.22.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.22.2.1  2003/01/26 20:56:06  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.22  2002/06/03 21:48:19  haase
   Add alias for opening string streams

   Revision 1.21  2002/05/19 13:20:33  haase
   Make arg to init string from string_stream_contents be fd_malloc'd

   Revision 1.20  2002/05/18 12:02:42  haase
   Made packets be in fd_malloc space, meaning that very large
   packets may be allocated with mmap.  This required implementing
   fd_mallocize to take a regular malloc'd block and return one which
   may be mmap'd.  It also took updates to other calls to fd_make_packet

   Revision 1.19  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.18  2002/04/03 18:16:40  haase
   Added READ-4BYTES primitive

   Revision 1.17  2002/04/02 22:21:07  haase
   Added revision info to man pages

   Revision 1.16  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
