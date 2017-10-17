/* C Mode */

/* mime.c
   Mime parsing for FDScript
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

static char vcid[] = "$Id: mime.c,v 1.32 2005/01/14 16:48:50 haase Exp $";

#include "fdwww.h"
#include "framerd/fdtext.h"

#define STRINGP FD_STRINGP
#define STRING_DATA FD_STRING_DATA

static struct FD_TEXT_ENCODING
  *default_mime_charset, *utf8_encoding, *latin1_encoding, *ucs2_encoding;

static fd_lisp type_slotid, type_boundary_slotid, type_charset_slotid;
static fd_lisp content_type_slot, content_encoding_slot, content_length_slot;
static fd_lisp content_xfer_encoding_slot;
static fd_lisp content_disposition_slot;
static fd_lisp prologue_slot, epilogue_slot, mime_type_slot, charset_slot;
static fd_lisp content_slot, text_symbol, mime_slots_slotid;

static fd_exception
  WeirdContentType=_("Unknown content type"),
  UnknownBinaryEncoding=_("Unknown binary encoding"),
  BadEncodedWord=_("Bad encoded word in mime header"),
  BadFieldName=_("Bad field name in mime header"),
  BadContentType=_("Malformed content type"),
  BadContentLength=_("Bad content length"),
  CharsetNotApplicable=_("charset only makes sense for text"),
  BoundaryNotApplicable=_("boundary only makes sense for multipart or message"),
  HeterogenousMultiPart=_(
    "Multipart documents can only be straight-through encoded"),
  MalformedMIMEMessage=_("Malformed MIME message"),
  UnterminatedMIMEData=_("Unterminated mime data");
  
#define JUST_HEAD 1
#define WITH_CONTENT 0

static char *read_quoted_printable(char *from,char *to,int *sizep);
static char *read_base64(char *from,char *to,int *sizep);

FASTOP void prim_add_consed(fd_lisp f,fd_lisp sl,fd_lisp v)
{
  fd_prim_add(f,sl,v); fd_decref(v);
}

static fd_lisp sstream_string(struct FD_STRING_STREAM *s)
{
  return fd_init_string(s->ptr,s->size);
}

static fd_lisp encode_packet(char *s,int len,char *encoding)
{
  len=strlen(s); /* don't trust the length in the message */
  if (encoding == NULL) {
    char *copy=fd_malloc(len);
    memcpy(copy,s,len);
    return fd_make_packet(len,copy);}
  else if ((strcasecmp(encoding,"BASE64")) == 0) {
    char *unencoded; int size; unencoded=read_base64(s,s+len,&size);
    return fd_make_packet(size,unencoded);}
  else if ((strcasecmp(encoding,"QUOTED-PRINTABLE")) == 0) {
    char *unencoded; int size;
    unencoded=read_quoted_printable(s,s+strlen(s),&size);
    return fd_make_packet(size,unencoded);}
  else {
    char *copy=fd_malloc(len);
    memcpy(copy,s,len);
    return fd_make_packet(len,copy);}
}

/* This does conversion of newline pairs into single newlines */
static void kludge_newlines(fd_u8char *string)
{
  fd_u8char *read=string, *write=read;
  while (*read) 
    if ((read[0]=='\r') && (read[1]=='\n')) {
      *write++='\n'; read=read+2;}
    else *write++=*read++;
  *write=0;
}

static fd_lisp try_to_encode_string
  (char *s,int len,char *encoding,struct FD_TEXT_ENCODING *charset)
{
  char *unencoded; fd_u8char *sdata; int size;
  if (encoding == NULL) {unencoded=s; size=len;}
  else if (strcasecmp(encoding,"BASE64") == 0)
    unencoded=read_base64(s,s+len,&size);
  else if (strcasecmp(encoding,"QUOTED-PRINTABLE") == 0)
    unencoded=read_quoted_printable(s,s+len,&size);
  else fd_raise_detailed_exception(UnknownBinaryEncoding,encoding);
  sdata=fd_make_utf8(unencoded,unencoded+size,charset);
  if (encoding) fd_free(unencoded,len+1); kludge_newlines(sdata);
  return fd_init_string(sdata,-1);
}

static fd_lisp encode_string
  (char *s,int len,char *encoding,struct FD_TEXT_ENCODING *charset)
{
  fd_lisp result=FD_VOID; int retry=0;
  FD_WITH_HANDLING
    result=try_to_encode_string(s,len,encoding,charset);
  FD_ON_EXCEPTION 
    if (fd_theException() == fd_InvalidChar) {
      retry=1; fd_clear_exception();}
    else fd_reraise();
  FD_END_HANDLING;
  if (retry) {
    struct FD_TEXT_ENCODING *dflt=fd_get_default_encoding();
    fd_warn("Can't render string in %s",charset->names[0]);
    if (charset == dflt)
      return try_to_encode_string(s,len,encoding,default_mime_charset);
    else return encode_string(s,len,encoding,dflt);}
  else if (FD_VOIDP(result))
    return fd_make_packet(len,fd_memdup(s,len));
  else return result;
}


static fd_lisp reverse_list(fd_lisp seq)
{
  fd_lisp result=FD_EMPTY_LIST;
  FD_DOLIST(elt,seq)
    result=FD_MAKE_PAIR(fd_incref(elt),result);
  return result;
}

static char *find_separator(char *sep,char *start,char *end)
{
  int seplen=strlen(sep);
  char byte1=*sep, *scan=memchr(start,byte1,end-start);
  while ((scan) && (scan+seplen<=end)) {
    if (memcmp(sep,scan,seplen) == 0) return scan;
    else scan=memchr(scan+1,byte1,(end-(scan+1)));}
  return NULL;
}


/* Handling encodings (quoted printable and base64) */

static char *read_quoted_printable(char *from,char *to,int *sizep)
{
  char *result=fd_malloc(to-from+1), *read=from, *write=result;
  int size=0;
  while (read < to) 
    if (*read == '=')
      if (read[1] == '\n') read=read+2;
      else if ((read[1] == '\r') && (read[2] == '\n')) read=read+3;
      else {
	char buf[4]; int coded_char;
	buf[0]=read[1]; buf[1]=read[2]; buf[2]=0;
	sscanf(buf,"%x",&coded_char); size++;
	*write++=coded_char; read=read+3;}
    else {*write++=*read++; size++;}
  *write=0; *sizep=size;
  return result;
}

static int encode_base64_byte(char c)
{
  if ((c>='A') && (c<='Z')) return c-'A';
  else if ((c>='a') && (c<='z')) return 26+(c-'a');
  else if ((c>='0') && (c<='9')) return 52+(c-'0');
  else if (c == '+') return 62;
  else if (c == '/') return 63;
  else if (c == '=') return 0;
  else return -1;
}

static int encode_base64_block(char *c)
{
  int first_byte=encode_base64_byte(c[0]);
  if (first_byte<0) return -1;
  else return
	 (first_byte<<18)|
	 ((encode_base64_byte(c[1]))<<12)|
	 ((encode_base64_byte(c[2]))<<6)|
	 ((encode_base64_byte(c[3]))<<0);
}

static char *read_base64(char *from,char *to,int *sizep)
{
  char *result=fd_malloc(to-from+1), *read=from, *write=result;
  int size=0;
  while (read < to) {
    int bytes=encode_base64_block(read);
    if (bytes < 0) read++;
    else {
      *write++=bytes>>16; *write++=(bytes>>8)&0xFF; *write++=(bytes)&0xFF;
      if (read[2] == '=') {*sizep=size+1; break;}
      else if (read[3] == '=') {*sizep=size+2; break;}
      else {size=size+3; read=read+4;}}}
  /* account for difference between fd_malloc() and size returned */
  fd_malloc_adjust(size-(to-from+1));
  *sizep = size;
  return result;
}

/* Translating encoded words in headers */

static struct FD_TEXT_ENCODING *lookup_charset(char *start,char *end)
{
  char buf[64];
  if (end-start > 63) return NULL;
  else {
    strncpy(buf,start,end-start); buf[end-start]=NUL;
    return fd_get_encoding(buf);}
}

static fd_u8char *try_to_make_utf8
  (char *start,char *end,struct FD_TEXT_ENCODING *enc)
{
  fd_u8char *result;
  FD_WITH_HANDLING
    result=fd_make_utf8(start,end,enc);
  FD_ON_EXCEPTION
    if (fd_theException() == fd_InvalidChar) {
      result=fd_make_utf8(start,end,default_mime_charset);
      fd_clear_exception();}
    else fd_reraise();
  FD_END_HANDLING;
  return result;
}

static fd_u8char *translate_header(char *string,int utf8p)
{
  struct FD_STRING_STREAM ss; fd_u8char *scan=string;
  FD_INITIALIZE_STRING_STREAM(&ss,strlen(string));
  while (*scan) {
    if ((scan[0]=='=') && (scan[1]=='?')) { /* Encoded word */
      char *charset_start=scan+2;
      char *charset_end=strchr(charset_start,'?');
      char text_encoding=((charset_end) ? (charset_end[1]) : (0));
      char *data_start=((charset_end) ? (charset_end+3) : (NULL));
      char *data_end=((data_start) ? (strstr(data_start,"?=")) : (NULL));
      struct FD_TEXT_ENCODING *enc; char *data=NULL; int size;
      if ((data_end == NULL) || (strchr("QqBb",text_encoding) == NULL)) {
	char buf[32]; strncpy(buf,scan,31); buf[31]=NUL;
	fd_warn("Bad encoded word: %s",buf);
	scan=scan+2;}
      else {
	fd_u8char *result;
	enc=lookup_charset(charset_start,charset_end);
	if ((text_encoding == 'Q') || (text_encoding == 'q'))
	  data=read_quoted_printable(data_start,data_end,&size);
	else if ((text_encoding == 'B') || (text_encoding == 'b'))
	  data=read_base64(data_start,data_end,&size);
	if (enc) result=try_to_make_utf8(data,data+size,enc);
	else result=try_to_make_utf8(scan,data_end+2,NULL);
	fd_sputs(&ss,result); fd_xfree(result);
	if (data) fd_free(data,(data_end-data_start)+1);
	scan=data_end+2;}}
    else if ((scan[0] == '\r') && (scan[1] == '\n')) {
      fd_sputc(&ss,'\n'); scan=scan+2;}
    else if (utf8p) {
      int ch=fd_sgetc(&scan); fd_sputc(&ss,ch);}
    else {
      int ch=*scan++; fd_sputc(&ss,ch);}}
  return ss.ptr;
}

/* Turning headers into slotmaps */

static fd_lisp header2slotmap(char *header,int utf8p)
{
  fd_lisp slotmap=fd_make_slotmap(8), slot;
  fd_u8char *utf8_header=translate_header(header,utf8p), *scan=utf8_header;
  struct FD_STRING_STREAM ostream; int ch=fd_sgetc(&scan);
  enum parse_state {field_name, field_value} ps=field_name;
  FD_INITIALIZE_STRING_STREAM(&ostream,512);
  while (ch>=0) 
    if (ps == field_name)
      if (!((isprint(ch)) && (ch<0x80)))
	fd_raise_exception(BadFieldName);
      else if (ch == ':') {
	slot=fd_intern(ostream.ptr,ostream.size);
	ostream.ptr[0]=0; ostream.size=0;
	ch=fd_sgetc(&scan);
	while ((ch>0) && (fd_isspace(ch)) && (ch != '\n')) ch=fd_sgetc(&scan); 
	ps=field_value;}
      else {fd_sputc(&ostream,ch); ch=fd_sgetc(&scan);}
    else if (ch == '\r') {
      int nch=fd_sgetc(&scan);
      if (nch != '\n') fd_sputc(&ostream,ch); ch=nch;}
    else if (ch == '\n') {
      int nch=fd_sgetc(&scan);
      if (nch == '\n') {
	/* You're done, so store the current field and return the slotmap */
	fd_prim_add(slotmap,mime_slots_slotid,slot);
	prim_add_consed(slotmap,slot,sstream_string(&ostream));
	fd_xfree(utf8_header);
	return slotmap;}
      else if (fd_isspace(nch)) {
	fd_sputc(&ostream,ch); fd_sputc(&ostream,nch);}
      else {
	fd_prim_add(slotmap,mime_slots_slotid,slot);
	prim_add_consed(slotmap,slot,sstream_string(&ostream));
	/* Reinitialize string stream */
	FD_INITIALIZE_STRING_STREAM(&ostream,512);
	ps=field_name; fd_sputc(&ostream,nch);}
      ch=fd_sgetc(&scan);}
    else {fd_sputc(&ostream,ch); ch=fd_sgetc(&scan);}
  fd_prim_add(slotmap,mime_slots_slotid,slot);
  prim_add_consed(slotmap,slot,sstream_string(&ostream));
  fd_xfree(utf8_header);
  return slotmap;
}

/* Handling compound headers */

static void do_add
  (fd_lisp entity,struct FD_STRING_STREAM *ps,struct FD_STRING_STREAM *vs)
{
  if (ps->size == 0) {}
  else if (vs->size)
    fd_prim_add(entity,fd_make_symbol(ps->ptr),fd_copy_string(vs->ptr));
  else fd_prim_add(entity,fd_make_symbol(ps->ptr),FD_TRUE);
}

static void handle_compound_header
   (fd_lisp entity,fd_u8char *header,int len,char *prefix)
{
  fd_u8char *scan=header;
  fd_u8char *limit=scan+len;
  if (strchr(scan,';')) {
    int c, plen=strlen(prefix)+1;
    fd_u8char *value_end=strchr(scan,';');
    fd_lisp direct_value=fd_make_substring(scan,value_end);
    struct FD_STRING_STREAM var_stream, val_stream;
    /* Add the direct value */
    fd_prim_add_consed(entity,fd_make_symbol(prefix),direct_value);
    /* Skip initial whitespace */
    scan=value_end+1; c=fd_sgetc(&scan);
    FD_INITIALIZE_STRING_STREAM(&var_stream,64);
    FD_INITIALIZE_STRING_STREAM(&val_stream,64);
    /* Initialize the prefix of the var name */
    fd_sputs(&var_stream,prefix); fd_sputs(&var_stream,".");
    while (1) { /* Iterate over x=y entries */
      /* Skip over whitespace */
      while (fd_isspace(c)) c=fd_sgetc(&scan);
      if (c < 0) break;
      /* Copy the parameter name */
      fd_sputc(&var_stream,fd_toupper(c)); while (1) { 
	c=fd_sgetc(&scan);
	if ((c == '=') || (c < 0)) break;
	else fd_sputc(&var_stream,fd_toupper(c));}
      if (c < 0) break;
      /* Copy the parameter value, handling quotes */
      c=fd_sgetc(&scan);
      while (fd_isspace(c)) c=fd_sgetc(&scan);
      if (c == '"') while (1) {
	c=fd_sgetc(&scan); 
	if ((c == '"') || (c < 0)) {
	  if (c == '"') c=fd_sgetc(&scan); break;}
	else fd_sputc(&val_stream,c);}
      else while (1) {
	if ((c == ';') || (c < 0)) break;
	else fd_sputc(&val_stream,c);
      	c=fd_sgetc(&scan);}
      /* Do the add */
      do_add(entity,&var_stream,&val_stream);
      if (c < 0) break; else c=fd_sgetc(&scan);
      var_stream.ptr[plen+1]=NUL; var_stream.size=plen;
      val_stream.ptr[0]=NUL; val_stream.size=0;}
    /* Do a final add if neccessary */
    do_add(entity,&var_stream,&val_stream);
    fd_xfree(var_stream.ptr); fd_xfree(val_stream.ptr);}
  else fd_prim_add_consed
	 (entity,fd_make_symbol(prefix),fd_copy_string(header));
}

/* Interperting headers to read the body */

static void interpret_headers
  (fd_lisp headers,char **mime_type,char **mime_subtype,
   char **boundary,char **encoding,int *clength,
   struct FD_TEXT_ENCODING **charset)
{
  fd_lisp given_encoding=fd_prim_get(headers,content_encoding_slot);
  fd_lisp content_type=fd_prim_get(headers,content_type_slot);
  fd_lisp content_disposition=fd_prim_get(headers,content_disposition_slot);
  fd_lisp content_length=fd_prim_get(headers,content_length_slot);
  if (FD_EMPTYP(given_encoding))
    given_encoding=fd_prim_get(headers,content_xfer_encoding_slot);
  if (FD_EMPTYP(given_encoding)) *encoding=fd_strdup("8bit");
  else if (STRINGP(given_encoding))
    *encoding=fd_strdup(STRING_DATA(given_encoding));
  else fd_raise_lisp_exception(UnknownBinaryEncoding,"",given_encoding);
  fd_decref(given_encoding);
  if (FD_STRINGP(content_disposition))
    handle_compound_header
      (headers,FD_STRING_DATA(content_disposition),
       FD_STRING_LENGTH(content_disposition),
       "DISPOSITION");
  fd_decref(content_disposition);
  if (FD_EMPTYP(content_type)) {
    fd_decref(content_length);
    *mime_type=fd_strdup("text"); *mime_subtype=fd_strdup("plain");
    *boundary=(char *)NULL; *charset=fd_get_encoding("US-ASCII");}
  else if (STRINGP(content_type)) {
    fd_u8char *typestring=STRING_DATA(content_type);
    fd_u8char *major_end=strchr(typestring,'/');
    fd_u8char *minor_end=strchr(typestring,';');
    handle_compound_header
      (headers,typestring,FD_STRING_LENGTH(content_type),"TYPE");
    if (major_end) {
      *major_end=NUL; *mime_type=fd_strdup(typestring); *major_end='/';
      if (minor_end) {
	*minor_end=NUL; *mime_subtype=fd_strdup(major_end+1); *minor_end=';';}
      else *mime_subtype=fd_strdup(major_end+1);}
    else if (minor_end) {
      *minor_end=0; *mime_type=fd_strdup(typestring); *minor_end=';';
      *mime_subtype = fd_strdup("");}
    else {
      *mime_type = fd_strdup(typestring);
      *mime_subtype = fd_strdup("");}
    fd_decref(content_type);
    if (FD_EMPTYP(content_length)) {if (clength < 0) *clength=0;}
    else if (FD_FIXNUMP(content_length)) {
      if (*clength != FD_FIXLISP(content_length))
	fd_warn(_("MIME content length mismatch"));
      if (FD_FIXLISP(content_length) < *clength)
	*clength=FD_FIXLISP(content_length);}
    else if (FD_STRINGP(content_length)) {
      int len; sscanf(FD_STRING_DATA(content_length),"%d",&len);
      if (*clength != len) fd_warn("MIME content length mismatch");
      if (len < *clength) *clength=len;
      fd_decref(content_length);}
    else fd_raise_exception(BadContentLength);
    prim_add_consed(headers,mime_type_slot,fd_parse_string(*mime_type));
    prim_add_consed(headers,mime_type_slot,fd_make_string(*mime_subtype));
    {
      fd_lisp charset_spec=fd_prim_get(headers,type_charset_slotid);
      if (FD_STRINGP(charset_spec))
	if (!((strcasecmp(*mime_type,"text") == 0) ||
	      (strcasecmp(*mime_type,"message") == 0) ||
	      (strcasecmp(*mime_type,"application") == 0)))
	  fd_warn("Charset arg doesn't make sense for %s",*mime_type);
	else {
	  struct FD_TEXT_ENCODING *enc=
	    fd_get_encoding(FD_STRING_DATA(charset_spec));
	  if (enc == NULL) {
	    fd_warn("Unknown charset %q",charset_spec);
	    fd_prim_add(headers,charset_slot,charset_spec);}
	  else prim_add_consed
		 (headers,charset_slot,fd_make_string(enc->names[0]));
	  *charset=enc;}
      fd_decref(charset_spec);}
    {
      fd_lisp boundary_spec=fd_prim_get(headers,type_boundary_slotid);
      if (FD_STRINGP(boundary_spec))
	if (!((strcasecmp(*mime_type,"multipart") == 0) ||
	      (strcasecmp(*mime_type,"message") == 0)))
	  fd_warn("Boundary arg doesn't make sense for %s",*mime_type);
	else {
	  *boundary=fd_strdup(FD_STRING_DATA(boundary_spec));}
      fd_decref(boundary_spec);}}
  else fd_raise_lisp_exception(WeirdContentType,"",content_type);
}

static void free4(char *s1,char *s2,char *s3,char *s4)
{
  if (s1) fd_xfree(s1);
  if (s2) fd_xfree(s2);
  if (s3) fd_xfree(s3);
  if (s4) fd_xfree(s4);
}

static fd_lisp parse_entity
  (char *packet,int len,int just_head,
   struct FD_TEXT_ENCODING *charset,int crlf)
{
  char *eoh, *header_end, *newline, *packet_end=packet+len;
  int eoh_size;
  if (crlf) {header_end="\r\n\r\n"; newline="\r\n"; eoh_size=4;}
  else {header_end="\n\n"; newline="\n"; eoh_size=2;}
  eoh=strstr(packet,header_end);
  if (*packet == '\n') return encode_string(packet+1,len-1,NULL,charset);
  else if ((packet[0] == '\r') && (packet[1] == '\n'))
    return encode_string(packet+2,len-2,NULL,charset);
  else if ((eoh == NULL) || (eoh > packet_end))
    return encode_string(packet,len,NULL,charset);
  else {
    fd_lisp headers, body=FD_EMPTY_LIST;
    char *content_start=eoh+eoh_size;
    char *header_bytes=fd_xmalloc(eoh-packet+1);
    char *boundary=NULL, *mime_type=NULL, *mime_subtype=NULL, *data_encoding;
    int content_length=len-(content_start-packet);
    if (just_head == JUST_HEAD) content_length=-1;
    /* Copy the header bytes */
    strncpy(header_bytes,packet,eoh-packet); header_bytes[eoh-packet]=0;
    /* Actually parse the headers and free the data */
    headers=header2slotmap(header_bytes,(charset == utf8_encoding));
    fd_xfree(header_bytes);
    if (just_head == JUST_HEAD) return headers;
    interpret_headers
      (headers,&mime_type,&mime_subtype,&boundary,&data_encoding,
       &content_length,&charset);
    if ((strcasecmp(data_encoding,"8bit") == 0) ||
	(strcasecmp(data_encoding,"7bit") == 0) ||
	(strcasecmp(data_encoding,"binary") == 0)) {
      free(data_encoding); data_encoding=NULL;}
    if (boundary) {
      int seplen;
      char separator[128], terminator[128], *start, *next;
      if (data_encoding)
	fd_raise_exception(HeterogenousMultiPart);
      /* Make up most of the separator */
      strcpy(separator,newline); strcat(separator,"--");
      strcat(separator,boundary);
      /* Copy the separator to the terminator */
      strcpy(terminator,separator);
      /* Terminate the separator */
      strcat(separator,newline);
      /* Terminate the terminator */
      strcat(terminator,"--");
      strcat(terminator,newline);
      seplen=strlen(separator); 
      /* We go back one (eoh-1) in case the prologue is empty,
         and the initial boundary is right after the header terminating \n\n */
      start=find_separator(separator,content_start-2,packet_end);
      if (start == NULL) fd_raise_exception(MalformedMIMEMessage);
      /* Add prologue */
      if (start > content_start)
	prim_add_consed
	  (headers,prologue_slot,fd_make_substring(content_start,start));
      while (1) {
	char *data_start=start+seplen;
	next=find_separator(separator,data_start,packet_end);
	if ((next == NULL) || (next >= packet_end)) break;
	body=FD_MAKE_PAIR
	  (parse_entity(data_start,next-data_start,just_head,charset,crlf),
	   body);
	start=next;}
      next=find_separator(terminator,start+seplen,packet_end);
      /* This next is actually the final boundary */
      if (next) {
	if (next+strlen(terminator) < packet_end) {
	  char *epilogue=next+strlen(terminator);
	  prim_add_consed
	    (headers,epilogue_slot,
	     encode_string(epilogue,packet_end-epilogue,
			   data_encoding,charset));}
	body=FD_MAKE_PAIR(parse_entity(start+seplen,next-(start+seplen),
				       just_head,charset,crlf),
			  body);}
      else fd_raise_exception(UnterminatedMIMEData);
      prim_add_consed(headers,content_slot,reverse_list(body));
      free4(mime_type,mime_subtype,boundary,data_encoding);
      fd_decref(body);
      return headers;}
    else if ((strcasecmp(mime_type,"text") == 0) ||
	     (strcasecmp(mime_type,"message") == 0)) {
      prim_add_consed
	(headers,content_slot,
	 encode_string(content_start,content_length,data_encoding,charset));
      free4(mime_type,mime_subtype,boundary,data_encoding);
      return headers;}
    else {
      prim_add_consed
	(headers,content_slot,
	 encode_packet(content_start,content_length,data_encoding));
      free4(mime_type,mime_subtype,boundary,data_encoding);
      return headers;}}
}

/* Writing MIME */

#if 0
static void write_mime_segment(fd_u8char *start,fd_u8char *end,FILE *f)
{
  fd_u8char *scan=start;
  while ((scan < end) && (*scan <0x80) &&
	 (!(iscntrl(*scan))) && (!((*scan == '?') || (*scan == '='))))
    scan++;
  if (scan == end) fwrite(start,1,end-start,f);
  else {
    fprintf(f,"=?utf8?Q?");
    scan=start; while (scan < end)
      if ((*scan < 0x80) && (!(iscntrl(*scan))) && (!(ispunct(*scan))))
	fputc(*scan++,c);
      else fprintf(f,"=%2x=",*scan++);
    fprintf(f,"?=");}
}

static void write_mime_string(fd_u8char *s,FILE *f)
{
  fd_u8char *scan=s;
  while (*scan) {
    fd_u8char *start=scan, *end=scan;
    int c=fd_sgetc(&scan);
    while ((scan < 0x80) && (!(iscntrl(c)))) {
      end=scan; c=fd_sgetc(&scan);}}
}

static void write_mime(fd_lisp slotmap,FILE *f)
{
  fd_lisp content=fd_prim_get(slotmap,content_slotid);
  fd_lisp fields=fd_prim_get(slotmap,mime_slots_slotid);
  FD_DO_CHOICES(field,fields) {
    fd_lisp value=fd_prim_get(slotmap,field);
    fd_u8char *value_string;
    if (FD_STRINGP(value))
      value_string=fd_strdup(FD_STRING_DATA(value));
    else value_string=fd_object_to_string(value);
    write_mime_string(FD_SYMBOL_NAME(field),f);
    fprintf(f,": ");
    write_mime_string(value_string,f);
    fd_xfree(value_string);}
  FD_END_DO_CHOICES;
}
#endif

/* FDScript Primitives */

static fd_lisp lisp_read_mime_cproc(fd_lisp input)
{
  if (FD_PACKETP(input)) 
    if ((FD_PACKET_DATA(input))[0] == NUL) { /* Assume UCS-2 */
      int crlfs=0;
      uchar *scan=FD_PACKET_DATA(input), *limit=scan+FD_PACKET_LENGTH(input);
      while (scan+1 < limit)
	if (scan[1] == '\n') break;
	else if ((scan[1] == '\r') && (scan+3 < limit) && (scan[3] == '\n')) {
	  crlfs=1; break;}
	else scan=scan+2;
      return parse_entity
	(FD_PACKET_DATA(input),FD_PACKET_LENGTH(input),
	 WITH_CONTENT,ucs2_encoding,crlfs);}
    else { /* Assume some ASCII-compatible encoding */
      int crlfs=0;
      uchar *scan=FD_PACKET_DATA(input), *limit=scan+FD_PACKET_LENGTH(input);
      while (scan < limit)
	if (scan[1] == '\n') break;
	else if ((scan[1] == '\r') && (scan+3 < limit) && (scan[3] == '\n')) {
	  crlfs=1; break;}
	else scan=scan+1;
      return parse_entity
	(FD_PACKET_DATA(input),FD_PACKET_LENGTH(input),
	 WITH_CONTENT,latin1_encoding,crlfs);}
  else if (FD_STRINGP(input))
    return parse_entity(FD_STRING_DATA(input),FD_STRING_LENGTH(input),
			WITH_CONTENT,utf8_encoding,0);
  else fd_type_error("not a string or packet",input);
}

static fd_lisp http_urlget(char *url)
{
  /* First, we get the data */
  int size; char *urlcontents=fd_http_get(url,&size);
  fd_lisp entity;
  if (urlcontents)
    if (strncmp(urlcontents,"\r\n",2) == 0) /* headerless content */
      entity=fd_make_string(urlcontents+2);
  else 
    /* We assume US-ASCII, but the header might tell us something else. */
    entity=parse_entity
      (urlcontents,size,WITH_CONTENT,fd_get_encoding("LATIN-0"),1);
  else {
    perror(url); FD_CLEAR_ERR(); return FD_EMPTY_CHOICE;}
  fd_xfree(urlcontents);
  if (STRINGP(entity)) {
    fd_lisp smap=fd_make_slotmap(3);
    fd_frame_add(smap,content_slot,entity);
    fd_frame_add(smap,fd_make_symbol("URL"),fd_make_string(url));
    fd_frame_add(smap,content_type_slot,text_symbol);
    fd_decref(entity); /* Get's ref'd by fd_frame_add */
    return smap;}
  else return entity;
}

static fd_lisp lisp_urlhead_cproc(fd_lisp arg)
{
  if (STRINGP(arg)) {
    char *url=fd_strdup(STRING_DATA(arg));
    char *urlcontents=fd_http_head(url,NULL);
    /* We assume US-ASCII, but the header might tell us something else. */
    fd_lisp entity=parse_entity
      (urlcontents,strlen(urlcontents),
       JUST_HEAD,fd_get_encoding("LATIN-1"),1);
    fd_xfree(urlcontents); fd_xfree(url);
    if (STRINGP(entity)) {
      fd_lisp smap=fd_make_slotmap(3);
      fd_frame_add(smap,content_slot,entity);
      fd_frame_add(smap,fd_make_symbol("URL"),arg);
      fd_frame_add(smap,content_type_slot,text_symbol);
      fd_decref(entity); /* Get's ref'd by fd_frame_add */
      return smap;}
    else return entity;}
  else fd_type_error("URL is not a string",arg);
}

/** Sending email **/

static fd_lisp lisp_send_email_lexpr(fd_lisp args)
{
  fd_lisp destination, content, fields;
  fd_get_args("SEND-EMAIL",args,&destination,FD_VOID,&content,FD_VOID,
	      &fields,FD_EMPTY_CHOICE,
	      NULL);
  if (STRINGP(content)) {
    /* Should be more general here... */
    char *converted_text=
      fd_localize_utf8(fd_strdata(content),latin1_encoding);
    fd_send_smtp_mail(fd_strdata(destination),converted_text,fields);
    fd_xfree(converted_text);
    return FD_VOID;}
  else if (FD_SLOTMAPP(content))
    if (FD_EMPTYP(fields)) {
      fd_lisp actual_content=FD_EMPTY_CHOICE, new_fields=fd_make_slotmap(16);
      int i=0, l=FD_SLOTMAP_SIZE(content);
      char *converted_text;
      while (i < l) {
	fd_lisp slot=FD_SLOTMAP_KEY(content,i);
	fd_lisp value=FD_SLOTMAP_VALUE(content,i);
	if (FD_LISP_EQ(slot,content_slot))
	  if (STRINGP(value)) 
	    actual_content=fd_incref(value);
	  else fd_raise_exception
		 (_("Only textual email content can currently be sent"));
	else fd_prim_add(new_fields,slot,value);
	i++;}
      converted_text=
	fd_localize_utf8
	(fd_strdata(actual_content),latin1_encoding);
      fd_send_smtp_mail(fd_strdata(destination),converted_text,new_fields);
      fd_xfree(converted_text); fd_decref(actual_content); fd_decref(new_fields);
      return FD_VOID;}
    else fd_raise_exception(_("Both args to SEND-EMAIL can't be slotmaps"));
  else fd_type_error(_("Email content must be string or slotmap"),content);
}

FDTEXT_EXPORT
/* fd_parse_mime:
    Arguments: a C string and an integer length
    Returns: a slotmap

  The slotmap is derived from MIME-parsing the string;
  the CONTENT slot of the returned slotmap is either a string
  or a list of slotmaps (for multipart mime messages).
*/
fd_lisp fd_parse_mime(char *packet,int len)
{
  return parse_entity(packet,len,WITH_CONTENT,latin1_encoding,1);
}

void initialize_mime_c()
{

  type_slotid=fd_make_symbol("TYPE");
  type_boundary_slotid=fd_make_symbol("TYPE.BOUNDARY");
  type_charset_slotid=fd_make_symbol("TYPE.CHARSET");

  content_slot=fd_make_symbol("CONTENT");
  content_type_slot=fd_make_symbol("CONTENT-TYPE");
  content_disposition_slot=fd_make_symbol("CONTENT-DISPOSITION");
  content_encoding_slot=fd_make_symbol("CONTENT-ENCODING");
  content_xfer_encoding_slot=fd_make_symbol("CONTENT-TRANSFER-ENCODING");
  content_length_slot=fd_make_symbol("CONTENT-LENGTH");
  mime_type_slot=fd_make_symbol("MIME-TYPE");
  charset_slot=fd_make_symbol("CHARSET");
  prologue_slot=fd_make_symbol("PROLOGUE");
  epilogue_slot=fd_make_symbol("EPILOGUE");
  text_symbol=fd_make_symbol("TEXT");
  mime_slots_slotid=fd_make_symbol("MIME-SLOTS");

  ucs2_encoding=fd_get_encoding("UCS-2");
  utf8_encoding=fd_get_encoding("UTF-8");
  latin1_encoding=fd_get_encoding("LATIN-1");
  default_mime_charset=latin1_encoding;

  fd_add_cproc(NULL,"READ-MIME",1,lisp_read_mime_cproc);
  fd_add_alias(NULL,"PARSE-RFC822","READ-MIME");

  fd_add_restricted_lexpr
    ("SEND-EMAIL",FD_NORMAL_LEXPR,lisp_send_email_lexpr);

  fd_add_restricted_cproc("URLHEAD",1,lisp_urlhead_cproc);

  fd_register_url_protocol("http",http_urlget);

  fd_register_source_file("mime",__DATE__,vcid);
}



/* File specific stuff */

/* The CVS log for this file
   $Log: mime.c,v $
   Revision 1.32  2005/01/14 16:48:50  haase
   Updated copyrights to 2005

   Revision 1.31  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.30  2004/03/08 12:57:48  haase
   Raw returns in strings are not translated into \r

   Revision 1.29  2003/12/18 03:41:34  haase
   Cleaned up separation of fdtext and fdwww

   Revision 1.28  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.27  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.26.2.4  2003/08/06 18:41:08  haase
   Updated copyright notices to 2003

   Revision 1.26.2.3  2003/07/05 19:04:29  haase
   Fixed bug with non-existent MIME content-length zeroing out actual content length

   Revision 1.26.2.2  2003/01/26 20:50:22  haase
   Misc. fixes, especially GC

   Revision 1.26.2.1  2002/09/26 02:04:38  haase
   Ralph Campbell fixes  MIME parsing, especially BASE64 encoding

   Revision 1.26  2002/06/14 17:11:28  haase
   Various removals to reflect deprecated models (like freeze/thaw-choice) or removed functionality (like super pool aliasing)

   Revision 1.25  2002/05/21 17:02:29  haase
   Remove spurious has_crlfs definition

   Revision 1.24  2002/05/19 10:12:30  haase
   Now checks packets to see if they have crlfs in them (don't just assume it).
   Handle the case of the empty field without gobbling up the subsequent field.
   Use new fd_intern to convert field names to slotids

   Revision 1.23  2002/05/12 15:10:18  haase
   Fixed eoh size bug

   Revision 1.22  2002/05/12 13:12:56  haase
   Fixes to make mime parsing more forgiving

   Revision 1.21  2002/05/07 08:01:53  haase
   Added some field sub-parsing to mime parsing (content-type and content-disposition)

   Revision 1.20  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.19  2002/04/16 10:56:18  haase
   Added handling for headerless reponses to http_urlget

   Revision 1.18  2002/04/06 18:51:57  haase
   Fixed default field arguments for send-email

   Revision 1.17  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
