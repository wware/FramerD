/* C Mode */

/* www.c
   Interet related technology primitives for FramerD
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

static char vcid[] = "$Id: www.c,v 1.4 2005/01/14 16:48:50 haase Exp $";


/* Initial definitions */

/* We want both of these to be fast. */

#include <time.h>
#include <sys/stat.h>
#include <assert.h>
#ifndef WIN32
#include <dirent.h>
#endif

#include "fdwww.h"
fd_lispenv fd_wwwtools_env;

/** Converting entity refs **/

static struct FD_HASHTABLE default_entities;

struct ENTITY_TABLE { char *entity; int code; };
static struct ENTITY_TABLE entities[]={
  {"nbsp",' '},
  {"quot",'"'},
  {"amp",'&'},
  {"lt",'<'},
  {"gt",'>'},
  {"iexcl",0x00A1},
  {"cent",0x00A2},
  {"pound",0x00A3},
  {"curren",0x00A4},
  {"yen",0x00A5},
  {"brvbar",0x00A6},
  {"sect",0x00A7},
  {"uml",0x00A8},
  {"copy",0x00A9},
  {"ordf",0x00AA},
  {"laquo",0x00AB},
  {"not",0x00AC},
  {"shy",0x00AD},
  {"reg",0x00AE},
  {"macr",0x00AF},
  {"deg",0x00B0},
  {"plusmn",0x00B1},
  {"sup2",0x00B2},
  {"sup3",0x00B3},
  {"acute",0x00B4},
  {"micro",0x00B5},
  {"para",0x00B6},
  {"middot",0x00B7},
  {"cedil",0x00B8},
  {"sup1",0x00B9},
  {"ordm",0x00BA},
  {"raquo",0x00BB},
  {"frac14",0x00BC},
  {"frac12",0x00BD},
  {"frac34",0x00BE},
  {"iquest",0x00BF},
  {"Agrave",0x00C0},
  {"Aacute",0x00C1},
  {"Acirc",0x00C2},
  {"Atilde",0x00C3},
  {"Auml",0x00C4},
  {"Aring",0x00C5},
  {"AElig",0x00C6},
  {"Ccedil",0x00C7},
  {"Egrave",0x00C8},
  {"Eacute",0x00C9},
  {"Ecirc",0x00CA},
  {"Euml",0x00CB},
  {"Igrave",0x00CC},
  {"Iacute",0x00CD},
  {"Icirc",0x00CE},
  {"Iuml",0x00CF},
  {"ETH",0x00D0},
  {"Ntilde",0x00D1},
  {"Ograve",0x00D2},
  {"Oacute",0x00D3},
  {"Ocirc",0x00D4},
  {"Otilde",0x00D5},
  {"Ouml",0x00D6},
  {"times",0x00D7},
  {"Oslash",0x00D8},
  {"Ugrave",0x00D9},
  {"Uacute",0x00DA},
  {"Ucirc",0x00DB},
  {"Uuml",0x00DC},
  {"Yacute",0x00DD},
  {"THORN",0x00DE},
  {"szlig",0x00DF},
  {"agrave",0x00E0},
  {"aacute",0x00E1},
  {"acirc",0x00E2},
  {"atilde",0x00E3},
  {"auml",0x00E4},
  {"aring",0x00E5},
  {"aelig",0x00E6},
  {"ccedil",0x00E7},
  {"egrave",0x00E8},
  {"eacute",0x00E9},
  {"ecirc",0x00EA},
  {"euml",0x00EB},
  {"igrave",0x00EC},
  {"iacute",0x00ED},
  {"icirc",0x00EE},
  {"iuml",0x00EF},
  {"eth",0x00F0},
  {"ntilde",0x00F1},
  {"ograve",0x00F2},
  {"oacute",0x00F3},
  {"ocirc",0x00F4},
  {"otilde",0x00F5},
  {"ouml",0x00F6},
  {"divide",0x00F7},
  {"oslash",0x00F8},
  {"ugrave",0x00F9},
  {"uacute",0x00FA},
  {"ucirc",0x00FB},
  {"uuml",0x00FC},
  {"yacute",0x00FD},
  {"thorn",0x00FE},
  {"yuml",0x00FF},
  {NULL,0}};

static fd_u8char *find_apos(fd_u8char *s)
{
  fd_u8char *apos=strstr(s,"\302\222");
  while (apos) {
    if ((apos > s) && (apos[-1]<0x80)) return apos;
    else apos=strstr(apos+1,"\302\222");}
  return apos;
}

static int convert_apostrophes(fd_u8char *s,int size)
{
  fd_u8char *apos=strstr(s,"\302\222"); 
  while (apos) {
    memmove(apos+1,apos+2,size-(apos+2-s)); *apos='\''; s[--size]='\0';
    apos=find_apos(apos+2);}
  return size;
}

#define strneq(s1,s2,n) (strncmp((s1),(s2),(n)) == 0)

static void convert_entities
  (struct FD_STRING_STREAM *out,fd_u8char *in,fd_lisp emaps)
{
  fd_u8char *scan=in, *amp=strchr(scan,'&'); int c=0;
  while (amp) {
    fd_u8char *end=strchr(amp,';'); int invalid_escape=0;
    /* Output everything up to the ampersand */
    if (amp-scan) fd_sputn(out,scan,amp-scan); scan=amp;
    /* Interpret an escape */
    if ((end == NULL) || (end-scan > 64)) invalid_escape=1;
    else if ((scan[1] < 0x80) && (isspace(scan[1]))) invalid_escape=1;
    else if ((scan[1]=='#') && (end-scan < 16)) {
      int success=0;
      if (scan[2] == 'x') success=sscanf(scan,"&#x%x;",&c);
      else success=sscanf(scan,"&#%d;",&c);
      if (success) fd_sputc(out,c); else invalid_escape=1;}
    /* XML builtins */
    else if ((end-scan == 3) && (strneq(scan,"&gt",3))) fd_sputc(out,'>');
    else if ((end-scan == 3) && (strneq(scan,"&lt",3))) fd_sputc(out,'<');
    else if ((end-scan == 4) && (strneq(scan,"&amp",3))) fd_sputc(out,'&');
    else if ((end-scan == 5) && (strneq(scan,"&quot",3))) fd_sputc(out,'"');
    else if ((end-scan == 5) && (strneq(scan,"&apos",3))) fd_sputc(out,'\'');
    else if (FD_FALSEP(emaps)) invalid_escape=1;
    else {
      fd_lisp entity=FD_EMPTY_CHOICE;
      FD_DO_CHOICES(map,emaps) {
	if (FD_PRIM_TYPEP(map,hashtable_type)) {
	  fd_hashtable h=(fd_hashtable)FD_CPTR_DATA(map);
	  fd_lisp e=fd_hashtable_strget(h,scan+1,(end-scan)-1);
	  if (FD_EMPTYP(entity)) entity=e; else fd_decref(e);}
	else fd_type_error(_("Not a hashtable"),map);}
      END_FD_DO_CHOICES;
      if (FD_EMPTYP(entity))
	entity=fd_hashtable_strget(&(default_entities),scan+1,(end-scan)-1);
      if (FD_STRINGP(entity)) fd_sputs(out,FD_STRING_DATA(entity));
      else if (FD_CHARACTERP(entity))
	fd_sputc(out,FD_CHAR_CODE(entity));
      else invalid_escape=1;
      fd_decref(entity);}
    if (invalid_escape)
      if (end) {
	fd_sputn(out,scan,(end+1)-scan); scan=end+1; amp=strchr(scan,'&');}
      else amp=NULL;
    else {
      scan=end+1; amp=strchr(scan,'&');}}
  fd_sputs(out,scan);
}

static fd_lisp lisp_convert_character_entities_lexpr(fd_lisp args)
{
  fd_lisp strings, tables, apos_flag, results=FD_EMPTY_CHOICE;
  fd_get_args("CONVERT-CHARACTER-ENTITIES",args,&strings,FD_VOID,
	      &tables,FD_EMPTY_CHOICE,
	      &apos_flag,FD_TRUE,NULL);
  {FD_DO_CHOICES(string,strings)
     if (!(FD_STRINGP(string)))
       fd_type_error(_("not a string"),string);
     else if (FD_STRING_LENGTH(string) == 0) {
       FD_ADD_TO_CHOICE(results,fd_incref(string));}
     else {
       struct FD_STRING_STREAM out; fd_lisp result;
       FD_INITIALIZE_STRING_STREAM
	 (&out,fd_roundup(fd_strlen(string),FD_STRING_CHUNK));
       convert_entities(&out,fd_strdata(string),tables);
       if (!(FD_FALSEP(apos_flag)))
	 out.size=convert_apostrophes(out.ptr,out.size);
       result=fd_init_string(out.ptr,out.size);
       FD_ADD_TO_CHOICE(results,result);}
  FD_END_DO_CHOICES;}
  return results;
}

/* New stuff */

void initialize_match_c(void);
void initialize_getnames_c(void);

EXPORTED
void fd_initialize_fdwww()
{
  struct ENTITY_TABLE *e=entities; 
  fd_lispenv menv=fd_make_module();
  fd_wwwtools_env=menv;
  
  fd_init_hashtable(&default_entities,256);
  while (e->entity) {
    fd_lisp key=fd_make_string(e->entity);
    fd_hashtable_set(&default_entities,key,FD_CODE_CHAR(e->code));
    fd_decref(key);
    e++;}

  initialize_mime_c();
  initialize_xml_c();
  initialize_xmleval_c();
  initialize_htmlgen_c();
  
  fd_add_lexpr(menv,"CONVERT-CHARACTER-ENTITIES",FD_ND_LEXPR,
	       lisp_convert_character_entities_lexpr);

  fd_register_module("FDWWW",menv,FD_SAFE_ENV,0);

  fd_register_source_file("www",__DATE__,vcid);
}







/* File specific stuff */

/* The CVS log for this file
   $Log: www.c,v $
   Revision 1.4  2005/01/14 16:48:50  haase
   Updated copyrights to 2005

   Revision 1.3  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.2  2003/12/05 14:58:47  haase
   Reimplemented the module subystem including separate modules.c file

   Revision 1.1  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.46  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.45  2003/09/07 18:23:23  haase
   Added src/text/xmleval.c for FDXML callouts

   Revision 1.44  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.43.2.5  2003/08/06 18:41:08  haase
   Updated copyright notices to 2003

   Revision 1.43.2.4  2003/08/05 21:16:22  haase
   Fixed stack growth on record reading

   Revision 1.43.2.3  2003/01/26 20:50:23  haase
   Misc. fixes, especially GC

   Revision 1.43.2.2  2002/09/26 02:03:38  haase
   Fixed collision between textlet bindings and global bindings by making textlet bindings always check and bind locally

   Revision 1.43.2.1  2002/09/24 19:09:25  haase
   Fixed error checking, nd-handling, etc for entity conversion

   Revision 1.43  2002/07/11 18:43:36  haase
   Added trigram/bigram extraction primitives

   Revision 1.42  2002/07/05 21:17:54  uid59704
   Made string-capitalize capitalize after . and dashes

   Revision 1.41  2002/07/05 11:20:03  haase
   Made string-capitalize capitalize after punctuation

   Revision 1.40  2002/07/03 02:30:07  haase
   Fixed stdspace to strip space prefix

   Revision 1.39  2002/07/01 14:50:48  haase
   More fixes to precision determination in timestring parsing

   Revision 1.38  2002/07/01 02:46:10  haase
   Fixed excess precision bug in fd_parse_timestring and fixed fencepost problem in STDSPACE

   Revision 1.37  2002/06/24 14:50:06  haase
   Added CSEGMENT to break at compound words

   Revision 1.36  2002/06/18 01:05:27  haase
   Fix hiccup in stdspace

   Revision 1.35  2002/06/02 20:54:14  haase
   Made STDSPACE imply string-trim

   Revision 1.34  2002/05/27 18:09:01  haase
   Fixed bug in entity conversion

   Revision 1.33  2002/05/07 08:03:40  haase
   Updated entity conversion to handle hashtable entity maps

   Revision 1.32  2002/05/01 21:46:32  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.31  2002/04/27 17:48:11  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.30  2002/04/20 20:46:23  haase
   Added STRING-CAPITALIZE

   Revision 1.29  2002/04/20 02:38:45  haase
   Fixed double GC problem with TEXTLET

   Revision 1.28  2002/04/20 01:52:33  haase
   Fixed textlet to bind non-deterministcally

   Revision 1.27  2002/04/19 13:19:52  haase
   Fixed bugs involving NULs in UTF-8 strings

   Revision 1.26  2002/04/19 00:18:14  haase
   Fixed some calls to fd_get_args to be null-terminated

   Revision 1.25  2002/04/17 00:30:23  haase
   src/text/text.c

   Revision 1.24  2002/04/08 16:39:45  haase
   Added more internal error checking to gather and related functions

   Revision 1.23  2002/04/04 04:38:13  haase
   Added STDSPACE

   Revision 1.22  2002/04/03 01:54:41  haase
   Fixed fd_get_args bugs

   Revision 1.21  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
