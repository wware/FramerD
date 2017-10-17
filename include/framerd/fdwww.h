/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: fdwww.h,v 1.12 2004/10/04 15:28:20 haase Exp $

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

  This is the file fdwww.h supporting html generation code by providing
   an HTTP stream abstraction.

*************************************************************************/

#ifndef FRAMERD_FDWWW_H /* If defined, skip the file */
#define FRAMERD_FDWWW_H

#ifndef FDWWW_EXPORT
#define FDWWW_EXPORT extern
#endif

FDWWW_EXPORT fd_lispenv fd_wwwtools_env;

#if ((FD_WITH_FASTCGI) && (HAVE_FCGIAPP_H) && (HAVE_LIBFCGI))
#define HAVE_FASTCGI 1
#else
#define HAVE_FASTCGI 0
#endif

#if (!(defined(HTMLGEN_EXPORT)))
#define HTMLGEN_EXPORT extern
#endif

#if (HAVE_FASTCGI)
#include <fcgiapp.h>
#endif

EXPORTED fd_lisp fd_parse_mime(char *data,int len);

enum display_level {get_value, show_value, hide_value};
enum http_generation_phase {
  http_any=0, http_head, http_body, html_head, html_body, html_frameset,
  html_end, xml_content };

#define FD_IS_XML 1
#define FD_TRANSITIONAL_XHTML 2
#define FD_CANONICAL_XML 4

typedef struct FD_HTTP_STREAM {
  enum {stdio, fcgi, sstream, nullstream} stream_type;
  int is_xml; enum http_generation_phase phase;
  fd_hashtable namespaces; int free_namespaces;
  union {
    struct FD_XFILE xfile;
    struct FD_STRING_STREAM *sstream;
#if (HAVE_FASTCGI)
    FCGX_Stream *fcgi;
#endif
  } stream;
} fd_htstream;

HTMLGEN_EXPORT void fd_start_http(char *mime);
HTMLGEN_EXPORT void fd_unparse_xml(fd_lisp,fd_lispenv,fd_htstream *);
HTMLGEN_EXPORT int fd_get_xml_handler(fd_lisp tag,fd_lispenv env,fd_lisp *handler);
HTMLGEN_EXPORT fd_lisp fd_xml_callout(fd_lisp,fd_lisp,fd_lispenv);
HTMLGEN_EXPORT void fd_set_http_phase(enum http_generation_phase p);

HTMLGEN_EXPORT void fd_start_http_output(fd_htstream *s);
HTMLGEN_EXPORT fd_htstream *fd_get_http_output(void);
HTMLGEN_EXPORT void fd_set_http_output_methods
  (void (*_puts)(char *,void *),void (*_putc)(int,void *),
   void (*_putn)(char *,int,void *));
HTMLGEN_EXPORT void fd_http_puts(char *,fd_htstream *);
HTMLGEN_EXPORT void fd_http_write_bytes(char *,int,fd_htstream *);
HTMLGEN_EXPORT void fd_html_write(fd_lisp);

/* XML functions */
EXPORTED fd_exception fd_XML_Parse_Error, fd_XML_Mismatch;
EXPORTED fd_lisp fd_make_xmltag(fd_lisp ns,fd_lisp base);
EXPORTED fd_lisp fd_xmltag_name(fd_lisp tag);
EXPORTED fd_lisp fd_xmltag_namespace(fd_lisp tag);
EXPORTED fd_lisp fd_xml_tag(fd_lisp x);
EXPORTED fd_lisp fd_xml_attributes(fd_lisp x);
EXPORTED fd_lisp fd_xml_content(fd_lisp x);
EXPORTED fd_lisp fd_parse_xml(fd_u8char *string,int err_level,fd_lisp ns);
EXPORTED fd_lisp fd_parse_html(fd_u8char *string);
EXPORTED int fd_xmltagp(fd_lisp tag);
EXPORTED fd_lisp fd_xml_get(fd_lisp xml,fd_lisp key);


#define FD_XMLTAGP(x) \
   ((FD_LRECORD_TYPEP(x,xmltag_tag)) || \
    ((FD_SYMBOLP(x)) && (fd_xmltagp(x))) || \
    ((FD_STRINGP(x)) && (fd_xmltagp(x))))

/* Modules for HTML and XML primitives */
HTMLGEN_EXPORT fd_lispenv fd_wwwtools_env, fd_html_env, fd_xml_env, fd_xmleval_env;

#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: fdwww.h,v $
   Revision 1.12  2004/10/04 15:28:20  haase
   Numerous fixes for WIN32/MINGW compilation

   Revision 1.11  2003/12/18 03:38:24  haase
   Cleaned up header file separation of fdtext and fdwww modules

   Revision 1.10  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.9  2003/09/20 18:04:42  haase
   Fixes and updates to fdxml and htmlgen

   Revision 1.8  2003/09/07 18:25:04  haase
   Added API access to XML structures, XML callouts, and http output

   Revision 1.7  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.6.2.7  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.6.2.6  2003/08/02 13:47:45  haase
   Made HTTP streams use XFILEs, and added nullstream type for noop streams

   Revision 1.6.2.5  2003/02/17 12:26:18  haase
   Changed html_start to http_body

   Revision 1.6.2.4  2002/08/21 22:06:25  haase
   Generalizations to HTTP generation to make it work better with non-HTML XML

   Revision 1.6.2.3  2002/08/21 01:53:21  haase
   Recoded XML/HTML generation to be more general and allow strings to be used as element names; first steps towards better namespace integration into XML generation

   Revision 1.6.2.2  2002/08/10 18:03:22  haase
   Made code use constructor expressions if available

   Revision 1.6.2.1  2002/08/09 16:40:25  haase
   Added FD_ prefix to configuration macros

   Revision 1.6  2002/05/27 13:00:46  haase
   Added external API for http output from C

   Revision 1.5  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
