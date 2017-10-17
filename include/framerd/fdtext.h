/*
  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: fdtext.h,v 1.11 2003/12/18 03:38:24 haase Exp $

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

  This is the file fdtext.h declaring data types and prototypes for FramerD's
   text manipulation functions.

*************************************************************************/

#ifndef FRAMERD_FDTEXT_H /* If defined, skip the file */
#define FRAMERD_FDTEXT_H

#ifndef FDTEXT_EXPORT
#define FDTEXT_EXPORT extern
#endif

FDTEXT_EXPORT fd_lispenv fd_texttools_env;

/* Matcher functions */

typedef fd_lisp
  (*tx_matchfn)(fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
typedef int
  (*tx_searchfn)(fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
typedef fd_lisp
  (*tx_extractfn)(fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);

FDTEXT_EXPORT fd_lisp fd_text_matcher
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
FDTEXT_EXPORT fd_lisp fd_text_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
FDTEXT_EXPORT int fd_text_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
FDTEXT_EXPORT fd_lisp fd_tx_closure(fd_lisp expr,fd_lispenv env);

FDTEXT_EXPORT void fd_add_match_operator
  (fd_u8char *label,tx_matchfn matcher,tx_searchfn searcher,tx_extractfn extract);
FDTEXT_EXPORT int fd_text_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);
FDTEXT_EXPORT fd_lisp fd_text_subst(fd_lisp pat,fd_lisp string);

struct TX_MATCH_OPERATOR {
  fd_lisp symbol; tx_matchfn matcher; tx_searchfn searcher; tx_extractfn extract;};

FDTEXT_EXPORT fd_lisp fd_tx_closure(fd_lisp expr,fd_lispenv env);

struct TX_CLOSURE { /* Text pattern closure */
  fd_lisp pattern; fd_lispenv env;};

/* Other special parsers */

FDTEXT_EXPORT fd_lisp fd_get_timepoints(fd_u8char *string);
FDTEXT_EXPORT void fd_extract_proper_names
  (fd_u8char *string,fd_lisp *into,
   fd_hashset title_abbrevs,fd_hashset stop_words,
   fd_hashset name_suffixes);
FDTEXT_EXPORT fd_lisp fd_parse_timestring(fd_u8char *string,int american,fd_lisp base);
FDTEXT_EXPORT char *fd_stem_english_word(fd_u8char *original);

FDTEXT_EXPORT fd_lisp fd_md5(fd_lisp string);

#define MATCH_IGNORE_CASE       (1)
#define MATCH_IGNORE_DIACRITICS (MATCH_IGNORE_CASE<<1)
#define MATCH_COLLAPSE_SPACES   (MATCH_IGNORE_DIACRITICS<<1)
#define MATCH_DO_BINDINGS       (MATCH_COLLAPSE_SPACES<<1)

#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: fdtext.h,v $
   Revision 1.11  2003/12/18 03:38:24  haase
   Cleaned up header file separation of fdtext and fdwww modules

   Revision 1.10  2003/11/29 14:28:21  haase
   Separated FDTEXT and FDWWW libraries

   Revision 1.9  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.8.2.1  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.8  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
