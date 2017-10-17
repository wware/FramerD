/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  $Id: dtcodes.h,v 1.13 2005/01/14 16:48:44 haase Exp $

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


/* DTYPE I/O definitions */

typedef enum dt_type {
  dt_invalid = 0x00,
  dt_null = 0x01,
  dt_bool = 0x02,
  dt_fixnum = 0x03,
  dt_float = 0x04,
  dt_packet = 0x05,
  dt_string = 0x06,
  dt_symbol = 0x07,
  dt_pair = 0x08,
  dt_vector = 0x09,
  dt_void = 0x0a,
  dt_compound = 0x0b,
  dt_error = 0x0c,
  dt_exception = 0x0d,
  dt_oid = 0x0e,
  dt_zstring = 0x0f,

  dt_extended_character = 0x40,
  dt_extended_numeric = 0x41,
  dt_framerd = 0x42
} dt_type_code;

typedef unsigned char dt_subcode;

enum dt_numeric_types {
  dt_small_bignum=0x00, dt_bignum=0x40, dt_double=0x01,
  dt_rational=0x81, dt_complex=0x82,
  dt_short_int_vector=0x03, dt_int_vector=0x43,
  dt_short_short_vector=0x04, dt_short_vector=0x44,
  dt_short_float_vector=0x05, dt_float_vector=0x45,
  dt_short_double_vector=0x06, dt_double_vector=0x46};
enum dt_character_types 
     { dt_ascii_char=0x00, dt_unicode_char=0x01,
       dt_unicode_string=0x42, dt_unicode_short_string=0x02,
       dt_unicode_symbol=0x43, dt_unicode_short_symbol=0x03,
       dt_unicode_zstring=0x44, dt_unicode_short_zstring=0x04};
enum dt_kr_types {
  dt_set=0xC0, dt_small_set=0x80, 
  dt_slotmap=0xC1, dt_small_slotmap=0x81,
  dt_hashtable=0xC2, dt_small_hashtable=0x82};

#define FD_UTF8_DTYPES (1<<0)


/* File specific stuff */

/* The CVS log for this file
   $Log: dtcodes.h,v $
   Revision 1.13  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.12  2004/08/24 15:24:30  haase
   Added a dt_hashtable type in the dt_framerd package

   Revision 1.11  2004/07/20 09:16:10  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.10  2004/03/31 11:19:55  haase
   Removed attempts at integrating slot schemas into the FramerD core

   Revision 1.9  2004/03/31 03:13:10  haase
   Many fixes and changes to the shared schema implementation

   Revision 1.8  2004/03/30 10:55:01  haase
   Defined _x versions of DTYPE writing to handle extended capabilities

   Revision 1.7  2003/08/31 16:58:21  haase
   Declarations and minor amendations

   Revision 1.6  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.5.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.5.2.1  2003/01/26 20:32:12  haase
   Introduced zstrings

   Revision 1.5  2002/04/30 13:48:33  haase
   Made packaged format for homogenous vectors be bytes rather than lisp vectors, saving space in the external DType representation

   Revision 1.4  2002/04/04 18:51:50  haase
   Renamed some size fields to length to indicate data ordering

   Revision 1.3  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
