/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2003 by beingmeta, inc. (A Delaware Corporation)
  All rights reserved.

  This file is part of FramerD, a representation language and semantic
  database developed by Kenneth B. Haase and his students at the Media
  Laboratory at the Massachusetts Institute of Technology in Cambridge,
  Massachusetts.  Research at the Media Lab is supported by funds and
  equipment from a variety of corporations and government sponsors whose
  contributions are gratefully acknowledged.


   Restricted non-commercial use of this program for research and
    educational purposes is permitted under the terms described
    in the file LICENSE accompanying this software.

*************************************************************************/

#if ((defined(WIN32)) && (!(defined(STATICLINK))))
#define DTYPES_EXPORT extern __declspec(dllimport)
#define FRAMERD_EXPORT extern __declspec(dllimport)
#define FDSCRIPT_EXPORT extern __declspec(dllimport)
#define FDTEXT_EXPORT extern __declspec(dllexport)
#else
#define DTYPES_EXPORT extern
#define FRAMERD_EXPORT extern
#define FDSCRIPT_EXPORT extern
#define FDTEXT_EXPORT extern
#define FDWWW_EXPORT 
#endif

#define INLINE_FASTOPS 1
#define DTYPES_SOURCE 0
#define FRAMERD_SOURCE 0
#define FDEVAL_CORE 0
#define FDSCRIPT_SOURCE 0

#define FD_INLINE_CHARACTER_OPS 1
#define FD_INLINE_STRING_STREAMS 1

#include "framerd/fdscript.h"
#include "framerd/fdwww.h"



