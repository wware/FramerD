/* -*- C -*-

  Copyright (c) 1994-2001 by the Massachusetts Institute of Technology.
  Copyright (c) 2001-2005 by beingmeta, inc. (A Delaware Corporation)
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
#define FDSCRIPT_EXPORT extern __declspec(dllexport)
#else
#define DTYPES_EXPORT extern
#define FRAMERD_EXPORT extern
#define FDSCRIPT_EXPORT extern
#endif

#define INLINE_FASTOPS 1
#define DTYPES_SOURCE 0
#define FRAMERD_SOURCE 0
#define FDEVAL_CORE 1
#define FDSCRIPT_SOURCE 1

#include "framerd/fdscript.h"

#if (!((defined(WIN32)) && (!(defined(STATICLINK)))))
#undef FDSCRIPT_EXPORT
#define FDSCRIPT_EXPORT 
#endif


