/* C Mode */

/* characters.c
   R4RS character primitives for FDScript
   Originally implemented by Ken Haase  and the Machine Understanding Group
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

static char vcid[] =
  "$Id: characters.c,v 1.7 2005/01/14 16:48:49 haase Exp $";

#include "fdscript.h"

static void check_character_arg(lisp ch)
{
  if (!(FD_CHARACTERP(ch))) fd_type_error("not a character",ch);
}

/*

essential procedure: char=? char1 char2

essential procedure: char<? char1 char2

essential procedure: char>? char1 char2

essential procedure: char<=? char1 char2

essential procedure: char>=? char1 char2

These procedures impose a total ordering on the set of characters. It
is guaranteed that under this ordering: 

    The upper case characters are in order. For example, (char<? #\A
    #\B) returns #t. 
    The lower case characters are in order. For example, (char<? #\a
    #\b) returns #t. 
    The digits are in order. For example, (char<? #\0 #\9) returns #t. 
    Either all the digits precede all the upper case letters, or vice
    versa. 
    Either all the digits precede all the lower case letters, or vice
    versa. 
*/

static lisp char_equal_p(lisp c1, lisp c2){
  check_character_arg(c1);
  check_character_arg(c2);

  return (CHAR_CODE(c1) == CHAR_CODE(c2)) ? FD_TRUE : FD_FALSE;

}

static lisp char_lt_p(lisp c1, lisp c2){
  check_character_arg(c1);
  check_character_arg(c2);

  return (CHAR_CODE(c1) < CHAR_CODE(c2)) ? FD_TRUE : FD_FALSE;

}

static lisp char_gt_p(lisp c1, lisp c2){
  check_character_arg(c1);
  check_character_arg(c2);

  return (CHAR_CODE(c1) > CHAR_CODE(c2)) ? FD_TRUE : FD_FALSE;

}

static lisp char_lt_equal_p(lisp c1, lisp c2){
  check_character_arg(c1);
  check_character_arg(c2);

  return (CHAR_CODE(c1) <= CHAR_CODE(c2)) ? FD_TRUE : FD_FALSE;

}

static lisp char_gt_equal_p(lisp c1, lisp c2){
  check_character_arg(c1);
  check_character_arg(c2);

  return (CHAR_CODE(c1) >= CHAR_CODE(c2)) ? FD_TRUE : FD_FALSE;

}


/*
Some implementations may generalize these procedures to take
more than two arguments, as with the corresponding numerical
predicates. 

essential procedure: char-ci=? char1 char2

essential procedure: char-ci<? char1 char2

essential procedure: char-ci>? char1 char2

essential procedure: char-ci<=? char1 char2

essential procedure: char-ci>=? char1 char2

These procedures are similar to char=? et cetera, but they treat
upper case and lower case letters as the same. For example,
(char-ci=? #\A #\a) returns #t. Some implementations may
generalize these procedures to take more than two arguments, as
with the corresponding numerical predicates. 

*/

static lisp char_ci_equal_p(lisp c1, lisp c2){
  check_character_arg(c1); check_character_arg(c2);

  return (toupper(CHAR_CODE(c1)) == toupper(CHAR_CODE(c2))) ? FD_TRUE : FD_FALSE;

}

static lisp char_ci_lt_p(lisp c1, lisp c2){
  check_character_arg(c1); check_character_arg(c2);

  return (toupper(CHAR_CODE(c1)) < toupper(CHAR_CODE(c2))) ? FD_TRUE : FD_FALSE;

}

static lisp char_ci_gt_p(lisp c1, lisp c2){
  check_character_arg(c1); check_character_arg(c2);

  return (toupper(CHAR_CODE(c1)) > toupper(CHAR_CODE(c2))) ? FD_TRUE : FD_FALSE;

}

static lisp char_ci_lt_equal_p(lisp c1, lisp c2){
  check_character_arg(c1); check_character_arg(c2);

  return (toupper(CHAR_CODE(c1)) <= toupper(CHAR_CODE(c2))) ? FD_TRUE : FD_FALSE;

}

static lisp char_ci_gt_equal_p(lisp c1, lisp c2){
  check_character_arg(c1); check_character_arg(c2);

  return (toupper(CHAR_CODE(c1)) >= toupper(CHAR_CODE(c2))) ? FD_TRUE : FD_FALSE;

}



/*

essential procedure: char-alphabetic? char

essential procedure: char-numeric? char

essential procedure: char-whitespace? char

essential procedure: char-upper-case? letter

essential procedure: char-lower-case? letter

These procedures return #t if their arguments are alphabetic,
numeric, whitespace, upper case, or lower case characters,
respectively, otherwise they return #f. The following remarks, which
are specific to the ASCII character set, are intended only as a guide:
The alphabetic characters are the 52 upper and lower case letters.
The numeric characters are the ten decimal digits. The whitespace
characters are space, tab, line feed, form feed, and carriage return. 

*/

static lisp
char_alphabetic_p(lisp c){
  check_character_arg(c);

  return (fd_isalpha(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

static lisp
char_alphanumeric_p(lisp c){
  check_character_arg(c);

  return (fd_isalnum(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

static lisp
char_numeric_p(lisp c){
  check_character_arg(c);

  return (fd_isdigit(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

static lisp
char_whitespace_p(lisp c){
  check_character_arg(c);

  return (fd_isspace(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

static lisp
char_upper_case_p(lisp c){
  check_character_arg(c);

  return (fd_isupper(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

static lisp
char_lower_case_p(lisp c){
  check_character_arg(c);

  return (fd_islower(CHAR_CODE(c))) ? FD_TRUE : FD_FALSE;  
}

/*


essential procedure: char->integer char

essential procedure: integer->char n

Given a character, char->integer returns an exact integer
representation of the character. Given an exact integer that is the
image of a character under char->integer, integer->char returns that
character. These procedures implement injective order
isomorphisms between the set of characters under the char<=?
ordering and some subset of the integers under the <= ordering. That
is, if (char<=? a b) => #t and (<= x y) => #t and x and y are in the
domain of integer->char, then 

(<= (char->integer a)
    (char->integer b))       =>  #t

(char<=? (integer->char x)
         (integer->char y))  =>  #t

*/

static lisp 
char2integer(lisp c){
  check_character_arg(c);

  return LISPFIX(CHAR_CODE(c));
}

static lisp
integer2char(lisp i){
  if (!(FIXNUMP(i))) fd_type_error("not an integer",i);

  if (FIXLISP(i) < 0 ||
      FIXLISP(i) > 65536)
    fd_raise_exception(_("INTEGER->CHAR: integer out of range for char"));

  return fd_make_character(FIXLISP(i));
}


/*

essential procedure: char-upcase char

essential procedure: char-downcase char

These procedures return a character char2 such that (char-ci=? char
char2). In addition, if char is alphabetic, then the result of
char-upcase is upper case and the result of char-downcase is lower case.

*/

static lisp
char_upcase(lisp c){
  check_character_arg(c);

  return CODE_CHAR(fd_toupper(CHAR_CODE(c)));
}

static lisp
char_downcase(lisp c){
  check_character_arg(c);

  return CODE_CHAR(fd_tolower(CHAR_CODE(c)));
}

static lisp
char_base(lisp c) 
{
  int code; fd_u8char *decomp;
  check_character_arg(c);
  code=CHAR_CODE(c);
  decomp=fd_decompose_char(code);
  if (decomp) {
    int base=fd_sgetc(&decomp);
    return CODE_CHAR(base);}
  else return c;
}

static lisp
char_lower_base(lisp c) 
{
  int code; fd_u8char *decomp;
  check_character_arg(c);
  code=CHAR_CODE(c);
  decomp=fd_decompose_char(code);
  if (decomp) {
    int base=fd_sgetc(&decomp);
    return CODE_CHAR(fd_tolower(base));}
  else return CODE_CHAR(fd_tolower(code));
}

FDSCRIPT_EXPORT
void fd_initialize_characters_c()
{

  fd_add_cproc(NULL,"CHAR=?",2,char_equal_p);
  fd_add_cproc(NULL,"CHAR<?",2,char_lt_p);
  fd_add_cproc(NULL,"CHAR>?",2,char_gt_p);
  fd_add_cproc(NULL,"CHAR<=?",2,char_lt_equal_p);
  fd_add_cproc(NULL,"CHAR>=?",2,char_gt_equal_p);
  fd_add_cproc(NULL,"CHAR-CI=?",2,char_ci_equal_p);
  fd_add_cproc(NULL,"CHAR-CI<?",2,char_ci_lt_p);
  fd_add_cproc(NULL,"CHAR-CI>?",2,char_ci_gt_p);
  fd_add_cproc(NULL,"CHAR-CI<=?",2,char_ci_lt_equal_p);
  fd_add_cproc(NULL,"CHAR-CI>=?",2,char_ci_gt_equal_p);
  fd_add_cproc(NULL,"CHAR-ALPHABETIC?",1,char_alphabetic_p);
  fd_add_cproc(NULL,"CHAR-ALPHANUMERIC?",1,char_alphanumeric_p);
  fd_add_cproc(NULL,"CHAR-NUMERIC?",1,char_numeric_p);
  fd_add_cproc(NULL,"CHAR-WHITESPACE?",1,char_whitespace_p);
  fd_add_cproc(NULL,"CHAR-UPPER-CASE?",1,char_upper_case_p);
  fd_add_cproc(NULL,"CHAR-LOWER-CASE?",1,char_lower_case_p);
  fd_add_cproc(NULL,"CHAR->INTEGER",1,char2integer);
  fd_add_cproc(NULL,"INTEGER->CHAR",1,integer2char);
  fd_add_cproc(NULL,"CHAR-UPCASE",1,char_upcase);
  fd_add_cproc(NULL,"CHAR-DOWNCASE",1,char_downcase);

  fd_add_cproc(NULL,"CHAR-BASE",1,char_base);
  fd_add_cproc(NULL,"CHAR-LOWER-BASE",1,char_lower_base);

  fd_register_source_file("characters",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: characters.c,v $
   Revision 1.7  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.6  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.4  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
