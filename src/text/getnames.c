/* C Mode */

/* getnames.c
   Extracting proper names from text
   Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.

   Copyright (C) 2001 Massachusetts Institute of Technology

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

static char vcid[] = "$Id: getnames.c,v 1.5 2003/08/27 10:53:30 haase Exp $";

#include "fdtext.h"
#if HAVE_WCHAR_H
#include <wchar.h>
#endif

EXPORTED fd_hashset fd_english_title_abbrevs;
EXPORTED fd_hashset fd_english_stop_words;
EXPORTED fd_hashset fd_name_suffixes;

#define starts_namep(c) ((fd_isupper(c)) || (fd_isdigit(c)))
#define embeddable_punctp(c) ((c == '-') || (c == '\'') || (c == '.') || (c == '_') || (c == '/'))

static int skip_space(fd_u8char **scanner)
{
  fd_u8char *result=*scanner;
  int c=fd_sgetc(scanner);
  while ((c>0) && (fd_isspace(c))) {
    result=*scanner; c=fd_sgetc(scanner);}
  *scanner=result;
  return c;
}

static int skip_word(fd_u8char **scanner)
{
  fd_u8char *result=*scanner;
  int c=fd_sgetc(scanner);
  while ((c>0) && (fd_isalnum(c))) {
    result=*scanner; c=fd_sgetc(scanner);}
  *scanner=result;
  return c;
}

static int get_char(fd_u8char *string)
{
  return fd_sgetc(&string);
}

static int skip_name_fragment(fd_u8char **scanner)
{
  fd_u8char *scan=*scanner;
  int c=skip_space(&scan);
  if (starts_namep(c)) {
    c=skip_word(&scan);
    while (embeddable_punctp(c)) {
      int nextc; fd_u8char *probe=scan;
      fd_sgetc(&probe); nextc=get_char(probe);
      if (fd_isalnum(nextc)) {
	scan=probe; c=skip_word(&scan);}
      else break;}
    *scanner=scan;
    if (c < 0) return 0; else return c;}
  else return -1;
}

static void add_proper_name
  (fd_lisp *into,fd_u8char *name_start,fd_u8char *name_end,
   fd_u8char *prefix_end,fd_hashset stop_words)
{
  fd_lisp string; fd_u8char buf[64], *scan=name_start;
  int c=-1, has_space=0;
  struct FD_STRING_STREAM ss;
  /* We skip over the first word if it is a stop word. */
  if ((prefix_end) && ((prefix_end-name_start) < 60)) {
    memcpy(buf,name_start,prefix_end-name_start);
    buf[prefix_end-name_start]=0;    
    if (fd_hashset_strget(stop_words,buf,prefix_end-name_start))
      if (name_end == prefix_end) return;
      else {
	scan=prefix_end; c=fd_sgetc(&scan);
	while (fd_isspace(c)) {
	  c=fd_sgetc(&scan);}}}
  FD_INITIALIZE_STRING_STREAM(&ss,name_end-name_start);
  /* We remove any apostrophe or apostrophe-s. */
  if (name_end-name_start > 2) {
    if (*(name_end-1) == '\'') name_end--;
    else if ((*(name_end-1) == 's') && (*(name_end-2) == '\''))
      name_end=name_end-2;}
  /* If we got a value for c while stripping the prefix, we keep it, since scan is already bumped past it. */
  if (c<=0) c=fd_sgetc(&scan);
  while (scan < name_end) {
    if (fd_isspace(c)) {
      fd_sputc(&ss,' '); has_space=1;
      while (fd_isspace(c)) {c=fd_sgetc(&scan);}}
    else {fd_sputc(&ss,c); c=fd_sgetc(&scan);}}
  fd_sputc(&ss,c);
  if ((has_space == 0) &&
      (fd_hashset_strget(stop_words,ss.ptr,ss.size))) {
    fd_xfree(ss.ptr);}
  else {
    string=fd_init_string(ss.ptr,ss.size);
    FD_ADD_TO_CHOICE(*into,string);}
}

static fd_u8char *extract_one_proper_name
  (fd_u8char *string,fd_lisp *into,
   fd_hashset title_abbrevs,fd_hashset stop_words,
   fd_hashset name_suffixes)
{
  fd_u8char *scan=string, *name_start=scan;
  fd_u8char *name_end=NULL, *prefix_end=NULL;
  int c=fd_sgetc(&scan);
  /* Find an upper case character */
  while ((c>0) && ((isdigit(c)) || (!(starts_namep(c))))) {
    name_start=scan; c=fd_sgetc(&scan);}
  /* Return if you ran out of characters to process */
  if (c<=0) return NULL;
  /* assert(starts_namep(c)); */
  /* Find the end of this word */
  scan=name_start;
  c=skip_name_fragment(&scan); prefix_end=name_end=scan;
  /* If it might be an abbreviated title, check it out. */
  if ((c == '.') && (name_end-name_start < 32)) {
      fd_u8char buf[32]; int abbrevp;
      /* Make a copy */
      memcpy(buf,name_start,name_end-name_start); buf[name_end-name_start]=0;
      abbrevp=fd_hashset_strget(title_abbrevs,buf,name_end-name_start);
      if (!(abbrevp)) {
	add_proper_name(into,name_start,name_end,NULL,stop_words);
	return name_end;}
      else {prefix_end=NULL; name_end=scan; c=fd_sgetc(&scan);}}
  if (fd_isspace(c)) { /* If it's a space, try extending */
    fd_u8char *wstart; name_end=scan;
    c=skip_space(&scan); wstart=scan; 
    while (starts_namep(c)) {
      c=skip_name_fragment(&scan);
      if ((c == '.') && (scan-wstart == 1)) {
	name_end=++scan; c=skip_space(&scan);}
      else if (c == 0) {
	name_end=scan; break;}
      else if ((fd_isspace(c))) {
	name_end=scan; c=skip_space(&scan);}
      else {name_end=scan; break;}}}
  else if (c == 0) /* This is the case where you ran into the
		      end of the string, so you use it. */
    name_end=scan;
  if (c == ',') {
    fd_u8char *probe=scan+1, *wstart, *wend; char buf[32];
    int c=skip_space(&probe);
    if (fd_isupper(c)) {
      wstart=probe; skip_name_fragment(&probe); wend=probe;
      if (wend-wstart < 32) {
	struct FD_STRING_STREAM ss; fd_u8char *tmp=wstart;
	int c=fd_sgetc(&tmp), lc=fd_tolower(c);
	FD_INITIALIZE_FIXED_STRING_STREAM(&ss,32,buf);
	fd_sputc(&ss,lc);
	while (tmp < wend) {
	  c=fd_sgetc(&tmp); lc=fd_tolower(c); fd_sputc(&ss,lc);}
	if (fd_hashset_strget(name_suffixes,buf,ss.size)) {
	  name_end=wend; scan=wend;}}}}
  if (name_end)
    add_proper_name(into,name_start,name_end,prefix_end,stop_words);
  else add_proper_name(into,name_start,name_start+strlen(name_start),
		       prefix_end,stop_words);
  return name_end;
}

FDTEXT_EXPORT
void fd_extract_proper_names
  (fd_u8char *string,fd_lisp *into,
   fd_hashset title_abbrevs,fd_hashset stop_words,
   fd_hashset name_suffixes)
{
  while (string)
    string=extract_one_proper_name
      (string,into,title_abbrevs,stop_words,name_suffixes);
}

static fd_u8char *title_abbrevs_init[]={
  "Dr.","Mr.","Mrs.","Ms.","Miss","Rep.","Rev.","Sen.","Gov.",
  "Col.","Lt.","Gen.","Sgt.","Capt.","Adm.","Mmme.","Sr.",
  "Pres.","Pvt.","Hon.","Herr.",NULL};
static fd_u8char *stop_words_init[]={
  "Thee",
  "Per",
  "If",
  "Though",
  "Well",
  "Itself",
  "Whilst",
  "He's",
  "Which",
  "Until",
  "Around",
  "I'd",
  "For",
  "Over",
  "One's",
  "And",
  "Or",
  "Who",
  "What",
  "Many",
  "Do",
  "Here",
  "As",
  "Our",
  "They",
  "The",
  "That",
  "Whenever",
  "Into",
  "Doth",
  "Above",
  "Hath",
  "Towards",
  "I",
  "Miss",
  "You're",
  "Her",
  "Those",
  "Across",
  "Near",
  "Did",
  "You",
  "You'll",
  "Therefore",
  "We've",
  "Had",
  "Without",
  "When",
  "Whichever",
  "No",
  "Another",
  "Can't",
  "S/p",
  "What's",
  "Out",
  "'tis",
  "Hadn't",
  "You've",
  "Ourselves",
  "Me",
  "May",
  "It'd",
  "Where",
  "From",
  "There's",
  "I'll",
  "Despite",
  "Don't",
  "By",
  "Whom",
  "Himself",
  "At",
  "Haven't",
  "Upon",
  "She",
  "Oneself",
  "That'd",
  "Not",
  "We'll",
  "That'll",
  "None",
  "Never",
  ":",
  "Hast",
  "Within",
  "However",
  "Every",
  "To",
  "This",
  "We'd",
  "Didn't",
  "Against",
  "!",
  "Aren't",
  "/",
  "Who",
  "Isn't",
  "That's",
  "Its",
  "They'd",
  "Since",
  "Doest",
  "Themselves",
  "Up",
  "Should",
  "It'll",
  "Having",
  "Amongst",
  "Might",
  "Whose",
  "Shouldn't",
  "My",
  "Whomever",
  "Past",
  "Down",
  "Some",
  "Amid",
  "How",
  "He'll",
  "Weren't",
  "Their",
  "It",
  "Is",
  "While",
  "We",
  "I've",
  "He'd",
  "You'd",
  "Amidst",
  "After",
  "These",
  ",",
  "Except",
  "There",
  "They've",
  "Like",
  "Along",
  "Doeth",
  "Can",
  "Have",
  "Wilt",
  "Doesn't",
  "Toward",
  "Noone",
  "Us",
  "?",
  "She's",
  "Him",
  "This'd",
  "It's",
  "An",
  "Would",
  "--",
  "One",
  "On",
  "We're",
  "Before",
  "Behind",
  "Yourselves",
  "Each",
  "Between",
  "Thus",
  "I'm",
  "Off",
  "Reg.",
  "Herself",
  "Yourself",
  "They're",
  "Any",
  "Because",
  "Who's",
  "During",
  "Who'd",
  "Ain't",
  "With",
  "Under",
  "Wouldn't",
  "She'll",
  "Other",
  "She'd",
  "Shalt",
  "About",
  "Could",
  "Unto",
  "Of",
  "+",
  "In",
  "Aka",
  "Didst",
  "His",
  "Won't",
  "Sans",
  "Whoever",
  "Wasn't",
  "He",
  "What",
  "This'll",
  "Below",
  "Hasn't",
  ".",
  "Through",
  "They'll",
  "Last",
  "Shall",
  "Your",
  "Couldn't",
  "Among",
  "Neath",
  "Cannot",
  "Them",
  "Why",
  "T'",
  "Our",
  "Onto",
  "Suppose",
  "Such",
  "Myself",
  "Will",
  "Has",
  "Underneath",
  "A",
  "But",
  "Must",
  "Does",
  "Contra",
  NULL};

static fd_u8char *state_abbrevs_init[]={
  "ar", "ark", "ma", "mass", "ny", "ct", "conn", "ri",
  "nh", "me", "vt", "dl", "del", "nj", "pa", "penn",
  "va", "nc", "sc", "ga", "fl", "fla", "al", "ala",
  "in", "il", "ill", "mn", "minn", "wy", "wyo",
  "dc", "d.c.", "wa", "wash", "ca", "calif", "or", "ore",
  "ms", "miss", "tn", "tenn", "la", "tx", "nm", "co", "col",
  "hi", "ak", "ia", "ar", "ariz", "nv", "nev", "nd", "sd",
  "oh", "ky", "mi", "mich", "wv", "wi", "wisc", "nb", "neb",
  "ka", "kans", "kan", "mo", "ut", "ok", "okla", "mt", "mon",
  NULL};

static fd_u8char *name_suffix_init[]={
  "phd", "scd", "ph.d", "sc.d", "md", "m.d.",
  "j.d" "esq", "jd", "dds", "m.div", "mdiv", "dd", "d.div",
  "jr", "sr", "ii", "iii", "iv", "v", "vi", "vii", "viii",
  "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi",
  "rn", "r.n.", "msw", "m.sw", "psyd", "psy.d", "licsw",
  "inc", "co", "corp", "ltd", "ab", "ag", "nv", "oy", "oyj", "llc", "plc",
  NULL};


void initialize_getnames_c()
{
  fd_u8char **tscan=title_abbrevs_init;
  fd_u8char **sscan=stop_words_init;
  fd_u8char **fscan=name_suffix_init;
  fd_u8char **sascan=state_abbrevs_init;
  fd_english_title_abbrevs=fd_make_hashset(64);
  fd_english_stop_words=fd_make_hashset(64);
  fd_name_suffixes=fd_make_hashset(64);
  while (*tscan) {
    fd_hashset_add(fd_english_title_abbrevs,fd_copy_string(*tscan));
    tscan++;}
  while (*sscan) {
    fd_hashset_add(fd_english_stop_words,fd_copy_string(*sscan));
    sscan++;}
  while (*fscan) {
    fd_hashset_add(fd_name_suffixes,fd_copy_string(*fscan));
    fscan++;}
  while (*sascan) {
    fd_hashset_add(fd_name_suffixes,fd_copy_string(*sascan));
    sascan++;}
}




/* File specific stuff */

/* The CVS log for this file
   $Log: getnames.c,v $
   Revision 1.5  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.4.2.1  2003/01/26 20:50:20  haase
   Misc. fixes, especially GC

   Revision 1.4  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
