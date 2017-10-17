/* C Mode */

/* match.c
   Regular expression primitives for FDScript
   Originally implemented by Ken Haase in the Machine Understanding Group
     at the MIT Media Laboratory.

   Copyright (C) 1999 Massachusetts Institute of Technology

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

static char vcid[] = "$Id: match.c,v 1.28 2004/07/20 09:16:15 haase Exp $";

#include "fdtext.h"
#if HAVE_WCHAR_H
#include <wchar.h>
#endif

typedef fd_u8char u8char;

fd_exception fd_TXInvalidPattern=_("Not a valid TX text pattern");

static fd_lisp label_symbol, star_symbol, plus_symbol;

#define string_ref(s) ((*(s) < 0x80) ? (*(s)) : (fd_utf8_string_ref(s)))

#define MATCH_SPECIAL \
   (MATCH_IGNORE_CASE|MATCH_IGNORE_DIACRITICS|MATCH_COLLAPSE_SPACES)

/* Define these without the FD_ prefix for perspicuity */

#define CAR FD_CAR
#define CDR FD_CDR

#define CHOICEP FD_CHOICEP
#define DO_CHOICES FD_DO_CHOICES
#define END_DO_CHOICES END_FD_DO_CHOICES
#define ADD_TO_CHOICE FD_ADD_TO_CHOICE

#define PAIRP FD_PAIRP
#define FIXNUMP FD_FIXNUMP 
#define STRINGP FD_STRINGP
#define LISPFIX FD_LISPFIX 
#define FIXLISP FD_FIXLISP
#define STRING_DATA FD_STRING_DATA
#define STRING_LENGTH FD_STRING_LENGTH

#define LISP_EQ FD_LISP_EQ

/** Utility functions **/

static int forward_char_proc(fd_u8char *s,int i)
{
  fd_u8char *next=fd_utf8_substring(s+i,1);
  if (next) return next-s; else return i+1;
}

#define forward_char(s,i) \
  ((s[i] == 0) ? (i) : (s[i] >= 0x80) ? (forward_char_proc(s,i)) : (i+1))

FASTOP int reduce_char(int ch,int flags)
{
  if (flags&MATCH_IGNORE_DIACRITICS) {
    int nch=fd_base_char(ch);
    if (nch>0) ch=nch;}
  if (flags&MATCH_IGNORE_CASE) ch=fd_tolower(ch);
  return ch;
}

static int get_previous_char(fd_u8char *string,int off)
{
  if (off == 0) return -1;
  else if (string[off-1] < 0x80) return string[off-1];
  else {
    int i=off-1, ch; fd_u8char *scan;
    while ((i>0) && (string[i]>=0x80) && (string[i]<0xC0)) i--;
    scan=string+i; ch=fd_sgetc(&scan);
    return ch;}
}

FASTOP int strmatcher
  (int flags,
   fd_u8char *pat,int patlen,
   fd_u8char *string,int off,int lim)
{
  if ((flags&MATCH_SPECIAL) == 0)
    if (strncmp(pat,string+off,patlen) == 0) return off+patlen;
    else return -1;
  else {
    int di=(flags&MATCH_IGNORE_DIACRITICS), si=(flags&MATCH_COLLAPSE_SPACES);
    fd_u8char *s1=pat, *s2=string+off, *end=s2, *limit=string+lim;
    int c1=fd_sgetc(&s1), c2=fd_sgetc(&s2);
    while ((c1>0) && (c2>0) && (s2 <= limit))
      if ((si) && (fd_isspace(c1)) && (fd_isspace(c2))) {
	while ((c1>0) && (fd_isspace(c1))) c1=fd_sgetc(&s1);
	while ((c2>0) && (fd_isspace(c2))) {
	  end=s2; c2=fd_sgetc(&s2);}}
      else if ((di) && (fd_ismodifier(c1)) && (fd_ismodifier(c2))) {
	while ((c1>0) && (fd_ismodifier(c1))) c1=fd_sgetc(&s1);
	while ((c2>0) && (fd_ismodifier(c2))) {
	  end=s2; c2=fd_sgetc(&s2);}}
      else if (c1 == c2) {
	c1=fd_sgetc(&s1); end=s2; c2=fd_sgetc(&s2);}
      else if (flags&(MATCH_IGNORE_CASE|MATCH_IGNORE_DIACRITICS))
	if (reduce_char(c1,flags) == reduce_char(c2,flags)) {
	  c1=fd_sgetc(&s1); end=s2; c2=fd_sgetc(&s2);}
	else return -1;
      else return -1;
    if (c1 < 0) /* If at end of pat string, you have a match */
      return end-string;
    else return -1;}
}

/** Match operator table **/

static struct TX_MATCH_OPERATOR *match_operators;

static int n_match_operators, limit_match_operators;

static void init_match_operators_table()
{
  match_operators=fd_xmalloc(sizeof(struct TX_MATCH_OPERATOR)*16);
  n_match_operators=0; limit_match_operators=16;
}

FDTEXT_EXPORT
void fd_add_match_operator
  (fd_u8char *label,
   tx_matchfn matcher,tx_searchfn searcher,tx_extractfn extract)
{
  fd_lisp sym=fd_make_symbol(label);
  struct TX_MATCH_OPERATOR *scan=match_operators, *limit=scan+n_match_operators;
  while (scan < limit) if (LISP_EQ(scan->symbol,sym)) break; else scan++;
  if (scan < limit) {scan->matcher=matcher; return;}
  if (n_match_operators >= limit_match_operators) {
    match_operators=fd_xrealloc
      (match_operators,
       sizeof(struct TX_MATCH_OPERATOR)*(limit_match_operators)*2);
    limit_match_operators=limit_match_operators*2;}
  match_operators[n_match_operators].symbol=sym;
  match_operators[n_match_operators].matcher=matcher;
  match_operators[n_match_operators].searcher=searcher;
  match_operators[n_match_operators].extract=extract;
  n_match_operators++;
}


/** The Matcher **/

static fd_lisp match_sequence
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags);

FDTEXT_EXPORT
fd_lisp fd_text_matcher
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off > lim) return FD_EMPTY_CHOICE;
  else if (FD_EMPTYP(pat)) return FD_EMPTY_CHOICE;
  else if (FD_STRINGP(pat))
    if (off == lim)
      if (STRING_LENGTH(pat) == 0) return LISPFIX(off);
      else return FD_EMPTY_CHOICE;
    else {
      int mlen=strmatcher
	(flags,STRING_DATA(pat),STRING_LENGTH(pat),
	 string,off,lim);
      if (mlen < 0) return FD_EMPTY_CHOICE;
      else return FD_LISPFIX(mlen);}
  else if (FD_CHOICEP(pat)) {
    fd_lisp answers=FD_EMPTY_CHOICE;
    DO_CHOICES(epat,pat) {
      fd_lisp answer=fd_text_matcher(epat,env,string,off,lim,flags);
      ADD_TO_CHOICE(answers,answer);}
    END_DO_CHOICES;
    return answers;}
  else if (FD_CHARACTERP(pat))
    if (off == lim) return FD_EMPTY_CHOICE;
    else {
      int code=FD_CHAR_CODE(pat);
      if (code < 0x7f)
	if (string[off] == code) return LISPFIX(off+1);
	else return FD_EMPTY_CHOICE;
      else if (code == string_ref(string+off)) 
	return LISPFIX(forward_char(string,1));
      else return FD_EMPTY_CHOICE;}
  else if (FD_VECTORP(pat))
    return match_sequence(pat,env,string,off,lim,flags);
  else if (FD_PAIRP(pat)) {
    fd_lisp head=CAR(pat);
    struct TX_MATCH_OPERATOR
      *scan=match_operators, *limit=scan+n_match_operators;
    while (scan < limit)
      if (LISP_EQ(scan->symbol,head)) break; else scan++; 
    if (scan < limit)
      return scan->matcher(pat,env,string,off,lim,flags);
    else fd_raise_lisp_exception
	   (fd_TXInvalidPattern,"for text matching",pat);}
  else if (FD_SYMBOLP(pat)) {
    fd_lisp v=fd_symeval(pat,env);
    fd_lisp result=fd_text_matcher(v,env,string,off,lim,flags);
    fd_decref(v); return result;}
  else if (FD_PRIM_TYPEP(pat,tx_closure_type)) {
    struct TX_CLOSURE *txc=FD_CPTR_DATA(pat);
    return fd_text_matcher(txc->pattern,txc->env,string,off,lim,flags);}
  else fd_raise_lisp_exception(fd_TXInvalidPattern,"in text matching",pat);
}

static fd_lisp match_sequence
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int i=0, l=FD_VECTOR_LENGTH(pat);
  fd_lisp state=LISPFIX(off);
  while (i < l) {
    fd_lisp epat=FD_VECTOR_REF(pat,i);
    fd_lisp next=FD_EMPTY_CHOICE;
    DO_CHOICES(pos,state) {
      fd_lisp npos=fd_text_matcher(epat,env,string,FIXLISP(pos),lim,flags);
      ADD_TO_CHOICE(next,npos);}
    END_DO_CHOICES;
    if (FD_EMPTYP(next)) {fd_decref(state); return FD_EMPTY_CHOICE;}
    else {fd_decref(state); state=next; i++;}}
  return state;
}

/** Extraction **/

static fd_lisp extract_sequence
   (fd_lisp pat,int pat_elt,fd_lispenv env,
    fd_u8char *string,int off,int lim,int flags);
static fd_lisp lists_to_vectors(fd_lisp lists);

static fd_lisp extract_text(fd_u8char *string,int start,fd_lisp ends)
{
  fd_lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(each_end,ends)
    if (FIXNUMP(each_end)) {
      fd_lisp extraction=FD_MAKE_PAIR
	(each_end,fd_make_substring
	 (string+start,string+FIXLISP(each_end)));
      ADD_TO_CHOICE(answers,extraction);}
  END_DO_CHOICES;
  return answers;
}

FDTEXT_EXPORT
fd_lisp fd_text_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off > lim) return FD_EMPTY_CHOICE;
  else if (FD_EMPTYP(pat)) return FD_EMPTY_CHOICE;
  else if (STRINGP(pat))
    if ((STRING_LENGTH(pat)) == 0)
      return FD_MAKE_PAIR(LISPFIX(off),fd_make_string(""));
    else if (off == lim) return FD_EMPTY_CHOICE;
    else {
      int mlen=
	strmatcher(flags,STRING_DATA(pat),STRING_LENGTH(pat),
		   string,off,lim);
      if (mlen<0) return FD_EMPTY_CHOICE;
      else return extract_text(string,off,FD_LISPFIX(mlen));}
  else if (CHOICEP(pat)) {
    fd_lisp answers=FD_EMPTY_CHOICE;
    DO_CHOICES(epat,pat) {
      fd_lisp extractions=fd_text_extract(epat,env,string,off,lim,flags);
      DO_CHOICES(extraction,extractions)
	if (FD_PAIRP(extraction)) {
	  ADD_TO_CHOICE(answers,fd_incref(extraction));}
      END_DO_CHOICES;
      fd_decref(extractions);}
    END_DO_CHOICES;
    return answers;}
  else if (FD_CHARACTERP(pat))
    if (off == lim) return FD_EMPTY_CHOICE;
    else {
      int code=FD_CHAR_CODE(pat);
      if (code < 0x7f)
	if (string[off] == code)
	  return FD_MAKE_PAIR(LISPFIX(off+1),pat);
	else return FD_EMPTY_CHOICE;
      else if (code == string_ref(string+off)) 
	return FD_MAKE_PAIR(LISPFIX(forward_char((string+off),1)),pat);
      else return FD_EMPTY_CHOICE;}
  else if (FD_VECTORP(pat)) {
    fd_lisp seq_matches=extract_sequence(pat,0,env,string,off,lim,flags);
    fd_lisp result=lists_to_vectors(seq_matches);
    fd_decref(seq_matches); return result;}
  else if (FD_PAIRP(pat)) {
    fd_lisp head=CAR(pat);
    struct TX_MATCH_OPERATOR
      *scan=match_operators, *limit=scan+n_match_operators;
    while (scan < limit)
      if (LISP_EQ(scan->symbol,head)) break; else scan++; 
    if (scan < limit)
      if (scan->extract)
	return scan->extract(pat,env,string,off,lim,flags);
      else {
	fd_lisp matches=scan->matcher(pat,env,string,off,lim,flags);
	fd_lisp answer=extract_text(string,off,matches); fd_decref(matches);
	return answer;}
    else fd_raise_lisp_exception(fd_TXInvalidPattern,"for text matching",pat);}
  else if (FD_SYMBOLP(pat)) {
    fd_lisp v=fd_symeval(pat,env);
    fd_lisp lengths=fd_text_matcher(v,env,string,off,lim,flags);
    fd_lisp answers=FD_EMPTY_CHOICE;
    DO_CHOICES(l,lengths) {
      fd_lisp extraction=
	FD_MAKE_PAIR(l,fd_make_substring(string+off,string+FIXLISP(l)));
      ADD_TO_CHOICE(answers,extraction);}
    END_DO_CHOICES;
    fd_decref(lengths); fd_decref(v);
    return answers;}
  else if (FD_PRIM_TYPEP(pat,tx_closure_type)) {
    struct TX_CLOSURE *txc=FD_CPTR_DATA(pat);
    return fd_text_extract(txc->pattern,txc->env,string,off,lim,flags);}
  else fd_raise_lisp_exception(fd_TXInvalidPattern,"in text matching",pat);
}

static fd_lisp extract_sequence
   (fd_lisp pat,int pat_elt,fd_lispenv env,
    fd_u8char *string,int off,int lim,int flags)
{
  int l=FD_VECTOR_LENGTH(pat);
  if (pat_elt == l) return FD_MAKE_PAIR(LISPFIX(off),FD_EMPTY_LIST);
  else {
    fd_lisp sub_matches=
      fd_text_extract(FD_VECTOR_REF(pat,pat_elt),env,string,off,lim,flags);
    fd_lisp results=FD_EMPTY_CHOICE;
    DO_CHOICES(sub_match,sub_matches)
      if (FIXLISP(CAR(sub_match)) <= lim) {
	int noff=FIXLISP(CAR(sub_match));
	fd_lisp remainders=extract_sequence
	  (pat,pat_elt+1,env,string,noff,lim,flags);
	DO_CHOICES(remainder,remainders) {
	  fd_lisp result=
	    FD_MAKE_PAIR(CAR(remainder),
			 FD_MAKE_PAIR(fd_incref(CDR(sub_match)),
				      fd_incref(CDR(remainder))));
	  ADD_TO_CHOICE(results,result);}
	END_DO_CHOICES;
	fd_decref(remainders);}
    END_DO_CHOICES;
    fd_decref(sub_matches);
    return results;}
}

static fd_lisp lists_to_vectors(fd_lisp lists)
{
  fd_lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(list,lists) {
    fd_lisp lsize=CAR(list), scan=CDR(list), vec; int i=0, lim=0;
    while (PAIRP(scan)) {lim++; scan=CDR(scan);}
    vec=fd_make_vector(lim);
    scan=CDR(list); while (i < lim) {
      FD_VECTOR_SET(vec,i,fd_incref(CAR(scan)));
      i++; scan=CDR(scan);}
    ADD_TO_CHOICE(answer,FD_MAKE_PAIR(lsize,vec));}
  END_DO_CHOICES;
  return answer;
}

/** Match repeatedly **/

static fd_lisp match_repeatedly
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,
   int flags,int zero_ok)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  fd_lisp state=LISPFIX(off); int count=0;
  if (zero_ok) {ADD_TO_CHOICE(match_points,LISPFIX(off));}
  while (1) {
    fd_lisp next_state=FD_EMPTY_CHOICE;
    DO_CHOICES(pos,state) {
      fd_lisp npos=fd_text_matcher(pat,env,string,FIXLISP(pos),lim,flags);
      DO_CHOICES(n,npos) {
	if (!((CHOICEP(state)) ? (fd_choice_containsp(n,state)) :
	      (LISP_EQ(state,n)))) {
	  ADD_TO_CHOICE(match_points,fd_incref(n));
	  ADD_TO_CHOICE(next_state,fd_incref(n));}}
      END_DO_CHOICES;
      fd_decref(npos);}
    END_DO_CHOICES;
    if (FD_EMPTYP(next_state))
      if (count == 0)
	if (zero_ok) {fd_decref(match_points); return state;}
	else {fd_decref(match_points); return FD_EMPTY_CHOICE;}
      else {fd_decref(state); return match_points;}
    else {fd_decref(state); count++; state=next_state;}}
}

static fd_lisp extract_repeatedly
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,
   int flags,int zero_ok)
{
  fd_lisp choices=FD_EMPTY_CHOICE;
  fd_lisp top=fd_text_extract(pat,env,string,off,lim,flags);
  if (FD_EMPTYP(top))
    if (zero_ok) return FD_MAKE_PAIR(LISPFIX(off),FD_EMPTY_LIST);
    else return FD_EMPTY_CHOICE;
  else {
    DO_CHOICES(each,top)
      if ((FD_PAIRP(each)) && (FIXNUMP(CAR(each))) &&
	  ((FIXLISP(CAR(each))) != off)) {
	fd_lisp size=CAR(each);
	fd_lisp extraction=CDR(each);
	fd_lisp singleton=FD_MAKE_LIST1(fd_incref(extraction));
	fd_lisp remainders=
	  extract_repeatedly(pat,env,string,fd_lisp2int(size),lim,flags,1);
	if (FD_EMPTYP(remainders)) {
	  fd_lisp last_item=FD_MAKE_PAIR(fd_incref(extraction),FD_EMPTY_LIST);
	  fd_lisp with_size=FD_MAKE_PAIR(size,last_item);
	  ADD_TO_CHOICE(choices,with_size);}
	else {
	  DO_CHOICES(remainder,remainders) {
	    fd_lisp item=FD_MAKE_PAIR
	      (fd_car(remainder),FD_MAKE_PAIR
	       (fd_incref(extraction),(fd_cdr(remainder))));
	    ADD_TO_CHOICE(choices,item);}
	  END_DO_CHOICES;
	  fd_decref(remainders);}
	ADD_TO_CHOICE(choices,FD_MAKE_PAIR(size,singleton));}
    END_DO_CHOICES;}
  fd_decref(top);
  return choices;
}

/** Match operations **/

static fd_lisp match_star
 (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  return match_repeatedly(pat_arg,env,string,off,lim,flags,1);
}
static int search_star
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return off;
}
static fd_lisp extract_star
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  fd_lisp extractions=extract_repeatedly(pat_arg,env,string,off,lim,flags,1);
  fd_lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(extraction,extractions) {
    fd_lisp size=CAR(extraction), data=CDR(extraction);
    fd_lisp pair=
      FD_MAKE_PAIR(size,FD_MAKE_PAIR(star_symbol,fd_incref(data)));
    ADD_TO_CHOICE(answer,pair);}
  END_DO_CHOICES;
  fd_decref(extractions);
  return answer;
}

static fd_lisp match_plus
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  return match_repeatedly(pat_arg,env,string,off,lim,flags,0);
}
static int search_plus
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search(pat_arg,env,string,off,lim,flags);
}
static fd_lisp extract_plus
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  fd_lisp extractions=extract_repeatedly(pat_arg,env,string,off,lim,flags,0);
  fd_lisp answer=FD_EMPTY_CHOICE;
  DO_CHOICES(extraction,extractions) {
    fd_lisp size=CAR(extraction), data=CDR(extraction);
    fd_lisp pair=
      FD_MAKE_PAIR(size,FD_MAKE_PAIR(plus_symbol,fd_incref(data)));
    ADD_TO_CHOICE(answer,pair);}
  END_DO_CHOICES;
  fd_decref(extractions);
  return answer;
}

/** Match NOT **/

static fd_lisp match_not
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  /* Find where there is a pat_arg starting */
  int pos=fd_text_search(pat_arg,env,string,off,lim,flags);
  if (pos == off) return FD_EMPTY_CHOICE;
  else {
    /* Enumerate every character position between here and there */
    int i=forward_char(string,off), last; fd_lisp result=FD_EMPTY_CHOICE;
    if ((pos < lim) && (pos > off))
      last=pos;
    else last=lim;
    while (i < last) {
      ADD_TO_CHOICE(result,LISPFIX(i));
      i=forward_char(string,i);}
    ADD_TO_CHOICE(result,LISPFIX(i));
    return result;}
}

static int search_not
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  fd_lisp match=fd_text_matcher
    (pat_arg,env,string,off,lim,(flags&(~MATCH_DO_BINDINGS)));
  if (FD_EMPTYP(match)) return off;
  else {
    int largest=off;
    DO_CHOICES(m,match) {
      int mi=FIXLISP(m);
      if (mi > largest) largest=mi;}
    END_DO_CHOICES;
    return largest;}
}

/** Match AND **/

static fd_lisp match_and
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat1=fd_get_arg(pat,1,FD_VOID);
  fd_lisp pat2=fd_get_arg(pat,2,FD_VOID);
  /* Find where there is a pat_arg starting */
  fd_lisp matches[2];
  matches[0]=fd_text_matcher(pat1,env,string,off,lim,flags);
  if (FD_EMPTYP(matches[0])) return FD_EMPTY_CHOICE;
  else {
    fd_lisp combined;
    matches[1]=fd_text_matcher(pat2,env,string,off,lim,flags);
    if (FD_EMPTYP(matches[1])) combined=FD_EMPTY_CHOICE;
    else combined=fd_intersect_choices(matches,2);
    fd_decref(matches[0]); fd_decref(matches[1]);
    return combined;}
}

static int search_and
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat1=fd_get_arg(pat,1,FD_VOID);
  fd_lisp pat2=fd_get_arg(pat,2,FD_VOID);
  int result=fd_text_search(pat1,env,string,off,lim,flags);
  while (result>=0) {
    fd_lisp match_result=match_and(pat,env,string,result,lim,flags);
    if (FD_EMPTYP(match_result))
      result=fd_text_search(pat,env,string,result+1,lim,flags);
    else {fd_decref(match_result); return result;}}
  return -1;
}

/** Match NOT> **/

static fd_lisp match_not_gt
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp pat_arg=fd_get_arg(pat,1,FD_VOID);
  int pos=fd_text_search(pat_arg,env,string,off,lim,flags);
  if (pos < 0) return LISPFIX(lim);
  /* else if (pos == off) return FD_EMPTY_CHOICE; */
  else if (pos >= lim) return LISPFIX(lim);
  else return LISPFIX(pos);
}

/** Match BIND **/

static fd_lisp match_bind
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp sym=fd_get_arg(pat,1,FD_VOID);
  fd_lisp spat=fd_get_arg(pat,2,FD_VOID);
  if ((flags)&(MATCH_DO_BINDINGS)) {
    fd_lisp ends=fd_text_matcher(spat,env,string,off,lim,flags);
    DO_CHOICES(end,ends) {
      fd_lisp substr=fd_make_substring(string+off,string+FIXLISP(end));
      fd_bind_value(sym,substr,env);}
    END_DO_CHOICES;
    return ends;}
  else return fd_text_matcher(spat,env,string,off,lim,flags);
}
static int search_bind
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,2,FD_VOID);
  return fd_text_search(spat,env,string,off,lim,flags);
}

static fd_lisp label_match 
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,2,FD_VOID);
  return fd_text_matcher(spat,env,string,off,lim,flags);
}

static int label_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,2,FD_VOID);
  return fd_text_search(spat,env,string,off,lim,flags);
}

static fd_lisp label_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp sym=fd_get_arg(pat,1,FD_VOID);
  fd_lisp spat=fd_get_arg(pat,2,FD_VOID);
  fd_lisp extractions=fd_text_extract(spat,env,string,off,lim,flags);
  fd_lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(extraction,extractions) {
    fd_lisp size=CAR(extraction), data=CDR(extraction);    
    fd_lisp lst=FD_MAKE_PAIR
      (size,FD_MAKE_LIST(3,CAR(pat),sym,fd_incref(data)));
    ADD_TO_CHOICE(answers,lst);}
  END_DO_CHOICES;
  fd_decref(extractions);
  return answers;
}

static fd_lisp subst_match 
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_matcher(spat,env,string,off,lim,flags);
}

static int subst_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search(spat,env,string,off,lim,flags);
}

static fd_lisp subst_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp spat=fd_get_arg(pat,1,FD_VOID);
  fd_lisp repl=fd_get_arg(pat,2,FD_VOID);
  fd_lisp extractions=fd_text_extract(spat,env,string,off,lim,flags);
  fd_lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(extraction,extractions) {
    fd_lisp size=CAR(extraction), data=CDR(extraction);    
    fd_lisp lst=FD_MAKE_PAIR
      (size,FD_MAKE_LIST(3,CAR(pat),fd_incref(data),fd_incref(repl)));
    ADD_TO_CHOICE(answers,lst);}
  END_DO_CHOICES;
  fd_decref(extractions);
  return answers;
}

/* Word match */

static int word_startp(fd_u8char *string,int off)
{
  int ch=get_previous_char(string,off);
  if (ch < 0) return 1;
  else if ((fd_isspace(ch)) || (fd_ispunct(ch))) return 1;
  else return 0;
}

static fd_lisp word_match
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp wpat=fd_get_arg(pat,1,FD_VOID);
  if ((off > 0) && ((word_startp(string,off)) == 0)) return FD_EMPTY_CHOICE;
  else {
    fd_lisp final_results=(FD_EMPTY_CHOICE);
    fd_lisp core_result=
      fd_text_matcher(wpat,env,string,off,lim,(flags|MATCH_COLLAPSE_SPACES));
    FD_DO_CHOICES(offset,core_result) {
      if (FD_FIXNUMP(offset)) {
	int noff=FD_FIXLISP(offset), complete_word=0;
	if (noff == lim) complete_word=1;
	else {
	  fd_u8char *ptr=string+noff;
	  int ch=fd_sgetc(&ptr);
	  if ((fd_isspace(ch)) || (fd_ispunct(ch))) complete_word=1;}
	if (complete_word) {
	  FD_ADD_TO_CHOICE(final_results,fd_incref(offset));}}
      else fd_type_error(_("Strange pattern result"),offset);}
    END_FD_DO_CHOICES;
    fd_decref(core_result); return final_results;}
}

static int get_next_candidate(fd_u8char *string,int off,int lim)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  while (scan < limit) {   /* Find another space */
    int ch=string_ref(scan);
    if ((fd_isspace(ch)) || (fd_ispunct(ch))) break;
    else if (*scan < 0x80) scan++;
    else fd_sgetc(&scan);}
  if (scan >= limit) return -1;
  else { /* Skip over the space */
    fd_u8char *probe=scan, *prev=scan; 
    while (probe < limit) {
      int ch=fd_sgetc(&probe);
      if ((fd_isspace(ch)) || (fd_ispunct(ch))) prev=probe;
      else break;}
    scan=prev;}
  if (scan >= limit) return -1;
  else return scan-string;
}

static int word_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp wpat=fd_get_arg(pat,1,FD_VOID); int match_result=-1;
  int cand;
  if (word_startp(string,off)) cand=off;
  else cand=get_next_candidate(string,off,lim);
  while ((cand >= 0) && (match_result < 0)) {
    fd_lisp matches=
      fd_text_matcher(wpat,env,string,cand,lim,(flags|MATCH_COLLAPSE_SPACES));
    if (FD_EMPTYP(matches)) {}
    else {
      FD_DO_CHOICES(match,matches) {
	int n=fd_fixlisp(match), ch;
	if (n == lim) {match_result=cand; break;}
	else {
	  fd_u8char *p=string+n; ch=fd_sgetc(&p);
	  if ((fd_isspace(ch)) || (fd_ispunct(ch))) {
	    match_result=cand; break;}}}
      END_FD_DO_CHOICES;}
    cand=get_next_candidate(string,cand,lim);
    fd_decref(matches);}
  return match_result;
}

static fd_lisp word_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp wpat=fd_get_arg(pat,1,FD_VOID);
  fd_lisp ends=
    fd_text_matcher(wpat,env,string,off,lim,(flags|MATCH_COLLAPSE_SPACES));
  fd_lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(end,ends) {
    fd_lisp substring=fd_make_substring(string+off,string+FIXLISP(end));
    ADD_TO_CHOICE(answers,FD_MAKE_PAIR(end,substring));}
  END_DO_CHOICES;
  fd_decref(ends);
  return answers;
}

/* Matching chunks */

static fd_lisp chunk_match
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp cpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_matcher(cpat,env,string,off,lim,flags);
}

static int chunk_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp cpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search(cpat,env,string,off,lim,flags);
}

static fd_lisp chunk_extract
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp cpat=fd_get_arg(pat,1,FD_VOID);
  fd_lisp ends=fd_text_matcher(cpat,env,string,off,lim,flags);
  fd_lisp answers=FD_EMPTY_CHOICE;
  DO_CHOICES(end,ends) {
    fd_lisp substring=fd_make_substring(string+off,string+FIXLISP(end));
    ADD_TO_CHOICE(answers,FD_MAKE_PAIR(end,substring));}
  END_DO_CHOICES;
  fd_decref(ends);
  return answers;
}

/** Case sensitive/insensitive **/

static fd_lisp match_ci
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_matcher
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_IGNORE_CASE)));
}
static int search_ci
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_search(fd_get_arg(pat,1,FD_VOID),env,string,off,lim,
			(flags|(MATCH_IGNORE_CASE)));
}
static fd_lisp extract_ci(fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_extract
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_IGNORE_CASE)));
}

static fd_lisp match_cs
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_matcher
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_CASE))));
}
static int search_cs
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_CASE))));
}
static fd_lisp extract_cs
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_extract
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_CASE))));
}

/* Diacritic insensitive and sensitive matching */

static fd_lisp match_di
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_matcher
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_IGNORE_DIACRITICS)));
}
static int search_di
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_search(fd_get_arg(pat,1,FD_VOID),env,string,off,lim,
			(flags|(MATCH_IGNORE_DIACRITICS)));
}
static fd_lisp extract_di
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_extract
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_IGNORE_DIACRITICS)));
}

static fd_lisp match_ds
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_matcher
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_DIACRITICS))));
}
static int search_ds
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_DIACRITICS))));
}
static fd_lisp extract_ds
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_extract
    (subpat,env,string,off,lim,(flags&(~(MATCH_IGNORE_DIACRITICS))));
}

/* Space collapsing matching */

static fd_lisp match_si
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_matcher
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_COLLAPSE_SPACES)));
}
static int search_si
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_search(fd_get_arg(pat,1,FD_VOID),env,string,off,lim,
			(flags|(MATCH_COLLAPSE_SPACES)));
}
static fd_lisp extract_si
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_extract
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_COLLAPSE_SPACES)));
}

static fd_lisp match_ss
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_matcher
    (subpat,env,string,off,lim,(flags&(~(MATCH_COLLAPSE_SPACES))));
}
static int search_ss
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_search
    (subpat,env,string,off,lim,(flags&(~(MATCH_COLLAPSE_SPACES))));
}
static fd_lisp extract_ss
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp subpat=fd_get_arg(pat,1,FD_VOID);
  return fd_text_extract
    (subpat,env,string,off,lim,(flags&(~(MATCH_COLLAPSE_SPACES))));
}

/** Canonical matching: ignore spacing, case, and diacritics */

static fd_lisp match_canonical
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_matcher
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_SPECIAL)));
}
static int search_canonical
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_search(fd_get_arg(pat,1,FD_VOID),env,string,off,lim,
			(flags|(MATCH_SPECIAL)));
}
static fd_lisp extract_canonical
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return fd_text_extract
    (fd_get_arg(pat,1,FD_VOID),env,
     string,off,lim,(flags|(MATCH_SPECIAL)));
}

/** EOL and BOL **/

static fd_lisp match_bol
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off == 0)
    return LISPFIX(0);
  else if ((string[off-1] == '\n') || (string[off-1] == '\r'))
    return LISPFIX(off);
  else return FD_EMPTY_CHOICE;
}

static int search_bol
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off == 0) return off;
  else if ((string[off-1] == '\n') || (string[off-1] == '\r')) return off;
  else {
    fd_u8char *scan=strchr(string+off,'\n');
    if (scan) return ((scan-string)+1);
    else {
      fd_u8char *scan=strchr(string+off,'\r');
      if (scan) return ((scan-string)+1);
      else return -1;}}
}

static fd_lisp match_eol
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off == lim) return LISPFIX(off);
  else if ((string[off] == '\n') || (string[off] == '\r'))
    return LISPFIX(off+1);
  else return FD_EMPTY_CHOICE;
}

static int search_eol
    (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off == lim) return off+1;
  else if (off > lim) return -1;
  else {
    fd_u8char *scan=strchr(string+off,'\n');
    if (scan) return ((scan-string));
    else {
      fd_u8char *scan=strchr(string+off,'\r');
      if (scan) return ((scan-string));
      else return lim;}}
}

/* Rest matching */

static fd_lisp match_rest
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return FD_LISPFIX(lim);
}
static int search_rest
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return off;
}
static fd_lisp extract_rest
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return FD_MAKE_PAIR(FD_LISPFIX(lim),fd_make_substring(string+off,string+lim));
}

/** Character match operations **/

static fd_lisp match_char_range
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int start=fd_char_code(fd_get_arg(pat,1,FD_VOID));
  int end=fd_char_code(fd_get_arg(pat,2,FD_VOID));
  int actual=string_ref(string+off);
  if (flags&(MATCH_IGNORE_CASE)) {
    start=fd_tolower(start); start=fd_tolower(end);
    actual=fd_tolower(start);}
  if ((actual >= start) && (actual <= end)) {
    fd_u8char *new_off=fd_utf8_substring(string+off,1);
    return LISPFIX(new_off-string);}
  else return FD_EMPTY_CHOICE;
}

static fd_lisp match_char_not_core
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags,int match_null_string)
{
  fd_lisp arg1=fd_get_arg(pat,1,FD_VOID);
  fd_u8char *scan=string+off, *last_scan=scan, *end=string+lim;
  int *break_chars, n_break_chars=0;
  if (!(STRINGP(arg1)))
    fd_type_error(_("CHAR-NOT arg not a string"),arg1);
  else {
    fd_u8char *scan=STRING_DATA(arg1); int c=fd_sgetc(&scan);
    break_chars=fd_malloc(sizeof(int)*STRING_LENGTH(arg1));
    while (!(c<0)) {
      if (flags&(MATCH_IGNORE_CASE))
	break_chars[n_break_chars++]=fd_tolower(c);
      else break_chars[n_break_chars++]=c;
      c=fd_sgetc(&scan);}}
  while (scan<end) {
    int i=0, hit=0; int ch=fd_sgetc(&scan);
    if (ch == -1) ch=0;
    if (flags&(MATCH_IGNORE_CASE)) ch=fd_tolower(ch);
    while (i < n_break_chars)
      if (break_chars[i] == ch) {hit=1; break;} else i++;
    if (hit)
      if (last_scan == string+off) {
	fd_free(break_chars,sizeof(int)*STRING_LENGTH(arg1));
	if (match_null_string) return LISPFIX(last_scan-string);
	else return FD_EMPTY_CHOICE;}
      else {
	fd_free(break_chars,sizeof(int)*STRING_LENGTH(arg1));
	return LISPFIX(last_scan-string);}
    else last_scan=scan;}
  fd_free(break_chars,sizeof(int)*STRING_LENGTH(arg1));
  return LISPFIX(lim);
}

static fd_lisp match_char_not
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return match_char_not_core(pat,env,string,off,lim,flags,0);
}
static fd_lisp match_char_not_star
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  return match_char_not_core(pat,env,string,off,lim,flags,1);
}

static fd_lisp isspace_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_isspace(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isspace_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  fd_u8char *scan=string+off, *limit=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  while (fd_isspace(ch)) 
    if (scan > limit) break;
    else {
      last=scan; ADD_TO_CHOICE(match_points,LISPFIX(last-string));
      ch=fd_sgetc(&scan);}
  if (last == string) return FD_EMPTY_CHOICE;
  else return match_points;
}
static int isspace_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isspace(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp spaces_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  while (fd_isspace(ch)) 
    if (scan > limit) break; else {last=scan; ch=fd_sgetc(&scan);}
  if (last == string+off) return FD_EMPTY_CHOICE;
  else return LISPFIX(last-string);
}
static int spaces_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isspace(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp spaces_star_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  while (fd_isspace(ch)) 
    if (scan > limit) break; else {last=scan; ch=fd_sgetc(&scan);}
  if (last == string) return FD_LISPFIX(off);
  else return LISPFIX(last-string);
}
static int spaces_star_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off > lim) return -1;
  else return off;
}

static fd_lisp isalnum_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_isalnum(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isalnum_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_isalnum(ch)) {
    while (fd_isalnum(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int isalnum_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isalnum(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp isword_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if ((fd_isalpha(ch)) || (ch == '-') || (ch == '_'))
    return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isword_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if ((fd_isalpha(ch)) || (ch == '-') || (ch == '_')) {
    while ((fd_isalpha(ch)) || (ch == '-') || (ch == '_')) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int isword_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if ((fd_isalpha(ch)) || (ch == '-') || (ch == '_')) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp isdigit_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_isdigit(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isdigit_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_isdigit(ch)) {
    while (fd_isdigit(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int isdigit_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isdigit(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp isalpha_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_isalpha(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isalpha_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_isalpha(ch)) {
    while (fd_isalpha(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int isalpha_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isalpha(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp ispunct_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_ispunct(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp ispunct_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_ispunct(ch)) {
    while (fd_ispunct(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int ispunct_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_ispunct(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp iscntrl_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_iscntrl(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp iscntrl_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_iscntrl(ch)) {
    while (fd_iscntrl(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int iscntrl_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_iscntrl(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp islower_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_islower(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp islower_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_islower(ch)) {
    while (fd_islower(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int islower_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_islower(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp isupper_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int ch=string_ref(string+off);
  if (fd_isupper(ch)) return LISPFIX(forward_char(string,off));
  else return FD_EMPTY_CHOICE;
}
static fd_lisp isupper_plus_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp match_points=FD_EMPTY_CHOICE;
  int ch=string_ref(string+off);
  if (fd_isupper(ch)) {
    while (fd_isupper(ch)) {
      off=forward_char(string,off);
      ADD_TO_CHOICE(match_points,LISPFIX(off));
      ch=string_ref(string+off);}
    return match_points;}
  else return FD_EMPTY_CHOICE;
}
static int isupper_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    int ch=string_ref(s);
    if (fd_isupper(ch)) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

static fd_lisp compound_word_match
    (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp embpunc=fd_get_arg(pat,1,FD_FALSE);
  fd_u8char *embstr=((FD_STRINGP(embpunc) ? (FD_STRING_DATA(embpunc)) : ((fd_u8char *)",-/.")));
  fd_u8char *scan=string+off, *limit=string+lim, *end;
  int ch=fd_sgetc(&scan);
  if (fd_isalnum(ch)) {
    end=scan;
    while (scan < limit) {
      while (fd_isalnum(ch)) {end=scan; ch=fd_sgetc(&scan);}
      if (strchr(embstr,ch)) {
	ch=fd_sgetc(&scan);
	if (fd_isalnum(ch)) continue;
	else return FD_LISPFIX(end-string);}
      else return FD_LISPFIX(end-string);}}
  else return FD_EMPTY_CHOICE;
}

/** Special matchers **/

#define islsym(c) \
   (!((fd_isspace(c)) || (c == '"') || (c == '(') || (c == '{') || \
      (c == '[') || (c == ')') || (c == '}') || (c == ']')))

static int lsymbol_startp(fd_u8char *string,int off)
{
  int ch=get_previous_char(string,off);
  if (ch < 0) return 1;
  else if (!(islsym(ch))) return 1;
  else return 0;
}

static fd_lisp islsym_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int i=off;
  if (lsymbol_startp(string,off) == 0) return FD_EMPTY_CHOICE;
  while (i < lim) {
    int ch=string_ref(string+i);
    if (islsym(ch)) i=forward_char(string,i);
    else break;}
  if (i > off) return LISPFIX(i);
  else return FD_EMPTY_CHOICE;
}
static int islsym_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=lsymbol_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (islsym(ch))) return prev-string;
    if (islsym(ch)) good_start=0; else good_start=1;}
  return -1;
}

static int csymbol_startp(fd_u8char *string,int off)
{
  int ch=get_previous_char(string,off);
  if (ch < 0) return 1;
  else if ((fd_isalnum(ch)) || (ch == '_')) return 0;
  else return 1;
}

static fd_lisp iscsym_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  int i=off; int ch=string_ref(string+off);
  if (csymbol_startp(string,off) == 0) return FD_EMPTY_CHOICE;
  if ((isalpha(ch)) || (ch == '_'))
    while (i < lim) {
      int ch=string_ref(string+i);
      if ((isalnum(ch)) || (ch == '_')) i=forward_char(string,i);
      else break;}
  if (i > off) return LISPFIX(i);
  else return FD_EMPTY_CHOICE;
}
static int iscsym_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=csymbol_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (isalpha(ch))) return prev-string;
    if (isalnum(ch) || (ch == '_'))
      good_start=0; else good_start=1;}
  return -1;
}

#define xmlnmcharp(x) \
  ((fd_isalnum(x)) || (x == '_') || (x == '-') || (x == '.') || (x == ':'))

static int xmlname_startp(fd_u8char *string,int off)
{
  int ch=get_previous_char(string,off);
  if (ch < 0) return 1;
  else if (fd_isalpha(ch)) return 0;
  else return 1;
}

static fd_lisp xmlname_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (xmlname_startp(string,off)) {
    int ch=string_ref(string+off);
    if (!(fd_isalpha(ch))) return FD_EMPTY_CHOICE;
    else {
      fd_u8char *scan=string+off, *limit=string+lim, *oscan=scan;
      int ch=fd_sgetc(&scan);
      if (!(fd_isalpha(ch))) return FD_EMPTY_CHOICE;
      else while (scan < limit)
	if (xmlnmcharp(ch)) {
	  oscan=scan; ch=fd_sgetc(&scan);}
	else break;
      if (xmlnmcharp(ch)) oscan=scan;
      return FD_LISPFIX(oscan-string);}}
  else return FD_EMPTY_CHOICE;
}
static int xmlname_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=xmlname_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (xmlnmcharp(ch))) return prev-string;
    if (fd_isalnum(ch))
      good_start=0; else good_start=1;}
  return -1;
}

static int xmlnmtoken_startp(fd_u8char *string,int off)
{
  int ch=get_previous_char(string,off);
  if (ch < 0) return 1;
  else if (xmlnmcharp(ch)) return 0;
  else return 1;
}

static fd_lisp xmlnmtoken_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (xmlnmtoken_startp(string,off)) {
    int ch=string_ref(string+off);
    if (!(xmlnmcharp(ch))) return FD_EMPTY_CHOICE;
    else {
      fd_u8char *scan=string+off, *limit=string+lim, *oscan=scan;
      int ch=fd_sgetc(&scan);
      if (!(xmlnmcharp(ch))) return FD_EMPTY_CHOICE;
      else while (scan < limit)
	if (xmlnmcharp(ch)) {
	  oscan=scan; ch=fd_sgetc(&scan);}
	else break;
      if (xmlnmcharp(ch)) oscan=scan;
      return FD_LISPFIX(oscan-string);}}
  else return FD_EMPTY_CHOICE;
}
static int xmlnmtoken_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=xmlnmtoken_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (xmlnmcharp(ch))) return prev-string;
    if (xmlnmcharp(ch))
      good_start=0; else good_start=1;}
  return -1;
}

#define apostrophep(x) ((x == '\''))

static fd_lisp aword_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *slim=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  if (word_startp(string,off) == 0) return FD_EMPTY_CHOICE;
  while ((scan<=slim) && ((fd_isalpha(ch)) || (apostrophep(ch)))) {
    last=scan; ch=fd_sgetc(&scan);}
  if (last > string+off) return LISPFIX(last-string);
  else return FD_EMPTY_CHOICE;
}
static int aword_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=word_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (isalpha(ch))) return prev-string;
    if (fd_isspace(ch)) good_start=1; else good_start=0;}
  return -1;
}

static fd_lisp lword_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *slim=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  if (word_startp(string,off) == 0) return FD_EMPTY_CHOICE;
  while ((scan<=slim) && ((fd_islower(ch)) || (apostrophep(ch)))) {
    last=scan; ch=fd_sgetc(&scan);}
  if (last > string+off) return LISPFIX(last-string);
  else return FD_EMPTY_CHOICE;
}
static int lword_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=word_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (fd_islower(ch))) return prev-string;
    if (fd_isspace(ch)) good_start=1; else good_start=0;}
  return -1;
}

static fd_lisp capword_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp matches=FD_EMPTY_CHOICE;
  fd_u8char *scan=string+off, *slim=string+lim, *last=scan;
  int ch=fd_sgetc(&scan);
  if (word_startp(string,off) == 0) return FD_EMPTY_CHOICE;
  if (!(fd_isupper(ch))) return matches;
  while ((fd_isalpha(ch)) || (apostrophep(ch)) || (ch == '-')) {
    if (ch == '-') FD_ADD_TO_CHOICE(matches,LISPFIX(last-string));
    last=scan; ch=fd_sgetc(&scan);}
  if (last > string+off) {
    FD_ADD_TO_CHOICE(matches,LISPFIX(last-string));}
  return matches;
}
static int capword_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *scan=string+off, *limit=string+lim;
  int good_start=word_startp(string,off);
  while (scan < limit) {
    fd_u8char *prev=scan; int ch=fd_sgetc(&scan);
    if ((good_start) && (fd_isupper(ch))) return prev-string;
    if (fd_isspace(ch)) good_start=1; else good_start=0;}
  return -1;
}

#define isoctdigit(x) ((isdigit(x)) && (x < '8'))

static fd_lisp anumber_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp base_arg=fd_get_arg(pat,1,FD_FALSE);
  int base=((FD_FIXNUMP(base_arg)) ? (FD_FIXLISP(base_arg)) : (10));
  fd_u8char *scan=string+off, *slim=string+lim, *last=scan;
  int prev_char=get_previous_char(string,off), ch=fd_sgetc(&scan);
  if (fd_isdigit(prev_char)) return FD_EMPTY_CHOICE;
  if (base == 8)
    while ((scan<=slim) && (ch<0x80) && (isoctdigit(ch))) {
      last=scan; ch=fd_sgetc(&scan);}
  else if (base == 16)
    while ((scan<=slim) && (ch<0x80) && (isxdigit(ch))) {
      last=scan; ch=fd_sgetc(&scan);}
  else if (base == 2)
    while ((scan<=slim) && (ch == '0') || (ch == '1')) {
      last=scan; ch=fd_sgetc(&scan);}
  else while ((scan<=slim) && (ch<0x80) && (fd_isdigit(ch))) {
    last=scan; ch=fd_sgetc(&scan);}
  if (last > string+off) return LISPFIX(last-string);
  else return FD_EMPTY_CHOICE;
}
static int anumber_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp base_arg=fd_get_arg(pat,1,FD_FALSE);
  int base=((FD_FIXNUMP(base_arg)) ? (FD_FIXLISP(base_arg)) : (10));
  fd_u8char *s=string+off, *sl=string+lim, *last=s;
  if (base == 2)
    while (s < sl) {
      int ch=fd_sgetc(&s);
      if ((ch == '1') || (ch == '0')) return last-string;
      else last=s;}
  else if (base == 8) {
    int ch=fd_sgetc(&s);
    if ((ch < 0x80) && (isoctdigit(ch))) return last-string;
    else last=s;}
  else if (base == 16) {
    int ch=fd_sgetc(&s);
    if ((ch < 0x80) && (isxdigit(ch))) return last-string;
    else last=s;}
  else {
    int ch=fd_sgetc(&s);
    if ((ch < 0x80) && (isdigit(ch))) return last-string;
    else last=s;}
    
  return -1;
}

#define is_not_mailidp(c) \
   ((!(fd_isprint(c))) || (fd_isspace(c)) || (c == '<') || \
       (c == ',') || (c == '(') || (c == '>') || (c == '<'))

static fd_lisp ismailid_match
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim; int atsign=0;
  while (s < sl) 
    if (*s == '@') {atsign=s-string; s++;}
    else {
      int ch=string_ref(s);
      if (*s < 0x80) s++;
      else s=fd_utf8_substring(s,1);
      if (is_not_mailidp(ch)) break;}
  if ((atsign) && (!(s == string+atsign+1)))
    if (s == sl) return LISPFIX((s-string)-1);
    else return LISPFIX((s-string));
  else return FD_EMPTY_CHOICE;
}

#define ismailid(x) ((x<128) && ((isalnum(x)) || (strchr(".-_",x) != NULL)))

static int ismailid_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *start=string+off, *slim=string+lim;
  fd_u8char *atsign=strchr(start,'@');
  while ((atsign) && (atsign < slim)) {
    if (atsign == start) start=atsign+1;
    else if (!(ismailid(atsign[0]))) start=atsign+1;
    else {
      fd_u8char *s=atsign-1; fd_lisp match;
      while (s > start) 
	if (!(ismailid(*s))) break; else s--;
      if (s != start) s++;
      match=ismailid_match(pat,NULL,string,s-string,lim,flags);
      if (!(FD_EMPTYP(match))) {
	fd_decref(match); return s-string;}
      else start=atsign+1;}
    atsign=strchr(start,'@');}
  return -1;
}

/* Hashset matches */

static fd_hashset to_hashset(fd_lisp arg)
{
  if (FD_PRIM_TYPEP(arg,hashset_type)) 
    return (fd_hashset)FD_CPTR_DATA(arg);
  else fd_type_error(_("not a hashtable"),arg);
}

static fd_lisp hashset_match
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp hs=fd_get_arg(pat,1,FD_VOID);
  fd_lisp cpat=fd_get_arg(pat,2,FD_VOID);
  fd_hashset h=to_hashset(hs);
  fd_lisp iresults=fd_text_matcher(cpat,env,string,off,lim,flags);
  fd_lisp results=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(possibility,iresults)
    if (fd_hashset_strget(h,string+off,FD_FIXLISP(possibility)-off)) {
      FD_ADD_TO_CHOICE(results,possibility);}
  END_FD_DO_CHOICES;
  return results;
}

static int hashset_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp hs=fd_get_arg(pat,1,FD_VOID);
  fd_lisp cpat=fd_get_arg(pat,2,FD_VOID);
  fd_hashset h=to_hashset(hs);
  int try=fd_text_search(cpat,env,string,off,lim,flags);
  while ((try >= 0) && (try < lim)) {
    fd_lisp matches=hashset_match(pat,env,string,try,lim,flags);
    if (!(FD_EMPTYP(matches))) {fd_decref(matches); return try;}
    else try=fd_text_search(cpat,env,string,try+1,lim,flags);}
  return -1;
}

static fd_lisp hashset_not_match
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp hs=fd_get_arg(pat,1,FD_VOID);
  fd_lisp cpat=fd_get_arg(pat,2,FD_VOID);
  fd_hashset h=to_hashset(hs);
  fd_lisp iresults=fd_text_matcher(cpat,env,string,off,lim,flags);
  fd_lisp results=FD_EMPTY_CHOICE;
  FD_DO_CHOICES(possibility,iresults)
    if (fd_hashset_strget(h,string+off,FD_FIXLISP(possibility)-off)) {}
    else {FD_ADD_TO_CHOICE(results,possibility);}
  END_FD_DO_CHOICES;
  return results;
}

static int hashset_not_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_lisp hs=fd_get_arg(pat,1,FD_VOID);
  fd_lisp cpat=fd_get_arg(pat,2,FD_VOID);
  fd_hashset h=to_hashset(hs);
  int try=fd_text_search(cpat,env,string,off,lim,flags);
  while ((try >= 0) && (try < lim)) {
    fd_lisp matches=hashset_not_match(pat,env,string,try,lim,flags);
    if (!(FD_EMPTYP(matches))) {fd_decref(matches); return try;}
    else try=fd_text_search(cpat,env,string,try+1,lim,flags);}
  return -1;
}

/** Search functions **/

static fd_u8char *strsearch
  (int flags,
   fd_u8char *pat,int patlen,
   fd_u8char *string,int off,int lim)
{
  fd_u8char buf[64], *scan=pat, *limit;
  int first_char=fd_sgetc(&scan), use_buf=0, c, cand;
  if ((flags&MATCH_COLLAPSE_SPACES) &&
      ((flags&(MATCH_IGNORE_CASE|MATCH_IGNORE_DIACRITICS)) == 0)) {
    struct FD_STRING_STREAM os; scan=pat; c=fd_sgetc(&scan);
    FD_INITIALIZE_FIXED_STRING_STREAM(&os,64,buf);
    while ((c>0) && (!(fd_isspace(c)))) {
      fd_sputc(&os,c); c=fd_sgetc(&scan);}
    use_buf=1;}
  else first_char=reduce_char(first_char,flags);
  scan=string+off; limit=string+lim; cand=scan-string;
  while (scan < limit) {
    if (use_buf) {
      fd_u8char *next=strstr(scan,buf);
      if (next) {
	cand=scan-string; scan++;}
      else return NULL;}
    else {
      c=fd_sgetc(&scan); c=reduce_char(c,flags);
      while ((c != first_char) && (scan < limit)) {
	cand=scan-string; c=fd_sgetc(&scan); c=reduce_char(c,flags);}
      if (c != first_char) return NULL;}
    if (cand > lim) return NULL;
    else {
      int matchlen=
	strmatcher(flags,pat,patlen,string,cand,lim);
      if (matchlen>0) return string+cand;}}
  return NULL;
}    

static int slow_search
   (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  fd_u8char *s=string+off, *sl=string+lim;
  while (s < sl) {
    fd_lisp result=fd_text_matcher(pat,env,string,s-string,lim,flags);
    if (!(FD_EMPTYP(result))) return s-string;
    else if (*s < 0x80) s++;
    else s=fd_utf8_substring(s,1);}
  return -1;
}

/* Top level functions */

FDTEXT_EXPORT
int fd_text_search
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int lim,int flags)
{
  if (off > lim) return -1;
  else if (FD_EMPTYP(pat)) return -1;
  else if (FD_STRINGP(pat)) {
    fd_u8char c=string[lim], *next; string[lim]='\0';
    if (flags&(MATCH_SPECIAL))
      next=strsearch(flags,STRING_DATA(pat),STRING_LENGTH(pat),
		     string,off,lim);
    else next=strstr(string+off,STRING_DATA(pat));
    string[lim]=c;
    if (next) return next-string;
    else return -1;}
  else if (FD_VECTORP(pat)) {
    fd_lisp initial=FD_VECTOR_REF(pat,0);
    int start=fd_text_search(initial,env,string,off,lim,flags);
    while ((start >= 0) && (start < lim)) {
      fd_lisp m=fd_text_matcher(pat,env,string,start,lim,flags);
      if (!(FD_EMPTYP(m))) {
	fd_decref(m); return start;}
      else start=fd_text_search
	     (initial,env,string,forward_char(string,start),lim,flags);}
    return -1;}
  else if (FD_CHARACTERP(pat)) {
    int c=fd_char_code(pat);
    if (c < 0x80) {
      fd_u8char c=string[lim], *next;
      string[lim]='\0'; next=strchr(string+off,c);
      if (next) return next-string;
      else return -1;}
    else {
      fd_u8char *s=string+off, *sl=string+lim;
      while (s < sl) {
	int ch=string_ref(s);
	if (ch == c) return s-string;
	else if (*s < 0x80) s++;
	else s=fd_utf8_substring(s,1);}
      return -1;}}
  else if (FD_CHOICEP(pat)) {
    int nlim=lim, loc=-1;
    DO_CHOICES(epat,pat) {
      int nxt=fd_text_search(epat,env,string,off,nlim,flags);
      if (nxt < 0) {}
      else if (nxt < nlim) {nlim=nxt; loc=nxt;}}
    END_DO_CHOICES;
    return loc;}
  else if (FD_PAIRP(pat)) {
    fd_lisp head=CAR(pat);
    struct TX_MATCH_OPERATOR
      *scan=match_operators, *limit=scan+n_match_operators;
    while (scan < limit)
      if (LISP_EQ(scan->symbol,head)) break; else scan++; 
    if (scan < limit)
      if (scan->searcher)
	return scan->searcher(pat,env,string,off,lim,flags);
      else return slow_search(pat,env,string,off,lim,flags);
    else fd_raise_lisp_exception(fd_TXInvalidPattern,"for text matching",pat);}
  else if (FD_SYMBOLP(pat)) {
    fd_lisp vpat=fd_symeval(pat,env);
    int result=fd_text_search(vpat,env,string,off,lim,flags);
    fd_decref(vpat); return result;}
  else if (FD_PRIM_TYPEP(pat,tx_closure_type)) {
    struct TX_CLOSURE *txc=FD_CPTR_DATA(pat);
    return fd_text_search(txc->pattern,txc->env,string,off,lim,flags);}
  else fd_raise_lisp_exception(fd_TXInvalidPattern,"in text searching",pat);
}

FDTEXT_EXPORT
int fd_text_match
  (fd_lisp pat,fd_lispenv env,fd_u8char *string,int off,int flags)
{
  fd_lisp extents=fd_text_matcher(pat,env,string,off,strlen(string),flags);
  int len=strlen(string), match=0;
  DO_CHOICES(extent,extents)
    if (FIXNUMP(extent))
      if (FIXLISP(extent) == len) {match=1; break;}
  END_DO_CHOICES;
  fd_decref(extents); return match;
}

FDTEXT_EXPORT
fd_lisp fd_tx_closure(fd_lisp expr,fd_lispenv env)
{
  struct TX_CLOSURE *txc=fd_malloc(sizeof(struct TX_CLOSURE));
  txc->pattern=fd_incref(expr); txc->env=fd_mallocd_env(env);
  return fd_make_cptr(tx_closure_type,txc);
}

static void free_tx_closure(fd_lisp x)
{
  struct TX_CLOSURE *txc=FD_CPTR_DATA(x);
  fd_decref(txc->pattern); fd_free_env(txc->env);
  fd_free(txc,sizeof(struct TX_CLOSURE));
  fd_qfree(FD_PTR_DATA(x,cptr),sizeof(struct FD_CPTR));
}

static void print_tx_closure(fd_lisp x,fd_string_stream ss)
{
  struct TX_CLOSURE *txc=FD_CPTR_DATA(x);
  fd_printf(ss,"#<TX-CLOSURE %q>",txc->pattern);
}

/** Initialization **/

void initialize_match_c()
{
  struct FD_TYPE_REGISTRY *r=fd_register_typecode(tx_closure_type);
  r->print_fcn=print_tx_closure; r->gc_fcn=free_tx_closure;

  init_match_operators_table();
  fd_add_match_operator("*",match_star,search_star,extract_star);
  fd_add_match_operator("+",match_plus,search_plus,extract_plus);
  fd_add_match_operator("NOT",match_not,search_not,NULL);
  fd_add_match_operator("AND",match_and,search_and,NULL);
  fd_add_match_operator("NOT>",match_not_gt,search_not,NULL);
  fd_add_match_operator("=",match_bind,search_bind,NULL);
  fd_add_match_operator("LABEL",label_match,label_search,label_extract);
  fd_add_match_operator("SUBST",subst_match,subst_search,subst_extract);

  fd_add_match_operator("MATCH-CASE",match_cs,search_cs,extract_cs);
  fd_add_match_operator("MC",match_cs,search_cs,extract_cs);
  fd_add_match_operator("IGNORE-CASE",match_ci,search_ci,extract_ci);
  fd_add_match_operator("IC",match_ci,search_ci,extract_ci);
  fd_add_match_operator("MATCH-DIACRITICS",match_ds,search_ds,extract_ds);
  fd_add_match_operator("MD",match_ds,search_ds,extract_ds);
  fd_add_match_operator("IGNORE-DIACRITICS",match_di,search_di,extract_di);
  fd_add_match_operator("ID",match_di,search_di,extract_di);
  fd_add_match_operator("MATCH-SPACING",match_ss,search_ss,extract_ss);
  fd_add_match_operator("MS",match_ss,search_ss,extract_ss);
  fd_add_match_operator("IGNORE-SPACING",match_si,search_si,extract_si);
  fd_add_match_operator("IS",match_si,search_si,extract_si);
  fd_add_match_operator
    ("CANONICAL",match_canonical,search_canonical,extract_canonical);

  fd_add_match_operator("BOL",match_bol,search_bol,NULL);
  fd_add_match_operator("EOL",match_eol,search_eol,NULL);
  fd_add_match_operator("CHAR-RANGE",match_char_range,NULL,NULL);
  fd_add_match_operator("CHAR-NOT",match_char_not,NULL,NULL);
  fd_add_match_operator("CHAR-NOT*",match_char_not_star,NULL,NULL);
  fd_add_match_operator("ISSPACE",isspace_match,isspace_search,NULL);
  fd_add_match_operator("ISSPACE+",isspace_plus_match,isspace_search,NULL);
  fd_add_match_operator("SPACES",spaces_match,spaces_search,NULL);
  fd_add_match_operator("SPACES*",spaces_star_match,spaces_star_search,NULL);
  fd_add_match_operator("ISALNUM",isalnum_match,isalnum_search,NULL);
  fd_add_match_operator("ISALNUM+",isalnum_plus_match,isalnum_search,NULL);
  fd_add_match_operator("ISWORD",isword_match,isword_search,NULL);
  fd_add_match_operator("ISWORD+",isword_plus_match,isword_search,NULL);
  fd_add_match_operator("ISALPHA",isalpha_match,isalpha_search,NULL);
  fd_add_match_operator("ISALPHA+",isalpha_plus_match,isalpha_search,NULL);
  fd_add_match_operator("ISDIGIT",isdigit_match,isdigit_search,NULL);
  fd_add_match_operator("ISDIGIT+",isdigit_plus_match,isdigit_search,NULL);
  fd_add_match_operator("ISPUNCT",ispunct_match,ispunct_search,NULL);
  fd_add_match_operator("ISPUNCT+",ispunct_plus_match,ispunct_search,NULL);
  fd_add_match_operator("ISCNTRL",iscntrl_match,iscntrl_search,NULL);
  fd_add_match_operator("ISCNTRL+",iscntrl_plus_match,iscntrl_search,NULL);
  fd_add_match_operator("ISUPPER",isupper_match,isupper_search,NULL);
  fd_add_match_operator("ISUPPER+",isupper_plus_match,isupper_search,NULL);
  fd_add_match_operator("ISLOWER",islower_match,islower_search,NULL);
  fd_add_match_operator("ISLOWER+",islower_plus_match,islower_search,NULL);
  fd_add_match_operator("LSYMBOL",islsym_match,islsym_search,NULL);
  fd_add_match_operator("CSYMBOL",iscsym_match,iscsym_search,NULL);
  fd_add_match_operator("XMLNAME",xmlname_match,xmlname_search,NULL);
  fd_add_match_operator("XMLNMTOKEN",xmlnmtoken_match,xmlnmtoken_search,NULL);
  fd_add_match_operator("AWORD",aword_match,aword_search,NULL);
  fd_add_match_operator("LWORD",lword_match,lword_search,NULL);
  fd_add_match_operator("ANUMBER",anumber_match,anumber_search,NULL);
  fd_add_match_operator("CAPWORD",capword_match,capword_search,NULL);
  fd_add_match_operator
    ("COMPOUND-WORD",compound_word_match,isalnum_search,NULL);
  fd_add_match_operator("MAILID",ismailid_match,ismailid_search,NULL);

  fd_add_match_operator("CHUNK",chunk_match,chunk_search,chunk_extract);

  /* Whitespace or punctuatation separated tokens */
  fd_add_match_operator("WORD",word_match,word_search,word_extract);
  fd_add_match_operator("PHRASE",word_match,word_search,word_extract);

  fd_add_match_operator("REST",match_rest,search_rest,extract_rest);
  fd_add_match_operator("HASHSET",hashset_match,hashset_search,NULL);
  fd_add_match_operator("HASHSET-NOT",hashset_not_match,hashset_not_search,NULL);

  label_symbol=fd_make_symbol("LABEL");
  star_symbol=fd_make_symbol("*");
  plus_symbol=fd_make_symbol("+");
}



/* File specific stuff */

/* The CVS log for this file
   $Log: match.c,v $
   Revision 1.28  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.27  2004/03/03 18:48:27  haase
   Added CHAR-NOT*

   Revision 1.26  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.25.2.3  2003/03/15 16:56:34  haase
   Fixed poor fix to fencepost bug

   Revision 1.25.2.2  2003/03/15 16:11:41  haase
   Fixed fencepost problems for text matching with not>

   Revision 1.25.2.1  2003/01/26 20:50:21  haase
   Misc. fixes, especially GC

   Revision 1.25  2002/07/02 15:04:30  haase
   Fixed match operations to except the empty choice as a pattern

   Revision 1.24  2002/07/01 02:51:18  haase
   Added COMPOUND-WORD primitive

   Revision 1.23  2002/06/22 23:19:08  haase
   Added xmlname/xmlnmtokens matcher

   Revision 1.22  2002/06/08 20:26:20  haase
   Fixed capword to handle dashed words

   Revision 1.21  2002/05/30 12:06:09  haase
   Fixed problem with NOT> not handling zero-length matches

   Revision 1.20  2002/04/22 18:26:22  haase
   Fixed leak in AND matcher

   Revision 1.19  2002/04/21 13:03:38  haase
   Added inclusion of apostrophes in words for whole-word matching

   Revision 1.18  2002/04/20 18:26:43  haase
   Fixes to word matching in the fdtext library, checking that the character before the word is whitespace or whatever punctuation is appropriate

   Revision 1.17  2002/04/20 14:59:46  haase
   Added AND operator to matching

   Revision 1.16  2002/04/17 00:30:23  haase
   src/text/text.c

   Revision 1.15  2002/04/02 21:41:08  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
