/* C Mode */

/* timefns.c
   Implements functions for manipulating times from DType/FramerD applications.
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

static char vcid[] = "$Id: timefns.c,v 1.17 2005/01/14 16:48:49 haase Exp $";

#include "dtypes.h"
#include <signal.h>
#include <math.h>

static lisp timestamp_symbol;

fd_exception fd_GMTIMEfailed=_("GMTIME failed in some way");

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
# else
# include <time.h>
# endif
#endif

/** Getting time information **/

int fd_tzoff=0;

#if FD_THREADS_ENABLED
fd_mutex tm_mutex;
#endif

DTYPES_EXPORT
/* fd_localtime
     Arguments: a pointer to a tm struct and a time_t value
     Returns: the time_t value or -1 if it failed

Fills the tm struct with the local time based on the time_t value.
This is threadsafe, since localtime is not. */
int fd_localtime(struct tm *tptr,time_t tick)
{
  struct tm *now;
  lock_mutex(&tm_mutex);
  now=localtime(&tick);
  if (now == NULL) {unlock_mutex(&tm_mutex); return -1;}
  tptr->tm_year=now->tm_year; tptr->tm_mon=now->tm_mon; 
  tptr->tm_yday=now->tm_yday; tptr->tm_mday=now->tm_mday; tptr->tm_wday=now->tm_wday; 
  tptr->tm_hour=now->tm_hour; tptr->tm_min=now->tm_min; tptr->tm_sec=now->tm_sec; 
#if HAVE_TM_GMTOFF
  tptr->tm_gmtoff=now->tm_gmtoff; tptr->tm_isdst=now->tm_isdst;
#endif
  if (tptr->tm_year < 200) tptr->tm_year=tptr->tm_year+1900;
  unlock_mutex(&tm_mutex);
  return tick;
}

DTYPES_EXPORT
/* fd_breakup_time:
     Arguments: a pointer to a tm struct, a time_t value, and an offset
     Returns: an integral timezone offset

Fills the tm struct with the broken down UTC time based on the time_t value.
This is threadsafe, locking the possible shared tptr. */
int fd_breakup_time(struct tm *tptr,time_t tick,int tzoff)
{
  struct tm *now; time_t fake=tick+tzoff;
  lock_mutex(&tm_mutex);
  now=gmtime(&fake);
  if (now == NULL) {unlock_mutex(&tm_mutex); return -1;}
  tptr->tm_year=now->tm_year; tptr->tm_mon=now->tm_mon; 
  tptr->tm_yday=now->tm_yday; tptr->tm_mday=now->tm_mday; 
  tptr->tm_wday=now->tm_wday; 
  tptr->tm_hour=now->tm_hour; tptr->tm_min=now->tm_min; 
  tptr->tm_sec=now->tm_sec;
#if HAVE_TM_GMTOFF
  tptr->tm_gmtoff=0; tptr->tm_isdst=0;
#endif
  if (tptr->tm_year < 200) tptr->tm_year=tptr->tm_year+1900;
  unlock_mutex(&tm_mutex);
  return tick;
}

static int compute_tzoff(time_t at)
{
  struct tm tptr;
  time_t mktime_result;
  fd_breakup_time(&tptr,at,0);
  tptr.tm_year=tptr.tm_year-1900;
  mktime_result=mktime(&tptr);
  return (at-mktime_result)+((tptr.tm_isdst)*3600);
}

static int days[]={
  0,31,59,90,120,151,181,212,243,273,304,334,365};
static time_t epoch_days;

static time_t get_days(int y,int m,int d)
{
  int leap=((m > 1) && ((y%4) == 0) && ((!((y%100) == 0) || ((y%400) == 0))));
  return ((y-1961)/4)*1461+((y-1961)%4)*365+days[m]+(d-1)+leap;
}

static time_t gm_mktime(struct tm *tptr)
{
  int days=get_days(tptr->tm_year,tptr->tm_mon,tptr->tm_mday)-epoch_days;
  return (days*86400+tptr->tm_hour*3600 + tptr->tm_min*60 + tptr->tm_sec);
}

DTYPES_EXPORT
/* fd_mktime:
     Arguments: a pointer to a tm struct and a time offset (from UTC) in seconds
     Returns: a time_t pointer

Returns the UTC time given a filled out tm structure and the offset of the
zone it was filled out in from UTC. */
time_t fd_mktime(struct tm *tptr,int tzoff)
{
  time_t result;
  result=gm_mktime(tptr);
  return result-tzoff;
}

/** Timestamp Functions **/

DTYPES_EXPORT
/* fd_timestamp_time:
     Arguments: a lisp pointer to a timestamp
     Returns: a time_t value
*/
time_t fd_timestamp_time(lisp timestamp)
{
  if (FD_LRECORD_TYPEP(timestamp,timestamp_symbol)) {
    lisp data=LRECORD_DATA(timestamp);
    if (FIXNUMP(data)) return (time_t)FIXLISP(data);
    else if ((VECTORP(data)) && (FIXNUMP(VECTOR_REF(data,0))))
      return FD_FIXLISP(VECTOR_REF(data,0));
    else fd_ctype_error("fd_timestamp_time",_("invalid timestamp"),timestamp);}
  else fd_ctype_error("fd_timestamp_time",_("not a timestamp"),timestamp);
}

struct TZENTRY {char *name; int tzoff;};
static struct TZENTRY tzones[]= {
  {"GMT",0},
  {"UT",0},
  {"UTC",0},  
  {"EST",-5*3600},
  {"EDT",-4*3600},
  {"CST",-6*3600},
  {"CDT",-5*3600},
  {"MST",-7*3600},
  {"MDT",-6*3600},
  {"PST",-8*3600},
  {"PDT",-7*3600},
  {"CET",1*3600},
  {"EET",2*3600},
  {NULL,0}};

static int lookup_tzname(char *string,int dflt)
{
  struct TZENTRY *zones=tzones;
  while ((*zones).name)
    if (strcasecmp(string,(*zones).name) == 0)
      return (*zones).tzoff;
    else zones++;
  return dflt;
}

DTYPES_EXPORT
/* fd_parse_tzspec:
     Arguments: a string and a default offset
     Returns: an offset from UTC

This uses a built in table but should really use operating system
facilities if they were even remotely standardized.
*/
int fd_parse_tzspec(char *s,int dflt)
{
  int hours=0, mins=0, secs=0, sign=1;
  char *offstart=strchr(s,'+');
  if (offstart == NULL) {
    offstart=strchr(s,'-');
    if (offstart) sign=-1;
    else return lookup_tzname(s,dflt);}
  sscanf(offstart+1,"%d:%d:%d",&hours,&mins,&secs);
  return sign*(hours*3600+mins*60+secs);
}

DTYPES_EXPORT
/* fd_init_xtime:
     Arguments: a pointer to an extended timestamp structure
     Returns: void

This takes a timestamp object and fills out an extended time pointer
structure which includes timezone and precision information.
*/
void fd_init_xtime(struct FD_XTIME *xtp)
{
  xtp->secs=0; fd_breakup_time(&(xtp->tptr),12*3600,0);
#if HAVE_TM_GMTOFF
  xtp->tptr.tm_isdst=-1;
  xtp->tptr.tm_gmtoff=0;
  xtp->tptr.tm_zone=0;
#endif
  xtp->precision=second; xtp->tzoff=0;
}

DTYPES_EXPORT
/* fd_timestamp_to_xtime:
     Arguments: a lisp pointer to a timestamp and a pointer to a timestamp structure
     Returns: -1 on error, the time as a time_t otherwise

This takes a timestamp object and fills out an extended time pointer
structure which includes timezone and precision information.
*/
time_t fd_timestamp_to_xtime(fd_lisp timestamp,struct FD_XTIME *xtp)
{
  fd_lisp data=fd_lrecord_data(timestamp,timestamp_symbol); 
  fd_init_xtime(xtp);
  if (FIXNUMP(data)) {
    xtp->secs=(time_t) FIXLISP(data);
    xtp->nsecs=0; xtp->precision=second;
    xtp->tzoff=0;}
  else if (!(VECTORP(data))) {
    decref(data); return (time_t)-1;}
  else {
    if (!(FIXNUMP(VECTOR_REF(data,0)))) {
      decref(data); return (time_t)-1;}
    else xtp->secs=(time_t)FIXLISP(VECTOR_REF(data,0));
    if (!(FIXNUMP(VECTOR_REF(data,1)))) {
      decref(data); return (time_t)-1;}
    else xtp->nsecs=FIXLISP(VECTOR_REF(data,1));
    if (!((FIXNUMP(VECTOR_REF(data,2))) &&
	  ((FIXLISP(VECTOR_REF(data,2)) < 10)))) {
      decref(data); return (time_t)-1;}
    else xtp->precision=(fd_tmprec)FIXLISP(VECTOR_REF(data,2));
    /* If precision is low, clear the nanoseconds in case it was inadvertently specified. */
    if (xtp->precision <= 6) xtp->nsecs=0;
    if (FIXNUMP(VECTOR_REF(data,3))) xtp->tzoff=FIXLISP(VECTOR_REF(data,3));
    else {
      decref(data); return (time_t)-1;}}
  fd_decref(data);
  fd_breakup_time(&xtp->tptr,xtp->secs,xtp->tzoff);
  return xtp->secs;
}

DTYPES_EXPORT
/* fd_iso8601_to_xtime:
     Arguments: a string and a pointer to a timestamp structure
     Returns: -1 on error, the time as a time_t otherwise

This takes an iso8601 string and fills out an extended time pointer which
includes possible timezone and precision information.
*/
time_t fd_iso8601_to_xtime(char *s,struct FD_XTIME *xtp)
{
  char *tzstart; int tzoff;
  int pos[]={-1,4,7,10,13,16,19,20}, nsecs=0, n_elts;
  if (strchr(s,'/')) return (time_t) -1;
  n_elts=sscanf(s,"%04u-%02u-%02uT%02u:%02u:%02u.%u",
		&xtp->tptr.tm_year,&xtp->tptr.tm_mon,
		&xtp->tptr.tm_mday,&xtp->tptr.tm_hour,
		&xtp->tptr.tm_min,&xtp->tptr.tm_sec,
		&nsecs);
  /* Give up if you can't parse anything */
  if (n_elts == 0) return (time_t) -1;
  else if ((n_elts < 6) && (strchr(s,':'))) return (time_t) -1;
  /* Adjust month */
  xtp->tptr.tm_mon--;
  xtp->precision=n_elts;
  if (n_elts <= 6) xtp->nsecs=0;
  if (n_elts == 7) {
    char *start=s+pos[n_elts], *scan=start; int zeros=0;
    while (*scan == '0') {zeros++; scan++;}
    while (isdigit(*scan)) scan++;
    xtp->nsecs=nsecs*(9-zeros);
    xtp->precision=xtp->precision+((scan-start)/3);
    tzstart=scan;}
  else tzstart=s+pos[n_elts];
  xtp->tzoff=fd_parse_tzspec(tzstart,xtp->tzoff);
  xtp->secs=fd_mktime(&(xtp->tptr),xtp->tzoff);
  return xtp->secs;
}

DTYPES_EXPORT
/* fd_xtime_to_timestamp:
     Arguments: a string and a pointer to a timestamp structure
     Returns: -1 on error, the time as a time_t otherwise

This takes an iso8601 string and fills out an extended time pointer which
includes possible timezone and precision information.
*/
fd_lisp fd_xtime_to_timestamp(struct FD_XTIME *xtp)
{
  lisp vec=fd_make_vector(4);
  FD_VECTOR_SET(vec,0,FD_LISPFIX(xtp->secs));
  FD_VECTOR_SET(vec,1,FD_LISPFIX(xtp->nsecs));
  FD_VECTOR_SET(vec,2,FD_LISPFIX((int)xtp->precision));
  FD_VECTOR_SET(vec,3,FD_LISPFIX((int)xtp->tzoff));
  return fd_make_lrecord(timestamp_symbol,vec);
}

DTYPES_EXPORT
/* fd_xtime_to_is08601:
     Arguments: a timestamp and a pointer to a string stream
     Returns: -1 on error, the time as a time_t otherwise

This takes an iso8601 string and fills out an extended time pointer which
includes possible timezone and precision information.
*/
int fd_xtime_to_iso8601(struct FD_XTIME *xtp,fd_string_stream ss)
{
  struct tm *tp=&(xtp->tptr);
  char buf[128], tzbuf[128];
  fd_breakup_time(&(xtp->tptr),xtp->secs,xtp->tzoff);
  switch (xtp->precision) {
  case year:
    sprintf(buf,"%04d",tp->tm_year); break;
  case month:
    sprintf(buf,"%04d-%02d",tp->tm_year,tp->tm_mon+1); break;
  case day:
    sprintf(buf,"%04d-%02d-%02d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday); break;
  case hour:
    sprintf(buf,"%04d-%02d-%02dT%02d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,tp->tm_hour); break;
  case minute:
    sprintf(buf,"%04d-%02d-%02dT%02d:%02d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,
	    tp->tm_hour,tp->tm_min); break;
  case second:
    sprintf(buf,"%04d-%02d-%02dT%02d:%02d:%02d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,
	    tp->tm_hour,tp->tm_min,tp->tm_sec); break;
  case millisecond:
    sprintf(buf,"%04d-%02d-%02dT%02d:%02d:%02d.%03d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,
	      tp->tm_hour,tp->tm_min,tp->tm_sec,
	    xtp->nsecs/1000000); break;
  case microsecond:
    sprintf(buf,"%04d-%02d-%02dT%02d:%02d:%02d.%06d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,
	    tp->tm_hour,tp->tm_min,tp->tm_sec,
	    xtp->nsecs/1000); break;
  case nanosecond:
    sprintf(buf,"%04d-%02d-%02dT%02d:%02d:%02d.%09d",
	    tp->tm_year,tp->tm_mon+1,tp->tm_mday,
	      tp->tm_hour,tp->tm_min,tp->tm_sec,
	    xtp->nsecs); break;}
  if (xtp->precision < 4) {}
  else if (xtp->tzoff) {
    char *sign=((xtp->tzoff<0) ? "-" : "+");
    int tzoff=((xtp->tzoff<0) ? (-((xtp->tzoff))) : (xtp->tzoff));
    int hours=tzoff/3600, minutes=(tzoff%3600)/60, seconds=tzoff%3600-minutes*60;
    if (seconds)
      sprintf(tzbuf,"%s%d:%02d:%02d",sign,hours,minutes,seconds);
    else sprintf(tzbuf,"%s%d:%02d",sign,hours,minutes);}
  else strcpy(tzbuf,"UTC");
  if (xtp->precision > day)
    fd_printf(ss,"%s%s",buf,tzbuf);
  else fd_printf(ss,"%s",buf);
  return fd_mktime(&(xtp->tptr),xtp->tzoff);
}

/* Old stuff */

static char timestring_buf[128];

DTYPES_EXPORT
/* fd_timestring:
     Arguments: none
     Returns: a printed ASCII representation of the local time
     */
char *fd_timestring()
{
  struct tm _now, *now=&_now; fd_localtime(now,time(NULL));
  sprintf(timestring_buf,"%02d:%02d:%02d",
	  now->tm_hour,now->tm_min,now->tm_sec);
  return timestring_buf;
}

DTYPES_EXPORT
/* fd_parse_iso8601:
     Arguments: a string
     Returns: a time_t
 Parses an iso8601 format date/time string into a time_t value.
*/
time_t fd_parse_iso8601(char *string)
{
  struct FD_XTIME xtime;
  return fd_iso8601_to_xtime(string,&xtime);
}

/** Getting the current time */

DTYPES_EXPORT
/* fd_get_now:
     Arguments: a pointer to an extended time pointer
     Returns: a time_t or -1 if it fails for some reason

  This will try and get the finest precision time it can.
*/
time_t fd_get_now(struct FD_XTIME *xtp)
{
#if HAVE_GETTIMEOFDAY
  struct timeval tv;
  if (gettimeofday(&tv,NULL) < 0) return (time_t) -1;
  xtp->secs=tv.tv_sec; xtp->nsecs=tv.tv_usec*1000;
  xtp->precision=microsecond; xtp->tzoff=fd_tzoff;
#elif HAVE_FTIME
    struct timeb tb; ftime(&tb);
    xtp->secs=tb.time; xtp->nsecs=tb.millitm*1000000;
    xtp->precision=millisecond; xtp->tzoff=fd_tzoff;
#else
    xtp->secs=time(NULL); xtp->nsecs=0;
    xtp->precision=second; xtp->tzoff=fd_tzoff;
#endif
    fd_breakup_time(&(xtp->tptr),xtp->secs,xtp->tzoff);
    return xtp->secs;
}

/** Sleeping **/

DTYPES_EXPORT
/* fd_sleep
    Arguments: an interval in seconds (a double)
    Returns: nothing

 This is a platform abstraction for sleeping that *may*
allow sub-second sleeps.
*/
void fd_sleep(double secs)
{
#if defined(WIN32)
      Sleep((int)(ceil(1000.0*secs)));
#elif HAVE_NANOSLEEP
      {
	struct timespec req, rem;
	req.tv_sec=(time_t)(floor(secs));
	req.tv_nsec=(long)(1000000000.0*(secs-req.tv_sec));
	/* Loop until you've used up your time */
	while (nanosleep(&req,&rem) < 0) {
	  FD_CLEAR_ERR(); req=rem;}}
#elif HAVE_USLEEP
      usleep((unsigned long)(1000000.0*secs));
#else
      sleep((int)secs);
#endif
}

/* Initialization */

void fd_initialize_timefns_c()
{
  fd_tzoff=compute_tzoff(time(NULL));
  epoch_days=get_days(1970,0,1);

#if FD_THREADS_ENABLED
  fd_init_mutex(&tm_mutex);
#endif

  timestamp_symbol=fd_make_symbol("TIMESTAMP0");

  fd_register_source_file("envfns",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: timefns.c,v $
   Revision 1.17  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.16  2004/07/20 09:16:15  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.15  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.14  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.13.2.2  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.13.2.1  2003/01/26 20:56:06  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.13  2002/07/03 03:10:37  haase
   Fixed leap year and zero-based month bug

   Revision 1.12  2002/07/01 02:40:30  haase
   Made ISO timestring parsing fail if the string includes a slash (forcing fd_parse_timestring to do the job itself)

   Revision 1.11  2002/06/23 11:35:53  haase
   Fixed bug where low precision timestamps kept high precision nsecs

   Revision 1.10  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.9  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.8  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.7  2002/04/02 21:39:34  haase
   Added log and emacs init entries to C source files

   Revision 1.6  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
