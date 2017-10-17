/* C Mode */

/* libdtypes.c
   Implements init function for the DTypes library, including consing and OS functions
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

static char vcid[] =
  "$Id: libdtypes.c,v 1.20 2005/01/14 16:48:44 haase Exp $";

#include <locale.h>
#include "dtypes.h"

fd_exception fd_ConfigSyntaxError=_("Error in config file syntax");

static int data_initialized=0, dtypes_initialized=0;
static int load_profile=0, load_config=1, profile_loaded=0;
static char *config_file=NULL; /* The config file which was loaded */

extern void fd_initialize_except_c(void);
extern void fd_initialize_i18n_c(void);
extern void fd_initialize_data_c(void);
extern void fd_initialize_hash_c(void);
extern void fd_initialize_io_c(void);
extern void fd_initialize_slotmaps_c(void);
extern void fd_initialize_choices_c(void);
extern void fd_initialize_oids_c(void);
extern void fd_initialize_xdata_c(void);
extern void fd_initialize_fdprintf_c(void);
extern void fd_initialize_fdmalloc_c(void);
extern void fd_initialize_envfns_c(void);
extern void fd_initialize_timefns_c(void);
extern void fd_initialize_network_c(void);

static void initialize_text_encoding()
{
  char *encoding_name=NULL;
  encoding_name=fd_string_getenv("TEXT-ENCODING");
#if (HAVE_LANGINFO_H)
  if (encoding_name == NULL)
    encoding_name=nl_langinfo(CODESET);
  if (encoding_name)
    encoding_name=fd_strdup(encoding_name);
#endif
  if (encoding_name == NULL) {
    char *lcdata=getenv("LC_ALL");
    if (lcdata == NULL) lcdata=getenv("LC_CTYPE");
    if (lcdata == NULL) lcdata=getenv("LANG");
    if (lcdata) {
      char *dot=strchr(lcdata,'.');
      if (dot) {
	char *chset=fd_strdup(dot+1);
	char *at=strchr(chset,'@');
	if (at) *at=0; encoding_name=chset;}}}
  if (encoding_name)
    fd_set_default_encoding(encoding_name);
  else fd_set_default_encoding("ASCII");
  if (encoding_name) fd_xfree(encoding_name);
}

DTYPES_EXPORT void fd_load_user_profile()
{
  char *profile;
  if (profile_loaded) {}
  else {
    profile_loaded=1;
    profile=fd_string_getenv("FRAMERD_PROFILE");
    if (profile) 
      if (strcmp(profile,"none") == 0) {fd_xfree(profile);}
      else {
	WITH_HANDLING {
	  fd_load_config(profile); fd_xfree(profile);}
	ON_EXCEPTION {
	  fd_xfree(profile); CLEAR_ERR();}
	END_HANDLING;}
    else {
      WITH_HANDLING {
	fd_load_config("~/.fdprofile");}
      ON_EXCEPTION {
	CLEAR_ERR();}
      END_HANDLING;}}
}

DTYPES_EXPORT char *fd_get_config_file()
{
  if (config_file) return config_file;
  else if (getenv("FRAMERD_CONFIG")) return getenv("FRAMERD_CONFIG");
  else return DEFAULT_FRAMERD_CONFIG;
}

DTYPES_EXPORT void fd_use_profile() { load_profile=1; }
DTYPES_EXPORT void fd_suppress_config() { load_config=0; }

fd_lisp FD_FALSE, FD_TRUE, FD_EMPTY_LIST, FD_EOF_OBJECT;
fd_lisp FD_VOID, FD_EMPTY_CHOICE, FD_QUOTED_EMPTY_CHOICE;

static void initialize_constants()
{
  FD_FALSE=FD_MAKE_FALSE;
  FD_TRUE=FD_MAKE_TRUE;
  FD_EMPTY_LIST=FD_MAKE_EMPTY_LIST;
  FD_EOF_OBJECT=FD_MAKE_EOF_OBJECT;
  FD_VOID=FD_MAKE_VOID;
  FD_EMPTY_CHOICE=FD_MAKE_EMPTY_CHOICE;
  FD_QUOTED_EMPTY_CHOICE=FD_MAKE_QUOTED_EMPTY_CHOICE;
}

DTYPES_EXPORT void fd_initialize_data()
{
  FD_CLEAR_ERR();
  if (data_initialized) return; else data_initialized=1;

  initialize_constants();
  fd_initialize_except_c();
  fd_initialize_fdmalloc_c();
  fd_initialize_data_c();
  fd_initialize_hash_c();
  fd_initialize_io_c();
  fd_initialize_choices_c();
  fd_initialize_oids_c();
  fd_initialize_slotmaps_c();
  fd_initialize_xdata_c();
  fd_initialize_network_c();
  fd_initialize_i18n_c();
  fd_initialize_fdprintf_c();
  fd_initialize_envfns_c();
  fd_initialize_timefns_c();
  fd_setup_signal_handlers();
}

DTYPES_EXPORT void fd_initialize_dtypes()
{
  char *locale_result;
  if (dtypes_initialized) return; else dtypes_initialized=1;
  fd_initialize_data();
  config_file=fd_string_getenv("FRAMERD_CONFIG");
  if (load_config) 
    if (config_file)
      if (strcmp(config_file,"none") == 0) {}
      else if (!(fd_file_existsp(config_file))) 
	fd_warn(_("Configuration file %s doesn't exist"),config_file);
      else {
	WITH_HANDLING {
	  fd_load_config(config_file);}
	ON_EXCEPTION {
	  CLEAR_ERR();}
	END_HANDLING;}
    else if (fd_file_existsp(DEFAULT_FRAMERD_CONFIG)) {
      WITH_HANDLING {
	config_file=fd_strdup(DEFAULT_FRAMERD_CONFIG);
	fd_load_config(DEFAULT_FRAMERD_CONFIG);}
      ON_EXCEPTION {
	CLEAR_ERR();}
      END_HANDLING;}
    else fd_warn(_("No configuration file"));
  else fd_notify(_("Configuration file suppressed"));
  if (load_profile) fd_load_user_profile();
  if ((locale_result=setlocale(LC_ALL,"")) == NULL) {
#if FD_ISSUE_LOCALE_WARNING
    char *errdata=strerror(errno);
    fd_notify(_("Invalid locale information, using default (error was %s)"),
	      errdata);
#endif
    FD_CLEAR_ERR();}
  else {
    /* fd_notify("locale_result=%s",locale_result); */
    FD_CLEAR_ERR();}
  setlocale(LC_NUMERIC,"POSIX");
  {WITH_HANDLING
     initialize_text_encoding();
  ON_EXCEPTION {
    fd_clear_exception();
    fd_warn(_("Error %m (%m) while initializing text encoding"),
	    fd_theException(),fd_exception_details());}
  END_HANDLING}
  textdomain("framerd");
  /* Initialize network timeouts */
  {
    fd_lisp ctimeout=fd_getenv("CONNECT_TIMEOUT");
    fd_lisp rtimeout=fd_getenv("RECEIVE_TIMEOUT");
    int ct=0, rt=0;
    if (FD_FIXNUMP(ctimeout)) ct=FD_FIXLISP(ctimeout);
    else if (FD_STRINGP(ctimeout)) 
      sscanf(FD_STRING_DATA(ctimeout),"%d",&ct);
    if (FD_FIXNUMP(rtimeout)) ct=FD_FIXLISP(rtimeout);
    else if (FD_STRINGP(rtimeout)) 
      sscanf(FD_STRING_DATA(rtimeout),"%d",&rt);
    fd_set_network_timeouts(ct,rt,0);}
  fd_show_startup_herald();
}

#if ((defined(WIN32)) && (!(defined(STATICLINK))))
BOOL APIENTRY DllMain( HANDLE hModule, 
                        DWORD ul_reason_for_call, 
                        LPVOID lpReserved )
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
      fd_initialize_dtypes(); break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      {}
    }
    return TRUE;
}
#endif


/* File specific stuff */

/* The CVS log for this file
   $Log: libdtypes.c,v $
   Revision 1.20  2005/01/14 16:48:44  haase
   Updated copyrights to 2005

   Revision 1.19  2004/07/20 09:16:11  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.18  2004/06/29 20:01:00  haase
   Clear errno after setlocale

   Revision 1.17  2004/03/31 21:04:11  haase
   Various unthreaded compile fixes

   Revision 1.16  2004/03/30 21:44:55  haase
   Separated out fd_initialize_data and fd_initialize_dtypes

   Revision 1.15  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.14  2003/09/30 19:07:51  haase
   Added conditionals for custom mutex attributes

   Revision 1.13  2003/09/10 11:20:00  haase
   Minor rearrangements of source code

   Revision 1.12  2003/08/27 10:53:28  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.2  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11.2.1  2002/09/26 02:11:26  haase
   Don't bother calling strerror if you're not going to use it

   Revision 1.11  2002/07/17 01:29:10  haase
   GET-CONFIG-FILE (and its C foundation) will now return the file where the configuration file would be loaded from if it existed

   Revision 1.10  2002/06/06 18:55:15  haase
   Clear errno before the dtype init so we don't end up claiming someone else's messes

   Revision 1.9  2002/04/15 18:23:03  haase
   Allow config or env vars to set network timeouts

   Revision 1.8  2002/04/02 21:39:30  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
