/* Mode: C */

/* analyze-index.c

   A command line utility for analyzing FramerD index files
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

static char vcid[] = "$Id: analyze-index.c,v 1.22 2006/07/08 23:24:33 haase Exp $";

#include <framerd/indextools.h>

static unsigned int lowhist[10]={0,0,0,0,0,0,0,0,0,0};
static int max_values=0, total_values=0;

struct RESULTS { int n_values, n_keys; };

static int sort_results(const void *v1,const void *v2)
{
  const struct RESULTS *r1=v1, *r2=v2;
  if (r1->n_values < r2->n_values) return -1;
  else if (r1->n_values > r2->n_values) return 1;
  else return 0;
}

void show_usage()
{
  fprintf(stderr,"       analyze-index [-d] <index> [histogram]\n");
  exit(1);
}

static char *make_isotime(char *buf,int size,time_t tick)
{
  struct tm tptr;
  fd_breakup_time(&tptr,tick,0);
  sprintf(buf,"%4d-%02d-%02dT%02d:%02d:%02d",
	  ((tptr.tm_year > 100) ? (tptr.tm_year) : (tptr.tm_year+1900)),
	  tptr.tm_mon+1,
	  tptr.tm_mday,
	  tptr.tm_hour,tptr.tm_min,tptr.tm_sec);
  return buf;
}

int locate_assoc_for_index(int k,struct FD_ASSOC *assocs,int n)
{
  int i=0; while (i < n)
    if (assocs[i].index == k) return i;
    else i++;
  return -1;
}

int
main(int argc, char *argv[])
{

  FILE *in;
  struct FD_ASSOC *assocs;
  struct FD_FILE_INDEX *ix;
  FILE *histogram=NULL;
  int i=0, n_slots=0, n_keys=0, hashv=0, n_misses=0, chain_max=0, chain_sum=0;
  time_t creation_time, repack_time, change_time;
  int major_version; off_t minor_version;
  int note_collisions=fd_int_getenv("NOTE_COLLISIONS",25);
  char timebuf[128];
  fd_lisp metadata;
  int dump = 0;
  
  if (argc >= 2 && strcmp(argv[1], "-d") == 0) {
    dump = 1; argc--; argv++;}
  if (argc == 3) {
    histogram=fd_fopen(argv[2],"w");
    if (histogram == NULL) {
      perror(_("Can't write histogram file")); exit(1);}}
  else if (argc != 2)
    show_usage();
      
  fd_initialize_framerd();

  if (fd_file_existsp(argv[1]) == 0) {
    fd_warn("The file %s does not exist",argv[1]);
    exit(1);}

  ix=(struct FD_FILE_INDEX *)fd_open_index(argv[1]);
  n_slots=ix->size; fd_close_index((fd_index)ix);

  in=fd_fopen(argv[1],"rb");

  metadata=fd_read_file_index_metadata
    (in,&major_version,&minor_version,&creation_time,&repack_time,&change_time);
  if (creation_time == 0)
    fprintf(stderr,_("The file index %s is prehistoric\n"),argv[1]);
  else fprintf(stderr,_("The file index %s was created at %s\n"),
	       argv[1],make_isotime(timebuf,128,creation_time));
  
  if (repack_time == 0)
    fprintf(stderr,_("The file index %s has no repack time information\n"),argv[1]);
  else fprintf(stderr,_("The file index %s was last repacked at %s\n"),
	       argv[1],make_isotime(timebuf,128,repack_time));

  if (change_time == 0)
    fprintf(stderr,_("The file index %s has no useful modification time information\n"),argv[1]);
  else fprintf(stderr,_("The file index %s was last modified at %s\n"),
	       argv[1],make_isotime(timebuf,128,change_time));

  fprintf(stderr,_("The file index %s has version id %d:%d\n"),
	  argv[1],major_version,minor_version);

  assocs=fd_read_assocs_from_index(in,&n_keys,NULL,&hashv,0,0,-1,argv[1],0);
  if (hashv)
    fprintf(stderr,_("The index uses the v2 hash function\n"));
  else fprintf(stderr,_("The index uses the v1 hash function\n"));
  fprintf(stderr,_("The index %s stores %d keys in %d slots\n"),
	  argv[1],n_keys,n_slots);

  if (dump) {
    for (i = 0; i < n_keys; i++) {
      printf("index: %u\n", assocs[i].index);
      fd_pprint_lisp(assocs[i].key, stdout, 80);
      fputc('\n', stdout);
      fd_pprint_lisp(assocs[i].values, stdout, 80);
      fputc('\n', stdout); } }

  i=0; while (i < n_keys) {
    unsigned int chain_length=0;
    unsigned int probe=assocs[i].hash%n_slots;
    unsigned int chain_width=((assocs[i].hash)%(n_slots-2))+1;
    unsigned int size=assocs[i].n_values;
    while (probe != assocs[i].index) {
      probe=(probe+chain_width)%n_slots; chain_length++;}
    if (chain_length > chain_max) chain_max=chain_length;
    if ((note_collisions) && (chain_length > note_collisions))
      fd_fprintf(stderr,";; Chain length of %d (hash=%d) for %q\n",
		 chain_length,assocs[i].hash,assocs[i].key);
    chain_sum=chain_sum+chain_length;
    if (chain_length) n_misses++;
    if (size > max_values) max_values=size;
    total_values=total_values+size;
    if (size < 10) lowhist[size]++;
    i++;}
  if (n_misses) {
    fprintf(stderr,
	    _("Of the %d keys; %d (%4.2f%%) are direct hits\n"),
	    n_keys, n_keys-n_misses,
	    (((double)(n_keys-n_misses))*100.0)/((double)n_keys));
    fprintf(stderr,
	    _("The %d misses average chains of %4.2f elements (max=%d)\n"),
	    n_misses,(((double)chain_sum+n_misses)/((double)n_misses)),
	    chain_max);
    fprintf(stderr,
	    _("Altogether, the average number of probes is %4.4f\n"),
	    (((double)chain_sum+n_keys)/((double)n_keys)));}
  else fprintf(stderr,
	       _("The index contains %d keys, all of which are direct hits\n"),
	       n_keys);
  if (n_keys)
    fprintf(stderr,
	    _("These keys refer to %d values, making %g references on average\n"),
	    total_values,(((double)total_values)/n_keys));
  fprintf(stderr,
	  _("The most values associated with a key is %d\n"),max_values);
  i=0; while (i < 10) {
    fprintf(stderr,
	    _("  %f%% (%d) of the keys have %d values;\n"),
	    (100.0*(double)lowhist[i])/n_keys,lowhist[i],i);
    i++;}
  if (histogram) {
    int j=0;
    struct FD_HASHTABLE table; fd_pair *elements;
    struct RESULTS *results;
    fd_init_hashtable(&table,n_keys/8);
    i=0; while (i < n_keys) {
      fd_lisp nv=FD_LISPFIX(assocs[i].n_values);
      fd_hashtable_increment(&table,nv,1);
      i++;}
    elements=table.table;
    results=fd_malloc(sizeof(struct RESULTS)*table.n_keys);
    i=0; j=0; while (i < table.n_slots) {
      if (elements[i]) {
	results[j].n_keys=FD_FIXLISP(elements[i]->cdr);
	results[j].n_values=FD_FIXLISP(elements[i]->car); j++;}
      i++;}
    qsort(results,table.n_keys,sizeof(struct RESULTS),sort_results);
    i=0; while (i < table.n_keys) {
      fprintf(histogram,"%d\t%d\n",results[i].n_values,results[i].n_keys);
      i++;}
    fd_fclose(histogram);}
  return 0;
}


/* File specific stuff */

/* The CVS log for this file
   $Log: analyze-index.c,v $
   Revision 1.22  2006/07/08 23:24:33  haase
   Verbosity controls for index maintenance functions

   Revision 1.21  2006/06/27 13:11:39  haase
   Fixed bug introduced in adding the new experimental hash function

   Revision 1.20  2005/01/14 16:48:45  haase
   Updated copyrights to 2005

   Revision 1.19  2004/11/10 17:21:44  haase
   Added memory mapped offsets to file indices

   Revision 1.18  2004/07/20 17:54:03  haase
   Debugging support

   Revision 1.17  2004/07/20 09:16:12  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.16  2004/07/16 14:09:26  haase
   more off_t fixes

   Revision 1.15  2004/04/27 17:35:39  haase
   Made analyze-index report hash function version

   Revision 1.14  2004/03/14 00:40:02  haase
   Added collission counting to analyze-index

   Revision 1.13  2004/03/12 21:18:50  haase
   Extend new indices to indextools etc.

   Revision 1.12  2003/08/27 10:53:29  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.11.2.3  2003/08/06 18:41:06  haase
   Updated copyright notices to 2003

   Revision 1.11.2.2  2003/01/26 20:40:27  haase
   Misc. fixes

   Revision 1.11.2.1  2002/09/26 02:09:59  haase
   Add -d flag to analyze-index

   Revision 1.11  2002/06/29 01:25:58  haase
   Made dbtest relocatable

   Revision 1.10  2002/06/03 21:51:21  haase
   Progress reports now provide more context

   Revision 1.9  2002/04/22 14:23:08  haase
   Added extended metadata to file pools and indices

   Revision 1.8  2002/04/10 03:02:10  haase
   Added version information to file pools and indices

   Revision 1.7  2002/04/03 01:33:09  haase
   Moved indextools out of FD_SOURCE core

   Revision 1.6  2002/04/02 21:39:32  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
