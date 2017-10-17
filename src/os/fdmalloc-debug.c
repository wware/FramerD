void *fd_malloc_debug(size_t bytes,char *filename,int lineno)
{
  void *malloc_result; int malloc_size;
  if (bytes > FD_MAX_MALLOC) fd_raise_exception(fd_HugeMalloc);
  else if (bytes == 0) return NULL;
  else malloc_size=roundup_size(bytes);
  if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE))
    malloc_result=huge_malloc(malloc_size);
  else malloc_result=malloc(malloc_size);
  if (malloc_result) {
    if (memlog)
      fprintf(memlog,"%08lx\t%08lx\t%d\t%ld\tfd_malloc\t%s\t%d\n",
	      malloc_result,malloc_result+malloc_size,
	      getpid(),elapsed_time(),filename,lineno);
    malloc_adjust(bytes);
    return malloc_result;}
  /* else */ 
  else {
    perror("fd_malloc");
    fd_raise_exception(fd_Out_Of_Memory);}
  /* Should never be reached */
  return NULL;
}

void *fd_xmalloc_debug(size_t bytes,char *filename,int lineno)
{
  void *malloc_result; int malloc_size;
  if (bytes == 0) return NULL;
  else malloc_size=roundup_size(bytes);
  malloc_result=malloc(malloc_size);
  if (malloc_result) {
    if (memlog)
      fprintf(memlog,"%08lx\t%08lx\t%d\t%ld\tfd_xmalloc\t%s\t%d\n",
	      malloc_result,malloc_result+malloc_size,
	      getpid(),elapsed_time(),filename,lineno);
    return malloc_result;}
  else {
    perror("fd_xmalloc");
    fd_raise_exception(fd_Out_Of_Memory);
    /* Should never be reached */
    return NULL;}
}

void *fd_realloc_debug(void *ptr,size_t new_size,size_t old_size,char *filename,int lineno)
{
  size_t malloc_size=roundup_size(new_size);
  size_t old_malloc_size=roundup_size(old_size);
  void *nptr;
  if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE)) {
    /* There may be a better way to do this */
    nptr=huge_malloc(malloc_size);
    if (nptr) memcpy(nptr,ptr,old_size);
    if (old_malloc_size >= HUGE_MALLOC_SIZE)
      huge_free(ptr,old_malloc_size);
    else free(ptr);}
  else nptr=realloc(ptr,malloc_size);
  if (nptr) {
    if (memlog) {
      fprintf(memlog,"%lx\t%lx\t%d\t%ld\tfd_realloc (free)\t%s\t%d\n",
	      ptr,ptr+old_size,
	      getpid(),elapsed_time(),filename,lineno);
      fprintf(memlog,"%lx\t%lx\t%d\t%ld\tfd_realloc (new)\t%s\t%d\n",
	      nptr,nptr+malloc_size,
	      getpid(),elapsed_time(),filename,lineno);}
    malloc_adjust(new_size-old_size);
    return nptr;}
  else fd_raise_exception(fd_ReallocFailed);
}

void *fd_xrealloc_debug(void *oldptr,size_t bytes,char *filename,int lineno)
{
  int new_size=roundup_size(bytes);
  void *malloc_result=realloc(oldptr,new_size);
  if (malloc_result) {
    if (memlog) {
      fprintf(memlog,"%08lx\t\t%d\t%ld\tfd_xrealloc (free)\t%s\t%d\n",
	      oldptr,
	      getpid(),elapsed_time(),filename,lineno);
      fprintf(memlog,"%08lx\t%08lx\t%d\t%ld\tfd_xrealloc (new)\t%s\t%d\n",
	      malloc_result,malloc_result+new_size,
	      getpid(),elapsed_time(),filename,lineno);}
    return malloc_result;}
  /* else */ 
  else {
    perror("fd_xremalloc");
    fd_raise_exception(fd_Out_Of_Memory);}
  /* Should never be reached */
  return NULL;
}

void fd_free_debug(void *ptr,size_t bytes,char *filename,int lineno)
{
  if (ptr == NULL)
    if (bytes == 0) return;
    else fd_raise_exception("Freeing NULL pointer");
  else {
    int malloc_size=roundup_size(bytes);
    if ((HUGE_MALLOC_SIZE) && (malloc_size >= HUGE_MALLOC_SIZE))
      huge_free(ptr,malloc_size);
    else free(ptr);
    if (memlog)
      fprintf(memlog,"%08lx\t%08lx\t%d\t%ld\tfd_free\t%s\t%d\n",
	      ptr,ptr+malloc_size,
	      getpid(),elapsed_time(),filename,lineno);
    malloc_adjust(-bytes);}
}

DTYPES_EXPORT
void fd_xfree_debug(void *ptr,char *filename,int lineno)
{
  if (memlog)
    fprintf(memlog,"%08lx\t\t%d\t%ld\tfd_xfree\t%s\t%d\n",
	    ptr,
	    getpid(),elapsed_time(),filename,lineno);
  if (ptr) free(ptr);
}



DTYPES_EXPORT
/* fd_qmalloc:
     Arguments: number of bytes
     Returns: allocated memory

  This maintains a free list for certain memory sizes and
   allocates them blocks at a time.
*/
void *_fd_qmalloc_debug(size_t bytes)
{
  return fd_qmalloc(bytes);
}

DTYPES_EXPORT
/* _fd_qmalloc_cons:
     Arguments: number of bytes
     Returns: allocated memory

  This also initializes the reference count, assuming
that the result will be a struct whose first int field
is the reference count.
*/
void *_fd_qmalloc_cons_debug(size_t bytes)
{
  return fd_qmalloc_cons(bytes);
}

DTYPES_EXPORT
/* fd_qfree:
     Arguments: a pointer and a number of bytes
     Returns: void

  This frees a cons allocated by fd_qmalloc which tries to
do free list maintainance.
*/
void _fd_qfree_debug(void *p,size_t bytes,char *file,int line)
{
  fd_qfree(p,bytes);
}

