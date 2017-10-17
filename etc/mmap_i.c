typedef unsigned int size_t;
typedef unsigned int off_t;

void *mmap(void *start,size_t len,int prot,int flags,int fd,off_t offset)
{
  void *retp=mmap(start,len,prot,flags,fd,offset);
  if (retp) iic_alloc(retp,len);
  return retp;
}

int munmap(void *start,size_t len)
{
  int result=munmap(start,len);
  iic_unalloc(start);
  return result;
}
