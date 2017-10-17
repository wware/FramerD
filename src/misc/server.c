/* C Mode */

/* server.c
   Implements server functions for FramerD
      The core code for DType client/server interactions is here.
   Originally implemented by Ken Haase in othe Machine Understanding Group
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

static char vcid[] = "$Id: server.c,v 1.44 2005/01/14 16:48:47 haase Exp $";


/** Lots of OS specific stuff **/

#define FD_SOURCE 1
#include "framerd/dtypes.h"
#include "framerd/server.h"

#include <time.h>
#include <assert.h>
#include <stdarg.h>

#if WIN32
# include <winsock.h>
# include <fcntl.h>
#define close closesocket
#elif defined(OS2)
# define BSD_SELECT 1
# include <types.h>
# define close soclose
static unsigned int sockets_initialized=0;
#else /* We're under Unix, boys... */
#if HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#if HAVE_SYS_UN_H
# include <sys/un.h>
#endif
#if HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#if HAVE_NETDB_H
# include <netdb.h>
#endif
#if HAVE_FCNTL_H
# include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#endif
#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
# define ioctlsocket ioctl
#endif
#endif

#include <signal.h>

#ifndef DTIO_BLOCK_SIZE
#define DTIO_BLOCK_SIZE 65536*8
#endif


#if ((defined(PF_UNIX)) && (!(defined(PF_LOCAL))))
#define PF_LOCAL PF_UNIX
#endif

#if ((defined(AF_UNIX)) && (!(defined(AF_LOCAL))))
#define AF_LOCAL AF_UNIX
#endif

#ifndef MAX_HOSTNAME
#define MAX_HOSTNAME 512
#endif
#define MAX_BACKLOG 10

static struct FD_CLIENT *clients;
static int client_count=0, highest_client=-1, max_clients=-1, busy_clients=0;

static int *sockets2clients=NULL;
static int sockets2clients_size=0;

static fd_set listen_set;
static int max_socket=0;
static int local_server=0;

#define NULL_FDS ((fd_set *)NULL)

static int testfd(int fd)
{
  fd_set rs; struct timeval tv;
  tv.tv_sec=0; tv.tv_usec=1;
  FD_SET(fd,&rs);
  return select(fd+1,&rs,NULL_FDS,NULL_FDS,&tv);
}

FILE *fd_transaction_log=NULL;
FILE *fd_server_log=NULL;

static fd_exception SocketClosed=_("Socket unexpectedly closed");

#if FD_THREADS_ENABLED
static fd_mutex client_manager_lock;
#endif

#if FD_THREADS_ENABLED
static fd_tld_key client_key;
#else
static struct FD_CLIENT *current_client=NULL;
#endif

/** Dealing with timeouts **/

/** WIN32 fdopen **/

#if WIN32
static FILE *socket2file(long socket_id,char *mode)
{
  if (((socket_id = _open_osfhandle(socket_id,_O_BINARY)) < 0) ||
      ((socket_id = dup(socket_id)) < 0))
    fd_raise_exception("WIN32/OSF socket open failed");
  return fdopen(socket_id,mode);
}
#else
#define socket2file fdopen
#endif

/** Local host access **/

static struct hostent localhost;

/* fd_use_localhost
   Arguments: none
   Returns: void
   
   Sets up servers to listen only as localhost 127.0.0.1. */
void fd_use_localhost()
{
  local_server=1;
}

/* Gets the hostname from a sockaddr */
static char *get_hostname(struct sockaddr_in *client_addr)
{
  struct hostent *host;
  fd_lock_mutex(&_fd_dns_access_lock);
  host=gethostbyaddr((const char *)&(client_addr->sin_addr),4,AF_INET);
  if (host) {
    char *host_name=fd_xmalloc(1+strlen(host->h_name));
    strcpy(host_name,host->h_name);
    fd_unlock_mutex(&_fd_dns_access_lock);
    return host_name;}
  else {
    char *host_name=fd_xmalloc(16);
    unsigned char *addr_bytes=((unsigned char *)&(client_addr->sin_addr));
    sprintf(host_name,"%d.%d.%d.%d",
	   addr_bytes[0],addr_bytes[1],addr_bytes[2],addr_bytes[3]);
    fd_unlock_mutex(&_fd_dns_access_lock);
    return host_name;}
}

/** Reading and Writing from clients **/

FASTOP int write_dtype_to_client(lisp expr,struct FD_CLIENT *s)
{
  struct FD_DBUF d; int result; d.start=fd_xmalloc(DTIO_BLOCK_SIZE); 
  d.ptr=d.start; d.end=d.start+DTIO_BLOCK_SIZE;
  fd_dwrite_dtype(expr,&d); 
#if (FD_USING_STDIO_SOCKETS)
  {
    int written=0, progress=0, limit=d.ptr-d.start;
    while ((written < limit) && (progress=fwrite(d.ptr+progress,1,limit-written,s->out))) {
      written=written+progress;}
    result=written;}
#else
  result=fd_sendall(s->socket,d.start,d.ptr-d.start,0);
#endif
  fd_xfree(d.start);
  return result;
}

FASTOP lisp read_dtype_from_socket(int socket)
{
  struct FD_DBUF d; lisp result;
  d.start=fd_xmalloc(DTIO_BLOCK_SIZE);
  d.ptr=d.start; d.end=d.start+DTIO_BLOCK_SIZE;
  fd_read_from_socket(&d,socket);
  if (d.ptr == d.start) fd_raise_exception(SocketClosed);
  while (!(fd_validate_dtype(d.start,d.ptr))) fd_read_from_socket(&d,socket);
  d.end=d.ptr; d.ptr=d.start;
  result=fd_dread_dtype(&d);
  fd_xfree(d.start);
  return result;
}

/** Work queues **/

#if (FD_THREADS_ENABLED)

struct WORK_QUEUE {
  fd_mutex lock;
  fd_condvar empty, full;
  int n_tasks, max_tasks; void **tasks;};

static struct WORK_QUEUE *make_work_queue(int max_tasks)
{
  struct WORK_QUEUE *wq=fd_malloc(sizeof(struct WORK_QUEUE));
  fd_init_mutex(&(wq->lock));
  pthread_cond_init(&(wq->empty),NULL);
  pthread_cond_init(&(wq->full),NULL);
  wq->n_tasks=0; wq->max_tasks=max_tasks;
  wq->tasks=fd_malloc(sizeof(void *)*max_tasks);
  return wq;
}

static void *pop_task(struct WORK_QUEUE *wq)
{
  void *task;
  pthread_mutex_lock(&(wq->lock));
  while (wq->n_tasks == 0) 
    pthread_cond_wait(&(wq->empty),&(wq->lock));
  task=wq->tasks[0];
  memmove(&(wq->tasks[0]),&(wq->tasks[1]),sizeof(void *)*(wq->n_tasks-1));
  wq->n_tasks--;
  pthread_mutex_unlock(&(wq->lock));
  return task;
}

static void push_task(struct WORK_QUEUE *wq,void *task)
{
  pthread_mutex_lock(&(wq->lock));
  while (wq->n_tasks == wq->max_tasks) 
    pthread_cond_wait(&(wq->full),&(wq->lock));
  wq->tasks[wq->n_tasks++]=task;
  pthread_cond_signal(&(wq->empty));
  pthread_mutex_unlock(&(wq->lock));
}

#endif

/** Server socket utils **/

static void init_server_socket(int socket_id)
{
  unsigned long nonblocking=1;
  /* We set the server socket to be non-blocking */
#if (defined(F_SETFL) && defined(O_NDELAY))
  fcntl(socket_id,F_SETFL,O_NDELAY);
#elif (defined(F_SETFL) && defined(O_NONBLOCK))
  fcntl(socket_id,F_SETFL,O_NONBLOCK);
#elif WIN32
  ioctlsocket(socket_id,FIONBIO,&nonblocking);
#else
  fd_warn("Can't set server socket to non-blocking");
#endif
}

static int open_tcp_server_socket(struct hostent *host,char *addr,int addrlen,int port)
{
  int socket_id=-1, on=0;
  struct sockaddr_in *server_address=
    (struct sockaddr_in *) fd_xmalloc(sizeof(struct sockaddr_in));
  memset(server_address,0,sizeof(struct sockaddr_in)); 
  server_address->sin_port=htons((short)port);
  server_address->sin_family=host->h_addrtype;
  memmove((char *)&(server_address->sin_addr),addr,addrlen);
  socket_id=socket(PF_INET,SOCK_STREAM,0);
  if (socket_id < 0) {
    fd_warn("Can't open socket (%d/%s)",errno,strerror(errno));
    return -1;}
  else if (setsockopt (socket_id, SOL_SOCKET, SO_REUSEADDR, (void *) &on, sizeof(on)) < 0) {
    fd_warn("Can't set SO_REUSEADDR (%d/%s)",errno,strerror(errno));
    return -1;}
  else if ((bind(socket_id,(struct sockaddr *) server_address,sizeof(struct sockaddr_in))) < 0) {
    fd_xfree(server_address);
    fd_warn("Can't bind socket %d to %d.%d.%d.%d:%d (%d/%s)",
	    socket_id,(uchar)addr[0],(uchar)addr[1],(uchar)addr[2],(uchar)addr[3],port,
	    errno,strerror(errno));
    return -1;}
  else if ((listen(socket_id,MAX_BACKLOG)) < 0) {
    fd_xfree(server_address);
    fd_warn("Can't listen on socket %d bound to to %d.%d.%d.%d:%d (%d/%s)",
	    socket_id,(uchar)addr[0],(uchar)addr[1],(uchar)addr[2],(uchar)addr[3],port,
	    errno,strerror(errno));
    return -1;}
  else {
    fd_xfree(server_address);
    init_server_socket(socket_id);
    fd_warn("Opened server socket for %s (%d.%d.%d.%d) at %d",
	    host->h_name,(uchar)addr[0],(uchar)addr[1],(uchar)addr[2],(uchar)addr[3],port);
    if (socket_id > max_socket) max_socket=socket_id;
    return socket_id;}
}

/* fd_open_tcp_server_socket:
     Arguments: a port number (an int)
     Returns: a socket id (an int)

  Opens a server socket listening on the given port.
Catches error conditions and sets things up for automatically
freeing the port on program exit. */
fd_set fd_open_tcp_server_socket(char *hostname,int port)
{
  fd_set sockets;
  struct sockaddr_in *server_address; 
  struct hostent *local_host;
  int socket_id, on=1; char **scan;
  fd_lock_mutex(&_fd_dns_access_lock);
  local_host=gethostbyname(hostname); CLEAR_ERR();
  if (local_host == NULL) {
    fd_unlock_mutex(&_fd_dns_access_lock);
    fd_raise_detailed_exception(_("Can't resolve local host name"),hostname);}
  scan=&(local_host->h_addr_list[0]); FD_ZERO(&sockets);
  while (*scan) {
    int socket_id=
      open_tcp_server_socket(local_host,*scan,local_host->h_length,port);
    if (socket_id>=0) FD_SET(socket_id,&sockets);
    scan++;}
  fd_unlock_mutex(&_fd_dns_access_lock);
  return sockets;
}

#if HAVE_SYS_UN_H
static int open_local_server_socket(char *filename);
EXPORTED
/* fd_open_local_server_socket:
     Arguments: a port number (an int)
     Returns: a socket id (an int)

  Opens a server socket listening on the given port.
Catches error conditions and sets things up for automatically
freeing the port on program exit. */
fd_set fd_open_local_server_socket(char *filename)
{
  fd_set result;
  int socket_id=open_local_server_socket(filename);
  FD_ZERO(&result);
  if (socket_id<0) return result;
  else {
    FD_SET(socket_id,&result); return result;}
}
static int open_local_server_socket(char *filename)
{
  struct sockaddr_un name;
  int socket_id=socket(PF_LOCAL,SOCK_STREAM,0);
  name.sun_family=AF_LOCAL; strcpy(name.sun_path,filename);
  if (socket_id < 0) {perror("Can't open socket"); return -1;}
  else if (bind(socket_id,(struct sockaddr *)&name,sizeof(struct sockaddr_un)) < 0) {
    perror("Can't bind socket"); return -1;}
  else if ((listen(socket_id,MAX_BACKLOG)) < 0) {
    perror("Can't listen on socket"); return -1;}
  else {
    init_server_socket(socket_id);
    if (socket_id > max_socket) max_socket=socket_id;
    return socket_id;}
}
#else
/* fd_open_local_server_socket:
     Arguments: a port number (an int)
     Returns: a socket id (an int)

  Stub signals error under WIN32 (no local domain sockets) */
fd_set fd_open_local_server_socket(char *filename)
{
  fd_raise_exception(_("No local sockets under WIN32"));
}
#endif

/* fd_open_server_socket:
     Arguments: a port number (an int)
     Returns: a socket id (an int)

  Opens a server socket listening on the given port.
Catches error conditions and sets things up for automatically
freeing the port on program exit. */
fd_set fd_open_server_socket(fd_u8char *spec)
{
  fd_u8char *at=strchr(spec,'@');
  if (at == NULL)
    if (local_server)
      return fd_open_tcp_server_socket("localhost",fd_get_portno(spec));
    else {
      char local_hostname[MAX_HOSTNAME+1];
      if (gethostname(local_hostname,MAX_HOSTNAME) < 0) {
	fd_notify(_("Can't determine hostname, using localhost"));
	return fd_open_tcp_server_socket("localhost",fd_get_portno(spec));}
      return fd_open_tcp_server_socket(local_hostname,fd_get_portno(spec));}
  else if (strcmp(at+1,"local") == 0) {
    fd_u8char *tmp=fd_xmalloc(at-spec+1); char *fname; fd_set sockets;
    strncpy(tmp,spec,at-spec); tmp[at-spec]=NUL;
    fname=fd_filename(tmp); fd_xfree(tmp);
    sockets=fd_open_local_server_socket(fname);
    fd_xfree(fname);
    return sockets;}
  else {
    char port_buf[32]; int port_no; char *name; fd_set sockets;
    if (at-spec >= 32) fd_raise_exception("Bad server spec");
    strncpy(port_buf,spec,at-spec); port_buf[at-spec]=NUL;
    port_no=fd_get_portno(port_buf);
    name=fd_make_os_string(at+1);
    sockets=fd_open_tcp_server_socket(name,port_no);
    fd_xfree(name);
    return sockets;}
}

/** Client functions **/

static void grow_sockets2clients(int client_socket);

/* set_client_busy:
   a pointer to a client and a flag (1 or 0) */
static void set_client_busy(int i,int flag)
{
  fd_lock_mutex(&(client_manager_lock));
  if (clients[i].socket>=0) {
    if (clients[i].busy == flag) {}
    else if (flag) busy_clients++; else busy_clients--;
    clients[i].busy=flag;
    if (flag) {FD_CLR(clients[i].socket,&listen_set);}
    else {FD_SET(clients[i].socket,&listen_set);}}
  fd_unlock_mutex(&(client_manager_lock));
}

/* new_client:
   a pointer to a client, initializes some slots and globals */
static int new_client(int client_socket,char *host_name)
{
  int i=0;
  fd_lock_mutex(&(client_manager_lock));
  while (i <= highest_client)
    if (clients[i].socket < 0) break; else i++;
  if (i >= max_clients) {
    fd_unlock_mutex(&(client_manager_lock));
    fd_raise_exception("Out of clients");}
  client_count++; if (i > highest_client) i=++highest_client;
  /* Client is now selected */
  if (client_socket > max_socket) max_socket=client_socket;
  /* Log it, if neccessary. */
  if (fd_server_log) {
    fprintf(fd_server_log,
	    _("client %d (%s, socket %d))\n"),
	    i,host_name,client_socket);
    fflush(fd_server_log);}
  /* Update the inverse table */
  if (client_socket >= sockets2clients_size)
    grow_sockets2clients(client_socket);
  sockets2clients[client_socket]=i;
  /* Initialize the data structure */
  clients[i].socket=client_socket; clients[i].eval_fcn=NULL; 
  clients[i].n_transactions=0; clients[i].data=NULL; clients[i].current_expr=FD_VOID;
  clients[i].id=host_name; clients[i].label=(FD_FALSE);
#if FD_USE_STDIO_WITH_SOCKETS
  clients[i].in=socket2file(client_socket,"r");
  clients[i].out=socket2file(client_socket,"w");
  /* If you have something, ignore dangling errors */
  if (clients[i].in) {FD_CLEAR_ERR();}
#endif
  FD_SET(client_socket,&listen_set); clients[i].busy=0;
  if (i > highest_client) highest_client=i;
  fd_unlock_mutex(&(client_manager_lock));
  return i;
}

static void grow_sockets2clients(int client_socket)
{
  int i=sockets2clients_size;
  int new_size=((client_socket/128)+1)*128;
  if (sockets2clients) {
    sockets2clients=fd_realloc
      (sockets2clients,sizeof(int)*new_size,
       sizeof(int)*sockets2clients_size);
    sockets2clients_size=new_size;
    while (i < sockets2clients_size) sockets2clients[i++]=-1;}
  else {
    sockets2clients=fd_malloc(sizeof(int)*new_size);
    sockets2clients_size=new_size;
    while (i < sockets2clients_size) sockets2clients[i++]=-1;}
}

/* fd_label_client:
   Arguments: a lisp pointer
   Returns: 1 if there is a current client
   Labels the client with the pointer */
int fd_label_client(lisp label)
{
  fd_client cl=fd_current_client();
  if (cl) {cl->label=incref(label); return 1;}
  else return 0;
}

static int get_free_client()
{
  int i=0;
}

static void close_client(int i)
{
  fd_lock_mutex(&(client_manager_lock));
  fd_lock_mutex(&(clients[i].lock));
  if (clients[i].socket < 0) {
    fd_warn("Client %d already closed!",i);
    fd_unlock_mutex(&(clients[i].lock));
    fd_unlock_mutex(&(client_manager_lock));
    return;}
  if (fd_server_log) 
    fprintf(fd_server_log,
	    _("\n(Closing #%d: (%s, socket %d) after %d transactions)\n"),
	    i,clients[i].id,clients[i].socket,clients[i].n_transactions/2);
  if (clients[i].close_fcn)
    clients[i].close_fcn(&clients[i]);
#if FD_USE_STDIO_WITH_SOCKETS
  fclose(clients[i].in);
  fclose(clients[i].out); 
  clients[i].out=NULL; clients[i].in=NULL; 
  CLEAR_ERR(); /* Closing both in and out may set errno */
#else
  close(clients[i].socket); 
#endif
  /* Clear inverse table */
  sockets2clients[clients[i].socket]=-1;
  FD_CLR((unsigned int)clients[i].socket,&listen_set);
  if (clients[i].busy) busy_clients--;
  clients[i].socket=-1; fd_xfree(clients[i].id); clients[i].id=NULL;
  if (i == highest_client) highest_client--; client_count--;
  fd_unlock_mutex(&(clients[i].lock));
  fd_unlock_mutex(&(client_manager_lock));
}

/** Server (select) loop **/

static int (*validate_fcn)(char *hostname)=NULL;

/* fd_set_client_validator:
    Arguments: a function which takes a string and returns an int
    Returns: void

  This sets the function used to determine whether a connection is allowed.
The validator function takes a hostname string and returns 1 if the client
host is allowed to connect to this server. */
void fd_set_client_validator(int (*vfcn)(char *hostname))
{
  validate_fcn=vfcn;
}

static int validate_client(char *hostname)
{
  if (validate_fcn) return validate_fcn(hostname);
  else return 1;
}

/* fd_current_client:
    Arguments: Returns the current client of this server

  This sets the function used to determine whether a connection is allowed.
The validator function takes a hostname string and returns 1 if the client
host is allowed to connect to this server. */
fd_client fd_current_client()
{
#if FD_THREADS_ENABLED
  return fd_tld_get(client_key);
#else
  return current_client;
#endif
}

/* Client request processing */

static void start_transaction(int i)
{
#if (FD_THREADS_ENABLED)
  pthread_mutex_lock(&(clients[i].lock));
  fd_tld_set(client_key,&(clients[i]));    
#else
  current_client=&(clients[i]);
#endif
}

static void cleanup_transaction(int i)
{
#if (FD_THREADS_ENABLED)
  fd_tld_set(client_key,NULL);    
  pthread_mutex_unlock(&(clients[i].lock));
#else
  current_client=NULL;
#endif
}

static fd_lisp read_request_from_client(int i)
{
  fd_lisp expr=FD_VOID;
#if FD_USE_STDIO_WITH_SOCKETS
  /* Probe the client */
  int c=getc(clients[i].in);
  if (fd_transaction_log) {
    fprintf(fd_transaction_log," [%d] ",c);
    fflush(fd_transaction_log);}
  /* If not at EOF, unget and continue */
  if (c != EOF) {
    ungetc(c,clients[i].in);
    expr=fd_fread_dtype(clients[i].in);}
  /* If at EOF, signal socket closed */
  else if (feof(clients[i].in)) fd_raise_exception(SocketClosed);
  /* Otherwise, you just don't have anything to do, so return 0 */
#if WIN32 /* feof doesn't seem to work under WIN32 right now */
  else fd_raise_exception(SocketClosed);
#else
  else {FD_CLEAR_ERR(); expr=FD_VOID;}
#endif
#else
  expr=read_dtype_from_socket(clients[i].socket);
#endif
  return expr;
}

/* This does the actual evaluation and logging of the client request */
static int handle_client_request(int i)
{
  int standard_return=1;
  fd_lisp expr, result;
  if (fd_transaction_log) {
    fprintf(fd_transaction_log,_("\n[%d:%d:%s] Request: "),
	    clients[i].socket,getpid(),clients[i].id);
    fflush(fd_transaction_log);}
  expr=read_request_from_client(i);
  fd_decref(clients[i].current_expr);
  clients[i].current_expr=expr;
  clients[i].n_transactions++;
  if (!(FD_VOIDP(expr)) && (fd_transaction_log)) {
    fd_print_lisp(expr,fd_transaction_log);
    fflush(fd_transaction_log);}
  result=
    clients[i].eval_fcn(expr,&standard_return,&(clients[i]));
  if (standard_return) {
    if (fd_transaction_log) {
      fprintf(fd_transaction_log,_("\n[%s] Reply:   "),clients[i].id);
      fd_print_lisp(result,fd_transaction_log);
      fprintf(fd_transaction_log,"\n");
      fflush(fd_transaction_log);}
    write_dtype_to_client(result,&(clients[i]));
    fflush(clients[i].out);
    clients[i].n_transactions++;
    set_client_busy(i,0);
    FD_SET(clients[i].socket,&listen_set);
    fd_decref(clients[i].current_expr);
    fd_decref(result);
    clients[i].current_expr=FD_VOID;
    return 1;}
  else return 0;
}

/* This handles a client request with error handling */
static void serve_client_request(int i)
{
  WITH_HANDLING {
    int standard_return=1; int c;
    start_transaction(i);
    if (clients[i].socket < 0) cleanup_transaction(i);
    else if (handle_client_request(i)) cleanup_transaction(i);}
  ON_EXCEPTION {
    FILE *whine; if (fd_server_log)
      whine=fd_server_log; else whine=stderr;
    set_client_busy(i,0);
    fd_decref(clients[i].current_expr);
    clients[i].current_expr=FD_VOID;
    cleanup_transaction(i);
    if (fd_theException() != SocketClosed)
      fd_fprintf(whine,
		 _("(Unexpected error on client %d (%s, socket %d):\n   %s (%s) %q)\n"),
		 i,clients[i].id,clients[i].socket,fd_theException(),
		 fd_exception_details(),fd_exception_object());
    fflush(whine);
    close_client(i);
    if (fd_theException() != fd_Out_Of_Memory) {fd_clear_exception();}
    else fd_reraise();}
  END_HANDLING;
}

/* Server loops */

#if (FD_THREADS_ENABLED)
static void *serve_client_loop(void *thread_arg)
{
  struct WORK_QUEUE *wq=thread_arg;
  int client_id;
  while (1) {
    int client_id=(long)pop_task(wq);
    if (clients[client_id].socket >= 0) {
      serve_client_request(client_id);
      if (clients[client_id].socket >= 0) {
	set_client_busy(client_id,0);}}}
}

static pthread_t *start_thread_pool(struct WORK_QUEUE *wq,int n)
{
  int i=0;
  pthread_t *thread_pool=fd_malloc(sizeof(pthread_t)*n);
  while (i < n) {
    pthread_create(&thread_pool[i],
		   pthread_attr_default,
		   serve_client_loop,(void *)wq);
    i++;}
  return thread_pool;
}
#endif

#if FD_THREADS_ENABLED
#if FD_USE_STDIO_WITH_SOCKETS
static void check_client(struct WORK_QUEUE *wq,long i)
{
  if (clients[i].socket < 0) return;
  else if (clients[i].busy) return;
  else if (feof(clients[i].in)) close_client(i);
  else if (testfd(clients[i].socket) == 0) {
    fprintf(stderr,"Erroneous select for socket %d\n",i);
    return;}
  else {
#if FD_THREADS_ENABLED
    set_client_busy(i,1);
    push_task(wq,(void *)i);
#else
    serve_client_request(i);
#endif
  }}
#else
static void check_client(struct WORK_QUEUE *wq,long i)
{
  if (clients[i].socket < 0) return;
  else if (clients[i].busy) return;
#if FD_THREADS_ENABLED
  set_client_busy(i,1);
  push_task(wq,(void *)i);
#else
  serve_client_request(i);
#endif
}
#endif
#endif

/** Servers, threaded and unthreaded **/

/* fd_start_server
    Arguments: a socket (an int) and an eval function.
    Returns: void
 The eval function takes a lisp object, a pointer to an integer, and
a client data structure.  The pointer is used to tell whether or not the
value returned by the procedure should be transmitted or discarded.  This
is for the case, where a handler might output an immediate result to the client
and do some subsequent computation.  */
void fd_start_server
  (fd_set server_sockets,fd_lisp (*server_eval)(fd_lisp,int *,fd_client))
{
#if FD_THREADS_ENABLED
  struct WORK_QUEUE *wq=
    make_work_queue(fd_int_getenv("SERVER_QUEUE",100));
  pthread_t *thread_pool=
    start_thread_pool(wq,fd_int_getenv("SERVER_THREADS",10));
#endif
  if (fd_server_log) {
    fprintf(fd_server_log,_("(Starting up server.."));
    fflush(fd_server_log);}
  client_count=0;
  listen_set=server_sockets;
  if (fd_server_log) {
    int i=0; while (i < max_socket)
      if (FD_ISSET(i,&server_sockets))
	fprintf(fd_server_log,_("..(server listening on socket %d)..)\n"),
		i++);
      else i++;
    fflush(fd_server_log);}
  /* Loop indefinitely, processing connections */
  while (1) {
    int i=0;
    fd_set listening;
    struct timeval _timeout, *timeout;
    _timeout.tv_sec=0; _timeout.tv_usec=500;
    fd_lock_mutex(&(client_manager_lock));
    listening=listen_set; timeout=((busy_clients) ? (&_timeout) : (NULL));
    fd_unlock_mutex(&(client_manager_lock));
    /* Wait for activity on one of your open sockets */
    while (select(max_socket+1,&listening,NULL_FDS,NULL_FDS,timeout) == 0) {
      fd_lock_mutex(&(client_manager_lock));
      listening=listen_set; timeout=((busy_clients) ? (&_timeout) : (NULL));
      fd_unlock_mutex(&(client_manager_lock));
      _timeout.tv_usec=500;}
    /* If the activity is on the server socket, open a new socket */
    while (i <= max_socket)
      if (!(FD_ISSET(i,&listening))) i++;
      else if (FD_ISSET(i,&server_sockets)) {
	struct sockaddr_in client_addr;
	int client_addr_length, client_socket;
	client_addr_length=sizeof(client_addr);
	client_socket=
	  accept(i,(struct sockaddr *) &client_addr,&client_addr_length);
	if ((client_socket >= 0) && (fd_server_log)) {
	  fprintf(fd_server_log,_("\n(New connection request..."));
	  fflush(fd_server_log);}
	if (client_socket < 0) 
	  /* If socket can't be opened, ... (what is this doing?) */
#ifdef EWOULDBLOCK
	  if (errno == EWOULDBLOCK) FD_CLEAR_ERR(); else
#endif
	    if (errno) perror("accept");
	    else {}
	else {
	  char *host_name=get_hostname(&client_addr);
	  if (validate_client(host_name)) {
	    int client_id=new_client(client_socket,host_name);
	    clients[client_id].eval_fcn=server_eval;}
	  else {
	    unsigned char reject[512];
	    reject[0]=dt_error;
	    reject[1]=dt_string;
	    reject[2]=reject[3]=reject[4]=0;
	    reject[5]=strlen("Access forbidden");
	    strcpy(reject+6,"Access forbidden");
	    send(client_socket,reject,strlen("Access forbidden")+6,0);
	    fd_warn(_("Refused access for %s"),host_name);
	    close(client_socket);}}
      i++;}
      else {
	int client_id=sockets2clients[i];
	if (client_id < 0)
	  fd_warn("Clientless socket %d!!!",i);
	else {
#if FD_THREADS_ENABLED
	    check_client(wq,client_id);
#else
	    /* We won't pay attention to the return value and just keep
	       scanning if this particular request could not get
	       processed. */
	    serve_client_request(client_id);
#endif
	    }
	i++;}}
}

/** Server utility functions **/

/* fd_set_server_log
     Arguments: two FILE pointers
     Returns: void

  Sets the logs used for recording connections and transactions.
  If either is NULL, those are not recorded. */
void fd_set_server_log(FILE *slog,FILE *tlog)
{
  if (slog) fd_server_log=slog;
  if (tlog) fd_transaction_log=tlog;
}

/* fd_list_clients
     Arguments: none
     Returns: a list of currently connected clients
*/
lisp fd_list_clients()
{
  lisp answer=(FD_EMPTY_CHOICE);
  int i=0;
  fd_lock_mutex(&client_manager_lock);
  while (i <= highest_client) {
    if (clients[i].socket >= 0) {
      lisp vec=fd_make_vector(5);
      FD_VECTOR_SET(vec,0,fd_make_string(clients[i].id));
      FD_VECTOR_SET(vec,1,fd_incref(clients[i].label));
      FD_VECTOR_SET(vec,2,FD_LISPFIX(clients[i].n_transactions/2));
      FD_VECTOR_SET(vec,3,FD_LISPFIX(clients[i].socket));
      if (clients[i].busy) {
	FD_VECTOR_SET(vec,3,fd_incref(clients[i].current_expr));}
      else {FD_VECTOR_SET(vec,3,FD_FALSE);}
      FD_ADD_TO_CHOICE(answer,vec);}
    i++;}
  fd_unlock_mutex(&client_manager_lock);
  return answer;
}

EXPORTED void fd_initialize_server_c()
{
  int i=0;
  char *addr1=fd_xmalloc(4);
  char **addr_list=fd_xmalloc(sizeof(char *));
  addr1[0]=127; addr1[1]=0; addr1[2]=0; addr1[3]=1;
  addr_list[0]=addr1;
  localhost.h_name="localhost";
  localhost.h_addrtype=AF_INET;
  localhost.h_length=4;
  localhost.h_addr_list=addr_list;
#if FD_THREADS_ENABLED
  fd_new_tld_key(&client_key,NULL);
#endif
  max_clients=fd_int_getenv("SERVER_CLIENTS",500);
  clients=fd_malloc(sizeof(struct FD_CLIENT)*max_clients);
#if FD_THREADS_ENABLED
  fd_init_mutex(&client_manager_lock);
  i=0; while (i < max_clients) {
    fd_init_mutex(&(clients[i].lock));
    i++;}
#endif

}


/* File specific stuff */

/* The CVS log for this file
   $Log: server.c,v $
   Revision 1.44  2005/01/14 16:48:47  haase
   Updated copyrights to 2005

   Revision 1.43  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.42  2004/07/19 16:57:13  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.41  2004/03/30 08:10:11  haase
   Support for using schemas

   Revision 1.40  2004/03/12 15:06:19  haase
   Fixed mutex locking error with client manager for fdserver

   Revision 1.39  2004/01/09 17:42:49  haase
   Rename client to fd_client for server code

   Revision 1.38  2003/12/25 02:43:39  haase
   Added current expr to list-clients

   Revision 1.37  2003/12/06 19:46:46  haase
   Fixes to datestamp/buildstamp handling

   Revision 1.36  2003/12/02 15:02:36  haase
   Added mutex protection for malloc'd environment reference counting

   Revision 1.35  2003/11/30 21:24:28  haase
   Fixed dangling lock

   Revision 1.34  2003/11/29 12:08:37  haase
   General cleanup of server library, includingfixes of busy waiting, thread conflicts, and sloppy select calls

   Revision 1.33  2003/11/28 14:39:43  haase
   Reduced busy waiting for the client manager in fdserver

   Revision 1.32  2003/11/25 12:48:56  haase
   Fixed wrapped environment pointers to refcount and free their environments

   Revision 1.31  2003/10/06 11:06:16  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.30  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.29.2.4  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.29.2.3  2003/02/16 21:41:23  haase
   Minor patches to ralphc patches

   Revision 1.29.2.2  2003/01/26 20:44:02  haase
   Misc. fixes especially some GC

   Revision 1.29.2.1  2002/08/09 19:09:19  haase
   Fixed a variable initialization

   Revision 1.29  2002/05/27 18:16:34  haase
   Added abstraction layer for thread-local data

   Revision 1.28  2002/05/20 16:39:41  haase
   Fixed typo in server reorgnization

   Revision 1.27  2002/05/20 15:20:29  haase
   Fixes to networking for Solaris

   Revision 1.26  2002/05/20 00:28:02  haase
   Fix malformed fwrite calls

   Revision 1.25  2002/05/19 13:19:44  haase
   Fix string leaks

   Revision 1.24  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.23  2002/04/28 20:37:52  haase
   Exported many network functions from libdtypes (timed connect, recv, send, etc) and removed the duplicate functionality from servers.c

   Revision 1.22  2002/04/27 17:47:18  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.21  2002/04/27 02:48:11  haase
   Added mutexes protecting DNS accesses

   Revision 1.20  2002/04/17 07:53:48  haase
   Made transaction log show socket of client

   Revision 1.19  2002/04/16 16:14:39  haase
   Fixed some inconsistent returns

   Revision 1.18  2002/04/16 13:16:45  haase
   Renamed FD_USE_STDIO_SOCKETS to clearer FD_USE_STDIO_WITH_SOCKETS

   Revision 1.17  2002/04/16 12:55:00  haase
   Fixes to non STDIO dteval based on timed_recv

   Revision 1.16  2002/04/13 15:12:37  haase
   Don't serve clients which have been closed

   Revision 1.15  2002/04/04 01:58:24  haase
   Rearranged use of stdio and raw sockets to fix some Solaris/Darwin problems

   Revision 1.14  2002/04/03 01:33:40  haase
   Fix noop version of fd_open_local_server_socket

   Revision 1.13  2002/04/02 21:39:33  haase
   Added log and emacs init entries to C source files

*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
