/* C Mode */

/* network.c
   Implements network utility functions for FramerD
      The core code for DType client/server interactions is here.
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

static char vcid[] = "$Id: network.c,v 1.65 2005/01/14 16:48:49 haase Exp $";

/** Lots of OS specific stuff **/
/** Initial declarations **/
/** Support for Windows sockets **/
/** Network database functions **/
/** Connecting TO servers **/
/** Restarting connections **/
/** Making and closing connections **/
/** Non STDIO access to sockets **/
/** dtype_eval for stdio sockets **/
/** dtype eval for non stdio sockets **/
/** fd_dtype_eval **/
/** TT (Touch-tone) encoding **/
/** Server socket utils **/
/** Server (select) loop **/
/** Threaded servers **/
/** Server utility functions **/
/** Sending SMTP mail **/
/** Accessing URLS **/
/** Posting to URLs **/
/** Initialization **/

#include "dtypes.h"
#include "signal.h"


/** Lots of OS specific stuff **/

#include <time.h>
#include <assert.h>
#include <stdarg.h>

#if defined(WIN32)
# include <winsock.h>
# include <fcntl.h>
#define MSG_WAITALL 0
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
#ifndef SUN_LEN
#define SUN_LEN(x) sizeof(*x)
#endif
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
#if HAVE_SYS_FILIO_H
# include <sys/filio.h>
#endif
#endif

#if ((defined(PF_UNIX)) && (!(defined(PF_LOCAL))))
#define PF_LOCAL PF_UNIX
#endif

#if ((defined(AF_UNIX)) && (!(defined(AF_LOCAL))))
#define AF_LOCAL AF_UNIX
#endif

#if FD_THREADS_ENABLED
static fd_mutex server_lookup_lock;
fd_mutex _fd_dns_access_lock;
#endif

fd_exception
  fd_SocketClosed=_("Socket closed"),
  fd_UnknownHost=_("Unknown host"),
  fd_BadServerSpec=_("Bad server specification"),
  fd_ConnectionFailed=_("Couldn't open connection"),
  fd_ConnectionTimeOut=_("Connection timed out"),
  fd_UnhandledURL=_("Can't handle URL type"),
  UrlPathTooLong=_("UrlPathTooLong");

static fd_exception BadChunk=_("Bad HTTP chunk spec");

static int connect_timeout=16, recv_timeout=8, send_timeout=8;

static char *agent_id="FramerD";

/** Initial declarations **/

#ifndef DTIO_BLOCK_SIZE
#define DTIO_BLOCK_SIZE 65536*8
#endif

#define MAX_SERVERS 100
#define MAX_HOSTNAME 100

static struct FD_SERVER servers[MAX_SERVERS];
static int server_count=0;

fd_exception fd_NetworkInitFailed=_("Couldn't initialize networking"),
  fd_NoSocket=_("Couldn't open socket for connection"),
  fd_NoConnection=_("Couldn't connect socket across network");

fd_exception fd_NetworkReadFailed=_("Network read failed"),
             fd_NetworkWriteFailed=_("Network write failed");

static int trace_dteval=0;

static void set_blocking(int socket,int flag)
{
  unsigned long nonblocking=((flag)?0:1);
  ioctlsocket(socket,FIONBIO,&nonblocking);
}

static char *read_string_from_file(FILE *f)
{
  char *buf=fd_xmalloc(DTIO_BLOCK_SIZE);
  int size=0, limit=DTIO_BLOCK_SIZE, c;
  while ((c=getc(f)) >= 0) {
    if (size == limit) {
      buf=fd_xrealloc(buf,limit*2); limit=limit*2;}
    buf[size++]=(char) c;}
  if (size == limit) {
    buf=fd_xrealloc(buf,limit*2); limit=limit*2;}
  buf[size++]='\0';
  return buf;
}

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

/** Dealing with timeouts **/

#if WIN32
static int set_noblock(int socket)
{
  unsigned long flags=1;
  ioctlsocket(socket,FIONBIO,&flags);
  return flags;
}
static int reset_flags(int fd,int flags)
{
  unsigned long flag_value=flags;
  return ioctlsocket(fd,FIONBIO,&flag_value);
}
#else
static int set_noblock(int fd)
{
  int oflags=fcntl(fd,F_GETFL), flags=oflags|O_NONBLOCK;
  fcntl(fd,F_SETFL,flags);
  return oflags;
}

static int reset_flags(int fd,int flags)
{
  return fcntl(fd,F_SETFL,flags);
}
#endif

#if WIN32
#define set_timeout_error()
#define test_timeout_errorp() (0)
#else
#define set_timeout_error() errno=ETIMEDOUT
#define test_timeout_errorp() (errno == ETIMEDOUT)
#endif

#if WIN32
static int inprogressp()
{
  int err=WSAGetLastError();
  /* fprintf(stderr,"inprogressp err=%d\n",err-WSABASEERR); */
  if ((err == WSAEINPROGRESS) || (err == WSAEWOULDBLOCK)) return 1;
  else return 0;
}
#else
#define inprogressp() (errno == EINPROGRESS)
#endif

static int wait_on_socket(int fd,int secs,int usecs,int rd,int wr,int exc)
{
  fd_set rs, ws, xs; int sresult=0; struct timeval timeout;
  FD_ZERO(&rs); FD_ZERO(&ws); FD_ZERO(&xs);
  if (rd) FD_SET(fd,&rs); if (wr) FD_SET(fd,&ws); if (exc) FD_SET(fd,&xs); 
  timeout.tv_sec=secs; timeout.tv_usec=usecs;
  return select(fd+1,&rs,&ws,&xs,&timeout);
}
  
/* timed_connect:
      Arguments: an interval in seconds (an int), an open socket,
                 a pointer to a sockaddr struct, and its length
      Returns: the socket id or -1 on failure
  Tries to connect to a particular network address with a timeout
  (in seconds). */
static int timed_connect
  (int secs,int socket_id,struct sockaddr *addr,int addr_len)
{
  int connect_result=0, wait_result=0, oflags=set_noblock(socket_id);
  while ((connect_result=connect(socket_id,addr,addr_len)) != 0) {
    if (errno == EAGAIN) {}
    else if (inprogressp())
      if (wait_on_socket(socket_id,secs,0,0,1,0) == 0) {
	set_timeout_error(); reset_flags(socket_id,oflags); return -1;}
      else {reset_flags(socket_id,oflags); return socket_id;}
    else break;
    CLEAR_ERR();}
  return connect_result;
}

DTYPES_EXPORT
/* fd_timed_recv:
      Arguments: an interval in seconds (an int), an open socket,
                 a pointer to a block of data, a number of bytes,
                 and some flags (an int) for recv()
      Returns: the number of bytes read or -1 on error
  Tries to read bytes from a connection, returning -1 if the
   connection times out.
*/
int fd_timed_recv(int secs,int socket_id,char *data,int len,int flags)
{
  int recv_result=0, wait_result=-1;
  wait_result=wait_on_socket(socket_id,secs,0,1,0,0);
  if (wait_result)
    return recv(socket_id,data,len,flags);
  else {
    set_timeout_error(); return -1;}
}

DTYPES_EXPORT
/* fd_sendall:
      Arguments: a socket, a pointer to a block of data, 
                 the length of the block of data, and
		 flags to pass to send()
      Returns: either zero or -1 (indictating an error)
  This sends all of the bytes in a block of data, repeatedly
calling send().  This will return -1, indicating a failure, if
the attempt to write times out.
*/
int fd_sendall(int socket,char *buf,int size,int flags)
{
  fd_set writefds;
  unsigned char *todo;
  int result, residue;
  /* This isn't guaranteed to write everything, so we have all this hair */
  /* result=send(socket,d.start,d.ptr-d.start,0); */
  todo=buf; residue=size;
  while (residue > 0) {
    int retval;
    FD_ZERO(&writefds);
    FD_SET(socket,&writefds);
    retval=wait_on_socket(socket,send_timeout,0,0,1,0);
    switch(retval) {
    case 1:
      result=send(socket,todo,residue,flags);
      if (result >= 0) {
	todo=todo+result;
	residue=residue-result;}
      else if (errno != EAGAIN)
	return result;
      FD_CLEAR_ERR();
      break;
    case 0:
      set_timeout_error();
      return -1;
    default:
      if ((errno != EINTR) && (errno != EAGAIN)) {
	fd_warn("Error (%s) on socket %d, retval=%d",
		strerror(errno),socket,retval);
	return -1;}
      continue;
    }
  }
  return 0;
}

#define BUFFER_DELTA 16384

DTYPES_EXPORT
/* fd_read_from_socket:
      Arguments: a pointer to an FD_DBUF struct and a socket
      Returns: nothing (void)
  This fills the expandable FD_DBUF struct with bytes read from
socket, stopping when recv() return 0 or the call to recv times out.
*/
void fd_read_from_socket(struct FD_DBUF *buf,int socket)
{
  int first_read=1;
#ifdef WIN32
  int err=0;
#endif
  while (1) {
    int this_chunk, available=buf->end-buf->ptr;
    if (available == 0) {
      int current_size=buf->end-buf->start;
      int current_offset=buf->ptr-buf->start;
      unsigned char *new_buf=
	fd_xrealloc(buf->start,current_size+BUFFER_DELTA);
      buf->start=new_buf;
      buf->ptr=new_buf+current_offset;
      buf->end=new_buf+current_size+BUFFER_DELTA;
      available=buf->end-buf->ptr;}
    errno=0;
    if (first_read) {
      first_read=0;
      this_chunk=fd_timed_recv(5,socket,buf->ptr,available,0);}
    else this_chunk=fd_timed_recv(0,socket,buf->ptr,available,0);
    if (test_timeout_errorp()) FD_CLEAR_ERR();
    if (this_chunk > 0) buf->ptr=buf->ptr+this_chunk;
    else if (this_chunk == 0) return;
#ifdef WIN32
    else if ((err=WSAGetLastError()) == WSAEWOULDBLOCK) return;
#else
    else if (errno == EWOULDBLOCK) {CLEAR_ERR(); continue;}
#endif
    else return;
  }
}

/** Support for Windows **/

#ifdef WIN32
#define close closesocket
static unsigned int sockets_initialized=0;
static void init_win32sockets()
{
  WORD wVersionRequested; 
  WSADATA wsaData; 
  int err; 
  int sockopt = SO_SYNCHRONOUS_NONALERT;
  
  wVersionRequested = MAKEWORD(2, 0); 
  
  err = WSAStartup(wVersionRequested, &wsaData); 
  
  /* Tell the user that we couldn't find a useable */ 
  /* winsock.dll.     */ 
  if (err != 0) fd_raise_exception(fd_NetworkInitFailed);
  /* Confirm that the Windows Sockets DLL supports 1.1.*/ 
  /* Note that if the DLL supports versions greater */ 
  /* than 1.1 in addition to 1.1, it will still return */ 
  /* 1.1 in wVersion since that is the version we */ 
  /* requested. */ 
  
  if ( LOBYTE( wsaData.wVersion ) != 2 || HIBYTE( wsaData.wVersion ) != 0 ) 
    {WSACleanup(); 
    fd_raise_exception(fd_NetworkInitFailed);}
  
  if (setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
		 (char *)&sockopt,sizeof(sockopt)) < 0) {
    perror("setsockopt:");}
}
#endif 

/** Network database functions **/

static struct hostent localhost;

/* Gets the localhost 127.0.0.1 */
static struct hostent *get_island_host()
{
  struct hostent *remote_host=gethostbyname("localhost");
  if (remote_host) return remote_host;
  else return &localhost;
}

/** Connecting TO servers **/

DTYPES_EXPORT
/* fd_open_tcp_socket:
     Arguments: a hostname (a string) and a port (an int)
                and an id (a string) and an error flag (an int)
     Returns: an open socket

     This utility function gets an open TCP socket for a host and port. */
int fd_open_tcp_socket
   (char *hostname,int port,char *id,int signal_error,char **fullname)
{
  long socket_id; int flag=1;
  struct hostent *remote_host;
  struct sockaddr_in server_address; 
  fd_lock_mutex(&_fd_dns_access_lock);
  if ((strcmp(hostname,"localhost") == 0))
    remote_host=get_island_host();
  else remote_host=gethostbyname(hostname);
  if (errno) {CLEAR_ERR();}
  if (remote_host == NULL) {
    fd_unlock_mutex(&_fd_dns_access_lock);
    fd_raise_detailed_exception(fd_UnknownHost,hostname);}
  else if (fullname) *fullname=fd_strdup(remote_host->h_name);
  socket_id=socket(PF_INET,SOCK_STREAM,0);
  if (socket_id < 0) {
    char *buf=fd_xmalloc(strlen(hostname)+10);
    if (id) 
      sprintf(buf,"%s[%d@%s] (%s)",id,port,hostname,strerror(errno));
    else sprintf(buf,"%d@%s (%s)",port,hostname,strerror(errno));
    fd_unlock_mutex(&_fd_dns_access_lock);
    if (signal_error) fd_raise_detailed_exception(fd_NoSocket,buf);
    else return -1;}
  server_address.sin_port=htons((short)port);
  memmove((char *) &((server_address).sin_addr),(char *) remote_host->h_addr,
	  remote_host->h_length);
  server_address.sin_family=remote_host->h_addrtype;
  fd_unlock_mutex(&_fd_dns_access_lock);
  if ((timed_connect(connect_timeout,socket_id,
		     (struct sockaddr *)&server_address,
		     sizeof(struct sockaddr_in))) < 0) {
    char *buf=fd_xmalloc(128);
    if (id) 
      sprintf(buf,"%s[%d@%s] (%s)",id,port,hostname,strerror(errno));
    else sprintf(buf,"%d@%s (%s)",port,hostname,strerror(errno));
    if (signal_error) {
      close(socket_id);
      fd_raise_detailed_exception(fd_NoConnection,buf);}
    else return -1;}
  return socket_id;
}

#if HAVE_SYS_UN_H
DTYPES_EXPORT
/* fd_open_file_socket:
     Arguments: a filename (a string) and a server id (a string) and an error flag (an int)
     Returns: an open socket

     This utility function gets an open socket for a local filenname. */
int fd_open_file_socket(char *filename,char *id,int signal_error)
{
  long socket_id;
  struct sockaddr_un servname;
  socket_id=socket(PF_LOCAL,SOCK_STREAM,0);
  if (socket_id < 0) {
    char *buf=fd_xmalloc(strlen(filename)+10);
    if (id) 
      sprintf(buf,"%s[@%s] (%s)",id,filename,strerror(errno));
    else sprintf(buf,"@%s (%s)",filename,strerror(errno));
    if (signal_error) fd_raise_detailed_exception(fd_NoSocket,buf);
    else return -1;}
  servname.sun_family=AF_LOCAL; strcpy(servname.sun_path,filename);
  if ((timed_connect(connect_timeout,socket_id,
		     (struct sockaddr *)&servname,
		     SUN_LEN(&servname))) < 0) {
    char *buf=fd_xmalloc(128);
    if (id) 
      sprintf(buf,"%s[@%s] (%s)",id,filename,strerror(errno));
    else sprintf(buf,"@%s (%s)",filename,strerror(errno));
    if (signal_error) {
      close(socket_id);
      fd_raise_detailed_exception(fd_NoConnection,buf);}
    else return -1;}
  return socket_id;
}
#else
int fd_open_file_socket(char *filename,char *id,int signal_error)
{
  fd_raise_exception(_("No local socket support"));
  return -1;
}
#endif

DTYPES_EXPORT
/* fd_init_connection
    Arguments: a pointer to an FD_SERVER struct,
               a host (a string), a port (an int),
	       and an id (a string or NULL)
    Returns: nothing

  Initializations a TCP/IP connection structure. */
void fd_init_connection(fd_server server,char *dest,int port,char *id)
{
  int socket_id;
  if (port < 0) socket_id=fd_open_file_socket(dest,id,1);
  else socket_id=fd_open_tcp_socket(dest,port,id,1,&(server->servername));
  server->port=port;
  if (id) server->id=fd_strdup(id); else server->id=id;
  if (port<0) server->servername=fd_strdup(dest);
  server->socket=socket_id;
  server->closefn=NULL;
#if FD_USE_STDIO_WITH_SOCKETS
  server->in=socket2file(socket_id,"rb"); 
  server->out=socket2file(socket_id,"wb");
  /* Clear dangling errors from fdopen. */
  if (server->in) {CLEAR_ERR();}
#endif
  if (errno) {CLEAR_ERR();}
  server->ref_count=1; server->traced=0;
}

DTYPES_EXPORT
/* fd_open_connection
    Arguments: a name (a string), a port (an int) and an id (a string)
    Returns: a server (a pointer to a FD_SERVER struct)

  Looks for a current connection to port@hostname and creates one
if it doesn't exist. */
fd_server fd_open_connection(char *name,int port,char *id)
{
  int i=0, free_connection=-1;
  char *realname=
    ((port<0) ? (fd_get_real_pathname(name)) : (fd_get_real_hostname(name)));
  if (realname == NULL)
    fd_raise_detailed_exception(fd_UnknownHost,name);
  lock_mutex(&server_lookup_lock);
  while (i < server_count)
    if ((servers[i].port == port) &&
	((strcmp(realname,servers[i].servername)) == 0)) {
      servers[i].ref_count++;
      unlock_mutex(&server_lookup_lock);
      fd_xfree(realname);
      return &servers[i];}
    else if (servers[i].socket < 0) {
      if (free_connection < 0) free_connection=i;
      i++;}
    else i++;
  fd_xfree(realname);
  if (free_connection < 0) {
    free_connection=server_count++;
    unlock_mutex(&server_lookup_lock);}
  else if (free_connection >= MAX_SERVERS) {
    unlock_mutex(&server_lookup_lock);
    fd_raise_exception(_("Too many open connections"));}
  else unlock_mutex(&server_lookup_lock);
  (void) fd_init_connection(&servers[free_connection],name,port,id);
  servers[free_connection].ref_count=1; servers[free_connection].traced=0;
  return &servers[free_connection];
}

DTYPES_EXPORT
/* fd_new_connection
    Arguments: a name (a string), a port (an int) and an id (a string)
    Returns: a server (a pointer to a FD_SERVER struct)

  Creates a new connection to port@hostname. */
fd_server fd_new_connection(char *name,int port,char *id)
{
  int i=0, free_connection=-1;
  char *realname=
    ((port<0) ? (fd_get_real_pathname(name)) : (fd_get_real_hostname(name)));
  if (realname == NULL)
    fd_raise_detailed_exception(fd_UnknownHost,name);
  lock_mutex(&server_lookup_lock);
  while (i < server_count)
    if (servers[i].socket < 0) {
      if (free_connection < 0) free_connection=i;}
    else i++;
  fd_xfree(realname);
  if (free_connection < 0) {
    free_connection=server_count++;
    unlock_mutex(&server_lookup_lock);}
  else if (free_connection >= MAX_SERVERS) {
    unlock_mutex(&server_lookup_lock);
    fd_raise_exception(_("Too many open connections"));}
  else unlock_mutex(&server_lookup_lock);
  (void) fd_init_connection(&servers[free_connection],name,port,id);
  servers[free_connection].ref_count=1; servers[free_connection].traced=0;
  return &servers[free_connection];
}

DTYPES_EXPORT
/* fd_get_server_count
    Arguments: none
    Returns: an integer

  Returns the number of servers you are currently connected to.
*/
int fd_get_server_count()
{
  int count=server_count, i=0;
  while (i < server_count)
    if (servers[i].port == 0) {count--; i++;} else i++;
  return count;
}

/** Getting primary host names */

DTYPES_EXPORT
char *fd_get_real_hostname(char *hostname)
{
  struct hostent *remote_host;
  fd_lock_mutex(&_fd_dns_access_lock);
  remote_host=gethostbyname(hostname);
  if (remote_host) {
    char *result=fd_strdup(remote_host->h_name);
    CLEAR_ERR(); fd_unlock_mutex(&_fd_dns_access_lock);
    return result;}
  else {
    CLEAR_ERR(); fd_unlock_mutex(&_fd_dns_access_lock);
    return NULL;}
}

/** Restarting connections **/

DTYPES_EXPORT
/* fd_restart_connection
    Arguments: a server
    Returns: a server

  Restarts the connection to server, returning NULL if the restart fails
*/
#define _str(x) ((x==NULL) ? ((fd_u8char*)"") : (x))

fd_server fd_restart_connection(fd_server s)
{
  struct sockaddr_in server_address; 
  struct hostent *remote_host; int socket_id; 
  fd_notify(_("Restarting connection to %s[%d@%s]"),
	    _str(s->id),s->port,s->servername);
  fd_lock_mutex(&(s->lock));
  close(s->socket);
  fd_lock_mutex(&_fd_dns_access_lock);
  if (strcmp(s->servername,"localhost") == 0)
    remote_host=get_island_host();
  else remote_host=gethostbyname(s->servername); 
  socket_id=socket(PF_INET,SOCK_STREAM,0);
  if (socket_id < 0) {
    char buf[256], *bufptr, *err=strerror(errno), *id=_str(s->id);
    if (strlen(id)+strlen(s->servername)+strlen(err) > 200)
      bufptr=fd_xmalloc(strlen(id)+strlen(s->servername)+strlen(err)+32);
    else bufptr=buf;
    sprintf(buf,"%s[%d]@%s (%s)",
	    _str(s->id),s->port,s->servername,strerror(errno));
    fd_unlock_mutex(&_fd_dns_access_lock);
    fd_raise_detailed_exception(fd_NoSocket,bufptr);
    return (fd_server) 0;}
  server_address.sin_port=htons((short)s->port);
  memmove((char *) &((server_address).sin_addr),
	  (char *) remote_host->h_addr,
	  remote_host->h_length);
  server_address.sin_family=remote_host->h_addrtype;
  fd_unlock_mutex(&_fd_dns_access_lock);
  if ((timed_connect(connect_timeout*4,socket_id,
		     (struct sockaddr *)&server_address,
		     sizeof(struct sockaddr_in)))
      < 0) {
    char *buf=fd_xmalloc(128);
    sprintf(buf,"%s[%d@%s] (%s)",
	    _str(s->id),s->port,s->servername,strerror(errno));
    fd_raise_detailed_exception(fd_NoConnection,buf);
    return (fd_server) 0;}
  s->socket=socket_id;
  fd_unlock_mutex(&(s->lock));
#if FD_USE_STDIO_WITH_SOCKETS
  s->in=socket2file(socket_id,"rb"); 
  s->out=socket2file(socket_id,"wb");
  if (s->in) {CLEAR_ERR();}
#endif
  return s;
}

/** Making and closing connections **/

DTYPES_EXPORT
/* fd_close_connection
    Arguments: a server
    Returns: nothing

  Closes the connection to server.
  This just does a close on the socket and nothing clever to
ensure that pending transactions are completed (though they should
throw out and restart).
*/
void fd_close_connection(fd_server s)
{
  TIDY_ERRNO("fd_close_connection preamble");
  lock_mutex(&server_lookup_lock);
  s->ref_count--; if (s->ref_count>0) {
    unlock_mutex(&server_lookup_lock);
    return;}
  if (s->socket < 0) {
    unlock_mutex(&server_lookup_lock);
    return;}
  if (s->closefn) s->closefn(s);
  /* Will I get into trouble for closing fclose on both the input and
     output streams and then calling close on the socket descriptor
     itself? */
#if FD_USE_STDIO_WITH_SOCKETS
  if (s->out) {
    fflush(s->out); fd_free_xfile(s->out); s->out=NULL;}
  if (s->in) {fd_free_xfile(s->in); s->in=NULL;}
#endif
  close(s->socket);
  s->socket=-1; s->port=0; 
  fd_xfree(s->servername); s->servername=NULL;
  if (s->id) {free(s->id); s->id=NULL;}
  TIDY_ERRNO("fd_close_connection");
  unlock_mutex(&server_lookup_lock);
}

DTYPES_EXPORT
/* fd_close_connections
    Arguments: none
    Returns: nothing

  Closes all active connections.
*/
void fd_close_all_connections()
{
  int i=0; while (i < server_count)
    fd_close_connection(&servers[i++]);
}

DTYPES_EXPORT
/* fd_connect
    Arguments: a string identifying a server
    Returns: a server

  Makes a connection to a particular server, signalling an error
if a connection cannot be made.
  The server identification has the form port@host, where
port is either a registered service or is 'touch-tone' encoded to
a port number
*/
fd_server fd_connect(char *spec)
{
  char *host=strchr(spec,'@'), port[32]; int portno;
  TIDY_ERRNO("fd_connect preamble");
  if (host) {
    fd_server answer; char *name;
    if (strcmp(host+1,"local") == 0) {
      name=fd_xmalloc(host-spec+1); portno=-1;
      strncpy(name,spec,host-spec); name[host-spec]=NUL;}
    else {
      int port_size=host-spec;
      if (port_size>=32)
	fd_raise_detailed_exception(fd_BadServerSpec,spec);
      else {
	strncpy(port,spec,port_size); port[port_size]=0;
	portno=fd_get_portno(port);
	name=fd_strdup(host+1);}}
    answer=fd_open_connection(name,portno,spec); fd_xfree(name);
    if (answer == NULL) fd_raise_exception(fd_ConnectionFailed);
    TIDY_ERRNO("fd_connect");
    return answer;}
  else fd_raise_exception(fd_BadServerSpec);
  return NULL; /* Never reached */
}

DTYPES_EXPORT
/* fd_try_to_connect
    Arguments: a string identifying a server
    Returns: a server

  Tries to make a connection to a particular server, returning NULL
if it fails.
  The server identification has the form port@host, where
port is either a registered service or is 'touch-tone' encoded to
a port number
*/
fd_server fd_try_to_connect(char *spec)
{
  char *host=strchr(spec,'@'), port[32]; int portno;
  TIDY_ERRNO("fd_connect preamble");
  if (host) {
    fd_server answer; char *name;
    if (strcmp(host+1,"local") == 0) {
      name=fd_xmalloc(host-spec+1); portno=-1;
      strncpy(name,spec,host-spec); name[host-spec]=NUL;}
    else {
      int port_size=host-spec;
      if (port_size>=32)
	fd_raise_detailed_exception(fd_BadServerSpec,spec);
      else {
	strncpy(port,spec,port_size); port[port_size]=0;
	portno=fd_get_portno(port);
	name=fd_strdup(host+1);}}
    answer=fd_open_connection(name,portno,spec); fd_xfree(name);
    TIDY_ERRNO("fd_connect");
    return answer;}
  else fd_raise_exception(fd_BadServerSpec);
  return NULL; /* Never reached */
}

DTYPES_EXPORT
/* fd_close_connections
     Arguments; none
     Returns: void

     Closes all active server connections.
*/
void fd_close_connections()
{
  int i=0; while (i < server_count)
    shutdown(servers[i++].socket,2);
}

/* DTYPE_EVAL for non stdio sockets */

FASTOP lisp read_dtype_from_server(fd_server s)
{
#if (FD_USE_STDIO_WITH_SOCKETS)
  return fd_fread_dtype(s->in);
#else
  int socket=s->socket;
  /* int oflags=set_noblock(socket); */
  struct FD_DBUF d; lisp result; int incomplete_dtype=1;
  d.start=fd_xmalloc(DTIO_BLOCK_SIZE);
  d.ptr=d.start; d.end=d.start+DTIO_BLOCK_SIZE;
  while (!(fd_validate_dtype(d.start,d.ptr)))
    fd_read_from_socket(&d,socket);
  d.end=d.ptr; d.ptr=d.start;
  result=fd_dread_dtype(&d);
  fd_xfree(d.start);
  /* reset_flags(socket,oflags); */
  return result;
#endif
}

FASTOP int write_dtype_to_server(lisp expr,fd_server s)
{
  struct FD_DBUF d; int result; d.start=fd_xmalloc(DTIO_BLOCK_SIZE); 
  d.ptr=d.start; d.end=d.start+DTIO_BLOCK_SIZE;
  fd_dwrite_dtype(expr,&d); 
#if (FD_USING_STDIO_SOCKETS)
  {
    int written=0, progress=0, limit=d.ptr-d.start;
    while ((written < limit) &&
	   (progress=fwrite(d.ptr+progress,1,limit-written,s->out))) {
      written=written+progress;}
    result=written;}
#else
  result=fd_sendall(s->socket,d.start,d.ptr-d.start,0);
#endif
  fd_xfree(d.start);
  return result;
}

/** dtype_eval for stdio sockets **/

static lisp dtype_eval(lisp expr,fd_server s)
{
  int c, sock_result;
  fd_lisp result;
  TIDY_ERRNO("pre send request");
  if (trace_dteval) fd_warn("Sending %q to %s",expr,s->id);
  sock_result=write_dtype_to_server(expr,s);
  if (trace_dteval) fd_warn("Sent %q (%d) to %s",expr,sock_result,s->id);
  if (sock_result < 0) fd_raise_exception(fd_NetworkWriteFailed);
  result=read_dtype_from_server(s);
  if (trace_dteval) fd_warn("On %s: %q ==> %q",s->id,expr,result);
  return result;
}

/** fd_dtype_eval **/

DTYPES_EXPORT
/* fd_dtype_eval
    Arguments: a lisp object and a server
    Returns: a lisp object

 Asks the server to evaluate the lisp object, returning the result
 and trying to restart the connection once if neccessary. */
lisp fd_dtype_eval(lisp expr,fd_server s)
{
  volatile lisp result=FD_EMPTY_CHOICE;
  volatile int try_again=0;
  TIDY_ERRNO("fd_dtype_eval preamble");
  lock_mutex(&(s->lock));
  {WITH_HANDLING 
     result=dtype_eval(expr,s);
  ON_EXCEPTION
    {CLEAR_ERR(); try_again=1; fd_clear_exception();}
  END_HANDLING;}
  unlock_mutex(&(s->lock));
  if (try_again) {
    volatile lisp v; fd_restart_connection(s);
    {UNWIND_PROTECT
      {lock_mutex(&(s->lock)); v=dtype_eval(expr,s);}
    ON_UNWIND
      unlock_mutex(&(s->lock));
    END_UNWIND}
    TIDY_ERRNO("fd_dtype_eval post try2");
    return v;}
  else {
    TIDY_ERRNO("fd_dtype_eval post");
    return result;}
}

DTYPES_EXPORT
/* fd_careful_dtype_eval
    Arguments: a lisp object and a server
    Returns: a lisp object

 Like fd_dtype_eval but signals an error if the remote server
returns an error or exception object.
*/
lisp fd_careful_dtype_eval(lisp expr,fd_server s)
{
  lisp result=fd_dtype_eval(expr,s);
  if (FD_EXCEPTIONP(result)) {
    fd_raise_lisp_exception
      ("Remote exception",s->id,EXCEPTION_DATA(result));}
  else if (FD_ERRORP(result)) {
    fd_raise_lisp_exception
      ("Remote error",s->id,EXCEPTION_DATA(result));}
  else return result;
}

DTYPES_EXPORT
/* fd_dtcall
    Arguments: a server, a function name (a string), 
               and a number of args ending with FD_VOID
    Returns: a lisp object

 Asks the server to apply the named function to the args,
 returning the result.  It calls fd_dtype_eval.  The arguments
 are quoted before being passed.
*/
lisp fd_dtcall(fd_server s,char *fcn,...)
{
  lisp expr=FD_MAKE_LIST1(fd_make_symbol(fcn)), *tail=&(CDR(expr));
  va_list args; va_start(args,fcn);
  while (1) {
    lisp arg=va_arg(args,lisp);
    if (FD_VOIDP(arg)) {
      lisp value=fd_dtype_eval(expr,s); decref(expr);
      return value;}
    else {
      lisp quoted=fd_quote_lisp(arg), new=FD_MAKE_LIST1(quoted);
      *tail=new; tail=&(CDR(new));}}
}

DTYPES_EXPORT
/* fd_careful_dtcall
    Arguments: a server, a function name (a string), 
               and a number of args ending with FD_VOID
    Returns: a lisp object

 Like fd_dtcall, but signals an error when the remote server
returns an error object.
*/
lisp fd_careful_dtcall(fd_server s,char *fcn,...)
{
  lisp expr=FD_MAKE_LIST1(fd_make_symbol(fcn)), *tail=&(CDR(expr));
  va_list args; va_start(args,fcn);
  while (1) {
    lisp arg=va_arg(args,lisp);
    if (FD_VOIDP(arg)) {
      lisp value=fd_careful_dtype_eval(expr,s); decref(expr);
      return value;}
    else {
      lisp quoted=fd_quote_lisp(arg), new=FD_MAKE_LIST1(quoted);
      *tail=new; tail=&(CDR(new));}}
}


DTYPES_EXPORT
/* fd_trace_dteval
    Arguments: 1 or 0
    Returns: 1 or 0

Turns on tracing of all remote DTYPE evaluation activity.
*/
int fd_trace_dteval(int flag)
{
  int old=trace_dteval;
  trace_dteval=flag;
  return old;
}


/** TT (Touch-tone) encoding **/

static char tt_codes[128]=
{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 0,1,2,3,4,5,6,7,8,9,-1,-1,-1,-1,-1,-1,
 -1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,
 7,7,7,7,8,8,8,9,9,9,9,-1,-1,-1,-1,-1,
 -1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,
 7,7,7,7,8,8,8,9,9,9,9,-1,-1,-1,-1,-1};


DTYPES_EXPORT
/* fd_get_portno
    Arguments: a string
    Returns: an integer

 Returns the port number identified by a particular string,
starting with the service database and doing touch-tone encoding
if that fails. */
int fd_get_portno(char *string)
{
  struct servent *service;
  if (errno) {TIDY_ERRNO("fd_get_portno preamble");}
  service=getservbyname(string,"tcp"); CLEAR_ERR();
  if (service) return ntohs(service->s_port);
  /* I've been told that /etc/services sometimes fails to contain http */
  else if (strcmp(string,"http") == 0) return 80;
  else {
    int sum=0;
    if (errno) {FD_CLEAR_ERR();}
    while (*string) {
      int code=tt_codes[(int)*string];
      if (code == -1) return -1;
      sum=sum*10+code; string++;}
    return sum;}
}

/** Sending SMTP mail **/

static void transact(fd_server s,char *msg,char *expect);

static char *get_mail_slot(lisp fields,lisp slot)
{
  lisp v=FD_VOID;
  if (SLOTMAPP(fields)) {
    fd_slotmap s=PTR_DATA(fields,slotmap);
    v=fd_slotmap_get(s,slot,FD_VOID);}
  else if (PAIRP(fields)) {
    DOLIST(assoc,fields)
      if ((PAIRP(assoc)) && (LISP_EQ(CAR(assoc),slot))) {
	if (PAIRP(CDR(assoc))) v=CAR(CDR(assoc)); else v=CDR(assoc);
	v=incref(v); break;}}
  else v=FD_VOID;
  if (!(STRINGP(v))) return NULL;
  else {
    char *s=fd_localize_utf8(STRING_DATA(v),fd_get_default_encoding());
    decref(v);
    return s;}
}

DTYPES_EXPORT
/* fd_send_smtp_mail
    Arguments: a destination (a string), a contents (a string), and
               a set of fields (a lisp object)
    Returns: nothing

 Uses a local SMTP connection to send mail to a particular individual with
 a particular set of fields and a particular contents. */
void fd_send_smtp_mail(char *dest,char *text,lisp fields)
{
  struct FD_SERVER sx, *s=&sx; char buf[1024];
  char *mailhost=fd_string_getenv("MAILHOST");
  char *maildomain=fd_string_getenv("MAILDOMAIN");
  char *given_email_address=get_mail_slot(fields,fd_make_symbol("FROM"));
  char *given_reply_to=get_mail_slot(fields,fd_make_symbol("REPLY-TO"));  
  char *reply_to, *email_address;
  if (mailhost == NULL) fd_raise_exception(_("Mailhost unknown"));
  if (given_email_address) email_address=given_email_address;
  else email_address=fd_string_getenv("FRAMERD_EMAIL");
  if (email_address == NULL)
    fd_raise_exception
      ("FRAMERD_EMAIL must be set for FROM field default");
  if (given_reply_to) reply_to=given_reply_to;
  else reply_to=fd_string_getenv("FRAMERD_REPLY_TO");
  fd_init_connection(s,mailhost,fd_get_portno("smtp"),NULL);
  fd_xfree(mailhost);
  recv(s->socket,buf,1024,0);
  if (maildomain) {
    sprintf(buf,"HELO %s\r\n",maildomain); transact(s,buf,"220");
    fd_xfree(maildomain);}
  sprintf(buf,"MAIL FROM: <%s>\r\n",email_address);
  transact(s,buf,"250");
  sprintf(buf,"RCPT TO:<%s>\r\n",dest);
  transact(s,buf,"250");
  transact(s,"DATA\r\n","354");
  if (reply_to) {
    sprintf(buf,"Reply-To: %s\r\n",reply_to);
    send(s->socket,buf,strlen(buf),0);
    fd_xfree(reply_to);}
  sprintf(buf,"To: %s\r\nFrom: %s\r\n",dest,email_address);
  fd_xfree(email_address);
  send(s->socket,buf,strlen(buf),0);
  if (FD_EMPTYP(fields)) {}
  else if (SLOTMAPP(fields)) {
    int i=0, l=SLOTMAP_SIZE(fields);
    while (i < l) {
      lisp slot=SLOTMAP_KEY(fields,i);
      lisp value=SLOTMAP_VALUE(fields,i);
      if (!(SYMBOLP(slot)))
	fd_ctype_error("fd_send_smtp_mail",_("header name not a symbol"),slot);
      if (!(STRINGP(value)))
	fd_ctype_error("fd_send_smtp_mail",_("header value not a string"),
		       value);	
      if (strlen(SYMBOL_NAME(slot))+STRING_LENGTH(value) > 1023)
	fd_raise_exception(_("header field is too long"));
      sprintf(buf,"%s: %s\r\n",SYMBOL_NAME(slot),STRING_DATA(value));
      send(s->socket,buf,strlen(buf),0);
      i++;}}
  else if (PAIRP(fields)) {
    lisp ptr=fields; while ((PAIRP(ptr)) && (PAIRP(CAR(ptr)))) {
      lisp field=CAR(ptr), slot=CAR(field), val;
      if (PAIRP(CDR(field))) val=CAR(CDR(field)); else val=CDR(field);
      if (!(SYMBOLP(slot)))
	fd_raise_exception(_("SMTP field name isn't symbol"));
      else if (!(STRINGP(val)))
	fd_raise_exception(_("SMTP field value isn't string"));
      else sprintf(buf,"%s: %s\r\n",SYMBOL_NAME(slot),STRING_DATA(val));
      send(s->socket,buf,strlen(buf),0);
      ptr=CDR(CDR(ptr));}}
  else fd_raise_exception(_("Invalid field argument for SMTP mail"));
  if (given_email_address == NULL) {
    sprintf(buf,"\n*** Generated and sent by a FramerD program\n");
    send(s->socket,buf,strlen(buf),0);	  
    sprintf(buf,"*** %s\r\n\r\n",fd_session_id());
    send(s->socket,buf,strlen(buf),0);}
  send(s->socket,text,strlen(text),0);
  transact(s,"\r\n.\r\n","250");
  transact(s,"QUIT\r\n","221");
  fd_close_connection(s);
}

static void transact(fd_server s,char *msg,char *expect)
{
  char buf[1024]; int recv_length, total_bytes=0;
  send(s->socket,msg,strlen(msg),0); buf[0]=NUL;
  while (((recv_length=fd_timed_recv(1,s->socket,buf+total_bytes,1024-total_bytes,0)) > 0) &&
	 (total_bytes<1024))
    total_bytes=total_bytes+recv_length;
  buf[total_bytes]=NUL;
#if 0 /* For debugging */
  fprintf(stderr,"Sent: %s",msg);
  fprintf(stderr,"Received: %s",buf);
#endif
  if (expect)
    if (strncmp(buf,expect,strlen(expect)) != 0)
      fd_raise_exception(buf);
}

/** Accessing URLS **/

struct PARSED_URL {
  char hostname[128]; char portspec[32]; char *path;};

static void parse_url(char *url, struct PARSED_URL *answer)
{
  if (strncmp(url,"http://",7))
    fd_raise_detailed_exception(fd_UnhandledURL,url);
  else {
    size_t host_size;
    char *host=url+7, *port=strchr(host,':'), *path=strchr(host,'/');
    /* In case there is no path, e.g. http://foo.com:88 */
    if (path == NULL) path=url+strlen(url);
    /* This is the case where the colon is really in the pathname
       and isn't a port specifier. */
    if ((port) && (path<port)) port=NULL;
    if (port) {
      port++;
      if ((path-port) >= 32 - 1)
	fd_raise_detailed_exception(fd_UnhandledURL,url);
      strncpy(answer->portspec,port,path-port);
      answer->portspec[path-port]=0;
      host_size=(port-1)-host;
      answer->path=path+1;}
    else if (path == NULL) {
      host_size = strlen(host);
      answer->path = "";}
    else {
      strcpy(answer->portspec,"http");
      host_size=path-host;
      answer->path=path+1;}
    if (host_size >= 128 - 1) fd_raise_detailed_exception(fd_UnhandledURL,url);
    strncpy(answer->hostname,host,host_size);
    answer->hostname[host_size]=0;}
}

static void grow_dbuf(struct FD_DBUF *buf,int size)
{
  int current_size=buf->end-buf->start;
  int current_offset=buf->ptr-buf->start;
  unsigned char *new_buf; int needed_size=current_offset+size+1, new_size=current_size;
  while (new_size < needed_size) new_size=new_size+BUFFER_DELTA;
  new_buf=fd_xrealloc(buf->start,current_size+new_size);
  buf->start=new_buf;
  buf->ptr=new_buf+current_offset;
  buf->end=new_buf+new_size;
}

static int readn(char *ptr,int len,fd_server s)
{
  int to_read=len, total_read=0;
  while (to_read>0) {
    int bytes_read=fd_timed_recv(recv_timeout,s->socket,ptr,to_read,0);
    if (bytes_read == 0) return total_read;
    else if ((bytes_read < 0) && (errno == EAGAIN)) {
      FD_CLEAR_ERR(); continue;}
    else if (bytes_read < 0) {
      fd_warn("recv problem: %d:%s",errno,strerror(errno));
      return total_read;}
    ptr=ptr+bytes_read; total_read=total_read+bytes_read;
    to_read=to_read-bytes_read;}
  *ptr=NUL;
  return total_read;
}

static int scan_headers
  (struct FD_DBUF *d,int pos,int *len,int *chunked,int *end)
{
  uchar *bol=d->start+pos, *eol=strchr(bol,'\n'), *last_eol=bol;
  while (eol) {
    last_eol=eol;
    if (eol == bol) {*end=eol-d->start+1; return *end;}
    else if ((eol-bol == 1) && (*bol == '\r')) {
      *end=eol-d->start+1; return *end;}
    else if (strncasecmp(bol,"content-length:",15) == 0) {
      char *scan=bol+15; while ((bol<eol) && (isspace(*scan))) scan++;
      sscanf(scan,"%d",len);}
    else if (strncasecmp(bol,"transfer-encoding:",18) == 0) {
      char *scan=bol+18; while ((bol<eol) && (isspace(*scan))) scan++;
      if (strncasecmp(scan,"chunked",7) == 0) *chunked=1;}
    bol=eol+1; eol=strchr(bol,'\n');}
  return last_eol-d->start;
}

static int read_headers
  (fd_server s,struct FD_DBUF *d,int *len,int *chunked,int *bstart)
{
  char buf[1025]; int bytes_read, scan_pos=0, response_code=0;
  while ((*bstart == 0) &&
	 (bytes_read=fd_timed_recv(recv_timeout,s->socket,buf,1024,0))) {
    /* Stop when there is no more data */
    if (bytes_read == 0) break;
    /* Or when the reader returns an error */
    else if (bytes_read < 0) return bytes_read;
    /* At the beginning pull out the HTTP response code to return it */
    if (scan_pos == 0) {
      int maj, min;
      sscanf(buf,"HTTP/%d.%d %d ",&maj,&min,&response_code);}
    /* Copy this buffer */
    grow_dbuf(d,bytes_read+1);  /* Make the buffer big enough */
    memcpy(d->ptr,buf,bytes_read); d->ptr=d->ptr+bytes_read;
    /* If there's a valid response code, try scanning headers */
    if (response_code)
      scan_pos=scan_headers(d,scan_pos,len,chunked,bstart);
    bytes_read=0;}
  /* Copy the final data */
  grow_dbuf(d,bytes_read+1);  /* Make the buffer big enough */
  memcpy(d->ptr,buf,bytes_read); d->ptr=d->ptr+bytes_read;
  /* If there's a valid response code, and it's past where you've
     looked already, scan the last bit of data as heades. */
  if ((response_code) && (scan_pos < *bstart))
    scan_headers(d,scan_pos,len,chunked,bstart);
  return response_code;
}

static int get_chunk_size(fd_server s)
{
  char buf[512], *write=buf; int size;
  while (fd_timed_recv(recv_timeout,s->socket,write,1,0))
    if (*write == '\n') break;
    else write++;
  *write++=NUL;
  if (sscanf(buf,"%x",&size) < 0) 
    fd_raise_detailed_exception(BadChunk,buf);
  else return size;
}

static char *get_data_start(char *chunk_start)
{
  char *eol=strchr(chunk_start,'\n');
  if (eol) return eol+1; else return eol;
}

/* This is special because read_headers may have read ahead to read part
   of the first chunks */
static int chunk_setup(fd_server s,struct FD_DBUF *dbuf,int start)
{
  uchar *chunk_start=dbuf->start+start, *data_start, *write=chunk_start;
  if (dbuf->ptr == chunk_start)
    /* If we've been lucky and we haven't overread the chunked data,
       we just return. */
    return;
  data_start=get_data_start(chunk_start);
  /* This is the compression loop, where chunks which have already been
     read get compressed (and chunk codes removed). */
  while (data_start) {
    int chunk_size;
    if (sscanf(chunk_start,"%x",&chunk_size) < 0)
      fd_raise_detailed_exception(BadChunk,chunk_start);
    if (chunk_size == 0) {
      *write=NUL; dbuf->ptr=write; return 1;}
    /* If you need to read some more, exit the compression loop */
    if (data_start+chunk_size > dbuf->ptr) break;
    memmove(write,data_start,chunk_size);
    write=write+chunk_size; *write=NUL;
    chunk_start=data_start+chunk_size;
    /* Move over final CRLF, forgiving of just LF */
    if (*chunk_start  == '\r') chunk_start++;
    if (*chunk_start  == '\n') chunk_start++;
    data_start=get_data_start(chunk_start);}
  /* Now we are at the start of a partial chunk.  First, if we've
     only over-read a part of the first line (the chunk size),
     we read the rest. */
  if (data_start == NULL) {
    char *write=dbuf->ptr; grow_dbuf(dbuf,32);
    while (fd_timed_recv(recv_timeout,s->socket,write,1,0))
      if (*write == '\n') break;
      else write++;
    dbuf->ptr=write; write[1]=NUL; data_start=dbuf->ptr;
    data_start=write++;}
  { /* Now we copy and read the whole chunk into a buffer.  */
    int chunk_size, already_read; char *buf;
    if (sscanf(chunk_start,"%x",&chunk_size) < 0) 
      fd_raise_detailed_exception(BadChunk,chunk_start);
    if (chunk_size == 0) {
      *write=NUL; dbuf->ptr=write; return 1;}
    already_read=dbuf->ptr-data_start;
    /* Allocate the buffer, with space for the final CRLF and a NUL */
    buf=fd_malloc(chunk_size+3);
    /* Copy what we've already read */
    memcpy(buf,data_start,already_read);
    /* Now we read the remainder, plus the CRLF */
    readn(buf+already_read,(chunk_size-already_read)+2,s);
    /* Reset dbuf as though the chunk hadn't been read at all */
    dbuf->ptr=chunk_start; *chunk_start=NUL;
    /* Now, we write our buffer into dbuf */
    grow_dbuf(dbuf,chunk_size);
    memcpy(dbuf->ptr,buf,chunk_size); dbuf->ptr=dbuf->ptr+chunk_size;
    *dbuf->ptr=NUL;
    /* Clean up */
    fd_free(buf,chunk_size);}
}

static void escape_path(char *path,char *buf,int bufsiz)
{
  uchar *write=buf, *limit=write+bufsiz-4, *read=path;
  while ((*read) && (write < limit)) 
    if (iscntrl(*read) || isspace(*read) || (*read >= 0x80)) {
      sprintf(write,"%%%02x",*read); read++; write=write+3;}
    else *write++=*read++;
  if (*read) fd_raise_detailed_exception(UrlPathTooLong,path);
  else {*write=NUL;}
}

DTYPES_EXPORT
/* fd_http_get
    Arguments: a string and a pointer to an int
    Returns: a string (actually a pointer to a byte array)
  Gets the contents of a remote URL as a character string, storing the
size in the second argument (if non-NULL)
*/
char *fd_http_get(char *url,int *sizep)
{  
  struct PARSED_URL purl;
  struct FD_SERVER sx, *s=&sx;
  char buf[1024], pathbuf[512], *data, *eol;
  struct FD_DBUF dbuf; char *body=NULL;
  int len=-1, chunked=0, body_start=0, response_code=0;
  parse_url(url,&purl);
  fd_init_connection(s,purl.hostname,fd_get_portno(purl.portspec),NULL);
  escape_path(purl.path,pathbuf,512);
  sprintf(buf,"GET /%s HTTP/1.1\r\nUser-Agent: %s\r\nHost: %s\r\n\r\n",
	  pathbuf,agent_id,purl.hostname);
  /* Send the request */
  fd_sendall(s->socket,buf,strlen(buf),0);
  /* Set up the dbuf for the headers */
  dbuf.ptr=dbuf.start=fd_xmalloc(1024); dbuf.end=dbuf.start+1024;
  /* This is a little kludge, to make a fake MIME field for the returned
     information; I hope it doesn't come back to bite me. */
  strcpy(dbuf.start,"HTTP-RESPONSE: "); dbuf.ptr=dbuf.start+strlen(dbuf.ptr);
  response_code=read_headers(s,&dbuf,&len,&chunked,&body_start);
  if (response_code < 0)
    fd_raise_detailed_exception("URLGET failed",url);
  else if ((response_code == 0) || (body_start == 0)) { /* Bad header! */
    int faux_header_len=strlen("HTTP-RESPONSE: ");
    memmove(dbuf.start+2,dbuf.start+faux_header_len,
	    (dbuf.ptr-dbuf.start)-faux_header_len);
    dbuf.ptr=dbuf.ptr-faux_header_len+2; *(dbuf.ptr)=NUL;
    /* Write empty line at start to make the answer
       a headerless MIME reponse */
    dbuf.start[0]='\r'; dbuf.start[1]='\n';
    body_start=0;}
  if (chunked)
    if (chunk_setup(s,&dbuf,body_start)) {}
    else {
      int chunk_size=get_chunk_size(s);
      while (chunk_size) {
	int actual_len;
	/* Read chunk and CRLF */
	grow_dbuf(&dbuf,chunk_size+2);
	actual_len=readn(dbuf.ptr,chunk_size+2,s);
	dbuf.ptr=dbuf.ptr+chunk_size; *(dbuf.ptr)=NUL;
	chunk_size=get_chunk_size(s);}}
  else if (len > 0) {
    int already_read=(dbuf.ptr-dbuf.start)-body_start;
    int bytes_read, needed=len-already_read;
    grow_dbuf(&dbuf,needed); bytes_read=readn(dbuf.ptr,needed,s);
    if (bytes_read != needed)
      fd_warn("Length mismatch for %s: expected %d, got %d",
	      url,len,bytes_read+already_read);
    dbuf.ptr=dbuf.ptr+bytes_read; *(dbuf.ptr)=NUL;}
  else fd_read_from_socket(&dbuf,s->socket);
  FD_CLEAR_ERR();
  data=dbuf.start; fd_close_connection(s);
  if (sizep) *sizep=dbuf.ptr-dbuf.start;
  return data;
}

DTYPES_EXPORT
/* fd_http_head
    Arguments: a string and a pointer to an int
    Returns: a string (actually a pointer to a byte array)
  Gets the head of a remote URL as a character string
*/
char *fd_http_head(char *url,int *sizep)
{  
  struct PARSED_URL purl;
  struct FD_SERVER sx, *s=&sx;
  char buf[1024], pathbuf[512], *data, *eol;
  struct FD_DBUF dbuf;
  int togo, len=-1, chunked=0, end_pos=0, response_code=0, body_start=0;
  parse_url(url,&purl);
  fd_init_connection(s,purl.hostname,fd_get_portno(purl.portspec),NULL);
  escape_path(purl.path,pathbuf,512);
  sprintf(buf,"HEAD /%s HTTP/1.1\r\nUser-Agent: %s\r\nHost: %s\r\n\r\n",
	  pathbuf,agent_id,purl.hostname);
  fd_sendall(s->socket,buf,strlen(buf),0);
  /* Initialize dbuf */
  dbuf.ptr=dbuf.start=fd_xmalloc(1024); dbuf.end=dbuf.start+1024;
  /* This is a little kludge, to make a fake MIME field for the returned
     information; I hope it doesn't come back to bite me. */
  strcpy(dbuf.start,"HTTP-RESPONSE: "); dbuf.ptr=dbuf.start+strlen(dbuf.ptr);
  response_code=read_headers(s,&dbuf,&len,&chunked,&body_start);
  if (response_code < 0) {
    fd_xfree(dbuf.start);
    fd_raise_detailed_exception("URLHEAD failed",url);}
  else if (response_code == 0) { /* Bad header! */
    fd_xfree(dbuf.start);
    fd_raise_detailed_exception
      ("URLHEAD failed (bad response code)",url);}
  FD_CLEAR_ERR();
  data=dbuf.start; fd_close_connection(s);
  if (sizep) *sizep=dbuf.ptr-dbuf.start;
  return data;
}

DTYPES_EXPORT
/* fd_urlstring
    Arguments: a string
    Returns: a string (actually a pointer to a byte array)
  Gets the contents of a remote URL as a character string, storing the
size in the second argument
*/
char *fd_http_string(char *url)
{
  return fd_http_get(url,NULL);
}

/** Initialization **/

DTYPES_EXPORT
void fd_set_network_timeouts(int connect,int receive,int send)
{
  if (connect > 0) connect_timeout=connect;
  if (receive > 0) recv_timeout=receive;
  if (send > 0) send_timeout=send;
}

void fd_initialize_network_c()
{
  char *addr1=fd_xmalloc(4);
  char **addr_list=fd_xmalloc(sizeof(char *));
  addr1[0]=127; addr1[1]=0; addr1[2]=0; addr1[3]=1;
  addr_list[0]=addr1;
  localhost.h_name="localhost";
  localhost.h_addrtype=PF_INET;
  localhost.h_length=4;
  localhost.h_addr_list=addr_list;
#if defined(OS2)
  if (!(sockets_initialized)) {sock_init(); sockets_initialized=1;}
#elif defined(WIN32)
  if (!(sockets_initialized)) {init_win32sockets(); sockets_initialized=1;}
#endif
#if FD_THREADS_ENABLED
  fd_init_mutex(&(server_lookup_lock));
  fd_init_mutex(&(_fd_dns_access_lock));
#endif

  atexit((fd_exit_proc_type)fd_close_connections);
  fd_register_source_file("network",__DATE__,vcid);
}


/* File specific stuff */

/* The CVS log for this file
   $Log: network.c,v $
   Revision 1.65  2005/01/14 16:48:49  haase
   Updated copyrights to 2005

   Revision 1.64  2004/11/16 22:26:39  haase
   Fix failure to init closefn for stdio interaction

   Revision 1.63  2004/07/20 09:16:14  haase
   Updated copyright year, removed dead code, and cleaned up some whitespace

   Revision 1.62  2004/07/19 16:57:14  haase
   Renamed FD_USING_THREADS to FD_THREADS_ENABLED

   Revision 1.61  2004/04/05 10:39:54  haase
   Fix chunk parsing problem in fd_http_get

   Revision 1.60  2004/02/08 17:01:14  haase
   Renamed fd_open_local_socket to fd_open_file_socket

   Revision 1.59  2004/02/08 16:56:50  haase
   Try to use DNS to lookup localhost

   Revision 1.58  2003/12/02 15:02:36  haase
   Added mutex protection for malloc'd environment reference counting

   Revision 1.57  2003/12/01 16:18:41  haase
   Check return values of fd_get_real_hostname

   Revision 1.56  2003/11/26 14:03:48  haase
   Added fd_new_connection

   Revision 1.55  2003/11/26 14:00:03  haase
   Fixed server allocation/unification bug where two connections to the same
   server might be created if an empty server (which had been closed) existed
   in the queue before an existing server.

   Revision 1.54  2003/10/06 11:06:17  haase
   Fixed many malloc/fd_malloc/fd_xmalloc inconsistencies

   Revision 1.53  2003/09/13 21:57:56  haase
   Fixed automatic closing of unused network connections

   Revision 1.52  2003/08/27 10:53:30  haase
   Merged 2.4 patches into trunk, started 2.5

   Revision 1.51.2.5  2003/08/06 18:41:07  haase
   Updated copyright notices to 2003

   Revision 1.51.2.4  2003/01/26 20:56:05  haase
   Various fixes, including replaces of fd_make_string with fd_copy_string

   Revision 1.51.2.3  2002/09/26 02:07:17  haase
   Various fixes (Thanks, Ralph)

   Revision 1.51.2.2  2002/08/09 16:58:31  haase
   Fix missing mutex lock

   Revision 1.51.2.1  2002/07/31 21:33:03  haase
   Fixed bugs in SMTP mailing

   Revision 1.51  2002/05/20 15:20:29  haase
   Fixes to networking for Solaris

   Revision 1.50  2002/05/20 01:56:31  haase
   Fixes to direct socket access

   Revision 1.49  2002/05/20 00:28:04  haase
   Fix malformed fwrite calls

   Revision 1.48  2002/05/18 19:21:45  haase
   WIN32 fixes

   Revision 1.47  2002/05/01 21:46:31  haase
   Renamed mutex/condvar/rwlock types to have fd_ prefixes

   Revision 1.46  2002/04/29 13:05:02  haase
   Made network reads more careful

   Revision 1.45  2002/04/29 11:20:00  haase
   Removed exported fd_timed_connect --- requires all of FramerD to include sockaddr and there's a better interrace in fd_open_tcp_socket

   Revision 1.44  2002/04/28 20:37:52  haase
   Exported many network functions from libdtypes (timed connect, recv, send, etc) and removed the duplicate functionality from servers.c

   Revision 1.43  2002/04/27 17:47:54  haase
   Moved mutex/lock init and destroy into FramerD abstraction layer

   Revision 1.42  2002/04/27 02:48:11  haase
   Added mutexes protecting DNS accesses

   Revision 1.41  2002/04/26 13:31:50  haase
   More changes with networking code

   Revision 1.40  2002/04/26 03:55:41  haase
   Various network code fixes for WIN32

   Revision 1.39  2002/04/18 20:38:34  haase
   Added send timeouts

   Revision 1.38  2002/04/17 17:50:39  haase
   Fixed some inconsistent returns

   Revision 1.37  2002/04/17 12:38:46  haase
   Fixes to networking code to get errors signalled as soon as possible (at connect, not write)

   Revision 1.36  2002/04/17 07:54:24  haase
   Handle EINPROGRESS correctly for connect

   Revision 1.35  2002/04/16 17:00:04  haase
   Fixed HTTP response parsing

   Revision 1.34  2002/04/16 13:16:45  haase
   Renamed FD_USE_STDIO_SOCKETS to clearer FD_USE_STDIO_WITH_SOCKETS

   Revision 1.33  2002/04/16 12:54:44  haase
   Fixes to non STDIO dteval based on timed_recv

   Revision 1.32  2002/04/16 11:18:48  haase
   Added escaping of fetched URL paths

   Revision 1.31  2002/04/16 10:55:27  haase
   Cleaned up code for timed_recv and timed_connect
   Made read_headers return the response code (or zero
   if there wasn't any)
   Made fd_http_head signal an error if there wasn't a response code
   Made fd_http_get return a headerless mime message (just the content,
   ma'am) when header parsing fails (by, for instance, there not being
   a response code).

   Revision 1.30  2002/04/16 00:13:53  haase
   Made timed_connect loop when EAGAIN is the errno.
   Removed fgetc probe from fd_dtype_eval (???)
   Made fd_set_timeout leave value untouched when arg is zero.

   Revision 1.29  2002/04/15 18:12:22  haase
   Did timeouts right (w/out alarm)

   Revision 1.28  2002/04/15 02:14:08  haase
   Added timeout for read_headers on URL connections to handle some strange sites

   Revision 1.27  2002/04/14 23:16:27  haase
   Fixed comment in network.c

   Revision 1.26  2002/04/11 03:07:10  haase
   Fixed old code which clobbered primary server name

   Revision 1.25  2002/04/10 18:59:52  haase
   Defined fd_get_real_hostname and made fd_open_tcp_connection (optionally) provide the primary hostname for the connection

   Revision 1.24  2002/04/08 15:05:02  haase
   Removed extraneous read_headers and handled the case of chunked transmissions with no content

   Revision 1.23  2002/04/06 19:31:12  haase
   Fixed readn to use STDIO again, added some warnings

   Revision 1.22  2002/04/06 18:51:24  haase
   Made HTTP and SMTP queries agree with specs by using CRLFs rather than simple newlines

   Revision 1.21  2002/04/04 01:58:24  haase
   Rearranged use of stdio and raw sockets to fix some Solaris/Darwin problems

   Revision 1.20  2002/04/02 21:09:18  haase
   New stuff at file end
 
*/

/* Emacs local variables
;;;  Local variables: ***
;;;  compile-command: "cd ../..; make" ***
;;;  End: ***
*/
