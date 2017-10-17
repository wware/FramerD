Summary:   Knowledge base infrastructure and R5RS scripting language
Name:      framerd
Version:   2.0
Release:   13
Source:    ftp://ftp.media.mit.edu/framerd/%{name}-%{version}.tar.gz
Copyright: GPL
Group:     Development/Languages 
%description
This is an extensive suite of libraries and programs for working with
very large (multi-million frame) knowledge bases.  It provides a distributed,
object-oriented database optimized for pointer-intensive data and
a versatile a scripting language based on R4 scheme which includes
special tools for text analysis and active web page generation.  The
scripting language also includes native support for
non-deterministic (PROLOG-style) programming in an applicative
framework.

%prep
%setup -q

%build
./configure --enable-shared --prefix=/usr
make XCFLAGS="-g0"

%install
make install

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
/usr/share/framerd/framerd.cfg
/usr/share/emacs/site-lisp/fdscript.el
/usr/lib/libfdtext.so
/usr/lib/libfdtext.so.2
/usr/lib//libfdtext.so.2.0
/usr/lib/libfdscript.so
/usr/lib/libfdscript.so.2
/usr/lib//libfdscript.so.2.0
/usr/lib/libframerd.so
/usr/lib/libframerd.so.2
/usr/lib//libframerd.so.2.0
/usr/lib/libdtypes.so
/usr/lib/libdtypes.so.2
/usr/lib//libdtypes.so.2.0
/usr/bin/fdcgi
/usr/bin/fdserver
/usr/bin/fdscript
/usr/bin/repack-file-index
/usr/bin/analyze-index
/usr/bin/dtcall
/usr/bin/fdconsole
/usr/bin/unzipf-file-index
/usr/bin/reset-file-pool
/usr/bin/repack-file-pool
/usr/bin/register-super-pool
/usr/bin/rebase-file-pool
/usr/bin/rebase-file-index
/usr/bin/make-super-pool
/usr/bin/make-pool-snapshot
/usr/bin/make-file-pool
/usr/bin/make-file-index
/usr/bin/list-file-pool
/usr/bin/index-add
/usr/bin/frame-get
/usr/bin/frame-create
/usr/bin/frame-add
/usr/bin/fdprofile
/usr/bin/fdinstall-script
/usr/bin/fdd
/usr/bin/fdconfig
/usr/bin/fdstopall
/usr/bin/fdstartall
/usr/bin/fdresetserver
/usr/bin/fdstopserver
/usr/bin/fdstartserver
%doc /usr/share/framerd/docs/c-manual.html
%doc /usr/share/framerd/docs/concepts.html
%doc /usr/share/framerd/docs/fdscript-guide.html
%doc /usr/share/framerd/docs/framerd-capi.html
%doc /usr/share/framerd/docs/framerd.css
%doc /usr/share/framerd/docs/r4rs.html
%doc /usr/share/framerd/docs/r4rs_toc.html
%doc /usr/share/framerd/docs/servers.html
%doc /usr/share/framerd/docs/users-guide.html
%doc /usr/share/framerd/docs/why.html
%doc /usr/share/framerd/docs/www-guide.html
/usr/include/framerd/makefile
/usr/include/framerd/fdscript.c
/usr/include/framerd/fdserver.c
/usr/include/framerd/server.h
/usr/include/framerd/config.h
/usr/include/framerd/win32-config.h
/usr/include/framerd/strstream.h
/usr/include/framerd/search.h
/usr/include/framerd/plugins.h
/usr/include/framerd/os.h
/usr/include/framerd/odb.h
/usr/include/framerd/lisp.h
/usr/include/framerd/index.h
/usr/include/framerd/framerd.h
/usr/include/framerd/fdwww.h
/usr/include/framerd/fdscript.h
/usr/include/framerd/except.h
/usr/include/framerd/eval.h
/usr/include/framerd/dtypes.h
/usr/include/framerd/dtcodes.h
/usr/include/framerd/cons.h
/usr/include/framerd/config.h.in
/usr/include/framerd/common.h
/usr/include/framerd/binio.h
/usr/include/framerd/analogy.h
