Summary:   Knowledge base infrastructure and R5RS scripting language
Name:      framerd
Version:   1.0
Release:   11
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

%package devel
Summary: FramerD/FDScript development files
Group:   Development/Languages
%description devel
This package contains the static libraries and include files for building
FramerD/FDScript applications.

%prep
%setup -q

%build
./configure --enable-shared-kernel --prefix=/usr
make XCFLAGS="-g0"

%install
make install

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
/usr/share/framerd/common
/usr/share/framerd/data
/usr/share/framerd/public
/usr/share/emacs/site-lisp/fdscript.el
/usr/lib/libfdtext.so
/usr/lib/libfdtext.so.1
/usr/lib//libfdtext.so.1.0
/usr/lib/libfdscript.so
/usr/lib/libfdscript.so.1
/usr/lib//libfdscript.so.1.0
/usr/lib/libframerd.so
/usr/lib/libframerd.so.1
/usr/lib//libframerd.so.1.0
/usr/lib/libdtypes.so
/usr/lib/libdtypes.so.1
/usr/lib//libdtypes.so.1.0
/usr/bin/fdcgi
/usr/bin/fdserver
/usr/bin/fdscript
/usr/bin/repack-file-index
/usr/bin/analyze-index
/usr/bin/index-get
/usr/bin/pool-get
/usr/bin/dtcall
/usr/bin/make-dtype
/usr/bin/print-dtype
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
%doc /usr/share/framerd/docs/tryit.gif
%doc /usr/share/framerd/docs/texto2.gif
%doc /usr/share/framerd/docs/start.gif
%doc /usr/share/framerd/docs/mu-banner.gif
%doc /usr/share/framerd/docs/inside.gif
%doc /usr/share/framerd/docs/fdlogo.gif
%doc /usr/share/framerd/docs/brico.gif
%doc /usr/share/framerd/docs/framerd.css
%doc /usr/share/framerd/docs/framerd-capi.html
%doc /usr/share/framerd/docs/www.html
%doc /usr/share/framerd/docs/why.html
%doc /usr/share/framerd/docs/threads.html
%doc /usr/share/framerd/docs/streams.html
%doc /usr/share/framerd/docs/shell.html
%doc /usr/share/framerd/docs/shell-scripts.html
%doc /usr/share/framerd/docs/servers.html
%doc /usr/share/framerd/docs/search.html
%doc /usr/share/framerd/docs/root.html
%doc /usr/share/framerd/docs/r4rs_toc.html
%doc /usr/share/framerd/docs/r4rs.html
%doc /usr/share/framerd/docs/printout.html
%doc /usr/share/framerd/docs/os.html
%doc /usr/share/framerd/docs/odb.html
%doc /usr/share/framerd/docs/listener.html
%doc /usr/share/framerd/docs/intntl.html
%doc /usr/share/framerd/docs/indices.html
%doc /usr/share/framerd/docs/index.html
%doc /usr/share/framerd/docs/hash.html
%doc /usr/share/framerd/docs/frames.html
%doc /usr/share/framerd/docs/fdtext.html
%doc /usr/share/framerd/docs/emacs.html
%doc /usr/share/framerd/docs/dtypes.html
%doc /usr/share/framerd/docs/customization.html
%doc /usr/share/framerd/docs/choices.html
%doc /usr/share/framerd/docs/c-manual.html
%doc /usr/share/framerd/docs/brico.html
%files devel
/usr/include/framerd/makefile
/usr/include/framerd/fdscript.c
/usr/include/framerd/fdserver.c
/usr/include/framerd/server.h
/usr/include/framerd/config.h
/usr/include/framerd/paths.h
/usr/include/framerd/win32-paths.h
/usr/include/framerd/win32-config.h
/usr/include/framerd/strstream.h
/usr/include/framerd/search.h
/usr/include/framerd/plugins.h
/usr/include/framerd/paths.h.in
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
/usr/lib/libfdtext.a
/usr/lib/libfdscript.a
/usr/lib/libframerd.a
/usr/lib/libdtypes.a
