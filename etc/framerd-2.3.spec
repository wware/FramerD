Summary:   Knowledge base infrastructure and R5RS scripting language
Name:      framerd
Version:   2.3
Release:   6
Source:    ftp://ftp.framerd.org/pub/sourceforge/framerd/%{name}-%{version}.tar.gz
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
./configure --without-fastcgi --without-tags --enable-shared --prefix=/usr --target=$RPM_ARCH-$RPM_OS
make XCFLAGS="-g0"

%install
make install
make install-elisp
make install-docs

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
/usr/share/framerd/framerd.cfg
/usr/share/emacs/site-lisp/fdscript.el
/usr/lib/libfdtext.so
/usr/lib/libfdtext.so.1.0
/usr/lib/libfdscript.so
/usr/lib/libfdscript.so.1.0
/usr/lib/libframerd.so
/usr/lib/libframerd.so.1.0
/usr/lib/libdtypes.so
/usr/lib/libdtypes.so.1.0
/usr/bin/fdcgi
/usr/bin/fdxml
/usr/bin/fdserver
/usr/bin/fdscript
/usr/bin/repack-file-index
/usr/bin/repack-file-pool
/usr/bin/copy-index
/usr/bin/analyze-index
/usr/bin/dtcall
/usr/bin/make-dtype
/usr/bin/print-dtype
/usr/bin/fd_add
/usr/bin/fd_drop
/usr/bin/fd_find
/usr/bin/fd_get
/usr/bin/fd_id
/usr/bin/fd_make
/usr/bin/fd_test
/usr/bin/fdconfig
/usr/bin/fdd
/usr/bin/make-file-pool
/usr/bin/list-file-pool
/usr/bin/reset-file-pool
/usr/bin/make-file-index
/usr/bin/reset-file-index
/usr/bin/index-load
/usr/sbin/make-super-pool
/usr/sbin/register-super-pool
/usr/sbin/rebase-file-pool
/usr/sbin/rebase-file-index
/usr/sbin/unzipf-file-index
/usr/sbin/fdservctl
/usr/sbin/fdresetserver
/usr/sbin/fdstopserver
/usr/share/framerd/modules/brico.fdx
/usr/share/framerd/encodings/BIG5
/usr/share/framerd/encodings/GB2312
/usr/share/framerd/encodings/SHIFT_JIS
/usr/share/framerd/encodings/EUC-KR
/usr/share/framerd/encodings/EUC-JP
/usr/share/framerd/encodings/EUC-TW
/usr/share/framerd/encodings/KOI8R
%doc /usr/share/doc/framerd-2.3/c-manual.html
%doc /usr/share/doc/framerd-2.3/concepts.html
%doc /usr/share/doc/framerd-2.3/fdscript-guide.html
%doc /usr/share/doc/framerd-2.3/framerd-capi.html
%doc /usr/share/doc/framerd-2.3/framerd.css
%doc /usr/share/doc/framerd-2.3/index.html
%doc /usr/share/doc/framerd-2.3/r4rs.html
%doc /usr/share/doc/framerd-2.3/r4rs_toc.html
%doc /usr/share/doc/framerd-2.3/servers.html
%doc /usr/share/doc/framerd-2.3/users-guide.html
%doc /usr/share/doc/framerd-2.3/why.html
%doc /usr/share/doc/framerd-2.3/www-guide.html
%doc /usr/share/doc/framerd-2.3/fdlogo.png
%doc /usr/man/man1/fdscript.1
%doc /usr/man/man1/fdserver.1
%doc /usr/man/man1/fdupdate.1
%doc /usr/man/man1/fdcgi.1
%doc /usr/man/man1/fdxml.1
%doc /usr/man/man1/fdservlet.1
%doc /usr/man/man1/mod_fdserv.4
%doc /usr/man/man1/dtcall.1
%doc /usr/man/man1/make-dtype.1
%doc /usr/man/man1/print-dtype.1
%doc /usr/man/man1/fdxs.1
%doc /usr/man/man1/fd_add.1
%doc /usr/man/man1/fd_drop.1
%doc /usr/man/man1/fd_find.1
%doc /usr/man/man1/fd_get.1
%doc /usr/man/man1/fd_test.1
%doc /usr/man/man1/fd_make.1
%doc /usr/man/man1/fdd.1
%doc /usr/man/man1/index-load.1
%doc /usr/man/man1/list-file-pool.1
%doc /usr/man/man1/make-file-pool.1
%doc /usr/man/man1/make-file-index.1
%doc /usr/man/man1/reset-file-pool.1
%doc /usr/man/man1/reset-file-index.1
%doc /usr/man/man1/repack-file-pool.1
%doc /usr/man/man1/repack-file-index.1
%doc /usr/man/man1/analyze-index.1
%doc /usr/man/man8/fdmanager.8
%doc /usr/man/man8/fdservctl.8
%doc /usr/man/man7/fdshell.7
/usr/include/framerd/server.h
/usr/include/framerd/config.h
/usr/include/framerd/win32-config.h
/usr/include/framerd/strstream.h
/usr/include/framerd/search.h
/usr/include/framerd/plugins.h
/usr/include/framerd/os.h
/usr/include/framerd/fdmalloc.h
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
/usr/include/framerd/common.h
/usr/include/framerd/binio.h

