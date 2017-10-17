Summary:   Knowledge development and application framework
Name:      framerd
Version:   2.4.3
Release:   3
Source:    ftp://ftp.framerd.org/pub/sourceforge/framerd/%{name}-%{version}.tar.gz
Copyright: GPL
Group:     Development/Languages 
BuildRoot: /tmp/fakeroot
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
./configure --without-fastcgi --without-tags --enable-shared \
	    "--with-readline=/usr/lib/libreadline.a /usr/lib/libncurses.a" \
            --prefix=/usr --with-config-file=/etc/framerd.cfg \
            --with-framerd-group=no \
            --target=$RPM_ARCH-$RPM_OS
make XCFLAGS="-g0"

%install
make
make DESTDIR="$RPM_BUILD_ROOT" XMODE=0755 FMODE=644 install-core install-modules  install-extras
make DESTDIR="$RPM_BUILD_ROOT" XMODE=0755 FMODE=644 install-elisp-blindly
make DESTDIR="$RPM_BUILD_ROOT" XMODE=0755 FMODE=644 install-init.d
install -m0755 etc/setup.fdx $RPM_BUILD_ROOT/usr/sbin/framerd-setup

%post
/sbin/ldconfig
/usr/sbin/framerd-setup /usr/share/framerd

%postun
/sbin/ldconfig

%files
#/etc/framerd.cfg
%attr (-,root,root) /etc/init.d/framerd
%attr (-,root,root) /usr/share/emacs/site-lisp/framerd/fdscript.el
%attr (-,root,root) /usr/lib/libfdtext.so
%attr (-,root,root) /usr/lib/libfdtext.so.1
%attr (-,root,root) /usr/lib/libfdtext.so.1.0
%attr (-,root,root) /usr/lib/libfdscript.so
%attr (-,root,root) /usr/lib/libfdscript.so.1
%attr (-,root,root) /usr/lib/libfdscript.so.1.0
%attr (-,root,root) /usr/lib/libframerd.so
%attr (-,root,root) /usr/lib/libframerd.so.1
%attr (-,root,root) /usr/lib/libframerd.so.1.0
%attr (-,root,root) /usr/lib/libdtypes.so
%attr (-,root,root) /usr/lib/libdtypes.so.1
%attr (-,root,root) /usr/lib/libdtypes.so.1.0
%attr (-,root,root) /usr/bin/fdcgi
%attr (-,root,root) /usr/bin/fdxml
%attr (-,root,root) /usr/bin/fdserver
%attr (-,root,root) /usr/bin/fdservlet
%attr (-,root,root) /usr/bin/fdscript
%attr (-,root,root) /usr/bin/repack-file-index
%attr (-,root,root) /usr/bin/repack-file-pool
%attr (-,root,root) /usr/bin/copy-index
%attr (-,root,root) /usr/bin/analyze-index
%attr (-,root,root) /usr/bin/dtcall
%attr (-,root,root) /usr/bin/make-dtype
%attr (-,root,root) /usr/bin/print-dtype
%attr (-,root,root) /usr/bin/fd_add
%attr (-,root,root) /usr/bin/fd_drop
%attr (-,root,root) /usr/bin/fd_find
%attr (-,root,root) /usr/bin/fd_get
%attr (-,root,root) /usr/bin/fd_id
%attr (-,root,root) /usr/bin/fd_make
%attr (-,root,root) /usr/bin/fd_test
%attr (-,root,root) /usr/bin/fdconfig
%attr (-,root,root) /usr/bin/fdd
%attr (-,root,root) /usr/bin/fdxs
%attr (-,root,root) /usr/bin/make-file-pool
%attr (-,root,root) /usr/bin/list-file-pool
%attr (-,root,root) /usr/bin/reset-file-pool
%attr (-,root,root) /usr/bin/make-file-index
%attr (-,root,root) /usr/bin/reset-file-index
%attr (-,root,root) /usr/bin/index-load
%attr (-,root,root) /usr/sbin/make-super-pool
%attr (-,root,root) /usr/sbin/rebase-file-pool
%attr (-,root,root) /usr/sbin/rebase-file-index
%attr (-,root,root) /usr/sbin/fdservctl
%attr (-,root,root) /usr/sbin/framerd-setup
%attr (-,root,root) /usr/share/framerd/modules/load-utils.fdx
%attr (-,root,root) /usr/share/framerd/modules/server-utils.fdx
%attr (-,root,root) /usr/share/framerd/modules/timespecs.fdx
%attr (-,root,root) /usr/share/framerd/modules/index-plus.fdx
%attr (-,root,root) /usr/share/framerd/modules/vpools.fdx
%attr (-,root,root) /usr/share/framerd/modules/brico.fdx
%attr (-,root,root) /usr/share/framerd/modules/brico-methods.fdx
%attr (-,root,root) /usr/share/framerd/modules/fd4web.fdx
%attr (-,root,root) /usr/share/framerd/modules/fd4web.css
%attr (-,root,root) /usr/share/framerd/modules/mt-utils.fdx
%attr (-,root,root) /usr/share/framerd/modules/fakecgi.fdx
%attr (-,root,root) %doc /usr/share/doc/framerd/c-manual.html
%attr (-,root,root) %doc /usr/share/doc/framerd/concepts.html
%attr (-,root,root) %doc /usr/share/doc/framerd/fdscript-guide.html
%attr (-,root,root) %doc /usr/share/doc/framerd/framerd-capi.html
%attr (-,root,root) %doc /usr/share/doc/framerd/framerd.css
%attr (-,root,root) %doc /usr/share/doc/framerd/index.html
%attr (-,root,root) %doc /usr/share/doc/framerd/r4rs.html
%attr (-,root,root) %doc /usr/share/doc/framerd/r4rs_toc.html
%attr (-,root,root) %doc /usr/share/doc/framerd/servers.html
%attr (-,root,root) %doc /usr/share/doc/framerd/users-guide.html
%attr (-,root,root) %doc /usr/share/doc/framerd/why.html
%attr (-,root,root) %doc /usr/share/doc/framerd/www-guide.html
%attr (-,root,root) %doc /usr/share/doc/framerd/fdlogo.png
%attr (-,root,root) %doc /usr/share/man/man1/analyze-index.1*
%attr (-,root,root) %doc /usr/share/man/man1/compare-file-pools.1*
%attr (-,root,root) %doc /usr/share/man/man1/dtcall.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_add.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_drop.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_find.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_get.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_id.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_make.1*
%attr (-,root,root) %doc /usr/share/man/man1/fd_test.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdcgi.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdconfig.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdd.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdscript.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdserver.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdservlet.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdupdate.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdxml.1*
%attr (-,root,root) %doc /usr/share/man/man1/fdxs.1*
%attr (-,root,root) %doc /usr/share/man/man1/file-pool-data.1*
%attr (-,root,root) %doc /usr/share/man/man1/framerd-setup.1*
%attr (-,root,root) %doc /usr/share/man/man1/index-load.1*
%attr (-,root,root) %doc /usr/share/man/man1/list-file-pool.1*
%attr (-,root,root) %doc /usr/share/man/man1/make-dtype.1*
%attr (-,root,root) %doc /usr/share/man/man1/make-file-index.1*
%attr (-,root,root) %doc /usr/share/man/man1/make-file-pool.1*
%attr (-,root,root) %doc /usr/share/man/man1/make-super-pool.1*
%attr (-,root,root) %doc /usr/share/man/man1/pool-diff.1*
%attr (-,root,root) %doc /usr/share/man/man1/print-dtype.1*
%attr (-,root,root) %doc /usr/share/man/man1/rebase-file-index.1*
%attr (-,root,root) %doc /usr/share/man/man1/rebase-file-pool.1*
%attr (-,root,root) %doc /usr/share/man/man1/repack-file-index.1*
%attr (-,root,root) %doc /usr/share/man/man1/repack-file-pool.1*
%attr (-,root,root) %doc /usr/share/man/man1/reset-file-index.1*
%attr (-,root,root) %doc /usr/share/man/man1/reset-file-pool.1*
%attr (-,root,root) %doc /usr/share/man/man4/mod_fdserv.4*
%attr (-,root,root) %doc /usr/share/man/man7/fdshell.7*
%attr (-,root,root) %doc /usr/share/man/man8/fdmanager.8*
%attr (-,root,root) %doc /usr/share/man/man8/fdservctl.8*
%attr (-,root,root)/usr/include/framerd/
%attr (-,root,root) /usr/include/framerd-2.4/server.h
%attr (-,root,root) /usr/include/framerd-2.4/config.h
%attr (-,root,root) /usr/include/framerd-2.4/win32-config.h
%attr (-,root,root) /usr/include/framerd-2.4/strstream.h
%attr (-,root,root) /usr/include/framerd-2.4/search.h
%attr (-,root,root) /usr/include/framerd-2.4/plugins.h
%attr (-,root,root) /usr/include/framerd-2.4/os.h
%attr (-,root,root) /usr/include/framerd-2.4/fdmalloc.h
%attr (-,root,root) /usr/include/framerd-2.4/odb.h
%attr (-,root,root) /usr/include/framerd-2.4/lisp.h
%attr (-,root,root) /usr/include/framerd-2.4/index.h
%attr (-,root,root) /usr/include/framerd-2.4/framerd.h
%attr (-,root,root) /usr/include/framerd-2.4/fdwww.h
%attr (-,root,root) /usr/include/framerd-2.4/fdscript.h
%attr (-,root,root) /usr/include/framerd-2.4/except.h
%attr (-,root,root) /usr/include/framerd-2.4/eval.h
%attr (-,root,root) /usr/include/framerd-2.4/dtypes.h
%attr (-,root,root) /usr/include/framerd-2.4/dtcodes.h
%attr (-,root,root) /usr/include/framerd-2.4/cons.h
%attr (-,root,root) /usr/include/framerd-2.4/common.h
%attr (-,root,root) /usr/include/framerd-2.4/binio.h


