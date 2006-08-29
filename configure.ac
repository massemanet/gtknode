AC_INIT(GTK+ 2 wrapper library for Erlang, 0.15, rlenglet@users.forge.objectweb.org, gtknode)
dnl Require autoconf version >=2.57.
AC_PREREQ(2.57)
AC_COPYRIGHT(Copyright (C) 2005 Romain Lenglet)
dnl AC_REVISION($Id$)

dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl C-related configuration.
AC_PROG_CC
AC_STDC_HEADERS

dnl Erlang-related configuration.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(gtknode, 0.15)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

dnl Python-related configuration (for the generator).
AM_PATH_PYTHON

dnl Information about GTK+2.
PKG_CHECK_MODULES(GTK, [gmodule-2.0 libglade-2.0 gtk+-2.0])
dnl Get the directory that contains the GTK+ include files.
gtk_include_dir=`echo "$GTK_CFLAGS" | sed -n -e 's/^.*-I\([[^ ]]*include\/gtk-2.0\).*$/\1/p'`
AC_SUBST([GTKINC], [$gtk_include_dir])
glib_include_dir=`echo "$GTK_CFLAGS" | sed -n -e 's/^.*-I\([[^ ]]*include\/glib-2.0\).*$/\1/p'`
AC_SUBST([GINC], [$glib_include_dir])
gtk_version="`$PKG_CONFIG --modversion gtk+-2.0`"
AC_SUBST([GTKVSN], [$gtk_version])

dnl Specify the files to instantiate.
AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	c_src/Makefile \
	priv/Makefile \
	priv/generator/Makefile \
	priv/examples/Makefile \
	priv/examples/points/Makefile \
	priv/examples/sherk/Makefile \
	priv/examples/top/Makefile \
	doc/Makefile \
	])

AC_OUTPUT

