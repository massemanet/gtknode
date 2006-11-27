AC_INIT(GTK+ 2 wrapper library for Erlang, 0.20, 
             mats.cronqvist@ericsson.com, gtknode)
dnl Require autoconf version >=2.59c. first one with erlang macros
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2005 Mats Cronqvist)
dnl AC_REVISION($Id$)

dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl C-related configuration.
AC_PROG_CC
AC_STDC_HEADERS

dnl Erlang-related configuration.
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

dnl Python-related configuration (for the generator).
AM_PATH_PYTHON

dnl AM_PATH_GTK(2.6,[], AC_MSG_ERROR(Cannot find GTK))

dnl Information about GTK+2.
PKG_CHECK_MODULES(GTK, [gmodule-2.0 libglade-2.0 gtk+-2.0])

dnl Get the directory that contains the GTK+ include files.
gtk_include_dir=`echo "$GTK_CFLAGS" | sed -n -e 's/^.*-I\([[^ ]]*include\/gtk-2.0\).*$/\1/p'`
AC_SUBST([GTKINC], [$gtk_include_dir])
glib_include_dir=`echo "$GTK_CFLAGS" | sed -n -e 's/^.*-I\([[^ ]]*include\/glib-2.0\).*$/\1/p'`
AC_SUBST([GINC], [$glib_include_dir])
gtk_version="`$PKG_CONFIG --modversion gtk+-2.0`"
AC_SUBST([GTKVSN], [$gtk_version])

dnl Enable doc link generation, iff not "no"
AC_ARG_ENABLE(
    [gtkdoclinks],
    [AS_HELP_STRING([--enable-gtkdoclinks=URL],
        [in the generated doc, generate links to the GTK+ docs (default is no);
        URL must begin with file:// (local) or http:// (www);
        URL should be set such that URL/gtk/ix01.html exists;
        for www, the http_proxy variable must be set properly])],
    [_gtkdoclinks=$enableval],
    [_gtkdoclinks=no])

AC_SUBST([GTKDOCLINKS], [$_gtkdoclinks])

dnl Specify the files to instantiate.
AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	c_src/Makefile \
	priv/Makefile \
	priv/generator/Makefile \
	priv/examples/Makefile \
	priv/examples/points/Makefile \
	priv/examples/top/Makefile \
	priv/examples/hello_world/Makefile \
	priv/examples/hello/Makefile \
	priv/examples/treeview/Makefile \
	doc/Makefile \
	])

AC_OUTPUT
