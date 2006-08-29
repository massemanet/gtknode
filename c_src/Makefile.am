bindir = $(ERLANG_INSTALL_LIB_DIR_gtknode)/priv/bin
bin_PROGRAMS = gtknode
gtknode_SOURCES = gtknode.c gtknode_cnode.c gtknode_gen.c gtknode_glade.c \
	gtknode_internal.c  gtknode_marshal.c  gtknode_structs.c gtknode.h
gtknode_CFLAGS = $(GTK_CFLAGS) -I$(ERLANG_LIB_DIR_erl_interface)/include
gtknode_LDADD = $(GTK_LIBS) -lpthread \
	-L$(ERLANG_LIB_DIR_erl_interface)/lib -lei

gtknodedir = $(ERLANG_INSTALL_LIB_DIR_gtknode)/priv/src
gtknode_DATA = $(gtknode_SOURCES) $(gtknode_INCLUDES)

