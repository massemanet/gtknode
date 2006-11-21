EXAMPLESDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/examples
erlsrcdir = $(EXAMPLESDIR)/treeview
erlsrc_DATA = treeview.erl treeview.glade treeview.gladep
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(erlsrcdir)
erlbeam_DATA = treeview.beam
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<
