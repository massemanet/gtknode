erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_gtknode)/src
erlsrc_DATA = gtknode.erl
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_gtknode)/ebin
erlbeam_DATA = gtknode.beam
CLEANFILES = $(erlbeam_DATA)

.SUFFIXES: .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
