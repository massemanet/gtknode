EXAMPLESDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/examples
erlsrcdir = $(EXAMPLESDIR)/sherk
erlsrc_DATA = sherk.erl sherk_tab.erl sherk_prof.erl \
sherk_tree.erl sherk_list.erl sherk_scan.erl sherk_aquire.erl \
sherk_target.erl sherk_ets.erl \
sherk.glade sherk.gladep
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(erlsrcdir)
erlbeam_DATA = sherk.beam sherk_tab.beam sherk_prof.beam sherk_aquire.beam \
sherk_scan.beam sherk_tree.beam sherk_ets.beam \
sherk_list.beam sherk_target.beam
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
