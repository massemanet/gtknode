EXAMPLESDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/examples
erlsrcdir = $(EXAMPLESDIR)/top
erlsrc_DATA = top.erl top_top.erl top.glade top.gladep
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(erlsrcdir)
erlbeam_DATA = top.beam top_top.beam
CLEANFILES = $(erlbeam_DATA)

all: top.beam top_top.beam

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
