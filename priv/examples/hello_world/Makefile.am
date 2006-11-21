EXAMPLESDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/examples
erlsrcdir = $(EXAMPLESDIR)/hello_world
erlsrc_DATA = hello_world.erl hello_world.glade hello_world.gladep
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(erlsrcdir)
erlbeam_DATA = hello_world.beam
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<
