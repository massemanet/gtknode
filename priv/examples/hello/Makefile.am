EXAMPLESDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/examples
erlsrcdir = $(EXAMPLESDIR)/hello
erlsrc_DATA = hello.erl
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(erlsrcdir)
erlbeam_DATA = hello.beam
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<
