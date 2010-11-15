# Final output files.
GEN_INCLUDES = g_generated.h gdk_generated.h gtk_generated.h
GEN_DOCS = gtk_funcs.html gdk_funcs.html g_funcs.html

# Automake-handled installation.
DOCDIR = $(ERLANG_INSTALL_LIB_DIR_gtknode)/doc
docdir = $(DOCDIR)
doc_DATA = $(GEN_DOCS)

generatordir = $(ERLANG_INSTALL_LIB_DIR_gtknode)/priv/src
generator_DATA = $(GEN_INCLUDES)

EXTRA_DIST = bio.erl generator.erl h2def.py \
	g_black.txt gdk_black.txt gtk_black.txt \
	g_white.txt gdk_white.txt gtk_white.txt \
	g_predefined.defs gdk_predefined.defs gtk_predefined.defs \
	$(GEN_DOCS)

# we try to generate documentation
# that has poper links to the GTK docs

# 1st part of the generator, in Python.
FIRSTGEN = $(srcdir)/h2def.py

# 2nd part of the generator, in Erlang.
BEAMFILES = generator.beam bio.beam

# Intermediate output files containing function definitions.
G_I = g_generated.defs
GDK_I = gdk_generated.defs
GTK_I = gtk_generated.defs
G_W_I = g_white_generated.defs
GDK_W_I = gdk_white_generated.defs
GTK_W_I = gtk_white_generated.defs
INTERMEDIATES = $(G_I) $(GDK_I) $(GTK_I) $(G_W_I) $(GDK_W_I) $(GTK_W_I)

CLEANFILES = $(GEN_INCLUDES) $(BEAMFILES) $(INTERMEDIATES) \
	g_funcs.html g_crap_funcs.txt g_crap_types.txt \
	gdk_funcs.html gdk_crap_funcs.txt gdk_crap_types.txt \
	gtk_funcs.html gtk_crap_funcs.txt gtk_crap_types.txt

# Input files.
STRUCTS = $(srcdir)/../../c_src/gtknode_structs.c
G_INCLUDES = $(wildcard $(GINC)/*/*.h)
GDK_INCLUDES = $(wildcard $(GTKINC)/gdk/*.h)
GDK_PIXBUF_INCLUDES = $(wildcard $(GDKPIXBUFINC)/gdk-pixbuf/*.h)
GTK_INCLUDES = $(wildcard $(GTKINC)/gtk/*.h)
G_WHITE_INCLUDES = \
	$(patsubst %, $(GINC)/%, $(shell cat $(srcdir)/g_white.txt))
GDK_WHITE_INCLUDES = \
	$(patsubst gdk-pixbuf/%, $(GDKPIXBUFINC)/gdk-pixbuf/%, \
		$(patsubst gdk/%, $(GTKINC)/gdk/%, $(shell cat $(srcdir)/gdk_white.txt)))
GTK_WHITE_INCLUDES = \
	$(patsubst %, $(GTKINC)/%, $(shell cat $(srcdir)/gtk_white.txt))
BLACKLISTS = $(srcdir)/g_black.txt $(srcdir)/gdk_black.txt \
	$(srcdir)/gtk_black.txt

all: $(GEN_INCLUDES)

$(GEN_DOCS) $(GEN_INCLUDES): $(BEAMFILES) $(INTERMEDIATES) $(STRUCTS) \
		$(srcdir)/g_black.txt $(srcdir)/gdk_black.txt \
		$(srcdir)/gtk_black.txt
	$(ERL) -noinput -run generator go "@GTKDOCLINKS@" \
		"$(GTKVSN)" "$(STRUCTS)" \
		g $(G_I) $(G_W_I) $(srcdir)/g_black.txt \
		g_generated.h g_funcs.html \
		g_crap_funcs.txt g_crap_types.txt \
		gdk $(GDK_I) $(GDK_W_I) $(srcdir)/gdk_black.txt \
		gdk_generated.h gdk_funcs.html \
		gdk_crap_funcs.txt gdk_crap_types.txt \
		gtk $(GTK_I) $(GTK_W_I) $(srcdir)/gtk_black.txt \
		gtk_generated.h gtk_funcs.html \
		gtk_crap_funcs.txt gtk_crap_types.txt \
		-s erlang halt

$(G_I): $(G_INCLUDES) $(srcdir)/g_predefined.defs $(FIRSTGEN)
	cp $(srcdir)/g_predefined.defs $@
	$(PYTHON) $(FIRSTGEN) $(G_INCLUDES) >> $@
$(GDK_I): $(GDK_INCLUDES) $(GDK_PIXBUF_INCLUDES) $(srcdir)/gdk_predefined.defs $(FIRSTGEN)
	cp $(srcdir)/gdk_predefined.defs $@
	$(PYTHON) $(FIRSTGEN) $(GDK_INCLUDES) $(GDK_PIXBUF_INCLUDES) >> $@
$(GTK_I): $(GTK_INCLUDES) $(srcdir)/gtk_predefined.defs $(FIRSTGEN)
	cp $(srcdir)/gtk_predefined.defs $@
	$(PYTHON) $(FIRSTGEN) $(GTK_INCLUDES) >> $@

$(G_W_I): $(G_WHITE_INCLUDES) $(srcdir)/g_white.txt $(FIRSTGEN)
	$(PYTHON) $(FIRSTGEN) $(G_WHITE_INCLUDES) > $@
$(GDK_W_I): $(GDK_WHITE_INCLUDES) $(srcdir)/gdk_white.txt $(FIRSTGEN)
	$(PYTHON) $(FIRSTGEN) $(GDK_WHITE_INCLUDES) > $@
$(GTK_W_I): $(GTK_WHITE_INCLUDES) $(srcdir)/gtk_white.txt $(FIRSTGEN)
	$(PYTHON) $(FIRSTGEN) $(GTK_WHITE_INCLUDES) > $@

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
