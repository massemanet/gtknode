## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

.PHONY: all

INPUTS   = gdk_black.txt gdk_white.txt g_predefined.defs gtk_predefined.defs \
           g_white.txt g_black.txt gdk_predefined.defs generate.sh \
           gtk_black.txt gtk_white.txt h2def.py ../ebin/generator.beam
OUTPUTS  = gdk_generated.h g_generated.h gtk_generated.h

all: $(patsubst %, build/%, $(OUTPUTS))

$(OUTPUTS): $(INPUTS)
	./generator/generate.sh
