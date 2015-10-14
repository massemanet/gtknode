## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

.PHONY: all

INPUTS  = gdk_black.txt gdk_white.txt gdk_predefined.defs \
            g_black.txt   g_white.txt   g_predefined.defs \
          gtk_black.txt gtk_white.txt gtk_predefined.defs \
          generate.sh h2def.py ../../ebin/generator.beam
OUTPUTS = $(patsubst %, build/%, gdk_generated.h g_generated.h gtk_generated.h)

all: build $(OUTPUTS)

build:
	mkdir build

$(OUTPUTS): $(INPUTS)
	./generate.sh
