## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

CC    ?= gcc
REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

.PHONY: all generate link

SRCS     = $(shell echo c_src/*.c)
OBJS     = $(patsubst c_src/%.c, c_src/%.o, $(SRCS))
ESRCS    = $(shell echo src/*.erl)
BEAMS    = $(patsubst src/%.erl, ebin/%.beam, $(ESRCS))
ERL_ROOT = $(shell escript get_erl_root.es)

all: dbg generate link

generate:
	./generator/generate.sh

link: generator/build/gtknode

generator/build/gtknode: $(OBJS)
	$(CC) \
	$(shell pkg-config --libs libglade-2.0) \
	$(shell pkg-config --libs gmodule-2.0) \
	$(OBJS) \
	-L$(ERL_ROOT)/usr/lib -lei \
	-o $@

c_src/%.o: c_src/%.c
	$(CC) \
	$(shell pkg-config --cflags libglade-2.0) \
	-I $(ERL_ROOT)/usr/include \
	-o $@ -c $<
