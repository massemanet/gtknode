## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

CC    ?= gcc
ERL   ?= $(shell readlink -e $(shell which erl))
REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

.PHONY: all compile ecompile generate ccompile

SRCS = $(shell echo c_src/*.c)
OBJS = $(SRCS:.c=.o)
ESRCS = $(shell echo src/*.erl)
BEAMS = $(ESRCS:src/.erl=.ebin/.beam)
ERL_ROOT = $(shell dirname $(shell dirname $(ERL)))

all: compile

compile: ecompile generate ccompile gtknode

ecompile: $(BEAMS)

ebin/%.beam: src/%.erl
	erlc +debug_info +warnings_as_errors -o ../ebin $<

generate:
	./priv/generator/generate.sh

c_src/%.o: c_src/%.c
	echo $(WHICH_ERL)
	$(CC) \
	$(shell pkg-config --cflags libglade-2.0) \
	-I $(ERL_ROOT)/usr/include \
	-o $@ -c $<

gtknode: $(OBJS)
	$(CC) \
	$(shell pkg-config --libs libglade-2.0) \
	$(shell pkg-config --libs gmodule-2.0) \
	-L$(ERL_ROOT)/usr/lib -lei \
	$(OBJS) -o priv/generator/build/gtknode

