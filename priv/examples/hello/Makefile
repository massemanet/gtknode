.PHONY: all erl c

EBIN = $(shell readlink -e ../../../ebin)
BEAMS = $(patsubst %.erl, $(EBIN)/%.beam, $(shell echo *.erl))
EXES = $(patsubst %.c, %.out, $(shell echo *.c))

all: erl c

c: $(EXES)

%.out: %.o
	cc $< $(shell pkg-config --libs gtk+-2.0) -o $@

%.o: %.c
	cc $(shell pkg-config --cflags gtk+-2.0) -o $@ -c $<

erl: $(BEAMS)

$(EBIN)/%.beam: %.erl
	erlc +debug_info -o $(EBIN) $<
