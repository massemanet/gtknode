.PHONY: erl

EBIN = $(shell readlink -e ../../../ebin)
BEAMS = $(patsubst %.erl, $(EBIN)/%.beam, $(shell echo *.erl))

erl: $(BEAMS)

$(EBIN)/%.beam: %.erl
	erlc +debug_info -o $(EBIN) $<
