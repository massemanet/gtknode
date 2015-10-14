## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

.PHONY: all examples compile cnode clean test
.PHONY: release release_patch release_minor release_major
.PHONY: eunit xref dialyze

all: compile

examples:
	make -C priv/examples

compile:
	@$(REBAR) compile skip_deps=true

cnode:
	make -C priv

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: eunit xref dialyze

#############################################################################
## release stuff

release_major: test
	./bin/release.sh major

release_minor: test
	./bin/release.sh minor

release_patch: test
	./bin/release.sh patch

release: release_patch

#############################################################################
## testing

eunit: compile
	@$(REBAR) eunit skip_deps=true

xref: compile
	@$(REBAR) xref skip_deps=true

dialyze: compile ~/.dialyzer_plt
	$(shell [ -d .eunit ] && rm -rf .eunit)
	dialyzer ebin -nn --no_spec --plt ~/.dialyzer_plt

~/.dialyzer_plt:
	-dialyzer --output_plt ${@} --build_plt \
           --apps erts kernel stdlib crypto ssl public_key inets \
                  eunit xmerl compiler runtime_tools mnesia syntax_tools
