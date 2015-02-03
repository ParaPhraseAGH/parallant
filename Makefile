ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

ERL=erl

REBAR=./rebar

.PHONY: all compile deps clean test skel update-deps

all: deps compile

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

skel:
	make clean
	$(REBAR) compile -D skel

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
		 --apps $(DEPS) -r deps -nn

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin -nn

compile:
	$(REBAR) skip_deps=true compile

ct:
	$(REBAR) ct skip_deps=true suites=parallant

test: all
	./test.sh
	make ct
	make dialyzer

clean:
	$(REBAR) clean skip_deps=true
