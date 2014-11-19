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
		 --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

compile:
	$(REBAR) skip_deps=true compile

test: all
	./test.sh

clean:
	$(REBAR) clean skip_deps=true
