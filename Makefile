ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

ERL=erl

REBAR=./rebar

.PHONY: all compile deps clean test update-deps

all: deps compile

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

test: all
	./test.sh

clean:
	$(REBAR) clean skip_deps=true
