ERL           := $(shell which erl)
REBAR         := $(shell which rebar)
ERLFLAGS      := -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

ifeq ($(REBAR),)
$(error "Rebar not available on PATH")
endif

ifeq ($(ERL),)
$(error "Erlang must be available on PATH")
endif

clean:
	@rm -rf $(CURDIR)/.rebar/erlcinfo $(CURDIR)/ebin/*.beam $(CURDIR)/.eunit $(CURDIR)/.rebar

clean-deps:
	@rm -rf $(CURDIR)/deps

distclean: clean-deps

deps:
	@$(REBAR) get-deps compile

compile:
	@$(REBAR) compile

reltool:
	cd $(CURDIR)/rel && rebar compile generate; cd $(CURDIR)

console:
	$(CURDIR)/rel/myapi/bin/myapi console

start:
	$(CURDIR)/rel/myapi/bin/myapi start | exit 0

test: start
	@$(REBAR) eunit

travis: clean distclean deps test

.PHONY: clean deps test compile reltool console
