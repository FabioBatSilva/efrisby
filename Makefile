BASEDIR := $(shell pwd)
ERL     := $(shell which erl)
REBAR   := $(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on PATH")
endif

ifeq ($(ERL),)
$(error "Erlang must be available on PATH")
endif

clean:
	@rm -rf $(BASEDIR)/.rebar/erlcinfo $(BASEDIR)/ebin/*.beam $(BASEDIR)/.eunit $(BASEDIR)/.rebar

clean-deps:
	@rm -rf $(BASEDIR)/deps

distclean: clean-deps

deps:
	@$(REBAR) get-deps compile

compile:
	@$(REBAR) compile

test: test_eunit

test_eunit:
	@$(REBAR) eunit

travis: clean distclean

.PHONY: clean deps test test_eunit test_inttest
