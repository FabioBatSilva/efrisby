ERL           := $(shell which erl)
REBAR         := $(shell which rebar)
PLTFILE       := $(CURDIR)/.deps.plt
APP_DEPS      := kernel stdlib eunit tools compiler erts inets
BUILD_PLT_INC := $(shell test -d deps && echo '-r deps')
ERLFLAGS      := -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
DIALYZER_INC  := $(shell test -d include && echo '-I include') $(shell test -d deps && echo '-I deps')

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

clean-plt:
	@rm -rf $(PLTFILE)

distclean: clean-deps

build-plt:
	@$(REBAR) build-plt

$(PLTFILE):
	- dialyzer --build_plt --apps $(APP_DEPS) $(BUILD_PLT_INC) --output_plt $(PLTFILE)

dialyzer: compile $(PLTFILE)
	@dialyzer --fullpath --plt $(PLTFILE) $(DIALYZER_INC) -pa $(CURDIR)/ebin -c src --src

deps:
	@$(REBAR) get-deps compile

compile:
	@$(REBAR) compile

shell:
	@$(ERL) $(ERLFLAGS) erl -eval "application:start(inets)"

test: test_eunit

test_eunit:
	@$(REBAR) eunit

travis: clean distclean deps test

.PHONY: clean deps test compile dialyzer
