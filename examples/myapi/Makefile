REBAR := $(shell which rebar3)

ifeq ($(REBAR),)
$(error "rebar3 is not available on your PATH")
endif

clean:
	@rm -rf $(CURDIR)/_build $(CURDIR)/ebin/*.beam

clean-deps:
	@rm -rf $(CURDIR)/deps

distclean: clean-deps

dialyzer:
	@$(REBAR) dialyzer

compile:
	@$(REBAR) compile

release:
	@$(REBAR) release

console:
	$(CURDIR)/_build/default/rel/myapi/bin/myapi console

start:
	$(CURDIR)/_build/default/rel/myapi/bin/myapi start | exit 0

test:
	@$(REBAR) eunit

.PHONY: clean deps compile dialyzer test
