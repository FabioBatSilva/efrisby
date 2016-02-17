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

shell:
	@$(REBAR) shell

test:
	@$(REBAR) eunit

test-cover:
	@$(REBAR) eunit --cover

travis: clean distclean dialyzer compile test-cover

.PHONY: clean deps compile dialyzer test-cover
