REBAR := $(shell which rebar3)

rebar-check:
ifndef REBAR
	@echo "rebar3 is not available on your PATH !!"
	@echo
	@echo "wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3"
	@echo "mv rebar3 /usr/local/bin/rebar3"
	exit 1
endif

clean:
	@rm -rf $(CURDIR)/_build $(CURDIR)/ebin/*.beam

clean-deps:
	@rm -rf $(CURDIR)/deps

distclean: clean-deps

dialyzer: rebar-check
	@$(REBAR) dialyzer

compile: rebar-check
	@$(REBAR) compile

shell: rebar-check
	@$(REBAR) shell

test: rebar-check
	@$(REBAR) eunit

test-cover: rebar-check
	@$(REBAR) eunit --cover

test-cover-html: rebar-check
	@$(REBAR) do eunit --cover , cover

travis: clean distclean dialyzer compile test-cover

.PHONY: clean deps compile dialyzer test-cover test-cover-html
