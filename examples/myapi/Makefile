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

release:
	@$(REBAR) release

test: rebar-check
	@$(REBAR) eunit

console:
	$(CURDIR)/_build/default/rel/myapi/bin/myapi console

start:
	$(CURDIR)/_build/default/rel/myapi/bin/myapi start | exit 0

stop:
	$(CURDIR)/_build/default/rel/myapi/bin/myapi stop

.PHONY: clean deps compile dialyzer test-cover
