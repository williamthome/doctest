all:

.PHONY: test

test:
	rebar3 as test do compile, ct, eunit
