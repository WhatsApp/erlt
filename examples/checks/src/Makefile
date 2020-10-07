ERLS := $(sort $(wildcard *.erlt))

test: erltc
	set -e; $(foreach erl, $(ERLS), ./_test_checks.escript $(erl);)

erltc:
	cd ../../.. && rebar3 compile

.PHONY: test
