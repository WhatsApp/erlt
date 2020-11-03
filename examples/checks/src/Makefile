ERLS := $(sort $(wildcard *.erlt))

export PATH := ../../../_build/default/bin:$(PATH)

test: erlt
	./_test_checks.escript $(ERLS)

erlt:
	cd ../../.. && rebar3 compile

.PHONY: test
