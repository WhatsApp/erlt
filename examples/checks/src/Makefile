ERLS := $(sort $(wildcard *.erlt))

export PATH := ../../../_build/default/bin:$(PATH)

test: erltc
	./_test_checks.escript $(ERLS)

erltc:
	cd ../../.. && rebar3 compile

.PHONY: test
