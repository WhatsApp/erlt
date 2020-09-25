ERLS := $(sort $(wildcard *.erlt))

test:
	set -e; $(foreach erl, $(ERLS), ./_test_checks.escript $(erl);)

.PHONY: test
