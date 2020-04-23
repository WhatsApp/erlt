ERLS := $(sort $(wildcard *.erl))

test:
	set -e; $(foreach erl, $(ERLS), ./_test_checks.escript $(erl);)

.PHONY: test
