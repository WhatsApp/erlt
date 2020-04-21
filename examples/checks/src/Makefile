ERLS := $(wildcard *.erl)

test:
	$(foreach erl, $(ERLS), ./_test_checks.escript $(erl);)
