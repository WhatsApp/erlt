ERLTC=../../_build/default/bin/erltc
BUILD=build
GENERATED=../src/erlt_build_statics.erl

$(GENERATED): $(BUILD)
	escript ./make_statics.escript
	cd ../.. && rebar3 fmt --write

$(BUILD): Makefile ./make_statics.escript $(ERLTC) $(wildcard *.erlt)
	rm -rf $(BUILD) $(GENERATED)
	@# env var is so we don't end up in the chicken-egg situation of requiring built-ins in order to generate built-ins
	SKIP_BUILT_INS=TRUE $(ERLTC) --build compile --src-dir . --build-dir $(BUILD) -o $(BUILD) +debug_info +report_warnings +deterministic *.erlt

*.erlt:

$(ERLTC):
	cd ../.. && rebar3 compile

test: $(GENERATED)
	# check that changes to built-ins have been committed
	git diff -w --exit-code $(GENERATED)

.PHONY: clean test

clean:
	rm -rf $(BUILD)

.PHONY: test clean
