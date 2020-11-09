ERLTC=../../_build/default/bin/erltc
OUT=build

$(OUT): Makefile $(ERLTC) $(wildcard *.erlt)
	rm -rf $(OUT)
	@# env var is so we don't end up in the chicken-egg situation of requiring built-ins in order to generate built-ins
	SKIP_BUILT_INS=TRUE $(ERLTC) --build compile --src-dir . --build-dir $(OUT) -o $(OUT) +debug_info +report_warnings *.erlt
	cd ../.. && rebar3 compile

*.erlt:

$(ERLTC):
	cd ../.. && rebar3 compile

clean:
	rm -rf $(OUT)

.PHONY: clean
