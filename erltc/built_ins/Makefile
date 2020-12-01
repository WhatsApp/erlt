ERLTC=../../_build/default/bin/erltc
OUT=build

../src/erlt_build_statics.erl: $(OUT)

$(OUT): FORCE
	cd ../.. && rebar3 compile
	mkdir -p ../priv
	make -C ../../sterlang
	rm -rf $(OUT)
	# generate type checker and put it in the priv dir so erltc can find it
	@# env var is so we don't end up in the chicken-egg situation of requiring built-ins in order to generate built-ins
	SKIP_BUILT_INS=TRUE $(ERLTC) --build compile --src-dir . --build-dir $(OUT) -o $(OUT) +debug_info +report_warnings +deterministic *.erlt
	escript ./make_statics.escript
	cd ../.. && rebar3 fmt --write

FORCE:
