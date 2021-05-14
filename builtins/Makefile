ERLTC_DIR = ../erltc

ERLTC = $(ERLTC_DIR)/_build/default/bin/erltc
OUTPUT_ERL = $(ERLTC_DIR)/src/erlt_build_statics.erl

BUILD_DIR = build


$(OUTPUT_ERL): $(BUILD_DIR)

$(BUILD_DIR): FORCE
	cd $(ERLTC_DIR) && rebar3 compile
	rm -rf $(BUILD_DIR)
	@# env var is so we don't end up in the chicken-egg situation of requiring built-ins in order to generate built-ins
	SKIP_BUILT_INS=TRUE $(ERLTC) --build compile --src-dir . --build-dir $(BUILD_DIR) -o $(BUILD_DIR) +debug_info +report_warnings +deterministic *.erlt
	escript ./make_statics.escript $(OUTPUT_ERL)
	cd .. && rebar3 fmt --write erltc/src/erlt_build_statics.erl

FORCE:
