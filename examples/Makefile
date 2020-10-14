DIRS := calc dev dev_dots dev_enum dev_struct doc elm_core erltodo

build:
	rebar3 compile

clean:
	rm -rf _build

test-ir: $(DIRS:%=test-ir/%)

clean-ir:
	find _build -name build -delete || true

test-beam: $(DIRS:%=test-beam/%)

ir: clean-ir build

# The generated .erl files are not actually used in compilation
# but are there for humans to read. Not actually "ir"
test-ir/%: ir
	@# only include .erl files
	diff -r -x sterlang -x '*.defs' -x '*.D' _build/default/lib/$*/build $*/ir-spec

# test `erlt --------> beam -> disassembly`
# matches `erlt -> erl -> beam -> disassembly`
test-beam/%: ir
	rm -rf _build/default/lib/$*/test_ouptput
	mkdir -p _build/default/lib/$*/test_output/from_erl
	mkdir -p _build/default/lib/$*/test_output/from_beam
	../test_util/disassemble.escript --beam _build/default/lib/$*/ebin _build/default/lib/$*/test_output/from_beam
	../test_util/disassemble.escript --erl _build/default/lib/$*/build _build/default/lib/$*/test_output/from_erl
	diff -r _build/default/lib/$*/test_output/from_beam _build/default/lib/$*/test_output/from_erl

update-ir-spec/%: ir
	@rm -rf $*/ir-spec
	@mkdir $*/ir-spec
	cp _build/default/lib/$*/build/*.erl $*/ir-spec

update-ir-spec: $(DIRS:%=update-ir-spec/%)

test-others:
	# TODO: port to remote structs and re-enable
	# $(MAKE) -C dev_module_record/src erl2c-records
	$(MAKE) -C checks/src test

test: test-ir test-beam test-others

.PHONY: build clean ir test-ir update-ir-spec test-beam

