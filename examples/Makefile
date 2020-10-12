DIRS := calc dev dev_dots dev_enum dev_struct doc elm_core erltodo

build:
	rebar3 compile

clean:
	rm -rf _build

test-ir: $(DIRS:%=test-ir/%)

# The generated .erl files are not actually used in compilation
# but are there for humans to read. Not actually "ir"
ir: $(DIRS:%=ir/%)

ir/%: clean build
	@mkdir -p _build/default/lib/$*/test_output 
	../test_util/beam_to_erl.escript _build/default/lib/$*/ebin _build/default/lib/$*/test_output/

test-ir/%: ir/%
	diff -r _build/default/lib/$*/test_output $*/ir-spec

update-ir-spec/%: ir
	@rm -rf $*/ir-spec
	cp -r _build/default/lib/$*/test_output $*/ir-spec

update-ir-spec: $(DIRS:%=update-ir-spec/%)

test-others:
	# TODO: port to remote structs and re-enable
	# $(MAKE) -C dev_module_record/src erl2c-records
	$(MAKE) -C checks/src test

test: test-ir test-others

.PHONY: build clean ir test-ir update-ir-spec

