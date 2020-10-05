DIRS := calc dev dev_dots dev_enum dev_struct doc elm_core erltodo

build:
	rebar3 erlt compile

clean:
	rm -rf _build

test-ir: $(DIRS:%=test-ir/%)

ir: $(DIRS:%=.ir/%)

.ir/%:
	rm -rf _build/ir/lib/$*
	rebar3 as ir erlt compile $*

test-ir/%: .ir/%
	diff -x '*.beam' -x '*.app' -r _build/ir/lib/$*/ebin $*/ir-spec

update-ir-spec/%: .ir/%
	rm -rf $*/ir-spec
	mkdir -p $*/ir-spec
	cp -r _build/ir/lib/$*/ebin/*.P $*/ir-spec

update-ir-spec: $(DIRS:%=update-ir-spec/%)

test-others:
	# TODO: port to remote structs and re-enable
	# $(MAKE) -C dev_module_record/src erl2c-records
	$(MAKE) -C checks/src test

test: test-ir test-others

.PHONY: build clean ir test-ir update-ir-spec

