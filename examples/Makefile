DIRS := dev/src doc/src elm-core/src dev_dots/src dev_enum/src dev_struct/src calc/src

-include ../dir.mk


TEST_DIRS := $(DIRS)


ir: $(TEST_DIRS:%=.ir/%)

# commenting generic iterator out as there's some custom handling depending on a
# dir
#
#test-ir: $(TEST_DIRS:%=.test-ir/%)

test-ir:
	$(MAKE) -C dev/src test-ir
	$(MAKE) -C doc/src test-ir
	$(MAKE) -C elm-core/src test-ir
	$(MAKE) -C dev_dots/src test-ir
	$(MAKE) -C dev_enum/src test-ir
	$(MAKE) -C dev_struct/src test-ir
	$(MAKE) -C calc/src test-ir

test-others:
	$(MAKE) -C dev_module_record/src erl2c-records
	$(MAKE) -C checks/src test

update-ir-spec: $(TEST_DIRS:%=.update-ir-spec/%)


.ir/%: %
	$(MAKE) -C $< ir

.test-ir/%: %
	$(MAKE) -C $< test-ir

.update-ir-spec/%: %
	$(MAKE) -C $< update-ir-spec

test:
	$(MAKE)
	$(MAKE) test-ir
	$(MAKE) test-others

.PHONY: $(TEST_DIRS) test test-others
