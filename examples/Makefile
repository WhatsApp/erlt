DIRS := dev/src elm-core/src dev_dots/src dev_enum/src

-include ../dir.mk


TEST_DIRS := $(DIRS)


ir: $(TEST_DIRS:%=.ir/%)

# commenting generic iterator out as there's some custom handling depending on a
# dir
#
#test-ir: $(TEST_DIRS:%=.test-ir/%)

test-ir:
	$(MAKE) -C dev/src test-ir
	$(MAKE) -C elm-core/src test-ir
	$(MAKE) -C dev_dots/src test-ir
	$(MAKE) -C dev_enum/src test-ir

update-ir-spec: $(TEST_DIRS:%=.update-ir-spec/%)


.ir/%: %
	$(MAKE) -C $< ir

.test-ir/%: %
	$(MAKE) -C $< test-ir

.update-ir-spec/%: %
	$(MAKE) -C $< update-ir-spec


.PHONY: $(TEST_DIRS)
