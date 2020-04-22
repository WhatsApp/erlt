THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT := $(dir $(THIS_MAKEFILE))


ERLBUILD := $(ROOT)/erlbuild/bin/erlbuild

# NOTE: erl2c can compile both erl2 and erl1
ERLBUILD_ERLC := $(ROOT)/erl2c/bin/erl2c


DEPS += erlbuild/src erl2c/src


include $(ROOT)/erlbuild/erlbuild.mk
include $(ROOT)/common.mk



# Helper targets for developement and hacking

IR_DIR = $(BUILD_DIR)/ir
IR_SPEC_DIR = ../ir-spec

MLS := $(sort $(wildcard $(BUILD_DIR)/ocaml/*.ml) $(wildcard $(BUILD_DIR)/ocaml/*.mli))

copy-ocaml:
	rm -f $(IR_DIR)/*.mli
	rm -f $(IR_DIR)/*.ml
	mkdir -p $(IR_DIR)
	$(foreach ml, $(MLS), cp $(ml) $(IR_DIR)/$(notdir $(ml));)

p:
	rm -f $(IR_DIR)/*.P
	mkdir -p $(IR_DIR)
	$(foreach erl, $(EXPLICIT_SOURCES), $(ERLBUILD_ERLC) -P -o $(IR_DIR) --build-dir $(BUILD_DIR) $(erl);)

ir: copy-ocaml p

test-ir: ir
	diff -r $(IR_DIR) $(IR_SPEC_DIR)

update-ir-spec: ir
	rm -rf $(IR_SPEC_DIR)
	cp -r $(IR_DIR) $(IR_SPEC_DIR)

.PHONY: copy-ocaml p ir test-ir update-ir-spec
