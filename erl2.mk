THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT := $(dir $(THIS_MAKEFILE))


ERLBUILD := $(ROOT)/erlbuild/bin/erlbuild

# NOTE: erl2c can compile both erl2 and erl1
ERLC_COMPILE := $(ROOT)/erl2c/bin/erl2c
ERLC_DEPSCAN := $(ERLC_COMPILE) +makedep2


include $(ROOT)/erlbuild/erlbuild.mk

# Helper targets for developement and hacking

IR_DIR = $(BUILD_DIR)/ir
IR_SPEC_DIR = ../ir-spec

MLS := $(sort $(wildcard $(BUILD_DIR)/ocaml/*.ml) $(wildcard $(BUILD_DIR)/ocaml/*.mli))

ocamlformat:
	rm -f $(IR_DIR)/*.mli
	rm -f $(IR_DIR)/*.ml
	mkdir -p $(IR_DIR)
	$(foreach ml, $(MLS), ocamlformat --enable-outside-detected-project $(ml) -o $(IR_DIR)/$(notdir $(ml));)

p:
	rm -f $(IR_DIR)/*.P
	mkdir -p $(IR_DIR)
	$(foreach erl, $(SOURCES), $(ERLC_COMPILE) -P -o $(IR_DIR) $(erl);)

ir: ocamlformat p

test-ir: ir
	diff -r $(BUILD_DIR)/ir ../ir-spec/

update-ir-spec: ir
	rm -rf $(IR_SPEC_DIR)
	cp -r $(IR_DIR) $(IR_SPEC_DIR)

.PHONY: ocamlformat p ir test-ir update-ir-spec
