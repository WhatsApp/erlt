## Copyright (c) Facebook, Inc. and its affiliates.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

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
