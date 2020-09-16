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


ERLBUILD := $(ROOT)/_build/default/bin/erlbuild

# NOTE: erltc can compile both erlt and erl1
ERLBUILD_ERLC := $(ROOT)/_build/default/bin/erltc


include $(ROOT)/erlbuild/erlbuild.mk
include $(ROOT)/common.mk



# Helper targets for developement and hacking

ERLBUILD_INPUTS := $(filter-out %_parse.erl, $(wildcard $(ROOT)/erlbuild/src/*.erl $(ROOT)/erltc/src/*.erl $(ROOT)/erltc/src/*.yrl))
$(ERLBUILD) : $(ERLBUILD_INPUTS)
	$(ECHO_1) "=== rebar3 compile"
	$(QUIET)cd $(ROOT); rebar3 compile

IR_DIR = $(BUILD_DIR)/ir
IR_SPEC_DIR = ../ir-spec

p:
	rm -f $(IR_DIR)/*.P
	mkdir -p $(IR_DIR)
	# xxx_lexer.erl and xxx_parser.erl are non-hermetic generated stuff, so filtering them out
	$(foreach erl, $(filter-out %_lexer.erl %_parser.erl ,$(ERLS)), $(ERLBUILD_ERLC) -P -o $(IR_DIR) --build-dir $(BUILD_DIR) $(erl);)

ir: p

test-ir: ir
	diff -r $(IR_DIR) $(IR_SPEC_DIR)

update-ir-spec: ir
	rm -rf $(IR_SPEC_DIR)
	cp -r $(IR_DIR) $(IR_SPEC_DIR)

.PHONY: p ir test-ir update-ir-spec
