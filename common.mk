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
THIS_DIR := $(patsubst $(ROOT)%,%,$(CURDIR))


export VERBOSE ?= 1

# full verbose
QUIET :=
ECHO_1 := @echo
ECHO_2 := @echo

# quiet
ifeq ($(VERBOSE),0)
QUIET := @
ECHO_1 := @true
ECHO_2 := @true
endif

# partially verbose
ifeq ($(VERBOSE),1)
QUIET := @
ECHO_2 := @true
endif


.PHONY: deps deps-clean build-deps print-deps clean-deps


ifeq ($(IS_DEPENDENCY),)
ifneq ($(ERLBUILD),)
erlbuild-compile: print-build-message deps

print-build-message: deps
	$(ECHO_1) "=== building $(THIS_DIR)"
endif
endif


ifeq ($(strip $(DEPS)),)
deps:
deps-clean:
else
deps:
ifeq ($(IS_DEPENDENCY),)
	$(QUIET)$(MAKE) print-deps > .deps.mk.tmp
	$(QUIET)sort -u < .deps.mk.tmp > .deps.mk && rm -f .deps.mk.tmp
	$(QUIET)$(MAKE) build-deps
endif
deps-clean:
ifeq ($(IS_DEPENDENCY),)
	$(QUIET)$(MAKE) clean-deps
endif
endif


PRINT_DEPS := $(DEPS:%=.print-dep/%)
BUILD_DEPS := $(DEPS:%=.build-dep/%)
CLEAN_DEPS := $(DEPS:%=.clean-dep/%)


ifeq ($(MAKECMDGOALS),print-deps)
print-deps: $(PRINT_DEPS)
	@echo build-deps: $(BUILD_DEPS)
	@echo clean-deps: $(CLEAN_DEPS)
ifneq ($(BUILD_DEPS_TARGET),)
ifneq ($(strip $(DEPS)),)
	@echo $(BUILD_DEPS_TARGET): $(BUILD_DEPS)
	@echo "TRANSITIVE_DEPS += $(DEPS)"
endif
endif

.print-dep/%: %
	@$(MAKE) --no-print-directory -C $(ROOT)/$< BUILD_DEPS_TARGET=.build-dep/$< print-deps
endif


ifeq ($(MAKECMDGOALS),build-deps)
.build-dep/%: %
	$(ECHO_1) "=== building dependency $<"
	$(QUIET)$(MAKE) --no-print-directory -C $(ROOT)/$< IS_DEPENDENCY=true

include .deps.mk
endif


ifeq ($(MAKECMDGOALS),clean-deps)
.clean-dep/%: %
	$(ECHO_1) "=== cleaning dependency $<"
	$(QUIET)$(MAKE) --no-print-directory -C $(ROOT)/$< IS_DEPENDENCY=true clean
-include .deps.mk
endif


# treating DEPS and TRANSITIVE_DEPS as PHONY is needed for auto rules above
ifneq ($(strip $(DEPS)),)
.PHONY: $(DEPS)
endif


ifneq ($(TRANSITIVE_DEPS)),)
.PHONY: $(TRANSITIVE_DEPS)
endif
