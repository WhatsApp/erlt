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


# erlbuild.template.mk -- template for incremental builds of ErlT
# low-level build system for Erlang
#
# This file should not be used directly, but instead
# is an implementation detail of erlt. See ./erlt_build.erl for details.
#
# Input parameters. MUST be defined above upon applyng this template.
#
#     input parameters:
#
#         - SOURCES -- .erlt files
#
#         - ERLC
#         - ERLBUILD_ERLC
#
#         - ERLC_FLAGS
#         - EBIN
#         - BUILD_DIR
#
# Caveats:
#
#    - we do not currently handle extra things unconventional for make. For
#      example, changing compiler flags (or the compiler itself) does not
#      trigger a rebuild
#    - similarly, removing a dependency doesn't trigger a rebuild -- see note
#      about phony target below


# Locally used variables
#
# NOTE: there should only one Makefile when invoked properly

THIS_MAKEFILE := $(MAKEFILE_LIST)

ifneq ($(words $(THIS_MAKEFILE)),1)
$(error "erlbuild.mk can not be included from another makefile")
endif

# check that parameters were passed correctly
ifneq ($(origin SOURCES),file)
$(error "missing SOURCES parameter")
endif
ifndef SOURCES
$(error "missing SOURCES parameter")
endif
ifneq ($(origin ERLC),file)
$(error "missing ERLC parameter")
endif
ifneq ($(origin ERLBUILD_ERLC),file)
$(error "missing ERLBUILD_ERLC parameter")
endif
ifneq ($(origin ERLC_FLAGS),file)
$(error "missing ERLC_FLAGS parameter")
endif
ifneq ($(origin EBIN),file)
$(error "missing EBIN parameter")
endif
ifneq ($(origin BUILD_DIR),file)
$(error "missing BUILD_DIR parameter")
endif

# quiet by default
VERBOSE ?= 0


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


ERLS := $(filter %.erlt,$(SOURCES))

# .beam files
BEAMS := $(addprefix $(EBIN)/,$(ERLS:.erlt=.beam))

# compile-time dependencies
DEPFILES := $(ERLS:%.erlt=$(BUILD_DIR)/%.d)

# declaration files
DEFS := $(ERLS:%.erlt=$(BUILD_DIR)/%.defs)

# NOTE: Erlang compilation and dependency scans are executed as recursive make
# invocations, because I couldn't figure out how to trick "make compile" into
# reloading the updated dependency graph during the same run. In theory it
# maybe possible, but clearly separating these build steps is simpler and much
# more reliable anyway.
#
# also, although we support generating .erl from .xrl, .yrl inline when running
# scan, it is better to run it as a separate explicit step before
# compilation
all: compile_erls


# NOTE: using -C $(CURDIR) to make it easier to reproduce individual steps
# by copy-pasting the command from the output listing
compile_erls:
	$(QUIET)$(MAKE) -C $(CURDIR) --no-print-directory -f $(THIS_MAKEFILE) scan
	$(QUIET)$(MAKE) -C $(CURDIR) --no-print-directory -f $(THIS_MAKEFILE) compile


$(EBIN):
	$(QUIET)mkdir -p $@

$(BUILD_DIR):
	$(QUIET)mkdir -p $@


ifeq ($(MAKECMDGOALS),compile)

# NOTE: checking that all .erl files are present and up-to-date
#
# NOTE: using @: action to silence "Nothing to be done for `...' message
compile: do_compile $(ERLS)
	@:

do_compile: $(BEAMS)

$(EBIN)/%.beam: %.erlt $(ERLBUILD_ERLC) | $(EBIN)
	$(ECHO_1) "compile $<"
	$(QUIET)$(ERLBUILD_ERLC) --build-phase compile --build-dir $(BUILD_DIR) $(ERLC_FLAGS) $<

# using explicit include, because .d have to be built and rebuilt explicitly by
# "scan" and we do not tolerate failures
include $(DEPFILES)

endif  # compile


# scan .erl files, one at a time and only when changed, to recalculate the
# order in which they need to be compiled
#
# for details on how this works: http://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
ifeq ($(MAKECMDGOALS),scan)

# NOTE: checking that all .erl files are present and up-to-date
#
# NOTE: using @: action to silence "Nothing to be done for `...' message
scan: do_scan $(ERLS)
	@:

do_scan: $(DEPFILES)


# NOTE: we need $(EBIN) directory to be present, because otherwise
# -include_lib("<this app>/...") will fail on a fresh build
#
# NOTE: we want phony targets (-MP) to gracefully handle the case when .hrl /
# .beam dependencies were purposefully deleted.
#
# NOTE: this phase generates not only .d files,  but **also** .defs files.
# We are not telling Make about this because it is hard to represent group targets
# targets on old versions of Make. This missing dependency information
# in our DAG is harmless because no rules depend on .defs yet.
#
# TODO: strictly speaking, missing dependenices should trigger .erl
# recompilation, but make doesn't allow to do this easily. Not handling this is
# still relatively safe, because if .erl files still depend on deleted
# dependencies, this would trigger compilation error in non-incremental CI
# builds.

$(BUILD_DIR)/%.d: %.erlt $(ERLBUILD_ERLC) | $(BUILD_DIR) $(EBIN)
	$(ECHO_1) "scan $<"
	$(QUIET)$(ERLBUILD_ERLC) --build-phase scan --build-dir $(BUILD_DIR) -M -MP -MF $@ $(ERLC_FLAGS) $<


# NOTE: including $(DEPFILES) to trigger rebuild of .d makefiles (i.e.
# dependency graph) on change of their dependencies (included .hrl files and
# parse_transform .beam files)
#
# using wildcard to avoid failing on non-existent files. They are legitimately
# missing before the first build cycle.
#
# XXX: instead of using wildcard, we could have erlbuild create empty .d files
# with 0 timestamp, if they are missing. (This could also potentially help with
# combining the scan and compile phases by having make automatically
# reload the included .d files between the phases. We'll probably need to make
# .beam depend on .d files for this to work.)

include $(wildcard $(DEPFILES))

endif  # scan

# XXX: should we simply rm -rf $(BUILD_DIR) here? It comes down to whether the
# build directory could used for other build modules or other functionality
clean:
	$(QUIET)rm -f $(BEAMS)
	$(QUIET)rm -f $(DEPFILES)
	$(QUIET)rm -f $(DEFS)
	$(QUIET)rmdir $(BUILD_DIR) 2>/dev/null || true

.PHONY: all clean compile_erls \
	compile scan \
        do_compile do_scan
