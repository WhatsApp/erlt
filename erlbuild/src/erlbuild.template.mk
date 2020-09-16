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


# erlbuild.template.mk -- template for 'erlbuild', a correct and simple
# low-level build system for Erlang
#
# It is meant to be called only by 'erlbuild' command. See erlbuild.erl for
# details.
#
# Input parameters. MUST be defined above upon applyng this template.
#
#     input parameters:
#
#         - SOURCES -- .erl, .xrl, .yrl files
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


ERL_SOURCES := $(filter %.erl,$(SOURCES))
XRL_SOURCES := $(filter %.xrl,$(SOURCES))
YRL_SOURCES := $(filter %.yrl,$(SOURCES))

LEEX_ERLS := $(XRL_SOURCES:.xrl=.erl)
YECC_ERLS := $(YRL_SOURCES:.yrl=.erl)
GENERATED_ERLS := $(LEEX_ERLS) $(YECC_ERLS)

ERLS := $(ERL_SOURCES) $(GENERATED_ERLS)

# .beam files
BEAMS := $(addprefix $(EBIN)/,$(ERLS:.erl=.beam))

# compile-time dependencies
DEPFILES := $(ERLS:%.erl=$(BUILD_DIR)/%.d)

# declaration files
DEFS := $(ERLS:%.erl=$(BUILD_DIR)/%.defs)

ENABLE_GENERATE := true


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
compile_erls: generate_erls
	$(QUIET)$(MAKE) -C $(CURDIR) --no-print-directory -f $(THIS_MAKEFILE) scan
	$(QUIET)$(MAKE) -C $(CURDIR) --no-print-directory -f $(THIS_MAKEFILE) compile


generate_erls: generate


$(EBIN):
	$(QUIET)mkdir -p $@

$(BUILD_DIR):
	$(QUIET)mkdir -p $@


ifeq ($(MAKECMDGOALS),compile)

# prevent implicit generation of .erl from .xrl/yrl
ENABLE_GENERATE :=

# NOTE: checking that all .erl files are present and up-to-date (some of them may be generated from .xrl/.yrl)
#
# NOTE: using @: action to silence "Nothing to be done for `...' message
compile: do_compile $(ERLS)
	@:

do_compile: $(BEAMS)

$(EBIN)/%.beam: %.erl $(ERLBUILD_ERLC) | $(EBIN)
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

# prevent implicit generation of .erl from .xrl/yrl
ENABLE_GENERATE :=

# NOTE: checking that all .erl files are present and up-to-date (some of them may be generated from .xrl/.yrl)
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
# TODO: strictly speaking, missing dependenices should trigger .erl
# recompilation, but make doesn't allow to do this easily. Not handling this is
# still relatively safe, because if .erl files still depend on deleted
# dependencies, this would trigger compilation error in non-incremental CI
# builds.

$(BUILD_DIR)/%.d: %.erl $(ERLBUILD_ERLC) | $(BUILD_DIR) $(EBIN)
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


ifdef ENABLE_GENERATE
# NOTE: using @: action to silence "Nothing to be done for `...' message
generate: do_generate
	@:

do_generate: $(GENERATED_ERLS)


$(LEEX_ERLS): %.erl: %.xrl
	$(ECHO_1) "generating .erl from $<"
	$(QUIET)$(ERLC) $<

$(YECC_ERLS): %.erl: %.yrl
	$(ECHO_1) "generating .erl from $<"
	$(QUIET)$(ERLC) $<

endif  # ENABLE_GENERATE


# XXX: should we simply rm -rf $(BUILD_DIR) here? It comes down to whether the
# build directory could used for other build modules or other functionality
clean:
	$(QUIET)rm -f $(BEAMS)
	$(QUIET)rm -f $(DEPFILES)
	$(QUIET)rm -f $(DEFS)
ifneq ($(strip $(GENERATED_ERLS)),)
	$(QUIET)rm -f $(GENERATED_ERLS)
endif
	$(QUIET)rmdir $(BUILD_DIR) 2>/dev/null || true


# One of the supported modes is when generation of .erl from .xrl/.yrl files
# runs inline during compile or scan. In such case, they are triggered
# by implicit rules. Adding such generated .erl files as .SECONDARY ensures
# that they don't get automatically deleted by make. Specifically, if they are
# automatically generated during scan phase, they need to be kept
# around through compile phase.
#
# NOTE: preventing empty .SECONDARY as it badly interferes with the dependency
# graph which has phony targets in it (see above). Basically, if .SECONDARY is
# empty, updating any dependency with a phony target would not trigger a
# rebuild
#
# NOTE: being extra cautious here and using $(strip ...) to remove whitespace
ifneq ($(strip $(GENERATED_ERLS)),)
.SECONDARY: $(GENERATED_ERLS)
endif


.PHONY: all clean generate_erls compile_erls \
	generate compile scan \
        do_generate do_compile do_scan
