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


# erlbuild.mk -- a simple build system for Erlang based on 'erlbuild'
#
# It builds .erl, .yrl, and .xrl files in one src directory. It does this
# correctly, incrementally, and in parallel.
#
# Source files are discovered automatically as *.{erl,xrl,yrl}
#
# To use erlbuild.mk, include or call this file from src/Makefile.
#
# It supports the following optional parameters passed as Makefile variables.
#
#         - VERBOSE          -- verbose output. Set to >= 2 to print exact steps; 0 to disable. Defaults to 1
#
#         - EBIN             -- path to the output directory, where compiled .beam files go
#
#                               Defaults to ../ebin
#
#         - ERLC_FLAGS       -- erlc flags, e.g. -I, -D, -W, -pa, -pz (see 'erlbuild -h' for full list)
#
#                               It's ok to omit -o $(EBIN), -pa $(EBIN), and -I ../include. They will be added
#                               automatically.
#
#         - SOURCES          -- explicitly proivided list of source files to build: .erl, .xrl, .yrl files without directory names
#         - EXCLUDE_SOURCES  -- when sources are discovered automatically, list of source files that should not be built
#
#         - ERLC             -- erlc command. Defaults to 'erlc'.
#         - ERLBUILD         -- erlbuild command. Defaults to 'erlbuild'.
#
#         - BUILD_DIR        -- directory for storing intermediate compilation state
#
#                               Defaults to $(EBIN)/../build
#
#         - DELETE_EXTRA_BEAMS  -- Whether to delete .beams that don't correspond to any of the source files.
#
#                               Defaults to true.
#
#
# Implementation notes:
#
# 1. The essense of building *.erl/yrl/xrl files in one src directory is
#    captured in one step:
#
#        erlbuild $(ERLC_FLAGS) *.{erl,yrl,xrl}
#
#    'erlbuild' generates a makefile and calls make to execute correct,
#    incremental, and parallel build. See erlbuild/README.md for details.


# export parameter to sub-makes called from compile/clean
export VERBOSE ?= 1

DELETE_EXTRA_BEAMS ?= true


# output and build directories
EBIN ?= ../ebin
BUILD_DIR ?= $(dir $(EBIN))build


# build ERLC_FLAGS
#
# append -o $(EBIN) -pa $(EBIN) -I ../include unless already present
#
# NOTE: using combination of subst() & ifeq() instead of filter(), because filter doesn't work with spaces
ifeq ($(subst -o $(EBIN),,$(ERLC_FLAGS)),$(ERLC_FLAGS))
ERLC_FLAGS += -o $(EBIN)
endif

ifeq ($(subst -pa $(EBIN),,$(ERLC_FLAGS)),$(ERLC_FLAGS))
ERLC_FLAGS += -pa $(EBIN)
endif

ifeq ($(subst -I ../include,,$(ERLC_FLAGS)),$(ERLC_FLAGS))
ERLC_FLAGS += -I ../include
endif


# build ERLBUILD_FLAGS
ERLBUILD_FLAGS :=

ifneq ($(VERBOSE),0)
ERLBUILD_FLAGS += -v$(VERBOSE)
endif

ERLBUILD_FLAGS += --build-dir $(BUILD_DIR)

ifdef ERLC
ERLBUILD_FLAGS += --erlc $(ERLC)
endif


ERLBUILD_FLAGS += $(ERLC_FLAGS)


# other input parameters

ERLBUILD ?= erlbuild
EXCLUDE_SOURCES ?=


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


# discover sources unless explicitly specified in SOURCES
#
# NOTE: this is not the same as SOURCES ?=, because we want := (i.e. simple
# expansion), and ?= is equivalent to = (i.e. recursive expansion)
ifndef SOURCES
SOURCES := $(filter-out $(EXCLUDE_SOURCES), $(wildcard *.erl *.xrl *.yrl))
endif


# full list of .erl and .beam files, incuding those generated from .xrl and .yrl files
ERLS        := $(addsuffix .erl,$(basename $(SOURCES)))
BEAMS       := $(addprefix $(EBIN)/,$(ERLS:.erl=.beam))


ERLBUILD_MK := $(BUILD_DIR)/erlbuild.mk


all: compile delete_extra_beams


clean: clean_beams


$(BEAMS): compile


# NOTE: checking that all source files are present just in case
compile: $(SOURCES)
	$(ECHO_2) "=== erlbuild.mk: $@"
	$(QUIET)$(ERLBUILD) --gen-only --makefile $(ERLBUILD_MK) $(ERLBUILD_FLAGS) $(SOURCES)
	$(QUIET)$(MAKE) --no-print-directory -f $(ERLBUILD_MK)


clean_beams:
ifneq ($(wildcard $(ERLBUILD_MK)),)
	$(QUIET)$(MAKE) --no-print-directory -f $(ERLBUILD_MK) clean
	$(QUIET)rm -f $(ERLBUILD_MK)
endif


ifeq ($(DELETE_EXTRA_BEAMS),true)
# calculating the list of extra .beam files to be deleted
EXISTING_BEAMS := $(wildcard $(EBIN)/*.beam)
EXTRA_BEAMS := $(filter-out $(BEAMS),$(EXISTING_BEAMS))
endif


delete_extra_beams: compile
ifneq ($(EXTRA_BEAMS),)
	$(ECHO_2) "=== erlbuild.mk: $@: $(EXTRA_BEAMS)"
	$(QUIET)rm -f $(EXTRA_BEAMS)
endif


.PHONY: all compile clean clean_beams delete_extra_beams
