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

ifeq ($(CURDIR)/,$(ROOT))
BUILD_DIRS := $(DIRS)
else
THIS_SUBDIR := $(patsubst $(ROOT)%,%,$(CURDIR))
BUILD_DIRS := $(DIRS:%=$(THIS_SUBDIR)/%)
endif


DEPS := $(BUILD_DIRS)


all: deps


clean: $(BUILD_DIRS:%=.clean-dir/%)


.clean-dir/%: %
	$(ECHO_1) "=== cleaning directory $<"
	$(QUIET)$(MAKE) --no-print-directory -C $(ROOT)/$< clean


include $(ROOT)/common.mk
