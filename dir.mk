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
