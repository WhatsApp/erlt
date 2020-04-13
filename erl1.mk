THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT := $(dir $(THIS_MAKEFILE))


ERLBUILD := $(ROOT)/erlbuild/bin/erlbuild


DEPS += erlbuild/src


include $(ROOT)/erlbuild/erlbuild.mk
include $(ROOT)/common.mk
