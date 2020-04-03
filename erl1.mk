THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT := $(dir $(THIS_MAKEFILE))


ERLBUILD := $(ROOT)/erlbuild/bin/erlbuild


include $(ROOT)/erlbuild/erlbuild.mk
