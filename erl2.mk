THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOT := $(dir $(THIS_MAKEFILE))


ERLBUILD := $(ROOT)/erlbuild/bin/erlbuild

# NOTE: erl2c can compile both erl2 and erl1
ERLC_COMPILE := $(ROOT)/erl2c/bin/erl2c
ERLC_DEPSCAN := $(ERLC_COMPILE) +makedep2


include $(ROOT)/erlbuild/erlbuild.mk
