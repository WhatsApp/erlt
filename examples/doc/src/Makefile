# erlbuild.mk parameters -- see documentation in ../../erlbuild/erlbuild.mk
ERLC_FLAGS := -Wall +debug_info #-Werror

# exclude sources that don't compile
EXCLUDE_SOURCES :=

# temporary workaround for 'make test-ir' which needs explicitly ordered list of sources
EXPLICIT_SOURCES := st_mapping.erl

include ../../../erl2.mk
