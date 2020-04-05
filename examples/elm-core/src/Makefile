# erlbuild.mk parameters -- see documentation in ../../erlbuild/erlbuild.mk
ERLC_FLAGS := -Wall +debug_info #-Werror

# exclude sources that don't compile
EXCLUDE_SOURCES := dict.erl dict_ffi.erl

# temporary workaround for 'make test-ir' which needs explicitly ordered list of sources
EXPLICIT_SOURCES := basics.erl tuple.erl maybe.erl result.erl list.erl #dict_ffi.erl dict.erl

include ../../../erl2.mk
