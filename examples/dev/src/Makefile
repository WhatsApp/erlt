# erlbuild.mk parameters -- see documentation in ../../erlbuild/erlbuild.mk
ERLC_FLAGS := -Wall +debug_info +nowarn_unused_function #-Werror

# exclude sources that don't compile
EXCLUDE_SOURCES :=

# temporary workaround for 'make test-ir' which needs explicitly ordered list of sources
EXPLICIT_SOURCES := mod01.erl mod02.erl depends_test.erl mod03.erl mod04.erl string_ffi.erl polymorphic_records.erl lib.erl match.erl import_type.erl import_enum.erl

include ../../../erl2.mk
