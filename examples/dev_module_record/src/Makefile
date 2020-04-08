# erlbuild.mk parameters -- see documentation in ../../erlbuild/erlbuild.mk
ERLC_FLAGS := -Wall +debug_info #-Werror

include ../../../erl2.mk

# temporary workaround for 'make test-ir' which needs explicitly ordered list of sources
EXPLICIT_SOURCES := ma_mod01.erl ma_mod02.erl ma_mod03.erl ma_mod04.erl ma_mod05.erl ma_mod06.erl ma_mod07.erl ma_mod08.erl \
		ma_mod09.erl ma_mod10.erl ma_mod11.erl ma_mod12.erl ma_mod13.erl

erl2c-records:
	rm -rf $(EBIN)
	rm -rf $(BUILD_DIR)
	mkdir -p $(EBIN)
	mkdir -p $(BUILD_DIR)
	$(foreach erl, $(EXPLICIT_SOURCES), $(ERLBUILD_ERLC) -pa $(EBIN) -o $(EBIN) --build-dir $(BUILD_DIR) $(erl);)

.PHONY: erl2c-records
