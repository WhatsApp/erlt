# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

# targets
ERLTC := ../bin/erltc


all: escripts


clean: clean_escripts


escripts: $(ERLTC)


clean_escripts:
	$(QUIET)rm -f $(ERLTC)


include ../../erl1.mk


# NOTE: these rules are below erl1.mk, because we need $(BEAMS) variable to be defined
$(ERLTC): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erltc $(BEAMS)
