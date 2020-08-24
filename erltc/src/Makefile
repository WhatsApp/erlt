# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

# targets
ERLTC := ../bin/erltc
ERLT2ETF := ../bin/erlt2etf


all: escripts


clean: clean_escripts


escripts: $(ERLTC) $(ERLT2ETF)


clean_escripts:
	$(QUIET)rm -f $(ERLTC)
	$(QUIET)rm -f $(ERLT2ETF)


include ../../erl1.mk


# NOTE: these rules are below erl1.mk, because we need $(BEAMS) variable to be defined
$(ERLTC): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erltc $(BEAMS)

$(ERLT2ETF): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erlt2etf $(BEAMS)
