# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

# targets
ERL2C := ../bin/erl2c
ERL2OCAML := ../bin/erl2ocaml
ERL2ETF := ../bin/erl2etf


all: escripts


clean: clean_escripts


escripts: $(ERL2C) $(ERL2OCAML) $(ERL2ETF)


clean_escripts:
	$(QUIET)rm -f $(ERL2C)
	$(QUIET)rm -f $(ERL2OCAML)
	$(QUIET)rm -f $(ERL2_AST_ETF)


include ../../erl1.mk


# NOTE: these rules are below erl1.mk, because we need $(BEAMS) variable to be defined
$(ERL2C): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2c $(BEAMS)

$(ERL2OCAML): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2ocaml $(BEAMS)

$(ERL2ETF): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2etf $(BEAMS)
