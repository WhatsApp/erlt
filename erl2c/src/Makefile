# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

# targets
ERL2C := ../bin/erl2c
ERL2OCAML := ../bin/erl2ocaml


all: escripts


clean: clean_escripts


escripts: $(ERL2C) $(ERL2OCAML)


clean_escripts:
	$(QUIET)rm -f $(ERL2C)
	$(QUIET)rm -f $(ERL2OCAML)


include ../../erl1.mk


# NOTE: these rules are below erl1.mk, because we need $(BEAMS) variable to be defined
$(ERL2C): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2c $(BEAMS)

$(ERL2OCAML): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2ocaml $(BEAMS)
