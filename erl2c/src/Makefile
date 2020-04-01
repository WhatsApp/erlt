
# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

ERL2C := ../bin/erl2c
ERL2OCAML := ../bin/erl2ocaml
ERLBUILD := ../../erlbuild/bin/erlbuild
ERLC := erlc  # looks like rebar3 exports this variable, but we don't want it to get picked up here


all: escripts


clean: clean_escripts


escripts: $(ERL2C) $(ERL2OCAML)


clean_escripts:
	$(QUIET)rm -f $(ERL2C)
	$(QUIET)rm -f $(ERL2OCAML)


include ../../erlbuild/erlbuild.mk


# NOTE: these rules are below erlbuild.mk, because we need $(BEAMS) variable to be defined
$(ERL2C): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2c $(BEAMS)

$(ERL2OCAML): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2ocaml $(BEAMS)
