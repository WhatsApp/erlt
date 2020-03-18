
# erlbuild.mk parameters
ERLC_FLAGS := -Wall -Werror +debug_info

ERL2C := ../bin/erl2c
ERLBUILD := ../../erlbuild/bin/erlbuild
ERLC := erlc  # looks like rebar3 exports this variable, but we don't want it to get picked up here


all: escript


clean: clean_escript


escript: $(ERL2C)


clean_escript:
	$(QUIET)rm -f $(ERL2C)


include ../../erlbuild/erlbuild.mk


# NOTE: these rule is below erlbuild.mk, because we need $(BEAMS) variable to be defined
$(ERL2C): $(BEAMS)
	$(ECHO_1) "making escript $@"
	$(QUIET)./make_escript $@ erl2c $(BEAMS)
