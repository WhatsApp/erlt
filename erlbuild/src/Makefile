# looks like rebar3 exports ERLC variable, but we don't want it to get picked up here
ERLC := erlc

EBIN ?= ../ebin

ERLC_FLAGS := -Wall -Werror +debug_info
ERLC_FLAGS += -o $(EBIN)
ERLC_FLAGS += -pa $(EBIN)
ERLC_FLAGS += -I ../include


ERLBUILD := ../bin/erlbuild
ERLBUILD_ERLC := ../bin/erlbuild-erlc


ERLS := $(wildcard *.erl)
BEAMS := $(addprefix $(EBIN)/,$(ERLS:.erl=.beam))


all: $(ERLBUILD) $(ERLBUILD_ERLC)
	@:


# TODO: be specific about which BEAMS are included in which script
$(ERLBUILD): $(BEAMS)
	./make_escript $@ erlbuild $(BEAMS)

$(ERLBUILD_ERLC): $(BEAMS)
	./make_escript $@ erlbuild_erlc $(BEAMS)


# useing explicit order for the parse transform and the file it embeds
$(EBIN)/erlbuild.beam: $(EBIN)/erlbuild_pt_util.beam erlbuild.template.mk


$(EBIN)/%.beam: %.erl | $(EBIN)
	$(ERLC) $(ERLC_FLAGS) $<


$(EBIN):
	mkdir -p $@


clean:
	rm -f $(ERLBUILD) $(ERLBUILD_ERLC) $(BEAMS)


include ../../common.mk
