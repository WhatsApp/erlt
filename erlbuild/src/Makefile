# looks like rebar3 exports ERLC variable, but we don't want it to get picked up here
ERLC := erlc

EBIN ?= ../ebin

ERLC_FLAGS := -Wall -Werror +debug_info
ERLC_FLAGS += -o $(EBIN)
ERLC_FLAGS += -pa $(EBIN)
ERLC_FLAGS += -I ../include


ERLBUILD := ../bin/erlbuild


ERLS := $(wildcard *.erl)
BEAMS := $(addprefix $(EBIN)/,$(ERLS:.erl=.beam))


all: $(ERLBUILD)


$(ERLBUILD): $(BEAMS)
	./make_escript $@ erlbuild $(BEAMS)


# useing explicit order for the parse transform and the file it embeds
$(EBIN)/erlbuild.beam: $(EBIN)/erlbuild_pt_util.beam erlbuild.template.mk


$(EBIN)/%.beam: %.erl | $(EBIN)
	$(ERLC) $(ERLC_FLAGS) $<


$(EBIN):
	mkdir -p $@


clean:
	rm -f $(ERLBUILD) $(BEAMS)
