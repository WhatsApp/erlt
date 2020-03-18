ERL2C_EXEC = ../bin/erl2c
EFLAGS :=

TESTS := $(sort $(wildcard *_SUITE.erl))
BEAMS := $(TESTS:.erl=.beam)

test: $(BEAMS)
	mkdir -p logs
	PATH="$(abspath $(dir $(ERL2C_EXEC))):$$PATH" ct_run -pa ../ebin -no-auto-compile -logdir logs -dir .
PHONY: test

%.beam:	%.erl
	$(ERL2C_EXEC) $(EFLAGS) $<
