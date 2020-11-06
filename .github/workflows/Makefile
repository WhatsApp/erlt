DHALLS := $(wildcard *.dhall)
YAMLS := $(DHALLS:%.dhall=%.yml)

all: $(YAMLS)

%.yml: %.dhall ./*.dhall ./shared/*.dhall
	dhall-to-yaml --generated-comment --file $< > $@

clean:
	rm -f $(YAMLS)

.PHONY: all clean
