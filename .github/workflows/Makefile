DHALLS := erlt.dhall sterlang.dhall
YAMLS := $(DHALLS:%.dhall=%.yml)

all: $(YAMLS)

%.yml: %.dhall
	dhall-to-yaml --generated-comment --file $< > $@

clean:
	rm -f $(YAMLS)

.PHONY: all clean
