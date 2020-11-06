.PHONY: all
all:
	make clean
	make generate
	make test
	make fmt

.PHONY: fmt
fmt:
	rebar3 fmt

.PHONY: clean
clean:
	(cd ../erltc && rm -rf _build)
	(cd kitty && make clean)
	(cd count && make clean)
	(cd count_impl && make clean)
	(cd count_header && make clean)
	(cd count_gadt && make clean)
	(cd countt && make clean)
	(cd rpctc && make clean)
	(cd hello_worldt && make clean)
	(cd count_headert && make clean)
	(cd count_implt && make clean)
	(cd count_gadt_implt && make clean)
	(cd count_gadt_headert && make clean)

.PHONY: generate
generate:
	(cd count_headert && make generate)
	(cd count_implt && make generate)
	(cd count_gadt_implt && make generate)
	(cd count_gadt_headert && make generate)

.PHONY: test
test:
	(cd kitty && make test)
	(cd count && make test)
	(cd count_impl && make test)
	(cd count_header && make test)
	(cd count_gadt && make test)
	(cd countt && make test)
	(cd rpctc && make test)
	(cd hello_worldt && make run)
	(cd count_headert && make generate)
	(cd count_headert && make run)
	(cd count_implt && make run)
	(cd count_gadt_implt && make run)
	(cd count_gadt_headert && make run)