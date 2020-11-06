.PHONY: run
run: release
	_build/release/bin/count

.PHONY: release
release:
	rebar3 as release escriptize

.PHONY: test
test:
	rebar3 ct

.PHONY: build
build:
	rebar3 compile

.PHONY: clean
clean:
	rm -rf _build
	rebar3 clean

.PHONY: generate
generate:
	(cd ../rpctc && make release)
	../rpctc/_build/release/bin/rpctc header ./src/count_server.erlt > ./src/count_server_boilerplate.hrl
	../rpctc/_build/release/bin/rpctc header ./src/string_server.erlt > ./src/string_server_boilerplate.hrl
	../rpctc/_build/release/bin/rpctc header ./src/count_singleton.erlt > ./src/count_singleton_boilerplate.hrl