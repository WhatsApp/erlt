.PHONY: run
run: release
	_build/release/bin/count

.PHONY: build
build:
	rebar3 compile

.PHONY: test
test:
	rebar3 ct

.PHONY: release
release:
	rebar3 as release escriptize

.PHONY: clean
clean:
	rm -rf _build
	rebar3 clean

.PHONY: generate
generate:
	(cd ../rpctc && make release)
	../rpctc/_build/release/bin/rpctc gadtimpl ./src/count_spec.erlt > ./src/count_server.erlt