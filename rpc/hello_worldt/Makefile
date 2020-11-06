.PHONY: run
run: release
	_build/release/bin/hello_worldt

.PHONY: release
release: build
	rebar3 as release escriptize

.PHONY: build
build:
	rebar3 compile

.PHONY: clean
clean:
	rm -rf _build
	rebar3 clean
