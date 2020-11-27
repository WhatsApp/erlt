build:
	../../scripts/dl-sbtn.sh
	cd ..; ../scripts/sbtn sterlangd
	ERL_FLAGS="-args_file dev.vm.args" rebar3 compile

.PHONY: build
