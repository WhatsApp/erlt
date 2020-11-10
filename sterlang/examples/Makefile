build:
	../../sbtn/dl.sh
	cd ..; ../sbtn/sbtn sterlangd
	ERL_FLAGS="-args_file dev.vm.args" rebar3 compile

.PHONY: build
