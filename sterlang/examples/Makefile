# test using sterlang running as a daemon. This is the main mode used during sterlang development
build:
	cd ..; sbtn sterlangd
	ERL_FLAGS="-args_file dev.vm.args" rebar3 compile


# test using sterlang jar file. This is a good balance for erltc CI on each push
# because it is closer what the users will use than sterlangd (daemon) and not as slow to build as a native binary
build-jar: build-sterlang-jar
	USE_STERLANG_JAR=true rebar3 compile


# we depend on `clean` because that will force rebar to copy over the priv dir containing
# sterlang.jar
build-sterlang-jar: clean
	make -C ..


clean:
	rm -rf _build


.PHONY: build build-jar build-sterlang-jar clean
