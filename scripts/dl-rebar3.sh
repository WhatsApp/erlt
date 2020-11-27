#!/usr/bin/env bash

VERSION='3.14.2'
DIR="$(dirname "${BASH_SOURCE[0]}")"
BIN="$DIR/bin/rebar3"


current_version() {
    chmod +x $BIN
    $BIN --version | tail -n 1 | cut -d' ' -f 2
}

download_rebar3() {
    echo "downloading rebar3 $VERSION"
    curl -L https://github.com/erlang/rebar3/releases/download/$VERSION/rebar3 -o $BIN
    which escript || echo "missing escript executable"
    which escript || exit 2
    local CUR=$(current_version) &&
    if [[ "$CUR" != "$VERSION" ]]
    then
        >&2 echo "error, downloaded rebar3 says its version is '$CUR', which doesn't match '$VERSION'"
        exit 1
    else
        echo "download successful"
        exit 0
    fi
}

main() {
    mkdir -p $(dirname $BIN)
    if [ ! -e $BIN ]
    then
        echo "no cached rebar3 found"
        download_rebar3
    else 
        # handling nonzero return code in case the rebar3 file is corrupted
        local CUR=$(current_version) || download_rebar3
        local CUR=$(current_version)
        if [[ "$VERSION" = "$CUR" ]]
        then
            # echo "cached rebar3 is up to date"
            exit 0
        else
            echo "cached rebar3 $CUR is the wrong version, expected "
            echo "${#CUR}"
            echo "${#VERSION}"
            # download_rebar3
        fi
    fi
}

main

