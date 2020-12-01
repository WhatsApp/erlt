#!/usr/bin/env bash

DIR=$(dirname "${BASH_SOURCE[0]}")
VERSION=$(cat $DIR/rebar-version)
BIN_DIR="$DIR/bin/$VERSION"
BIN="$BIN_DIR/rebar3"


download_rebar3() {
    echo "downloading rebar3 $VERSION"
    curl -L https://github.com/erlang/rebar3/releases/download/$VERSION/rebar3 -o $BIN || exit 2
    chmod +x $BIN
    which escript || echo "missing escript executable"
    which escript || exit 4
    local CUR=$($BIN --version | tail -n 1 | cut -d' ' -f 2)
    if [[ "$CUR" != "$VERSION" ]]
    then
        >&2 echo "error, downloaded rebar3 says its version is '$CUR', which doesn't match '$VERSION'"
        rm $BIN
        exit 1
    else
        echo "download successful"
        exit 0
    fi
}

main() {
    mkdir -p $BIN_DIR
    if [ ! -f $BIN ] || [ ! -x $BIN ]; then
        rm -rf $BIN
        echo "no cached rebar3 found"
        download_rebar3
    fi
}

main
