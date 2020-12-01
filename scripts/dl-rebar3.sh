#!/usr/bin/env bash

DIR=$(dirname "${BASH_SOURCE[0]}")
VERSION=$(cat $DIR/rebar-version)
BIN_DIR="$DIR/bin/$VERSION"
BIN="$BIN_DIR/rebar3"

# if the curl command comes back with a 404 page or something
# we want to find out something is wrong.
# Unfortunately, the checksums aren't stable across downloads and rebar3 --version is slooow
BIN_MIN_SIZE=234922 # .25 the size of rebar 3.14.2

download_rebar3() {
    echo "downloading rebar3 $VERSION"
    curl -L https://github.com/erlang/rebar3/releases/download/$VERSION/rebar3 -o $BIN || exit 2
    SIZE=$(wc -c $BIN | awk '{print $1}')
    if [ $SIZE -lt $BIN_MIN_SIZE ]; then
        >&2 echo "error: the downloaded rebar3 was unexpectedly small: $SIZE"
        exit 3
    fi

    chmod +x $BIN
    which escript || echo "missing escript executable"
    which escript || exit 4
}

main() {
    mkdir -p $BIN_DIR
    if [ ! -f $BIN ]; then
        echo "no cached rebar3 found"
        download_rebar3
    fi
}

main

