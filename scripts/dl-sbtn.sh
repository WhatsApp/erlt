#!/usr/bin/env bash

# see https://github.com/sbt/sbtn-dist/blob/develop/download.sh
MAC_URL=https://ci.appveyor.com/api/buildjobs/ruigr3j84oxc7dxr/artifacts/client%2Ftarget%2Fbin%2Fsbtn
LINUX_URL=https://ci.appveyor.com/api/buildjobs/kyfi02j2n4qtvyef/artifacts/client%2Ftarget%2Fbin%2Fsbtn
SCRIPT=`realpath -s $0`
SCRIPT_PATH=`dirname $SCRIPT`
SBTN_PATH="$SCRIPT_PATH/bin/sbtn"

mkdir -p $(dirname $SBTN_PATH)

if test -f $SBTN_PATH; then
    exit 0
fi

echo "downloading sbtn for the first time, - a few secs please"

if [ "$(uname)" == "Darwin" ]; then
  curl -L $MAC_URL > $SBTN_PATH
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  curl -L $LINUX_URL > $SBTN_PATH
fi

chmod +x $SBTN_PATH

