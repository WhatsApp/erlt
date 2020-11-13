#!/usr/bin/env bash

MAC_URL=https://ci.appveyor.com/api/buildjobs/of0lq2bfsexh68xl/artifacts/client%2Ftarget%2Fbin%2Fsbtn
LINUX_URL=https://ci.appveyor.com/api/buildjobs/geyi0cyk85u9rnhg/artifacts/client%2Ftarget%2Fbin%2Fsbtn
SCRIPT=`realpath -s $0`
SCRIPT_PATH=`dirname $SCRIPT`
SBTN_PATH="$SCRIPT_PATH/sbtn"

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
