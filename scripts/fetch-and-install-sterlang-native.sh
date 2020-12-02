#!/usr/bin/env bash

# intended to be run from CI

# inspired by
# https://github.com/dsaltares/fetch-gh-release-asset/blob/b5bf388/fetch_github_asset.sh

INPUT_FILE=sterlang-linux
GITHUB_REPOSITORY=whatsapp/erlt

# this is needed for private repos, CI should set it
TOKEN=$GITHUB_TOKEN

API_URL="https://$TOKEN:@api.github.com/repos/$REPO"
RELEASE_DATA=$(curl $API_URL/releases/${INPUT_VERSION})
ASSET_ID=$(echo $RELEASE_DATA | jq -r ".assets | map(select(.name == \"${INPUT_FILE}\"))[0].id")
TAG_VERSION=$(echo $RELEASE_DATA | jq -r ".tag_name" | sed -e "s/^v//" | sed -e "s/^v.//")

if [[ -z "$ASSET_ID" ]]; then
  echo "Could not find asset id"
  exit 1
fi

curl \
  -J \
  -L \
  -H "Accept: application/octet-stream" \
  "$API_URL/releases/assets/$ASSET_ID" \
  -o ${INPUT_FILE}

echo "::set-output name=version::$TAG_VERSION"

# install sterlang
mkdir -p erltc/priv
rm -f erltc/priv/sterlang
rm -f erltc/priv/sterlang.jar
mv sterlang-linux erltc/priv/sterlang
