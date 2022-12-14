#!/bin/sh
# Ported from https://raw.githubusercontent.com/jaspervdj/stylish-haskell/master/scripts/latest.sh

set -e

PACKAGE=stylish-haskell
echo Downloading and running $PACKAGE...

RELEASE_URL=$(curl --silent https://github.com/haskell/$PACKAGE/releases | egrep '[^\"]*expanded_assets/v[0-9|.]+' -m 1 -o)
echo release_url:
echo $RELEASE_URL
RELEASE=$(curl --silent $RELEASE_URL)
URL=https://github.com/$(echo $RELEASE | grep -o '\"[^\"]*-linux-x86_64\.tar\.gz\"' | sed s/\"//g | head -n1)
echo url:
echo $URL
VERSION=$(echo $URL | sed -e 's/.*-\(v[\.0-9]\+-linux-x86_64\)\.tar\.gz/\1/')
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
