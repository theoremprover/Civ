#!/bin/bash

echo "updateversion.sh"
pushd `dirname $0` >/dev/null

DT=`date`
sed -i -e "s/^dateString = \".*\"/dateString = \"$DT\"/g" Version.hs

GH=`git log --pretty=format:'%h' -n 1`
sed -i -e "s/^gitHash = \".*\"/gitHash = \"$GH\"/g" Version.hs

popd >/dev/null
