#!/bin/bash

pushd `dirname $0`

DT=`date`
sed -i -e "s/^dateString = \".*\"/dateString = \"$DT\"/g" Version.hs

popd