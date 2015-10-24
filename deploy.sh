#!/bin/bash

pushd ~/Civ

rm -rf dist
yesod keter
scp -i ~/.ssh/amazonEC2_privatekey Civ.keter ubuntu@civ.thinking-machines.net:/opt/keter/incoming

popd
