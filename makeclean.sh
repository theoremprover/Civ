#!/bin/bash

cd ~/Civ
rm -rf state
rm -rf dist
cabal clean
touch Settings/StaticFiles.hs
yesod devel
