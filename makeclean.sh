#!/bin/bash

cd ~/Civ
rm -rf state
rm -rf dist
touch Settings/StaticFiles.hs
yesod devel
