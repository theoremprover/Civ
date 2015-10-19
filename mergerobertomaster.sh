#!/bin/sh
git checkout master
git pull
git merge robert
git push origin master
git checkout robert
