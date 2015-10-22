#!/bin/sh
git checkout master
git pull
git merge robert
git commit -a -m "merged"
git push origin master
git checkout robert
