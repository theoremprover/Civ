#!/bin/sh
git checkout robert
./gitcommitpush.sh
git merge origin master

git checkout master
git pull
git merge robert
git commit -a -m "merged"
git push origin master

git checkout robert
