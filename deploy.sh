!#/bin/bash

pushd ~/Civ
yesod keter
scp -i ~/.ssh/amazonEC2_privatekey Civ.keter ubuntu@civ.thinking-machines.net:/opt/keter/incoming
