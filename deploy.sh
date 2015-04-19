!#/bin/bash

pushd ~/Civ
yesod keter
strip ~/Civ/dist/build/Civ/Civ
scp -i ~/.ssh/amazonEC2_privatekey Civ.keter ubuntu@civ.thinking-machines.net:/opt/keter/incoming
