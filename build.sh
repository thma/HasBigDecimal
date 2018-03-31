#!/bin/sh

# this scripts builds the piserver demo app and deploys it as an
# OpenFaas function.
# OpenFaas must be installed. https://www.openfaas.com/

# build haskell app
stack install

# copy installed app to local folder
cp -L ~/.local/bin/piServer .

# copy haskell runtime libs to local folder (about 5MB)
./prepare-haskell-libs.sh

# build faas docker container & deploy it
sudo faas build -f piserver.yml  && faas deploy -f piserver.yml
