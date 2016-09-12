#! /bin/bash

set -e
echo "Building newguy-server..."
stack build
strip `stack exec -- which newguy-server`
echo "Creating bundle..."
cp `stack exec -- which newguy-server` newguy-server
tar -czvf newguy-server.keter newguy-server config ql-ui/assets
rm newguy-server
# scp ./newguy-server.keter user@host:/opt/keter/incoming/newguy-server.keter
rm newguy-server.keter
