#!/bin/bash

echo "Working directory: `pwd`"

RESOLVED_LINKS=$(readlink -f "${BASH_SOURCE[0]}")
DIR=`dirname $RESOLVED_LINKS`

mkdir -p public

# compile server code
# compile client code
# start the server
elm-make "src/ServerMain.elm" --output "target/server_main.js" \
    && elm-make client/ClientMain.elm --output public/client.js \
    && node "$DIR/server.js" "`pwd`"

