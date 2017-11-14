#!/bin/bash

echo "Working directory: `pwd`"

mkdir -p target
mkdir -p public

# compile server code
# compile client code
# start the server
elm-make "src/Server.elm" --output "target/server.js" \
    && elm-make src/Client.elm --output public/client.js \
    && node server.js

