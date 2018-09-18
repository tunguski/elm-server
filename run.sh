#!/bin/bash

# compile server code
# compile client code
# start the server
elm make "example/Example.elm" --output "main.js" \
    && elm make client/ClientMain.elm --output public/example.js \
    && node server.js
