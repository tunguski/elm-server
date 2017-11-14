#!/bin/bash

# compile server code
# compile client code
# start the server
elm-make "example/Example.elm" --output "main.js" \
    && elm-make client/Main.elm --output public/example.js \
    && node server.js
