#!/bin/bash

elm-make "example/Example.elm" --output "main.js" && \
if [ -e "client/Main.elm" ]; then
  elm-make client/Main.elm --output public/example.js 
fi && \
node server.js
