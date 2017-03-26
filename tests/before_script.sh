#!/bin/bash

npm install

git clone https://github.com/tunguski/elm-spa-example.git client

elm-make --yes example/Example.elm --output main.js

node server.js &

sleep 2

ps auxwf | grep node

