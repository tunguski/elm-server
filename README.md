# Simple web server using Elm 0.17

[![Build Status](https://travis-ci.org/tunguski/elm-server.svg?branch=master)](https://travis-ci.org/tunguski/elm-server)

## Usage

Install node dependencies

    npm install

Run server

    ./run.sh


## What does it do?

* builds server side application
* starts node server for request forwarding to Elm code

## Interesting features

* Server is able to process additional http requests (and generally all ```Tasks```) asynchronously before generating response.
* Capable of serving many clients concurrently.
