# Fully featured web server using Elm 0.17

## Work in progress !

[![Build Status](https://travis-ci.org/tunguski/elm-server.svg?branch=master)](https://travis-ci.org/tunguski/elm-server)

##### Can it read files?

No

##### Can it send mails?

No

##### Can it store sessions?

Nope

##### So why the fuck do you call it fully featured?

Because it can use Rest API's.

##### ???

Need session? Use [Redis](http://redis.io/) and [Rest API](http://webd.is/).

Need storage? Use [Mongo](https://www.mongodb.com/) and [Rest API](http://restheart.org/).

Need emails? Use [Mailgun](https://www.mailgun.com/) and [Rest API](https://documentation.mailgun.com/api_reference.html).

Simple.

## Usage

Install node dependencies

    npm install

Run server

    ./run.sh


### What does it do?

* builds server side application
* starts node server that forwards requests to Elm code

### Interesting features

* Server is able to process additional http requests (and generally all ```Tasks```) asynchronously before generating response.
* Capable of serving many clients concurrently.

## Writing server side application

Start with reading [example/Example.elm](example/Example.elm).

Server side Elm application consists of two functions:

```elm
init : Request -> (Response, List (Cmd msg))
update : Request -> msg -> Response -> (Response, List (Cmd msg))
```

and program composition:

```elm
import Server exposing (..)

main =
  Server.program init update
```

#### init

* takes ```Request``` - that is the request we need to serve
* returns "empty" response; empty means it won't be send before finishing all tasks
    
#### update

* takes ```Request``` - same request object as passed to ```init```
* takes ```msg``` - that is a message returned by performing ```Task```
* takes ```Response``` - response state returned by ```init``` or last ```update``` invocation
* returns tuple consisting of new partial response and list of commands to perform

Server processes all internal http requests until there is no more ```Cmd```s and returns the state of ```Response``` from that moment.

### Request and response model

```elm
type alias Request =
  { id : String
  , url : String
  , method : String
  }


type alias Response =
  { idRequest : String
  , statusCode : Int
  , body : String
  }
```

### Mongodb Support

At this moment it is a very basic support.

#### get

```elm
get : String -> (Json.Decoder item) -> (item -> m) -> String -> Cmd (DbMsg m)
get baseUrl decoder msg collection =
  Http.get decoder
    (concat [ baseUrl, collection ])
    |> Task.mapError toString
    |> Task.perform 
        ErrorOccurred
        (DataFetched << msg)
```

```get``` function takes 

* ```baseUrl``` (with db name included),
* json to record decoder,
* application's Msg function ```(item -> Msg)```,
* object/collection name.

This is a utility function for loading data.

#### listDocuments

There is another utility function for getting full collection:

```elm
listDocuments : String -> (Json.Decoder item) -> (Collection item -> m) -> String -> Cmd (DbMsg m)
listDocuments baseUrl decoder msg collection =
  get baseUrl (collectionDecoder decoder) msg collection
```

It is a shortcut for loading collection of documents.

#### Mongodb in example

In Example.elm it is used like this:

```elm
import MongoDB exposing (..)

getRepoInfos : Cmd (DbMsg Requests)
getRepoInfos =
  listDocuments
    db 
    repoInfoDecoder
    GetColl
    "coll"
```

where

```elm
-- mongo database location
db = "http://admin:changeit@localhost:8888/testdb/"


-- simple record
type alias RepoInfo =
  { id : String 
  , name : String
  }


-- decoder from json to elm record
repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo 
    (at ["_id" ] <| "$oid" := string)
    ("name" := string)


-- request types with return values    
type Requests
  = GetDb MongoDb
  | GetColl (Collection RepoInfo)


-- type alias that combines mongo message with application's internal messages
type alias Msg 
  = DbMsg Requests
```

