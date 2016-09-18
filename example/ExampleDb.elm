module ExampleDb exposing (..)


import Json.Decode as Json exposing (..)


import MongoDb exposing (..)


db = "http://admin:changeit@localhost:8888/testdb/"


get : (Json.Decoder item) -> (DbMsg item -> msg) -> String -> Cmd msg 
get = MongoDb.get db


put : String -> String -> (DbMsg String -> m) -> Cmd m
put = MongoDb.put db


listDocuments : (Json.Decoder item) -> (DbMsg (Collection item) -> msg) -> String -> Cmd msg 
listDocuments = MongoDb.listDocuments db


