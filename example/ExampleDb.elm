module ExampleDb exposing (..)


import Json.Decode as Json exposing (..)
import Task exposing (Task)
import Http exposing (Error)


import MongoDb exposing (DbMsg, Collection)


db = "http://admin:changeit@localhost:8888/testdb/"


get : (Json.Decoder item) -> String -> Task Error item
get = MongoDb.get db


put : String -> String -> (DbMsg String -> m) -> Cmd m
put = MongoDb.put db


listDocuments : (Json.Decoder item) -> (DbMsg (Collection item) -> msg) -> String -> Cmd msg 
listDocuments = MongoDb.listDocuments db


