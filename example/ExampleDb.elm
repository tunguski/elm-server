module ExampleDb exposing (..)


import Json.Decode as Json exposing (..)
import Task exposing (Task)
import Http exposing (Error)


import MongoDb exposing (DbMsg, Collection)


db = "http://admin:changeit@localhost:8888/testdb/"


get : (Json.Decoder item) -> String -> Task Error item
get = MongoDb.get db


put : String -> String -> Task Error String
put = MongoDb.put db


listDocuments : (Json.Decoder item) -> String -> Task Error (Collection item)
listDocuments = MongoDb.listDocuments db


