module ExampleDb exposing (..)


import Json.Decode as Json exposing (..)
import Task exposing (Task)
import Http exposing (Error)


import BaseModel exposing (Collection)
import MongoDb exposing (DbMsg)
import UserModel exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import SessionModel exposing (..)


dbUrl = "http://admin:changeit@localhost:8888/testdb/"


db = MongoDb.createDb dbUrl


createDbCollection = MongoDb.createDbCollection dbUrl


onError : (x -> Task y a) -> Task x a -> Task y a
onError fn task =
    Task.onError task fn


ignoreError : Task y a -> Task x a -> Task y a
ignoreError errorTask task =
    Task.onError task (\error -> errorTask)


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen fn task =
    Task.andThen task fn


andThenReturn : Task x b -> Task x a -> Task x b
andThenReturn fn task =
    Task.andThen task (\result -> fn)


games = 
  createDbCollection "games" 
    gameDecoder
    gameEncoder


awaitingTables = 
  createDbCollection "awaitingTables" 
    awaitingTableDecoder
    awaitingTableEncoder


users =
  createDbCollection "users"
    userDecoder
    userEncoder


sessions =
  createDbCollection "session"
    sessionDecoder
    sessionEncoder


