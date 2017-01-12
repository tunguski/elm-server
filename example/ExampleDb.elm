module ExampleDb exposing (..)


import MongoDb exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (..)


dbUrl =
  "http://admin:changeit@localhost:8888/testdb/"


createDbCollection =
  restCollection dbUrl


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


