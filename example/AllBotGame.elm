module AllBotGame exposing (..)


import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (games)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import TichuLogic exposing (..)


allBotsGame : ApiPartApi msg -> String -> Int -> Task Error String
allBotsGame api name seed =
    put name (
        initGame name (GameConfig Bots) True seed
            [ AwaitingTableUser "bot 1" api.request.time True False
            , AwaitingTableUser "bot 2" api.request.time True False
            , AwaitingTableUser "bot 3" api.request.time True False
            , AwaitingTableUser "bot 4" api.request.time True False
            ]
        ) games
    |> andThenReturn (succeed "")


