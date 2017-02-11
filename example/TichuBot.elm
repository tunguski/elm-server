module TichuBot exposing (..)


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
import UserModel exposing (..)
import UrlParse exposing (..)


{-| Maybe make bot's move. Returned task describes what bot want's to do.
-}
tableChanged : String -> Game -> Maybe (Task String String)
tableChanged botName gameState =
    Nothing



