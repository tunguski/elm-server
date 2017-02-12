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
import TichuLogic exposing (..)
import UserModel exposing (..)
import UrlParse exposing (..)


{-| Maybe make bot's move. Returned task describes what bot want's to do.
-}
tableChanged : String -> Game -> Maybe (Task Error String)
tableChanged botName gameState =
    let
        actualPlayer = getActualPlayer gameState.round
    in
        case actualPlayer.name == botName of
            True ->
                case hasCard MahJong actualPlayer of
                    True ->
                        handWithParsedCards
                            gameState
                            gameState.round
                            actualPlayer
                            (buildHandParams botName gameState gameState.round actualPlayer [ MahJong ])
                        |> (\result ->
                            case result of
                                Ok task ->
                                    task
                                    |> map toString
                                    |> Just
                                Err err ->
                                    Debug.log ("Error when tried to play cards!" ++ toString err) Nothing
                        )


                    False ->
                        Debug.log "ignore!" Nothing

            False ->
                Nothing


        --declareGrandTichu (session, declare) =
        --    case declare of
        --        True ->
        --            gamesWithSession session
        --            |> postCommand (tableName ++ "/declareGrandTichu")
        --        False ->
        --            gamesWithSession session
        --            |> postCommand (tableName ++ "/seeAllCards")
        --exchangeCards (cards, session) =
        --     gamesWithSession session
        --     |> withBody (encodeCards cards)
        --     |> postCommand (tableName ++ "/exchangeCards")

                --Pass session ->
                --    gamesWithSession session
                --    |> postCommand (tableName ++ "/pass")

                --Play session list ->
                --    gamesWithSession session
                --    |> withBody (encodeCards list)
                --    |> postCommand (tableName ++ "/hand")

                --Tichu session ->
                --    gamesWithSession session
                --    |> postCommand (tableName ++ "/declareTichu")

                --GiveDragon session body ->
                --    gamesWithSession session
                --    |> withBody body
                --    |> postCommand (tableName ++ "/giveDragon")
