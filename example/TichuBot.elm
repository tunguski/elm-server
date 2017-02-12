module TichuBot exposing (..)


import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (games)
import TichuRest as TR
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (..)
import UrlParse exposing (..)


gamesWithSession token =
    TR.games
    |> withHeader "X-Test-Session" token


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
                        gamesWithSession actualPlayer.name
                        |> withBody (encodeCards [ MahJong ])
                        |> Debug.log "play mahjong!"
                        |> postCommand (gameState.name ++ "/hand")
                        |> Just

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
