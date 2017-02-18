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
                        playCards botName gameState [ MahJong ]

                    False ->
                        case List.head gameState.round.table of
                            Nothing ->
                                List.head actualPlayer.hand
                                |> Maybe.andThen (\c ->
                                    playCards botName gameState [ c ]
                                )

                            Just [ card ] ->
                                actualPlayer.hand
                                |> List.filter (\c -> cardWeight c > cardWeight card)
                                |> List.head
                                |> Maybe.andThen (\c ->
                                    playCards botName gameState [ c ]
                                )
                            _ ->
                                Nothing
                                --Debug.log "ignore!" Nothing

            False ->
                Nothing


playCards botName gameState cards =
    let
        actualPlayer = getActualPlayer gameState.round
    in
        hand
            botName
            gameState
            gameState.round
            actualPlayer
            cards
        |> (\result ->
            case result of
                Ok task ->
                    task
                    |> map toString
                    |> Just
                Err err ->
                    Debug.log ("Error when tried to play cards!" ++ toString err) Nothing
        )


