module TichuBot exposing (..)


import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import TichuLogic exposing (..)


{-| Maybe make bot's move. Returned task describes what bot want's to do.
-}
tableChanged : String -> Game -> Maybe (Task Error String)
tableChanged botName game =
    let
        actualPlayer = getActualPlayer game.round
        param = buildHandParams botName game game.round actualPlayer []
    in
        botMove botName game game.round actualPlayer param
        |> Maybe.map (processingResultToTask >> map toString)


{-| Maybe make bot's move. Returned task describes what bot want's to do.
-}
botMove : String -> Game -> Round -> Player -> HandParams ->
          Maybe (Result (Int, String) (Task Error Response))
botMove botName game round player param =
    Nothing
    |> executeIf (not player.sawAllCards) (\_ ->
        if False then
            declareGrandTichu botName game round player
            |> Just
            |> Debug.log "declareGrandTichu"
        else
            seeAllCards botName game round player
            |> Just
            |> Debug.log "seeAllCards"
    )
    |> executeIf (player.sawAllCards && player.cardsOnHand == 14) (\_ ->
        -- maybe declare tichu
        Nothing
    )
    |> executeIf (player.name == botName) (\_ ->
        let
            playHand = playCards botName game player
        in
            case hasCard MahJong player of
                True ->
                    playHand [ MahJong ]
                    |> Debug.log "mahjong"

                False ->
                    case List.head game.round.table of
                        Nothing ->
                            List.head player.hand
                            |> Maybe.andThen (List.singleton >> playHand)
                            |> Debug.log "lowest card"

                        Just [ card ] ->
                            player.hand
                            |> List.filter (\c -> cardWeight c > cardWeight card)
                            |> List.head
                            |> Maybe.andThen (List.singleton >> playHand)
                            |> Debug.log "highest card"
                        _ ->
                            pass botName game round player
                            |> Just
                            |> Debug.log "pass"
    )
    |> executeIf (True) (\_ ->
        Nothing
    )
    |> executeIf (True) (\_ ->
        Nothing
    )


executeIf : Bool -> (() -> Maybe a) -> Maybe a -> Maybe a
executeIf condition exec step =
    case step of
        Just _ ->
            step
        Nothing ->
            if condition then
                exec ()
            else
                Nothing


playCards botName gameState actualPlayer cards =
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
                |> (Ok >> Just)
            Err err ->
                Debug.log ("Error when tried to play cards!" ++ toString err) Nothing
    )


