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
        player = getPlayer game.round botName
        param = buildHandParams botName game game.round player []
    in
        botMove botName game game.round player param
        |> Maybe.map (processingResultToTask >> map toString)


{-| Maybe make bot's move. Returned task describes what bot want's to do.
-}
botMove : String -> Game -> Round -> Player -> HandParams ->
          Maybe (Result (Int, String) (Task Error Response))
botMove botName game round player param =
    Nothing
    -- declare grand tichu or see all cards
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
    -- exchange cards
    |> executeIf (player.sawAllCards) (\_ ->
        case player.exchange of
            Just _ ->
                Nothing
            Nothing ->
                Maybe.map3 (\a b c ->
                    exchangeCards botName game round player (Ok [a, b, c])
                )
                (player.hand |> List.reverse |> List.head)
                (player.hand |> List.head)
                (player.hand |> List.drop 1 |> List.head)

    )
    -- maybe declare tichu
    |> executeIf (player.sawAllCards && player.cardsOnHand == 14) (\_ ->
        Nothing
    )
    -- give dragon
    |> executeIf ((
        case round.tableHandOwner of
            Just owner ->
                owner == round.actualPlayer
                && (getNthPlayer round owner).name == botName
            _ ->
                False
        ) && (
        case List.head round.table of
            Just [ Dragon ] -> True
            _ -> False
    )) (\_ ->
        giveDragon botName game round player False
        |> Just
        |> Debug.log "give the dragon"
    )
    -- play cards or pass
    |> executeIf (player.name == botName
            && ((List.filterMap .exchange round.players |> List.length) == 4)) (\_ ->
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
                            |> Maybe.map (Debug.log "higher card")
                            |> Maybe.andThen (
                                List.singleton
                                >> playHand
                            )
                            |> Maybe.withDefault (
                                pass botName game round player
                                |> Debug.log "pass"
                            )
                            |> Just
                        _ ->
                            pass botName game round player
                            |> Just
                            |> Debug.log "pass"
    )
    |> executeIf (False) (\_ ->
        Nothing
    )
    -- if game is finished, ignore any move
    |> (\move ->
        case game.finished of
            Just _ ->
                Nothing
            _ ->
                move
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


