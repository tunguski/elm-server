module TichuLogic exposing (..)

import Array
import List exposing (..)
import Http exposing (Error(..))
import Maybe exposing (andThen)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Time exposing (Time)
import Task exposing (..)


import Common exposing (..)
import ExampleDb exposing (games)
import Server exposing (..)
import Rest exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (User)


orElse : Bool -> String -> Maybe String -> Maybe String
orElse condition errorText maybe =
    case maybe of
        Just text ->
            Just text
        Nothing ->
            case condition of
                True -> Nothing
                False -> Just errorText


executeIfNoError : Result (Int, String) (Task Error Response)->
                   Maybe String ->
                   Result (Int, String) (Task Error Response)
executeIfNoError exec maybe =
    case maybe of
        Just error ->
            Err (400, error)
        Nothing ->
            exec


processingResultToTask : Result (Int, String) (Task Error Response) -> Task Error Response
processingResultToTask result =
    case result of
        Ok task ->
            task
        Err (code, message) ->
            response code message |> Task.succeed


type alias HandParams =
    { isActualPlayer : Bool
    , isOpenDemand : Bool
    , hasDemandedCard : Bool
    , parsedCards : Cards
    , trick : Maybe Combination
    , allowedTrick : Bool
    , isDemandedCardInTrick : Bool
    , isBomb : Bool
    , bombEnoughPower : Bool
    }


okSucceed task =
    task
    |> andThenReturn (statusResponse 200 |> Task.succeed)
    |> Ok


hand : String -> Game -> Round -> Player -> Cards ->
       Result (Int, String) (Task Error Response)
hand userName table round player cards =
    let
        param =
            buildHandParams userName table round player cards
        doPlayCards =
            put table.name
                (playHandAndUpdateRound table round player param)
                games
            |> okSucceed
        playCards =
            if param.allowedTrick then
                doPlayCards
            else
                Err (400, "Trick does not match the one on table")
    in
        Nothing
        |> orElse (List.all (flip hasCard <| player) param.parsedCards) "Tried to play card you don't own"
        |> orElse (List.all (.exchange >> isNothing >> not) round.players)
                  "You have to exchange cards first"
        |> executeIfNoError (
            if param.isActualPlayer then
                if param.isOpenDemand then
                    if param.hasDemandedCard then
                        if param.isDemandedCardInTrick then
                            -- play, switch to next player and return ok
                            playCards
                        else
                            Err (400, "You have to play demanded card")
                    else
                        -- play, switch to next player and return ok
                        playCards
                else
                    if param.isBomb && param.bombEnoughPower then
                        -- play, switch to next player and return ok
                        doPlayCards
                    else
                        -- play, switch to next player and return ok
                        playCards
            else
                if param.isBomb then
                    if param.bombEnoughPower then
                        -- play, switch to next player and return ok
                        doPlayCards
                    else
                        Err (400, "Too weak bomb")
                else
                    Err (400, "You are not actual player")
        )


buildHandParams userName table round player cards =
    { isActualPlayer = (getActualPlayer round).name == userName
    , isOpenDemand = openDemand round
    , hasDemandedCard = openDemandMatch round
    , parsedCards = cards
    , trick = parseTrick cards
    , allowedTrick = allowedCombination (List.head round.table) cards
    , isDemandedCardInTrick = cardInTrick round.demand cards
    , isBomb = bomb (parseTrick cards)
    , bombEnoughPower = True
    }


playHandAndUpdateRound table round player param =
    case param.parsedCards of
        [ Dog ] ->
            { table |
                round =
                    { round | actualPlayer = (round.actualPlayer + 2) % 4 }
                    |> setTableHandOwnerAsActualPlayer player
                    |> maybeIncActualPlayer
                    |> updatePlayerPlayingHand player param
                    |> maybeSetWinner (removeCards param.parsedCards player.hand) round.actualPlayer
                    |> (\round ->
                        let
                            player = getActualPlayer round
                        in
                            modifyPlayer
                                player.name
                                (\player -> { player
                                    | collected = [ [ [ Dog ] ] ] ++ player.collected
                                })
                                round
                    )
                    -- putCardsOnTable param.parsedCards
            }
        _ ->
            { table |
                round =
                    round
                    |> updatePlayerPlayingHand player param
                    |> setActualPlayer player
                    |> setTableHandOwnerAsActualPlayer player
                    |> maybeIncActualPlayer
                    |> maybeSetWinner (removeCards param.parsedCards player.hand) round.actualPlayer
                    |> putCardsOnTable param.parsedCards
            }
    |> maybeEndRound


pass userName table round player =
    Nothing
    |> orElse player.sawAllCards "Before playing you have to decide playing Grand Tichu or not"
    |> orElse ((getActualPlayer round).name == userName) "Not an actual player"
    |> orElse (not <| openDemandMatch round) "You have demanded card"
    |> orElse (not <| hasCard MahJong player) "You have MahJong"
    |> orElse (not <| isNothing round.tableHandOwner) "You cannot pass if you won last table"
    |> executeIfNoError (
        -- switch to next player and return ok
        put table.name
            { table | round = incActualPlayer round }
            games
        |> okSucceed
    )


exchangeCards : String -> Game -> Round -> Player -> Result String (List Card) -> Result (Int, String) (Task Error Response)
exchangeCards userName table round player exchangeCards =
    Nothing
    |> orElse (isNothing player.exchange) "You have exchanged already"
    |> executeIfNoError (
        let
            hasAllCards =
                case exchangeCards of
                    Ok cards ->
                        List.all (flip hasCard <| player) cards
                    Err _ ->
                        False
        in
            if hasAllCards then
                case exchangeCards of
                    Ok [a, b, c] ->
                        put table.name
                            ({ table | round =
                                modifyPlayer userName
                                    (\player -> { player | exchange = Just (a, b, c) })
                                    round
                            }
                            |> (\t ->
                                case List.all (\p -> not <| isNothing p.exchange) t.round.players of
                                    True ->
                                        exchangeCardsBetweenPlayers t
                                    False ->
                                        t
                            ))
                        games
                        |> okSucceed
                    Ok _ ->
                        Err (400, "You have to exchange exactly 3 cards")
                    Err msg ->
                        Err (400, "Could not decode exchanged cards")
            else
                Err (400, "Tried to exchange card you don't own")
    )


{-| If condition is met (pass player to it), then
    execute update function on player.
-}
updateAndReturnIf condition update userName table round player =
    if condition player then
        put table.name { table | round =
            modifyPlayer userName update round
        } games
        |> okSucceed
    else
        Err (400, "")


{-| Declare tichu

1. Check if player may declare tichu - did not put any card in this round
2. Modify state

-}
declareTichu userName table round player =
    updateAndReturnIf
        (.cardsOnHand >> (==) 14)
        (\player -> { player | tichu = True })
        userName table round player


{-| Declare grand tichu

1. Check if player may declare grand - saw only 8 cards
2. Modify state

-}
declareGrandTichu userName table round player =
    updateAndReturnIf
        (.sawAllCards >> not)
        (\player -> { player
                    | grandTichu = True
                    , sawAllCards = True
                    })
        userName table round player


{-| If player does not want to play grand tichu, he may see all cards

1. Check if player saw all cards
2. Modify state

-}
seeAllCards userName table round player =
    updateAndReturnIf
        (.sawAllCards >> not)
        (\player -> { player | sawAllCards = True })
        userName table round player


giveDragon userName table round player giveToNext =
    Nothing
    |> orElse (
        case round.tableHandOwner of
            Just owner ->
                owner == round.actualPlayer
                && (getNthPlayer round owner).name == userName
            _ ->
                False
    ) "Not an actual player or not owner of Dragon"
    |> orElse (case List.head round.table of
        Just [ Dragon ] -> True
        _ -> False
    ) "There is no Dragon on top of table"
    |> executeIfNoError (
        let
            index =
                (round.actualPlayer + (if giveToNext then 1 else 3)) % 4
        in
            -- switch to next player and return ok
            put table.name
                { table | round = giveDragonTo index round }
                games
            |> okSucceed
    )


