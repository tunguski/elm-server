module TichuLogic exposing (..)

import Array
import List exposing (..)
import Http exposing (Error(..))
import Maybe exposing (andThen)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Time exposing (Time)
import Task exposing (..)


import ExampleDb exposing (games)
import Server exposing (..)
import Rest exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (User)


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


handWithParsedCards : Game -> Round -> Player -> HandParams -> Result (Int, String) (Task Error Response)
handWithParsedCards table round player param =
    let
        doPlayCards =
            put table.name
                (playHandAndUpdateRound table round player param)
                games
            |> andThenReturn (statusResponse 200 |> Task.succeed)
            |> Ok
        playCards =
            if param.allowedTrick then
                doPlayCards
            else
                Err (400, "Trick does not match the one on table")
    in
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


buildHandParams userName table round player cards =
    { isActualPlayer = (getActualPlayer round).name == userName
    , isOpenDemand = openDemand round
    , hasDemandedCard = openDemandMatch round
    , parsedCards = cards
    , trick = Debug.log "trick" <| parseTrick cards
    , allowedTrick = allowedCombination (List.head round.table) cards
    , isDemandedCardInTrick = cardInTrick round.demand cards
    , isBomb = bomb (parseTrick cards)
    , bombEnoughPower = True
    }


