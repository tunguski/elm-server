module Game exposing (..)


import Array
import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (..)
import UrlParse exposing (..)


gamesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
gamesApiPart api =
    P "games"
        [ S (\id ->
            [ F
                (\() ->
                    case api.request.method of
                        Get ->
                            getGame api id

                        _ ->
                            statusResponse 405 |> Result
                )
            , PF "seeAllCards" (\() -> seeAllCards api id)
            , PF "exchangeCards" (\() -> exchangeCards api id)
            , PF "declareTichu" (\() -> declareTichu api id)
            , PF "declareGrandTichu" (\() -> declareGrandTichu api id)
            , PF "pass" (\() -> pass api id)
            , PF "hand" (\() -> hand api id)
            ]
          )
        ]


getActualPlayer : Round -> Player
getActualPlayer round =
    Array.get (round.actualPlayer % 4) round.players
    |> defaultCrash ("Malformed state getActualPlayer: " ++ toString round)


openDemand round =
    case round.demand of
        Just r -> not round.demandCompleted
        Nothing -> False


openDemandMatch round =
    case round.demand of
        Just r ->
            List.any (\card ->
                case card of
                    NormalCard suit rank ->
                        (not round.demandCompleted) && (rank == r)
                    _ ->
                        False
            ) (getActualPlayer round).hand
        Nothing ->
            False


defaultCrash text item =
    case item of
        Just value -> value
        Nothing -> Debug.crash text


getPlayer round name =
    Array.filter (.name >> (==) name) round.players
    |> Array.get 0
    |> defaultCrash ("Malformed state getPlayer: " ++ name ++ ": " ++ toString round)


modifyPlayer round name function =
    { round | players =
        Array.map (\player ->
            case player.name == name of
                True -> function player
                False -> player
        ) round.players
    }


doWithTable api id function =
    api.doWithSession (\session ->
        get id games |> andThen (\table ->
            function session table table.round
                (getPlayer table.round session.username))
    )


orElse : Bool -> String -> Maybe String -> Maybe String
orElse condition errorText maybe =
    case maybe of
        Just text ->
            Just text
        Nothing ->
            case condition of
                True -> Nothing
                False -> Just errorText


executeIfNoError exec maybe =
    case maybe of
        Just error ->
            response 400 (Debug.log "error: " error) |> Task.succeed
        Nothing ->
            exec


{-| Pass player's move

1. Check is it actual player
2. Check if player may pass - there is no requirement or player does not have that card
3. Update actual player
4. Save state

-}
pass : ApiPartApi msg -> String -> Partial msg
pass api id =
    doWithTable api id (\session table round player ->
        Nothing
        |> orElse ((getActualPlayer round).name == session.username) "Not an actual player"
        |> orElse (not <| openDemandMatch round) "You have demanded card"
        |> executeIfNoError (
                -- switch to next player and return ok
                put table.name { table |
                    round = { round | actualPlayer = round.actualPlayer + 1 % 4 } } games
                |> andThenReturn (statusResponse 200 |> Task.succeed)
        )
    )


{-| Exchange cards

1. Check is it start of round
2. Check if player did not exchange cards
3. Check if player did not
4. Update actual player
5. Save state

-}
exchangeCards : ApiPartApi msg -> String -> Partial msg
exchangeCards api id =
    doWithTable api id (\session table round player ->
        let
            exchangeCards = decodeCards api.request.body
        in
            case player.exchange of
                Nothing ->
                    case exchangeCards of
                        Ok (a :: b :: c :: []) ->
                            put table.name { table | round =
                                modifyPlayer round session.username
                                    (\player -> { player | exchange = Just (a, b, c) })
                            } games
                            |> andThenReturn (statusResponse 200 |> Task.succeed)
                        Ok _ ->
                            statusResponse 400 |> Task.succeed
                        Err msg ->
                            statusResponse 400 |> Task.succeed
                _ ->
                    statusResponse 400 |> Task.succeed
    )


{-| Place a hand on table

1. Check if it is actual player or played a bomb (higher if there was bomb before)
2. Place hand on table
3. Remove cards from player's hand
4. Update actual player
5. Save state

-}
hand : ApiPartApi msg -> String -> Partial msg
hand api id =
    doWithTable api id (\session table round player ->
        let
            isActualPlayer =
                (getActualPlayer round).name == session.username
            hasDemandedCard = openDemandMatch round
            isOpenDemand = openDemand round
            trick = parseTrick round session.username
            isDemandedCardInTrick = cardInTrick round.demand player.selection
            isBomb = bomb trick
            bombEnoughPower = True
        in
            if isActualPlayer then
                if isOpenDemand then
                    if hasDemandedCard then
                        if isDemandedCardInTrick then
                            statusResponse 400 |> Task.succeed
                        else
                            statusResponse 400 |> Task.succeed
                    else
                        statusResponse 400 |> Task.succeed
                else
                    -- play, switch to next player and return ok
                    put table.name { table |
                        round = { round
                            | actualPlayer = round.actualPlayer + 1 % 4
                        }
                    } games
                    |> andThenReturn (statusResponse 200 |> Task.succeed)
            else
                if isBomb then
                    if bombEnoughPower then
                        -- play, switch to next player and return ok
                        put table.name { table |
                            round = { round
                                | actualPlayer = round.actualPlayer + 1 % 4
                            }
                        } games
                        |> andThenReturn (statusResponse 200 |> Task.succeed)
                    else
                        statusResponse 400 |> Task.succeed
                else
                    statusResponse 400 |> Task.succeed
    )


{-| Declare tichu

1. Check if player may declare tichu - did not put any card in this round
2. Modify state

-}
declareTichu : ApiPartApi msg -> String -> Partial msg
declareTichu api id =
    updateAndReturnIf api id
        (.cardsOnHand >> (==) 14)
        (\player -> { player | tichu = True })


{-| Declare grand tichu

1. Check if player may declare grand - saw only 8 cards
2. Modify state

-}
declareGrandTichu : ApiPartApi msg -> String -> Partial msg
declareGrandTichu api id =
    updateAndReturnIf api id
        (.sawAllCards >> not)
        (\player -> { player
                    | grandTichu = True
                    , sawAllCards = True
                    })


{-| If player does not want to play grand tichu, he may see all cards

1. Check if player saw all cards
2. Modify state

-}
seeAllCards : ApiPartApi msg -> String -> Partial msg
seeAllCards api id =
    updateAndReturnIf api id
        (.sawAllCards >> not)
        (\player -> { player | sawAllCards = True })


{-| If condition is met (pass player to it), then
    execute update function on player.
-}
updateAndReturnIf api id condition update =
    doWithTable api id (\session table round player ->
        if condition player then
            put table.name { table | round =
                modifyPlayer round session.username update
            } games
            |> andThenReturn (statusResponse 200 |> Task.succeed)
        else
            statusResponse 400 |> Task.succeed
    )


{-| Return awaiting table with id.
-}
getGame : ApiPartApi msg -> String -> Partial msg
getGame api id =
    api.doWithSession
        (\session ->
            get id games
            |> andThen
                (\table ->
                    put id
                        { table
                        | users = Array.map (\user ->
                                        if user.name /= session.username then
                                            user
                                        else
                                            { user | lastCheck = api.request.time }
                                    )
                                    table.users
                        } games
                    |> andThenReturn (
                        let
                            round = table.round
                        in
                            ({ table
                             | round =
                                { round
                                | players = Array.map (\player ->
                                        if player.name == session.username then
                                            if player.sawAllCards then
                                                player
                                            else
                                                { player
                                                | hand = List.take 8 player.hand
                                                }
                                        else
                                            { player
                                            | hand = []
                                            , exchange = Nothing
                                            }
                                    ) round.players
                                , seed = 0
                                }
                             , seed = 0
                             }
                            |> (encode gameEncoder >> okResponse >> Task.succeed)))
                )
            |> onError logErrorAndReturn
        )


