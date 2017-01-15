module Game exposing (..)


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
        |> orElse player.sawAllCards "Before playing you have to decide playing Grand Tichu or not"
        |> orElse ((getActualPlayer round).name == session.username) "Not an actual player"
        |> orElse (not <| openDemandMatch round) "You have demanded card"
        |> orElse (not <| hasCard MahJong player) "You have MahJong"
        |> orElse (not <| ifNothing round.tableHandOwner) "You cannot pass if you won last table"
        |> executeIfNoError (
            -- switch to next player and return ok
            put table.name { table | round =
                incActualPlayer round
                |> maybeCollectCards
            } games
            |> andThenReturn (statusResponse 200 |> Task.succeed)
        )
    )


ifNothing maybe =
    case maybe of
        Just _ -> False
        Nothing -> True


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
        Nothing
        |> orElse (ifNothing player.exchange) "You have exchanged already"
        |> executeIfNoError (
            let
                exchangeCards = decodeCards api.request.body
                hasAllCards =
                    case exchangeCards of
                        Ok cards ->
                            List.all (flip hasCard <| player) cards
                        Err _ ->
                            False
            in
                if hasAllCards then
                case exchangeCards of
                    Ok (a :: b :: c :: []) ->
                        put table.name
                            ({ table | round =
                                modifyPlayer session.username
                                    (\player -> { player | exchange = Just (a, b, c) })
                                    round
                            }
                            |> (\t ->
                                case List.all (\p -> not <| ifNothing p.exchange) t.round.players of
                                    True ->
                                        exchangeCardsBetweenPlayers t
                                    False ->
                                        t
                            ))
                        games
                        |> andThenReturn (statusResponse 200 |> Task.succeed)
                    Ok _ ->
                        response 400 "You have to exchange exactly 3 cards" |> Task.succeed
                    Err msg ->
                        response 400 "Could not decode exchanged cards" |> Task.succeed
                else
                    response 400 "Tried to exchange card you don't own" |> Task.succeed
        )
    )


handWithParsedCards table round player param =
    let
        playCards =
            put table.name ({ table |
                round = incActualPlayer round
                    |> modifyPlayer player.name
                        (\player -> { player
                            | hand = removeCards param.parsedCards player.hand
                            , cardsOnHand = List.length (removeCards param.parsedCards player.hand)
                        })
                    |> putCardsOnTable param.parsedCards
                    |> setTableHandOwnerAsActualPlayer round.actualPlayer
                    |> Debug.log "handWithParsedCards"
            }
            |> maybeEndRound
            ) games
            |> andThenReturn (statusResponse 200 |> Task.succeed)
    in
        if param.isActualPlayer then
            if param.isOpenDemand then
                if param.hasDemandedCard then
                    if param.isDemandedCardInTrick then
                        if param.allowedTrick then
                            playCards
                        else
                            response 400 "Trick does not match the one on table" |> Task.succeed
                    else
                        response 400 "You have to play demanded card" |> Task.succeed
                else
                    if param.allowedTrick then
                        playCards
                    else
                        response 400 "Trick does not match the one on table" |> Task.succeed
            else
                -- play, switch to next player and return ok
                playCards
        else
            if param.isBomb then
                if param.bombEnoughPower then
                    -- play, switch to next player and return ok
                    put table.name ({ table | round =
                        incActualPlayer round
                        |> setTableHandOwnerAsActualPlayer round.actualPlayer
                        |> Debug.log "handWithParsedCards"
                    }
                    |> maybeEndRound
                    ) games
                    |> andThenReturn (statusResponse 200 |> Task.succeed)
                else
                    response 400 "Too weak bomb" |> Task.succeed
            else
                response 400 "You are not actual player" |> Task.succeed


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
        case decodeCards api.request.body of
            Ok cards ->
                Nothing
                |> orElse (List.all (flip hasCard <| player) cards) "Tried to play card you don't own"
                |> orElse (List.all (\player ->
                        case player.exchange of
                            Just _ -> True
                            Nothing -> False
                    ) round.players
                ) "You have to exchange cards first"
                |> executeIfNoError (
                    handWithParsedCards
                        table
                        round
                        player
                        { isActualPlayer = (getActualPlayer round).name == session.username
                        , isOpenDemand = openDemand round
                        , hasDemandedCard = openDemandMatch round
                        , parsedCards = cards
                        , trick = parseTrick cards
                        , allowedTrick = allowedCombination (List.head round.table) cards
                        , isDemandedCardInTrick = cardInTrick round.demand cards
                        , isBomb = bomb (parseTrick cards)
                        , bombEnoughPower = True
                        }
                )
            _ ->
                response 400 "Malformed cards" |> Task.succeed
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
                modifyPlayer session.username update round
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
                        | users = List.map (\user ->
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
                                | players = List.map (\player ->
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


