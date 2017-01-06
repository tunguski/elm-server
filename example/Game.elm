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
            , PF "declareTichu" (\() -> declareTichu api id)
            , PF "declareGrandTichu" (\() -> declareGrandTichu api id)
            , PF "pass" (\() -> pass api id)
            , PF "hand" (\() -> hand api id)
--            , PF "" (\() ->  api id)
--            , PF "" (\() ->  api id)
            ]
          )
        ]


{-| Pass player's move

1. Check is it actual player
2. Check if player may pass - there is no requirement or player does not have that card
3. Update actual player
4. Save state

-}
pass api id =
    doWithTable api id (\session table ->
        statusResponse 500 |> Task.succeed
    )


{-| Place a hand on table

1. Check if it is actual player or played a bomb (higher if there was bomb before)
2. Place hand on table
3. Remove cards from player's hand
4. Update actual player
5. Save state

-}
hand api id =
    doWithTable api id (\session table ->
        statusResponse 500 |> Task.succeed
    )


{-| Declare tichu

1. Check if player may declare tichu - did not put any card in this round
2. Modify state

-}
declareTichu api id =
    doWithTable api id (\session table ->
        statusResponse 500 |> Task.succeed
    )


{-| Declare grand tichu

1. Check if player may declare grand - saw only 8 cards
2. Modify state

-}
declareGrandTichu api id =
    doWithTable api id (\session table ->
        statusResponse 500 |> Task.succeed
    )


doWithTable api id function =
    api.doWithSession (\session ->
        get id games |> andThen (function session)
    )


{-| Return awaiting table with id.
-}
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
                    |> andThenReturn
                        (table |> (encode gameEncoder >> okResponse >> Task.succeed))
                )
            |> onError logErrorAndReturn
        )


