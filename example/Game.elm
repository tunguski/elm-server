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
            ]
          )
        ]


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


