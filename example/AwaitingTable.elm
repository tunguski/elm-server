module AwaitingTable exposing (..)


import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error)


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


awaitingTablesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
awaitingTablesApiPart api =
    P "awaitingTables"
        [ S
            (\id ->
                [ F
                    (\() ->
                        case parseRequestMethod api.request.method of
                            Get ->
                                api.doWithSession
                                    (\session ->
                                        get id awaitingTables
                                            |> andThen
                                                (\table ->
                                                    put id 
                                                        { table
                                                            | users =
                                                                List.map
                                                                    (\user ->
                                                                        case user of
                                                                            ( name, time ) ->
                                                                                if name /= session.username then
                                                                                    user
                                                                                else
                                                                                    ( name, api.request.time )
                                                                    )
                                                                    table.users
                                                        } awaitingTables
                                                        |> andThenReturn
                                                            (table |> (encode awaitingTableEncoder >> okResponse >> Task.succeed))
                                                )
                                            |> onError
                                                (\error ->
                                                    let
                                                        x =
                                                            Debug.log "error" error
                                                    in
                                                        statusResponse 404 |> Task.succeed
                                                )
                                    )

                            Post ->
                                api.doWithSession
                                    (\session ->
                                        (get id awaitingTables)
                                            -- create new table only if table was not found in db
                                            |>
                                                onError
                                                    (\error ->
                                                        let
                                                            table =
                                                                AwaitingTable id
                                                                    [ ( session.username, api.request.time ) ]
                                                        in
                                                            put id table awaitingTables
                                                                |> andThenReturn (Task.succeed table)
                                                    )
                                            -- if table exists, return error information that name is reserved
                                            -- return created table
                                            |>
                                                andThen
                                                    (encode awaitingTableEncoder >> okResponse >> Task.succeed)
                                    )

                            _ ->
                                statusResponse 405
                                    |> Result
                    )
                , P "join"
                    [ F
                        (\() ->
                            Result (okResponse ("[TODO] Joined: " ++ id))
                        )
                    ]
                ]
            )
        , F
            (\() ->
                listDocuments awaitingTables
                    |> andThen
                        (\tables ->
                            let
                                toUpdate =
                                    List.filter
                                        (.users >> List.any (\( name, time ) -> (time + (5 * second) < api.request.time)))
                                        tables.elements

                                updated =
                                    List.map
                                        (\table ->
                                            { table
                                                | users =
                                                    List.filter (\( name, time ) -> (time + (5 * second) > api.request.time)) table.users
                                            }
                                        )
                                        toUpdate

                                toRemove =
                                    (List.filter (.users >> List.isEmpty) tables.elements)
                                        ++ (List.filter (.users >> List.isEmpty) updated)
                            in
                                Task.sequence
                                    ((List.map (\table -> delete table.name awaitingTables) toRemove)
                                        ++ (updated
                                                |> List.filter (.users >> List.isEmpty >> not)
                                                |> List.map (\table -> put table.name table awaitingTables)
                                           )
                                    )
                                    |> andThen
                                        (\r ->
                                            listDocuments awaitingTables
                                        )
                        )
                    |> Task.attempt
                        (\result ->
                            case result of
                                Ok tables ->
                                    api.sendResponse
                                        (okResponse
                                            (encodeCollection awaitingTableEncoder tables)
                                        )
                                Err error ->
                                  (error |> toString >> response 500 >> api.sendResponse)
                        )
                    |> Command
            )
        ]


