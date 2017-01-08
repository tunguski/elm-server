module Table exposing (..)

import String
import Task exposing (..)
import Http exposing (Error)


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import UrlParse exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)


tablesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
tablesApiPart api =
    P "tables"
        [ S
            (\id ->
                [ F
                    (\() ->
                        api.doWithSession (\session ->
                            get id games
                            |> andThen (\table ->
                                table |> (encode gameEncoder >> okResponse >> Task.succeed)
                            )
                            |> onError (\error ->
                                let
                                    x =
                                        Debug.log "error" error
                                in
                                    statusResponse 404 |> Task.succeed
                            )
                        )
                    )
                ]
            )
        , F
            (\() ->
                case api.request.method of
                    Post ->
                        api.doWithSession (\session ->
                            (get api.request.body games)
                            -- create new table only if table was not found in db
                            |> onError (\error ->
                                let
                                    table =
                                        initGame "test" 0 []
                                in
                                    put api.request.body table games
                                    |> andThenReturn (Task.succeed table)
                            )
                            -- if table exists, return error information that name is reserved
                            -- return created table
                            |> andThen (encode gameEncoder >> okResponse >> Task.succeed)
                        )

                    Get ->
                        listDocuments games
                            |> Task.attempt (\result ->
                                case result of
                                    Ok tables ->
                                        api.sendResponse
                                            (okResponse
                                                (encodeCollection gameEncoder tables)
                                            )
                                    Err error ->
                                      (error |> toString >> response 500 >> api.sendResponse)
                            )
                            |> Command

                    _ ->
                        statusResponse 405
                        |> Result
            )
        ]


