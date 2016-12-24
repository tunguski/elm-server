module Table exposing (..)

import String
import Task exposing (..)
import Http exposing (Error)
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
    Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
tablesApiPart request doWithSession sendResponse =
    P "tables"
        [ S
            (\id ->
                [ F
                    (\() ->
                        doWithSession
                            (\session ->
                                get id games
                                    |> andThen
                                        (\table ->
                                            table |> (encode gameEncoder >> okResponse >> Task.succeed)
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
                    )
                ]
            )
        , F
            (\() ->
                case Debug.log "method" (String.toLower request.method) of
                    "post" ->
                        doWithSession
                            (\session ->
                                (get request.body games)
                                    -- create new table only if table was not found in db
                                    |>
                                        onError
                                            (\error ->
                                                let
                                                    table =
                                                        initialGame "test"
                                                in
                                                    put request.body table games
                                                        |> andThenReturn (Task.succeed table)
                                            )
                                    -- if table exists, return error information that name is reserved
                                    -- return created table
                                    |>
                                        andThen
                                            (encode gameEncoder >> okResponse >> Task.succeed)
                            )

                    "get" ->
                        listDocuments games
                            |> Task.attempt
                                (\result ->
                                    case result of
                                        Ok tables ->
                                            sendResponse
                                                (okResponse
                                                    (encodeCollection gameEncoder tables)
                                                )
                                        Err error ->
                                          (error |> toString >> response 500 >> sendResponse)
                                )
                            |> Command

                    _ ->
                        statusResponse 405
                            |> Result
            )
        ]
