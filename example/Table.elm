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
                    Get ->
                        api.doWithSession (\session ->
                            listDocuments games
                            |> andThen (
                                encodeCollection gameEncoder
                                >> okResponse
                                >> Task.succeed
                            )
                            |> onError (toString >> response 500 >> Task.succeed)
                        )

                    _ ->
                        statusResponse 405 |> Result
            )
        ]


