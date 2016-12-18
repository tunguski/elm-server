module Table exposing (..)


import String
import Task exposing (Task)
import Http exposing (Error (BadResponse))


import BaseModel exposing (..)
import ExampleDb exposing (..) 
import Server exposing (..)
import SessionModel exposing (Session)
import UrlParse exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)


tablesApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
tablesApiPart request doWithSession sendResponse =
  P "tables" 
    [ S (\id -> 
          [ F (\() -> 
                doWithSession (\session ->
                  games.get id
                    |> andThen 
                        (\table ->
                          table |> (encode gameEncoder >> okResponse >> Task.succeed)
                        )
                    |> onError (\error -> 
                        let
                          x = Debug.log "error" error
                        in
                          statusResponse 404 |> Task.succeed
                    )
                )
              )
          ]
        )
    , F (\() ->
            case Debug.log "method" (String.toLower request.method) of
              "post" ->
                doWithSession (\session ->
                  (games.get request.body)
                    -- create new table only if table was not found in db
                    |> onError (\error ->
                      let
                        table = 
                          initialGame "test"
                      in
                        games.put request.body table
                        |> andThenReturn (Task.succeed table)
                    )
                    -- if table exists, return error information that name is reserved

                    -- return created table
                    |> andThen 
                        (encode gameEncoder >> okResponse >> Task.succeed)
                )
              "get" -> 
                games.all
                  |> Task.perform
                       (toString >> response 500 >> sendResponse)
                       (\tables -> sendResponse (okResponse 
                         (encodeCollection gameEncoder tables)))
                  |> Command
              _ ->
                statusResponse 405
                  |> Result
        )
    ]


