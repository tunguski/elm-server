module Table exposing (..)


import String
import Task exposing (Task)
import Http exposing (Error)


import BaseModel exposing (..)
import ExampleDb exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import UrlParse exposing (..)
import TableModel exposing (..)


getTable : String -> Task Error Table
getTable idTable =
  get tableDecoder ("tables/" ++ idTable)


putTable : String -> Table -> Task Error String
putTable name table =
  put ("tables/" ++ name)
    (encode tableEncoder table)


tablesApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
tablesApiPart request doWithSession sendResponse =
  P "tables" 
    [ I (\id -> 
          [ F (\() -> 
                  Result (response 404 (toString id))
              )
          ]
        )
    , F (\() ->
            case Debug.log "method" (String.toLower request.method) of
              "post" ->
                doWithSession (\session ->
                  (getTable request.body)
                    -- create new table only if table was not found in db
                    `Task.onError` (\error ->
                      let
                        table = 
                          (Table request.body [ session.username ])
                      in
                        putTable request.body table
                        `Task.andThen` (\r -> Task.succeed table)
                    )
                    -- if table exists, return error information that name is reserved

                    -- return created table
                    `Task.andThen` (\table -> 
                      succeedTaskToString <| Debug.log "table" table
                    )
                )
              "get" -> 
                get (collectionDecoder tableDecoder) "tables"
                  |> Task.perform
                       (toString >> response 500 >> sendResponse)
                       (\tables -> sendResponse (okResponse 
                         (encodeCollection tableEncoder tables)))
                  |> Command
              _ ->
                statusResponse 405
                  |> Result
                  )
    ]



