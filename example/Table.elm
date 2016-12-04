module Table exposing (..)


import String
import Task exposing (Task)
import Http exposing (Error (BadResponse))


import BaseModel exposing (..)
import ExampleDb as Db 
import Server exposing (..)
import SessionModel exposing (Session)
import UrlParse exposing (..)
import TableModel exposing (..)


getTable : String -> Task Error Table
getTable idTable =
  Db.get (collectionDecoder tableDecoder) 
         ("tables?filter=" ++ (("{'name':'" ++ idTable ++ "'}")))
    `Task.andThen` (\tables ->
      case tables.elements of
        h :: t ->
          Task.succeed h
        _ ->
          Task.fail (BadResponse 404 "") 
    )


putTable : String -> Table -> Task Error String
putTable name table =
  Db.put ("tables/" ++ name)
    (encode tableEncoder table)


andThen = flip Task.andThen
onError = flip Task.onError


tablesApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
tablesApiPart request doWithSession sendResponse =
  P "tables" 
    [ S (\id -> 
          [ P "join"
              [ F (\() -> 
                    Result (okResponse ("[TODO] Joined: " ++ id))
                  )
              ]
          , F (\() -> 
                doWithSession (\session ->
                  getTable id
                    |> andThen 
                        (encode tableEncoder >> okResponse >> Task.succeed)
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
                  (getTable request.body)
                    -- create new table only if table was not found in db
                    |> onError (\error ->
                      let
                        table = 
                          (Table request.body [ session.username ])
                      in
                        putTable request.body table
                        `Task.andThen` (\r -> Task.succeed table)
                    )
                    -- if table exists, return error information that name is reserved

                    -- return created table
                    |> andThen 
                        (encode tableEncoder >> okResponse >> Task.succeed)
                )
              "get" -> 
                Db.get (collectionDecoder tableDecoder) "tables"
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


