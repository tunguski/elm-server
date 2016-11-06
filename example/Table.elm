module Table exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String
import Result exposing (toMaybe)
import Task exposing (Task)
import Http exposing (Error)


import MongoDb exposing (DbMsg, Collection, collectionDecoder)
import ExampleDb exposing (..)
import Server exposing (..)
import Session exposing (Session)
import UrlParse exposing (..)


type alias Table =
  { name : String
  , players : List String
  }


dateParser : Maybe String -> Maybe Date
dateParser input =
  case input of
    Just text ->
      text |> fromString >> toMaybe
    Nothing ->
      Nothing


tableDecoder : Decoder Table 
tableDecoder =
  Json.object2 Table
    ("name" := string)
    ("players" := list string)


maybeEncodeDate maybe =
  case maybe of
    Just date ->
      JE.float <| Date.toTime date
    Nothing ->
      JE.null


listToValue encoder list =
  JE.list (List.map encoder list)


tableEncoder : Table -> Value
tableEncoder table =
  JE.object 
    [ ("name", JE.string table.name)
    , ("players", listToValue JE.string table.players)
    ]


encodeTable : Table -> String 
encodeTable table =
  JE.encode 0 <| tableEncoder table


getTable : String -> Task Error Table
getTable idTable =
  get tableDecoder ("tables/" ++ idTable)


putTable : String -> Table -> Task Error String
putTable name table =
  put ("tables/" ++ name)
    (JE.encode 0 <| tableEncoder table)


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
                         (JE.encode 0 <| listToValue tableEncoder tables.elements)))
                  |> Command
              _ ->
                statusResponse 405
                  |> Result
                  )
    ]



