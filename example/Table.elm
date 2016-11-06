module Table exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String
import Result exposing (toMaybe)
import Task exposing (Task)
import Http exposing (Error)


import MongoDb exposing (DbMsg, Collection)
import ExampleDb exposing (..)
import Server exposing (..)
import UrlParse exposing (..)


type alias Table =
  { username : String
  , token : String
  , loginTime : Maybe Date
  , lastRequestTime : Maybe Date
  , idUser : String
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
  Json.object5 Table
    ("username" := string)
    ("token" := string)
    (map dateParser <| maybe <| "loginTime" := string)
    (map dateParser <| maybe <| "lastRequestTime" := string)
    ("idUser" := string)


maybeEncodeDate maybe =
  case maybe of
    Just date ->
      JE.float <| Date.toTime date
    Nothing ->
      JE.null


tableEncoder : Table -> Value
tableEncoder table =
  JE.object 
    [ ("username", JE.string table.username)
    , ("token", JE.string table.token)
    , ("loginTime", maybeEncodeDate table.loginTime)
    , ("lastRequestTime", maybeEncodeDate table.lastRequestTime)
    , ("idUser", JE.string table.idUser)
    ]


encodeTable : Table -> String 
encodeTable table =
  JE.encode 0 <| tableEncoder table


getTable : String -> Task Error Table
getTable idTable =
  get tableDecoder ("table/" ++ idTable)


tablesApiPart : Request
    -> ((a -> Task Error Response) -> Partial msg)
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
            case String.toLower request.method of
              "post" ->
                doWithSession (\session ->
                  (getTable "fds")
                    `Task.andThen` succeedTask
                )
              "get" -> 
                get Json.string ("tables") 
                  `Task.andThen` succeedTask
                  |> Task.perform
                       (\error -> sendResponse (response 500 (toString error)))
                       (\table -> sendResponse (okResponse (toString table)))
                  |> Command
              _ ->
                statusResponse 405
                  |> Result
                  )
    ]



