module Session exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Result exposing (toMaybe)


import MongoDb exposing (DbMsg, Collection)
import ExampleDb exposing (..)


type alias Session =
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


sessionDecoder : Decoder Session 
sessionDecoder =
  Json.object5 Session
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


sessionEncoder : Session -> Value
sessionEncoder session =
  JE.object 
    [ ("username", JE.string session.username)
    , ("token", JE.string session.token)
    , ("loginTime", maybeEncodeDate session.loginTime)
    , ("lastRequestTime", maybeEncodeDate session.lastRequestTime)
    , ("idUser", JE.string session.idUser)
    ]


encodeSession : Session -> String 
encodeSession session =
  JE.encode 0 <| sessionEncoder session


getSession : String -> (DbMsg Session -> msg) -> Cmd msg
getSession idSession msg =
  get sessionDecoder msg ("session/" ++ idSession)


