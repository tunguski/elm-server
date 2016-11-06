module SessionModel exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


import BaseModel exposing (..)


type alias Session =
  { username : String
  , token : String
  , loginTime : Maybe Date
  , lastRequestTime : Maybe Date
  , idUser : String
  }


sessionDecoder : Decoder Session 
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| maybe <| "loginTime" := string)
    (map dateParser <| maybe <| "lastRequestTime" := string)
    ("idUser" := string)


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


