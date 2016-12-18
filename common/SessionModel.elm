module SessionModel exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


import BaseModel exposing (..)


type alias Session =
  { username : String
  , token : String
  , loginTime : Date
  , lastRequestTime : Date
  , idUser : String
  }


sessionDecoder : Decoder Session 
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| "loginTime" := float)
    (map dateParser <| "lastRequestTime" := float)
    ("idUser" := string)


sessionEncoder : Session -> Value
sessionEncoder session =
  JE.object 
    [ ("username", JE.string session.username)
    , ("token", JE.string session.token)
    , ("loginTime", JE.float <| Date.toTime session.loginTime)
    , ("lastRequestTime", JE.float <| Date.toTime session.lastRequestTime)
    , ("idUser", JE.string session.idUser)
    ]


encodeSession : Session -> String 
encodeSession session =
  JE.encode 0 <| sessionEncoder session


