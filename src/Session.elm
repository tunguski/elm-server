module Session exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Result exposing (toMaybe)


type alias Session =
  { username : String
  , token : String
  , loginTime : Maybe Date
  , lastRequestTime : Maybe Date
  , idUser : String
  }


dateParser : String -> Maybe Date
dateParser input =
  input |> fromString >> toMaybe



sessionDecoder : Decoder Session 
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| "loginTime" := string)
    (map dateParser <| "lastRequestTime" := string)
    ("idUser" := string)


