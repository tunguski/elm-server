module BaseModel exposing (..)


import Date exposing (Date, fromString)
import Result exposing (toMaybe)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


type alias Collection item =
  { name : String
  , description : Maybe String
  , elements : List item
  }


collectionDecoder : Decoder item -> Decoder (Collection item)
collectionDecoder itemDecoder =
  Json.object3 Collection 
    ("_id" := Json.string) 
    (maybe ("desc" := Json.string))
    (at ["_embedded", "rh:doc"] <| Json.list itemDecoder)


listDecoder : Decoder item -> Decoder (List item)
listDecoder =
  Json.list


listToValue encoder list =
  JE.list (List.map encoder list)


encodeCollection encoder collection =
  JE.encode 0 <| listToValue encoder collection.elements


encode encoder item =
  JE.encode 0 <| encoder item


maybeEncodeDate maybe =
  case maybe of
    Just date ->
      JE.float <| Date.toTime date
    Nothing ->
      JE.null


dateParser : Maybe String -> Maybe Date
dateParser input =
  case input of
    Just text ->
      text |> fromString >> toMaybe
    Nothing ->
      Nothing


