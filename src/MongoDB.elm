module MongoDB exposing (..)


import Http exposing (..)
import Task
import Json.Decode as Json exposing (..)
import String exposing (concat)


get : String -> (Json.Decoder item) -> (item -> m) -> String -> Cmd (DbMsg m)
get baseUrl decoder msg collection =
  Http.get decoder
    (concat [ baseUrl, collection ])
    |> Task.mapError toString
    |> Task.perform 
        ErrorOccurred
        (DataFetched << msg)


listDocuments : String -> (Json.Decoder item) -> (Collection item -> m) -> String -> Cmd (DbMsg m)
listDocuments baseUrl decoder msg collection =
  get baseUrl (collectionDecoder decoder) msg collection


getDatabaseDescription : String -> (MongoDb -> m) -> Cmd (DbMsg m)
getDatabaseDescription baseUrl msg =
  get baseUrl mongoDbDecoder msg ""


type DbMsg msg
  = DataFetched msg
  | ErrorOccurred String


put decoder url body =
   fromJson decoder <|
   Http.send defaultSettings
    { verb = "PUT"
    , headers = []
    , url = url
    , body = body
    }


delete decoder url =
   fromJson decoder <|
   Http.send defaultSettings
    { verb = "DELETE"
    , headers = []
    , url = url
    , body = empty
    }


type alias MongoDb =
  { name : String
  , description : Maybe String
  , collections : List String
  }


mongoDbDecoder : Decoder MongoDb
mongoDbDecoder =
  Json.object3 MongoDb 
    ("_id" := Json.string)
    (maybe ("desc" := Json.string))
    (at ["_embedded", "rh:coll"] <| (Json.list ("_id" := Json.string)))


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


