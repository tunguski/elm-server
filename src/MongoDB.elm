module MongoDB exposing (..)


import Http exposing (..)
import Task exposing (..)
import Json.Decode exposing (..)
import Json.Decode as Json exposing ((:=))
import String exposing (concat)


listDocuments : String -> (Json.Decoder item) -> (item -> m) -> String -> Cmd (DbMsg m)
listDocuments baseUrl decoder msg collection =
  Http.get (collectionDecoder decoder) 
    (concat [ baseUrl, collection ])
    |> Task.mapError toString
    |> Task.perform 
        (ErrorOccurred)
        (\data -> DataFetched (msg data))


getDatabaseDescription : String -> (item -> m) -> Cmd (DbMsg m)
getDatabaseDescription baseUrl msg =
  listDocuments baseUrl mongoDbDecoder msg ""


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
    (at ["_embedded", "rh:coll"] <| (Json.list Json.string))


type alias Collection item =
  { name : String
  , description : String
  , elements : List item
  }


collectionDecoder : Decoder item -> Decoder (Collection item)
collectionDecoder itemDecoder =
  Json.object3 Collection 
    ("_id" := Json.string) 
    ("desc" := Json.string) 
    (at ["_embedded", "rh:doc"] <| Json.list itemDecoder)


