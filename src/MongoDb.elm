module MongoDb exposing (..)


import Http exposing (..)
import Task exposing (Task)
import Json.Decode as Json exposing (..)
import String exposing (concat)
import Platform.Cmd as Cmd


type DbMsg msg
  = DataFetched msg
  | ErrorOccurred Http.Error


perform : (DbMsg value -> m) -> Task Error value -> Cmd m
perform msg task = 
  task
    |> Task.perform ErrorOccurred DataFetched
    |> Cmd.map msg


get : String -> (Json.Decoder item) -> String -> Task Error item
get baseUrl decoder collection =
  Http.get decoder (baseUrl ++ collection)


listDocuments : String -> (Json.Decoder item) -> String -> Task Error (Collection item)
listDocuments baseUrl decoder collection =
  get baseUrl (collectionDecoder decoder) collection


getDatabaseDescription : String -> Task Error MongoDb
getDatabaseDescription baseUrl =
  get baseUrl mongoDbDecoder ""


put : String -> String -> String -> (DbMsg String -> m) -> Cmd m
put baseUrl url body msg =
   (Http.send defaultSettings
    { verb = "PUT"
    , headers = [ ("Content-Type", "application/json") ]
    , url = baseUrl ++ url
    , body = Http.string body
    })
    |> fromJson Json.string
    |> perform msg


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


