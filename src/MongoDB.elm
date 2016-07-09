module MongoDB exposing (..)


import Http exposing (..)
import Task exposing (..)
import Json.Decode exposing (..)
import Json.Decode as Json exposing ((:=))
import String exposing (concat)


type alias RestBackend value =
  { get     : Decoder value -> String -> Task Error value
  , post    : Decoder value -> String -> Body -> Task Error value
  , put     : Decoder value -> String -> Body -> Task Error value
  , delete  : Decoder value -> String -> Task Error value
  }


type alias MongoDbApi =
  { create : String -> String
  , find : String -> String
  , get : String -> String
  , getById : String -> String
  }


httpRest : RestBackend value
httpRest = RestBackend
  Http.get
  Http.post
  (\decoder url body ->
   fromJson decoder <|
   Http.send defaultSettings
    { verb = "PUT"
    , headers = []
    , url = url
    , body = body
    })
  (\decoder url ->
   fromJson decoder <|
   Http.send defaultSettings
    { verb = "DELETE"
    , headers = []
    , url = url
    , body = empty
    })



database : RestBackend value -> String -> MongoDbApi
database rest url =
  { get = (\s -> concat [ url, s ])
  , getById = (\s -> concat [ url, s ])
  , find = (\s -> concat [ url, s ])
  , create = (\s -> concat [ url, s ])
  }


type alias MongoDb =
  { name : String
  , description : Maybe String
  }


mongoDbDecoder : Decoder MongoDb
mongoDbDecoder =
  Json.object2 MongoDb 
    ("_id" := Json.string) 
    (maybe ("desc" := Json.string))


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


