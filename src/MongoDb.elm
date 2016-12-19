module MongoDb exposing (DbMsg(..), Db, DbCollection, createDb, createDbCollection, perform)


import Http exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Platform.Cmd as Cmd
import Result exposing (Result(..))
import String exposing (concat)
import Task exposing (Task)


import BaseModel exposing (Collection, collectionDecoder)
import Rest 


type DbMsg msg
  = DataFetched msg
  | ErrorOccurred Http.Error


type alias Db item =
  { getString : String -> Task Error String
  , get : (Json.Decoder item) -> String -> Task Error item
  , put : String -> String -> Task Error String
  , delete : String -> Task Error String
  , listDocuments : (Json.Decoder item) -> String -> Task Error (Collection item)
  }


type alias DbCollection item =
  { get : String -> Task Error item
  --, post : item -> Task Error String
  , put : String -> item -> Task Error String
  , delete : String -> Task Error String
  , all : Task Error (Collection item)
  --, find : String -> Task Error (Collection item)
  }


createDb : String -> Db item
createDb url =
    Db (getString url)
       (get url)
       (put url)
       (delete url)
       (listDocuments url)


createDbCollection : String -> String -> Json.Decoder item -> (item -> JE.Value) -> DbCollection item
createDbCollection url name decoder encoder =
    DbCollection (get (url ++ name ++ "/") decoder)
                 (\id item -> put (url ++ name ++ "/") id (JE.encode 2 <| encoder item))
                 (delete (url ++ name ++ "/"))
                 (listDocuments url decoder name)


mapRestMsg msg =
  case msg of
    Ok value ->
      DataFetched value
    Err error ->
      ErrorOccurred error


perform : (DbMsg value -> m) -> Task Error value -> Cmd m
perform msg task = 
  Rest.perform (mapRestMsg >> msg) task


get : String -> (Json.Decoder item) -> String -> Task Error item
get = Rest.get


getString : String -> String -> Task Error String
getString baseUrl collection =
  Http.getString <| Debug.log "url" (baseUrl ++ collection)


listDocuments : String -> (Json.Decoder item) -> String -> Task Error (Collection item)
listDocuments baseUrl decoder collection =
  get baseUrl (collectionDecoder decoder) collection


getDatabaseDescription : String -> Task Error MongoDb
getDatabaseDescription baseUrl =
  get baseUrl mongoDbDecoder ""


put : String -> String -> String -> Task Error String
put = Rest.put


delete = Rest.delete


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


