module MongoDb exposing (DbMsg(..), Db, DbCollection, createDb, createDbCollection, perform)


import Http exposing (..)
import Task exposing (Task)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String exposing (concat)
import Platform.Cmd as Cmd
import BaseModel exposing (Collection, collectionDecoder)


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


perform : (DbMsg value -> m) -> Task Error value -> Cmd m
perform msg task = 
  task
    |> Task.perform ErrorOccurred DataFetched
    |> Cmd.map msg


get : String -> (Json.Decoder item) -> String -> Task Error item
get baseUrl decoder collection =
  Http.get decoder <| Debug.log "url" (baseUrl ++ collection)


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
put baseUrl url body =
   (Http.send defaultSettings
    { verb = "PUT"
    , headers = [ ("Content-Type", "application/json") ]
    , url = baseUrl ++ url
    , body = Http.string body
    })
    |> Task.mapError (\error ->
      case error of
        RawTimeout -> Timeout
        RawNetworkError -> NetworkError
    )
    |> Task.map (\response ->
      case response.value of
        Text body -> body
        _ -> ""
    )
    --|> fromJson Json.string


delete baseUrl url =
   (Http.send defaultSettings
    { verb = "DELETE"
    , headers = []
    , url = baseUrl ++ url
    , body = empty
    })
    |> Task.mapError (\error ->
      case error of
        RawTimeout -> Timeout
        RawNetworkError -> NetworkError
    )
    |> Task.map (\response ->
      case response.value of
        Text body -> body
        _ -> ""
    )


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


