module MongoDb exposing (..)


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


perform : (DbMsg value -> m) -> Task Error value -> Cmd m
perform msg task = 
  task
    |> Task.perform ErrorOccurred DataFetched
    |> Cmd.map msg


get : String -> (Json.Decoder item) -> String -> Task Error item
get baseUrl decoder collection =
  Http.get decoder <| Debug.log "url" (baseUrl ++ collection)


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


