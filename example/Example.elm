module Example exposing (..)


import Html exposing (..)
import Html.App as Html
import String exposing (concat)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Debug
import Dict exposing (Dict, empty)


import Server exposing (..)
import MongoDB exposing (..)


main =
  Server.program init update


db = database httpRest "http://localhost:8888/"
rest = httpRest


-- UPDATE


type Msg item
  = DataFetched Requests item
  | ErrorOccurred Requests String


type Requests
  = GetDb
  | GetColl


init : Initializer (Msg (Collection RepoInfo))
init request =
  ( Response request.id 200 "", [ listDocuments "coll" ] )


update : Updater (Msg item)
update request msg response =
  case msg of
    DataFetched reqType repositories ->
      ({ response
         | body = concat [ request.url, "\n", toString repositories, "\n\n", db.get request.url ] }, [])
    ErrorOccurred reqType text ->
      ({ response | body = Debug.log "Error" text }, [])


type alias RepoInfo =
  { name : String
--  , id : Int
  }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object1 RepoInfo 
--    ("id" := Json.int) 
    ("name" := Json.string)


getDatabaseDescription : Cmd (Msg MongoDb)
getDatabaseDescription =
  rest.get mongoDbDecoder 
    "http://admin:changeit@localhost:8888/testdb"
    |> Task.mapError toString
    |> Task.perform (ErrorOccurred GetDb) (DataFetched GetDb)



listDocuments : String -> Cmd (Msg (Collection RepoInfo))
listDocuments collection =
  rest.get (collectionDecoder repoInfoDecoder) 
    (concat 
      [ "http://admin:changeit@localhost:8888/testdb/"
      , collection 
      ])
    |> Task.mapError toString
    |> Task.perform (ErrorOccurred GetColl) (DataFetched GetColl)



