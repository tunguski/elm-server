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


-- UPDATE


type Requests
  = GetDb MongoDb
  | GetColl (List RepoInfo)


type alias Msg = DbMsg Requests


db = "http://admin:changeit@localhost:8888/testdb/"


initResponse : Cmd Msg -> (Response, List (Cmd Msg))
initResponse request =
  ( Response request.id 200 "", [ request ] )



init : Initializer Msg
init request =
    case request.url of
      "" ->
        initResponse getResponseInfos

      _ ->
        initResponse <| getDatabaseDescription db GetDb


update : Updater Msg
update request msg response =
  case msg of
    DataFetched reqType ->
      ({ response
         | body = concat [ request.url, "\n", toString repositories ] }, [])

    ErrorOccurred reqType text ->
      ({ response 
          | statusCode = 500
          , body = Debug.log "Error" text }, [])


type alias RepoInfo =
  { id : String 
  , name : String
  }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo 
    (Json.at ["_id" ] <| "$oid" := Json.string)
    ("name" := Json.string)


getRepoInfos : Cmd (DbMsg Requests)
getRepoInfos =
  listDocuments
    db 
    (Json.list repoInfoDecoder)
    GetColl
    "coll"


