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

db = database "http://localhost:8888/"

-- UPDATE


type Msg
  = DataFetched (List RepoInfo)
  | ErrorOccurred String


init : Initializer Msg
init request =
  ( Response request.id 200 "", [ fetchData ] )


update : Updater Msg
update request msg response =
  case msg of
    DataFetched repositories ->
      ({ response
         | body = concat [ request.url, "\n", toString repositories, "\n\n", db.get request.url ] }, [])
    ErrorOccurred text ->
      ({ response | body = text }, [])


type alias RepoInfo =
  { id : Int
  , name : String
  }

repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo ("id" := Json.int) ("name" := Json.string)


repoInfoListDecoder : Json.Decoder (List RepoInfo)
repoInfoListDecoder =
  Json.list repoInfoDecoder


fetchData : Cmd Msg
fetchData =
  Http.get repoInfoListDecoder "https://api.github.com/users/nytimes/repos"
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DataFetched

