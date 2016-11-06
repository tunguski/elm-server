module Repo exposing (..)


import Json.Decode as Json exposing (..)
import Task exposing (Task)
import Http exposing (Error)


import BaseModel exposing (Collection)
import MongoDb exposing (DbMsg)
import ExampleDb exposing (..)


type alias RepoInfo =
  { id : String 
  , name : String
  }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo 
    (at ["_id" ] <| "$oid" := string)
    ("name" := string)


getRepoInfos : Task Error (Collection RepoInfo)
getRepoInfos =
  listDocuments repoInfoDecoder "coll"


