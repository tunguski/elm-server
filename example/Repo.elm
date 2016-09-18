module Repo exposing (..)


import Json.Decode as Json exposing (..)


import MongoDb exposing (DbMsg, Collection)
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


getRepoInfos : (DbMsg (Collection RepoInfo) -> msg) -> Cmd msg
getRepoInfos msg =
  listDocuments repoInfoDecoder msg "coll"


