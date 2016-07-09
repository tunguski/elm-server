module Example exposing (..)


import Http
import Json.Decode as Json exposing (..)
import Task
import Debug


import Server exposing (..)
import MongoDB exposing (..)


main =
  Server.program init update


-- UPDATE


type Requests
  = GetDb MongoDb
  | GetColl (Collection RepoInfo)


type alias Msg 
  = DbMsg Requests


db = "http://admin:changeit@localhost:8888/testdb/"


initResponse : Request -> Cmd Msg -> (Response, List (Cmd Msg))
initResponse request query =
  ( Response request.id 200 "", [ query ] )


init : Initializer Msg
init request =
  let
    x = Debug.log "Request: " request
  in
    case request.url of
      "/" ->
        initResponse request getRepoInfos 

      "/db" ->
        initResponse request <| getDatabaseDescription db GetDb

      _ ->
        ( Response request.id 404 "", [] )


update : Updater Msg
update request msg response =
  case msg of
    DataFetched reqType ->
      case reqType of
        GetDb mongoDb ->
          ({ response
             | body = toString mongoDb }, [])

        GetColl repositories ->
          ({ response
             | body = toString repositories }, [])

    ErrorOccurred text ->
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
    (at ["_id" ] <| "$oid" := string)
    ("name" := string)


getRepoInfos : Cmd (DbMsg Requests)
getRepoInfos =
  listDocuments
    db 
    repoInfoDecoder
    GetColl
    "coll"


