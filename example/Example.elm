module Example exposing (..)


import Http exposing (Error(..))
import Json.Decode as Json exposing (..)
import Task
import Debug


import MongoDb exposing (..)
import Server exposing (..)
import Session exposing (..)


main =
  Server.program init update


-- UPDATE


type Msg 
  = GetDb (DbMsg MongoDb)
  | GetColl (DbMsg (Collection RepoInfo))
  | GetSession (DbMsg Session)


type alias State =
  { session : Maybe Session
  , data : Maybe String
  }


type alias ServerState = Server.State Msg State


type alias StateUpdater = Server.StateUpdater Msg State 


db = "http://admin:changeit@localhost:8888/testdb/"


initResponseTuple : Request -> Cmd Msg -> ServerState 
initResponseTuple request query =
  ( (Server.initResponse request.id, State Nothing Nothing ), [ query ] )


init : Initializer Msg State
init request =
    case request.url of
      "/" ->
        initResponseTuple request getRepoInfos 

      "/session" ->
        initResponseTuple request getSession

      "/testDb" ->
        initResponseTuple request <| getDatabaseDescription db GetDb

      _ ->
        let
          debugLog = Debug.log "Not found" request.url
        in
          ( ( initResponseStatus request.id 404, State Nothing Nothing), [] )


updateBody : (data -> String) -> data -> StateUpdater 
updateBody getter data =
  (\((response, state), cmds) ->
    (({ response | body = getter data }, state), []))


updateStatus : (data -> Int) -> data -> StateUpdater 
updateStatus getter data = 
  (\((response, state), cmds) ->
    (({ response | statusCode = getter data }, state), []))


errorProcessor : ServerState -> DbMsg a -> (a -> StateUpdater) -> ServerState 
errorProcessor st msg fn =
  case msg of
    DataFetched data ->
      (fn data) st
    ErrorOccurred error ->
      case Debug.log "Http.Error" error of
        Timeout ->
          updateStatus (always 500) () st
        NetworkError ->
          updateStatus (always 500) () st
        UnexpectedPayload payload ->
          (updateStatus (always 500) >> updateBody (always payload)) () st 
        BadResponse statusCode body ->
          (updateStatus (always statusCode) >> updateBody (always body)) () st 


update : Updater Msg State
update request msg (response, state) =
  let
    st = ((response, state), [])
    process = errorProcessor st
  in
    case msg of
      GetDb dbMsg ->
        process dbMsg <| updateBody toString

      GetColl dbMsg ->
        process dbMsg <| updateBody toString

      GetSession dbMsg ->
        process dbMsg <| updateBody toString


type alias RepoInfo =
  { id : String 
  , name : String
  }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo 
    (at ["_id" ] <| "$oid" := string)
    ("name" := string)


get : (Json.Decoder item) -> (DbMsg item -> Msg) -> String -> Cmd Msg 
get = MongoDb.get db


listDocuments : (Json.Decoder item) -> (DbMsg (Collection item) -> Msg) -> String -> Cmd Msg 
listDocuments = MongoDb.listDocuments db


--getRepoInfos : Cmd Requests 
getRepoInfos =
  listDocuments repoInfoDecoder GetColl "coll"


--getSession : Cmd Requests
getSession =
  get sessionDecoder GetSession "session"


