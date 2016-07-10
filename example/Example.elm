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


type Requests
  = GetDb MongoDb
  | GetColl (Collection RepoInfo)
  | GetSession Session 


type alias Msg 
  = DbMsg Requests


type alias State =
  { session : Maybe Session
  , data : Maybe String
  }


type alias StateUpdater = Server.StateUpdater Msg State 


db = "http://admin:changeit@localhost:8888/testdb/"


initResponseTuple : Request -> Cmd Msg -> Server.State Msg State 
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
          x = Debug.log "Not found" request.url
        in
          ( ( initResponseStatus request.id 404, State Nothing Nothing), [] )


updateBody : String -> StateUpdater 
updateBody body ((response, state), cmds) =
  (({ response | body = body }, state), [])


updateStatus : Int -> StateUpdater 
updateStatus statusCode ((response, state), cmds) =
  (({ response | statusCode = statusCode }, state), [])


update : Updater Msg State
update request msg (response, state) =
  let
    st = ((response, state), [])
  in
    case msg of
      DataFetched reqType ->
        case reqType of
          GetDb mongoDb ->
            updateBody (toString mongoDb) st 
  
          GetColl repositories ->
            updateBody (toString repositories) st 
  
          GetSession session ->
            updateBody (toString session) st 
  
      ErrorOccurred error ->
        case Debug.log "Http.Error" error of
          Timeout ->
            updateStatus 500 st 
          NetworkError ->
            updateStatus 500 st 
          UnexpectedPayload payload ->
            updateStatus 500 >> updateBody payload <| st 
          BadResponse statusCode body ->
            updateStatus statusCode >> updateBody body <| st 


type alias RepoInfo =
  { id : String 
  , name : String
  }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
  Json.object2 RepoInfo 
    (at ["_id" ] <| "$oid" := string)
    ("name" := string)


get : (Json.Decoder item) -> (item -> m) -> String -> Cmd (DbMsg m)
get = MongoDb.get db


listDocuments : (Json.Decoder item) -> (Collection item -> m) -> String -> Cmd (DbMsg m)
listDocuments = MongoDb.listDocuments db


getRepoInfos : Cmd (DbMsg Requests)
getRepoInfos =
  listDocuments repoInfoDecoder GetColl "coll"


getSession : Cmd (DbMsg Requests)
getSession =
  get sessionDecoder GetSession "session"


