module Example exposing (..)


import Http exposing (Error(..))
import Json.Decode as Json exposing (..)
import Task
import Debug
import Random exposing (..) 
import Base64


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
  | WithSession (DbMsg Session)
  | NewSessionToken Int 


type Action 
  = LoadDb String
  | LoadCollection String


type alias State =
  { session : Maybe Session
  , data : Maybe String
  , nextActions : List Action 
  }


type alias ServerState = Server.State Msg State


type alias StateUpdater = Server.StateUpdater Msg State


db = "http://admin:changeit@localhost:8888/testdb/"


emptyState = State Nothing Nothing []


addActions : List Action -> ServerState -> ServerState 
addActions actions ((response, state), cmds) =
  ((response, { state | nextActions = actions }), cmds)


initResponseTuple : Request -> Cmd Msg -> ServerState 
initResponseTuple request query =
  ( (Server.initResponse request.id, emptyState), [ query ] )


init : Initializer Msg State
init request =
    case request.url of
      "/" ->
        initResponseTuple request getRepoInfos 

      "/session" ->
        initResponseTuple request getSession

      "/logged/testDb" ->
        initResponseTuple request getSession
          |> addActions [ LoadDb "testDb" ]

      "/testDb" ->
        initResponseTuple request <| getDatabaseDescription db GetDb

      _ ->
        let
          debugLog = Debug.log "Not found" request.url
        in
          ( ( initResponseStatus request.id 404, emptyState), [] )


updateBody : (data -> String) -> data -> StateUpdater 
updateBody getter data =
  (\((response, state), cmds) ->
    (({ response | body = getter data }, state), []))


updateStatus : (data -> Int) -> data -> StateUpdater 
updateStatus getter data = 
  (\((response, state), cmds) ->
    (({ response | statusCode = getter data }, state), []))


type alias ErrorProcessor a = ServerState -> DbMsg a -> (a -> StateUpdater) -> ServerState
type alias BadResponseProcessor = Int -> String -> StateUpdater 


returnBadResponse : BadResponseProcessor 
returnBadResponse statusCode body =
  (updateStatus (always statusCode) >> updateBody (always body)) ()


badResponseProcessor : BadResponseProcessor -> ErrorProcessor a
badResponseProcessor badResponse st msg fn =
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
          badResponse statusCode body st


errorProcessor : ErrorProcessor a
errorProcessor st msg fn =
  badResponseProcessor returnBadResponse st msg fn


maybeCreateNewSession : BadResponseProcessor 
maybeCreateNewSession statusCode body =
  case Debug.log "maybeCreateNewSession" statusCode of
    404 ->
      \((response, state), cmds) -> 
        ((response, state), [ generate NewSessionToken (Random.int minInt maxInt) ] )
    _ ->
      returnBadResponse statusCode body


setBodyToString : a -> StateUpdater
setBodyToString =
  updateBody toString


setCookie : String -> String -> StateUpdater
setCookie name value ((response, state), cmds) =
  (({ response | headers = (name, value) :: response.headers }, state), cmds)


update : Updater Msg State
update request msg (response, state) =
  let
    st = ((response, state), [])
    process = errorProcessor st
  in
    case msg of
      GetDb dbMsg ->
        process dbMsg setBodyToString 

      GetColl dbMsg ->
        process dbMsg setBodyToString 

      GetSession dbMsg ->
        badResponseProcessor
          maybeCreateNewSession 
          st
          dbMsg 
          setBodyToString

      WithSession dbMsg ->
        badResponseProcessor
          returnBadResponse 
          st
          dbMsg 
          setBodyToString

      NewSessionToken token ->
        updateBody toString token <| setCookie "Set-Cookie" 
          ("SESSIONID=" ++ (toString token) ++ ";") st 


executeAction : Request -> StateUpdater 
executeAction request ((response, state), cmds) =
  case Debug.log "nextActions" state.nextActions of
    [] -> 
      ((response, state), cmds)

    LoadDb name :: tail ->
      ((response, state), [])

    LoadCollection name :: tail ->
      ((response, state), [])


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


getRepoInfos : Cmd Msg
getRepoInfos =
  listDocuments repoInfoDecoder GetColl "coll"


getSession : Cmd Msg
getSession =
  get sessionDecoder GetSession "session"


