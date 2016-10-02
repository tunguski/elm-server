module Example exposing (..)


import Base64
import Debug
import Dict
import Http exposing (Error(..))
import Json.Decode as Json exposing (..)
import Random exposing (..) 
import String
import Task


import MongoDb exposing (DbMsg(..), Collection, MongoDb, getDatabaseDescription)
import Server exposing (..)
import Session exposing (..)
import Repo exposing (..)
import ExampleDb exposing (..)


main =
  Server.program init update


-- UPDATE


type Msg 
  = LoadSession (DbMsg Session)
  | PutSession (DbMsg String)
  | GetDb (DbMsg MongoDb)
  | GetColl (DbMsg (Collection RepoInfo))
  | NewSessionToken Int 


type Action 
  = DoNothing 
  | ReturnSession 
  | LoadTables
  | CreateTable
  | MaybeCreateSession 
  | LoadDb String
  | LoadCollection String


type alias State =
  { session : Maybe Session
  , data : Maybe String
  , nextAction : Action 
  }


type alias ServerState = Server.State Msg State


type alias StateUpdater = Server.StateUpdater Msg State




emptyState = State Nothing Nothing DoNothing


withAction : Action -> ServerState -> ServerState 
withAction action ((response, state), cmd) =
  ((response, { state | nextAction = action }), cmd)


initResponseTuple : Request -> Cmd Msg -> ServerState 
initResponseTuple request query =
  ( (Server.initResponse request.id, emptyState), Just query )


doWithResponseAndSession : Request -> Action -> ServerState 
doWithResponseAndSession request action =
  let
    idSession =
      case Dict.get "SESSIONID" <| getCookies request of
        Just id -> id
        Nothing -> "empty"
  in
    initResponseTuple request (getSession idSession LoadSession)
      |> withAction action


init : Initializer Msg State
init request =
    let
      doWithSession = doWithResponseAndSession request
    in
      case request.url of
        "/api" ->
          initResponseTuple request <| getRepoInfos GetColl
  
        "/api/session" ->
          doWithSession ReturnSession
  
        "/api/session/guest" ->
          doWithSession MaybeCreateSession
  
        "/api/tables" ->
          case String.toLower request.method of
            "post" ->
              Debug.log "method" <| doWithSession CreateTable
            _ ->
              doWithSession LoadTables
  
        "/api/logged/testDb" ->
          doWithSession <| LoadDb "testDb"
  
        "/api/testDb" ->
          initResponseTuple request <| getDatabaseDescription db GetDb
  
        _ ->
          let
            debugLog = Debug.log "Not found" request.url
          in
            ( ( initResponseStatus request.id 404, emptyState), Nothing )


updateBody : (data -> String) -> data -> StateUpdater 
updateBody getter data =
  (\((response, state), cmd) ->
    (({ response | body = getter data }, state), cmd))


updateStatus : (data -> Int) -> data -> StateUpdater 
updateStatus getter data = 
  (\((response, state), cmd) ->
    (({ response | statusCode = getter data }, state), cmd))


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
        ((response, state), Just ( generate NewSessionToken (Random.int minInt maxInt) ) )
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
    st = ((response, state), Nothing)
    process = errorProcessor st
  in
    case msg of
--      WithSession dbMsg ->
--        badResponseProcessor
--          returnBadResponse 
--          st
--          dbMsg 
--          setBodyToString

      LoadSession dbMsg ->
        let
          updatedState =
            case Debug.log "session" dbMsg of
              DataFetched session ->
                ((response, { state | session = Just session }), Nothing)
              ErrorOccurred err ->
                ((response, state), Nothing)
        in
          executeAction request updatedState

      GetDb dbMsg ->
        process dbMsg setBodyToString 

      GetColl dbMsg ->
        process dbMsg setBodyToString 

      NewSessionToken token ->
        let
          stringToken = toString token
          newSession = Session stringToken stringToken Nothing Nothing stringToken
          putResult = 
            ExampleDb.put ("session/" ++ stringToken) (encodeSession newSession) 
            PutSession
        in
          ((response, { state | session = Just newSession}), Just putResult)

      PutSession dbMsg ->
        case state.session of
          Just session ->
            updateBody encodeSession session st
            |>
            setCookie "Set-Cookie" 
              ("SESSIONID=" ++ session.token ++ "; Path=/;")
          Nothing ->
            Debug.crash "Should have session"


executeAction : Request -> StateUpdater 
executeAction request ((response, state), cmd) =
  case state.nextAction of
    DoNothing -> 
      ((response, state), cmd)

    ReturnSession ->
      case state.session of
        Just session ->
          updateBody encodeSession session ((response, state), cmd)
        Nothing ->
          ((response, state), cmd)

--    GetSession dbMsg ->
--      badResponseProcessor
--        maybeCreateNewSession 
--        st
--        dbMsg 
--        setBodyToString

    MaybeCreateSession ->
      case Debug.log "session" state.session of
        Just session ->
          setBodyToString state.session ((response, state), cmd)
        Nothing ->
          ((response, state), Just ( generate NewSessionToken (Random.int minInt maxInt) ) )
            |> withAction DoNothing

    LoadTables ->
      ((response, state), Nothing)

    CreateTable ->
      ((response, state), Nothing)
--      let
--        stringToken = toString token
--        newSession = Session stringToken stringToken Nothing Nothing stringToken
--        putResult = 
--          ExampleDb.put ("session/" ++ stringToken) (encodeSession newSession) 
--          PutSession
--      in
--        ((response, { state | session = Just newSession}), Just putResult)

    LoadDb name ->
      ((response, state), Nothing)

    LoadCollection name ->
      ((response, state), Nothing)


