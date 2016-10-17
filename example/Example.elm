module Example exposing (..)


import Base64
import Debug
import Dict
import Http exposing (Error(..))
import Json.Decode as Json exposing (..)
import RandomTask exposing (..)
import String
import Task exposing (Task)


import MongoDb exposing (DbMsg(..), Collection, MongoDb, getDatabaseDescription, perform)
import Server exposing (..)
import Session exposing (..)
import Repo exposing (..)
import ExampleDb exposing (..)


main =
  Server.program init update


-- UPDATE


type Msg 
  = SendResponse Response


getIdSession : Request -> String
getIdSession request =
  Debug.log "idSession" <|
    case Dict.get "SESSIONID" <| getCookies request of
      Just id -> id
      Nothing -> "empty"


withSession : Request -> (Session -> Task Error Response) -> Partial Msg
withSession request action =
  getSession (getIdSession request)
    |> processTask action


withSessionMaybe : Request -> (Error -> Msg) -> (Session -> Task Error Response) -> Partial Msg
withSessionMaybe request errorProcessor action =
  getSession (getIdSession request)
    |> processTaskWithError errorProcessor action


processTaskWithError : (x -> Msg) -> (item -> Task x Response) -> Task x item -> Partial Msg
processTaskWithError errorProcessor eval task =
  Task.andThen task eval
    |> Task.perform
         errorProcessor
         (\item -> SendResponse item)
    |> Command


processTask : (item -> Task x Response) -> Task x item -> Partial Msg
processTask eval task =
  processTaskWithError
    (\error ->
      let
        debug = Debug.log "error" error
      in
        SendResponse (statusResponse 500))
    eval
    task


-- init : Request -> Partial Msg
init : Initializer Msg
init request =
    let
      doWithSession = withSession request
      ok = (toString >> okResponse >> Task.succeed)
    in
      case request.url of
        "/api" ->
          getRepoInfos 
            |> processTask ok
--      GetColl dbMsg ->
--        process dbMsg setBodyToString 
  
        "/api/session" ->
          withSessionMaybe request
            (\error ->
              case error of
                BadResponse status body ->
                  SendResponse (statusResponse 404)
                _ ->
                  SendResponse (statusResponse 500)
            ) ok
--    ReturnSession ->
--      case state.session of
--        Just session ->
--          updateBody encodeSession session ((response, state), cmd)
--        Nothing ->
--          ((response, state), cmd)
  
        "/api/session/guest" ->
          getSession (getIdSession request)
            `Task.onError` (\error ->
              --generate NewSessionToken (Random.int minInt maxInt)
              case error of
                BadResponse status body ->
--    NewSessionToken token ->
--      let
--        stringToken = toString token
--        newSession = Session stringToken stringToken Nothing Nothing stringToken
--        putResult = 
--          ExampleDb.put ("session/" ++ stringToken) (encodeSession newSession) 
--          PutSession
--      in
--        ((response, { state | session = Just newSession}), Just putResult)
                  RandomTask.randomInt
                    `Task.andThen` (\token ->
                        let
                          stringToken = toString token
                          newSession = Session stringToken stringToken Nothing Nothing stringToken
                        in
                          ExampleDb.put ("session/" ++ stringToken) (Debug.log "newSession" <| encodeSession newSession) 
                            `Task.andThen` (\s -> Task.succeed newSession)
                    )
                _ ->
                  Task.fail error
            )
            |> Task.perform
                 (\error ->
                   let
                     x = Debug.log "error" error
                   in
                     SendResponse (statusResponse 500))
                 (\session -> SendResponse (okResponse (encodeSession session)))
            |> Command
--    404 ->
--      \((response, state), cmds) -> 
--        ((response, state), Just ( generate NewSessionToken (Random.int minInt maxInt) ) )
--    _ ->
--      returnBadResponse statusCode body
--          (\session ->
--            case Debug.log "session" session of
--              Just session ->
--                okResponse (toString session)
--              Nothing ->
--                okResponse ("fixme!")
----                generate NewSessionToken (Random.int minInt maxInt)
--          )
--    MaybeCreateSession ->
--      case Debug.log "session" state.session of
--        Just session ->
--          setBodyToString state.session ((response, state), cmd)
--        Nothing ->
--          ((response, state), Just ( generate NewSessionToken (Random.int minInt maxInt) ) )
--            |> withAction DoNothing
  
--        "/api/tables" ->
--          case String.toLower request.method of
--            "post" ->
--              doWithSession (\session ->
--              )
--    CreateTable ->
----      ((response, state), Nothing)
--      let
--        tableName = request.body
--        getResult = 
--          ExampleDb.get ("table/" ++ tableName)
--          PutSession
--      in
--        ((response, { state | session = Just newSession}), Just putResult)
--            _ ->
--              doWithSession LoadTables
--    LoadTables ->
--      ((response, state), Nothing)
  
        "/api/logged/testDb" ->
          doWithSession ok
          --<| LoadDb "testDb"
--    LoadDb name ->
--      ((response, state), Nothing)
  
        "/api/testDb" ->
          getDatabaseDescription db
            |> processTask ok
--      GetDb dbMsg ->
--        process dbMsg setBodyToString 
  
        _ ->
          let
            debugLog = Debug.log "Not found" request.url
          in
            Result (statusResponse 404)
--            ( ( initResponseStatus request.id 404, emptyState), Nothing )


--badResponseProcessor badResponse st msg fn =
--  case msg of
--    DataFetched data ->
--      (fn data) st
--    ErrorOccurred error ->
--      case Debug.log "Http.Error" error of
--        Timeout ->
--          updateStatus (always 500) () st
--        NetworkError ->
--          updateStatus (always 500) () st
--        UnexpectedPayload payload ->
--          (updateStatus (always 500) >> updateBody (always payload)) () st 
--        BadResponse statusCode body ->
--          badResponse statusCode body st


--maybeCreateNewSession : BadResponseProcessor 
--maybeCreateNewSession statusCode body =
--  case Debug.log "maybeCreateNewSession" statusCode of
--    404 ->
--      \((response, state), cmds) -> 
--        ((response, state), Just ( generate NewSessionToken (Random.int minInt maxInt) ) )
--    _ ->
--      returnBadResponse statusCode body


setCookie : String -> String -> Response -> Response
setCookie name value response =
  { response | headers = (name, value) :: response.headers }


-- update : Request -> Msg -> Partial Msg
update : Updater Msg
update request msg =
  case Debug.log "update msg" msg of
    SendResponse response -> 
      Result response
--    LoadSession dbMsg ->
--      let
--        updatedState =
--          case Debug.log "session" dbMsg of
--            DataFetched session ->
--              ((response, { state | session = Just session }), Nothing)
--            ErrorOccurred err ->
--              ((response, state), Nothing)
--      in
--        executeAction request updatedState
--
--    GetDb dbMsg ->
--      process dbMsg setBodyToString 
--
--    GetColl dbMsg ->
--      process dbMsg setBodyToString 
--
--    NewSessionToken token ->
--      let
--        stringToken = toString token
--        newSession = Session stringToken stringToken Nothing Nothing stringToken
--        putResult = 
--          ExampleDb.put ("session/" ++ stringToken) (encodeSession newSession) 
--          PutSession
--      in
--        ((response, { state | session = Just newSession}), Just putResult)
--
--    PutSession dbMsg ->
--      case state.session of
--        Just session ->
--          updateBody encodeSession session st
--          |>
--          setCookie "Set-Cookie" 
--            ("SESSIONID=" ++ session.token ++ "; Path=/;")
--        Nothing ->
--          Debug.crash "Should have session"


