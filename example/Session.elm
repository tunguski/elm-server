module Session exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Result exposing (toMaybe)
import Task exposing (Task)
import Http exposing (Error(..))
import RandomTask exposing (..)


import MongoDb exposing (DbMsg, Collection)
import ExampleDb exposing (..)
import Server exposing (..)
import UrlParse exposing (..)


type alias Session =
  { username : String
  , token : String
  , loginTime : Maybe Date
  , lastRequestTime : Maybe Date
  , idUser : String
  }


dateParser : Maybe String -> Maybe Date
dateParser input =
  case input of
    Just text ->
      text |> fromString >> toMaybe
    Nothing ->
      Nothing


sessionDecoder : Decoder Session 
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| maybe <| "loginTime" := string)
    (map dateParser <| maybe <| "lastRequestTime" := string)
    ("idUser" := string)


maybeEncodeDate maybe =
  case maybe of
    Just date ->
      JE.float <| Date.toTime date
    Nothing ->
      JE.null


sessionEncoder : Session -> Value
sessionEncoder session =
  JE.object 
    [ ("username", JE.string session.username)
    , ("token", JE.string session.token)
    , ("loginTime", maybeEncodeDate session.loginTime)
    , ("lastRequestTime", maybeEncodeDate session.lastRequestTime)
    , ("idUser", JE.string session.idUser)
    ]


encodeSession : Session -> String 
encodeSession session =
  JE.encode 0 <| sessionEncoder session


getSession : String -> Task Error Session
getSession idSession =
  get sessionDecoder ("session/" ++ idSession)


sessionApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Request -> (Error -> msg) -> (Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
sessionApiPart request doWithSession withSessionMaybe sendResponse =
  P "session" 
  [ P "guest"
    [ F (\() ->
            getSession (getIdSession request)
              `Task.onError` (\error ->
                case error of
                  BadResponse status body ->
                    RandomTask.randomInt
                      `Task.andThen` (\token ->
                          let
                            stringToken = toString token
                            newSession = 
                              Session stringToken stringToken Nothing Nothing stringToken
                          in
                            ExampleDb.put 
                              ("session/" ++ stringToken) 
                              (Debug.log "newSession" <| encodeSession newSession) 
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
                       sendResponse (statusResponse 500))
                   (\session -> 
                     sendResponse 
                       (setCookie "Set-Cookie" 
                          ("SESSIONID=" ++ session.token ++ "; Path=/;")
                          (okResponse (encodeSession session))
                       )
                   )
              |> Command
        )
    ]
  , F (\() ->
          withSessionMaybe request
            (\error ->
              case error of
                BadResponse status body ->
                  sendResponse (statusResponse 404)
                _ ->
                  sendResponse (statusResponse 500)
            )
            --succeedTask
            (encodeSession >> okResponse >> Task.succeed)
      ) 
  ]
