module Session exposing (..)


import Date
import Task exposing (Task)
import Http exposing (Error(..))
import RandomTask exposing (..)


import BaseModel exposing (..)
import ExampleDb exposing (..)
import Server exposing (..)
import UrlParse exposing (..)
import SessionModel exposing (..)
import UserModel exposing (..)


sessionApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Request -> (Error -> msg) -> (Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
sessionApiPart request doWithSession withSessionMaybe sendResponse =
  P "session" 
  [ P "guest"
    [ F (\() ->
            sessions.get (getIdSession request)
              |> onError (\error ->
                case error of
                  BadResponse status body ->
                    RandomTask.randomInt
                      |> andThen (\token ->
                          let
                            stringToken = toString token
                            newSession = 
                              Session stringToken stringToken (Date.fromTime request.time) (Date.fromTime request.time) stringToken
                          in
                            sessions.put stringToken newSession
                              |> andThen (\s ->
                                  users.put stringToken (User stringToken stringToken "guest" stringToken 1500)
                                    |> andThenReturn (Task.succeed newSession)
                              )
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
                BadResponse 404 body ->
                  sendResponse (statusResponse 404)
                _ ->
                  sendResponse (statusResponse 500)
            )
            --succeedTask
            (encodeSession >> okResponse >> Task.succeed)
      ) 
  ]
