module Session exposing (..)


import Task exposing (Task)
import Http exposing (Error(..))
import RandomTask exposing (..)


import MongoDb exposing (DbMsg, Collection, maybeEncodeDate, dateParser)
import ExampleDb exposing (..)
import Server exposing (..)
import UrlParse exposing (..)
import SessionModel exposing (..)


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
