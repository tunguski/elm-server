module Session exposing (sessionApiPart)

import Date
import Dict
import Task exposing (..)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import RandomTask exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import Server exposing (..)
import SessionModel exposing (..)
import Rest exposing (..)
import UrlParse exposing (..)
import UserModel exposing (..)


sessionApiPart :
    ApiPartApi msg
    -> (Request -> (Error -> msg) -> (Session -> Task Error Response) -> Partial msg)
    -> Parse (Partial msg)
sessionApiPart api withSessionMaybe =
    P "session"
        [ P "guest"
            [ F (\() -> getGuestSession api withSessionMaybe)
            ]
        , F
            (\() ->
                withSessionMaybe api.request
                    (\error ->
                        case error of
                            BadStatus response ->
                                case response.status.code of
                                    404 ->
                                      api.sendResponse (statusResponse 404)
                                    code ->
                                      api.sendResponse (statusResponse code)
                            _ ->
                                api.sendResponse (statusResponse 500)
                    )
                    --succeedTask
                    (encodeSession >> okResponse >> Task.succeed)
            )
        ]


getGuestSession :
    ApiPartApi msg
    -> (Request -> (Error -> msg) -> (Session -> Task Error Response) -> Partial msg)
    -> Partial msg
getGuestSession api withSessionMaybe =
    (case containsParam "forceNew" api.request of
        True ->
            fail (BadStatus (fakeResponse 404)) 
        False ->
            executeIfIdSessionExists api.request (\id -> get id sessions)
    )
    |> onError (processGetSessionError api.request)
    |> Task.attempt (\result ->
       case result of
           Ok session ->
               api.sendResponse <|
                   case containsParam "noHeader" api.request of
                       True ->
                           (okResponse (encodeSession session))
                       False ->
                           (setCookie "Set-Cookie"
                               ("SESSIONID=" ++ session.token ++ "; Path=/;")
                               (okResponse (encodeSession session))
                   )
           Err error ->
               let
                   x =
                       Debug.log "error" error
               in
                   api.sendResponse (statusResponse 500)
    )
    |> Command


processGetSessionError request error =
    case error of
        BadStatus response ->
            RandomTask.randomInt
            |> andThen (\token ->
               let
                   stringToken =
                       toString token

                   newSession =
                       Session stringToken stringToken (Date.fromTime request.time) 
                           (Date.fromTime request.time) stringToken
               in
                   put stringToken newSession sessions
                   |> andThen (\s ->
                       put stringToken (User stringToken stringToken "guest" stringToken 1500) users
                       |> andThenReturn (Task.succeed newSession)
                   )
            )

        _ ->
            Task.fail error


