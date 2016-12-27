module Session exposing (..)

import Date
import Task exposing (..)
import Http exposing (Error(..))
import RandomTask exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import Server exposing (..)
import SessionModel exposing (..)
import Rest exposing (..)
import UrlParse exposing (..)
import UserModel exposing (..)


sessionApiPart :
    Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Request -> (Error -> msg) -> (Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
sessionApiPart request doWithSession withSessionMaybe sendResponse =
    P "session"
        [ P "guest"
            [ F
                (\() ->
                    get (getIdSession request) sessions
                        |> onError
                            (\error ->
                                case error of
                                    BadStatus response ->
                                        RandomTask.randomInt
                                            |> andThen
                                                (\token ->
                                                    let
                                                        stringToken =
                                                            toString token

                                                        newSession =
                                                            Session stringToken stringToken (Date.fromTime request.time) (Date.fromTime request.time) stringToken
                                                    in
                                                        put stringToken newSession sessions
                                                            |> andThen
                                                                (\s ->
                                                                    put stringToken (User stringToken stringToken "guest" stringToken 1500) users
                                                                        |> andThenReturn (Task.succeed newSession)
                                                                )
                                                )

                                    _ ->
                                        Task.fail error
                            )
                        |> Task.attempt
                            (\result ->
                                case result of
                                    Ok session ->
                                        sendResponse <|
                                            case containsParam "noHeader" request of
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
                                            sendResponse (statusResponse 500)
                            )
                        |> Command
                )
            ]
        , F
            (\() ->
                withSessionMaybe request
                    (\error ->
                        case error of
                            BadStatus response ->
                                case response.status.code of
                                    404 ->
                                      sendResponse (statusResponse 404)
                                    code ->
                                      sendResponse (statusResponse code)
                            _ ->
                                sendResponse (statusResponse 500)
                    )
                    --succeedTask
                    (encodeSession >> okResponse >> Task.succeed)
            )
        ]
