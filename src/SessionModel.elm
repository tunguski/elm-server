module SessionModel exposing (Session, encodeSession, sessionDecoder, sessionEncoder)

import BaseModel exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Time exposing (Posix)


type alias Session =
    { username : String
    , token : String
    , loginTime : Posix
    , lastRequestTime : Posix
    , idUser : String
    }


sessionDecoder : Decoder Session
sessionDecoder =
    Json.map5 Session
        (field "username" string)
        (field "token" string)
        (field "loginTime" longPosix)
        (field "lastRequestTime" longPosix)
        (field "idUser" string)


sessionEncoder : Session -> Value
sessionEncoder session =
    JE.object
        [ ( "username", JE.string session.username )
        , ( "token", JE.string session.token )
        , ( "loginTime", JE.int <| Time.posixToMillis session.loginTime )
        , ( "lastRequestTime", JE.int <| Time.posixToMillis session.lastRequestTime )
        , ( "idUser", JE.string session.idUser )
        ]


encodeSession : Session -> String
encodeSession session =
    JE.encode 0 <| sessionEncoder session
