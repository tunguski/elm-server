module SessionModel exposing (Session, encodeSession, sessionDecoder, sessionEncoder)

import BaseModel exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Time


type alias Session =
    { username : String
    , token : String
    , loginTime : Int
    , lastRequestTime : Int
    , idUser : String
    }


sessionDecoder : Decoder Session
sessionDecoder =
    Json.map5 Session
        (field "username" string)
        (field "token" string)
        (field "loginTime" longInt)
        (field "lastRequestTime" longInt)
        (field "idUser" string)


sessionEncoder : Session -> Value
sessionEncoder session =
    JE.object
        [ ( "username", JE.string session.username )
        , ( "token", JE.string session.token )
        , ( "loginTime", JE.int session.loginTime )
        , ( "lastRequestTime", JE.int session.lastRequestTime )
        , ( "idUser", JE.string session.idUser )
        ]


encodeSession : Session -> String
encodeSession session =
    JE.encode 0 <| sessionEncoder session
