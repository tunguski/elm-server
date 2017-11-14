module SessionModel exposing (..)

import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import BaseModel exposing (..)


type alias Session =
    { username : String
    , token : String
    , loginTime : Date
    , lastRequestTime : Date
    , idUser : String
    }


sessionDecoder : Decoder Session
sessionDecoder =
    Json.map5 Session
        (field "username" string)
        (field "token" string)
        (map dateParser <| field "loginTime" longFloat)
        (map dateParser <| field "lastRequestTime" longFloat)
        (field "idUser" string)


sessionEncoder : Session -> Value
sessionEncoder session =
    JE.object
        [ ( "username", JE.string session.username )
        , ( "token", JE.string session.token )
        , ( "loginTime", JE.float <| Date.toTime session.loginTime )
        , ( "lastRequestTime", JE.float <| Date.toTime session.lastRequestTime )
        , ( "idUser", JE.string session.idUser )
        ]


encodeSession : Session -> String
encodeSession session =
    JE.encode 0 <| sessionEncoder session


