module UserModel exposing (User, userDecoder, userEncoder)

import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Time


type alias User =
    { id : String
    , name : String
    , email : String
    , token : String
    , rank : Int
    }


userDecoder : Decoder User
userDecoder =
    Json.map5 User
        (field "id" string)
        (field "name" string)
        (field "email" string)
        (field "token" string)
        (field "rank" int)


userEncoder : User -> Value
userEncoder user =
    JE.object
        [ ( "id", JE.string user.id )
        , ( "name", JE.string user.name )
        , ( "email", JE.string user.email )
        , ( "token", JE.string user.token )
        , ( "rank", JE.int user.rank )
        ]
