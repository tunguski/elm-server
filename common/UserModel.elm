module UserModel exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


type alias User =
  { id : String
  , name : String
  , email : String
  , token : String
  , rank : Int
  }


userDecoder : Decoder User 
userDecoder =
  Json.object5 User
    ("id" := string)
    ("name" := string)
    ("email" := string)
    ("token" := string)
    ("rank" := int)


userEncoder : User -> Value
userEncoder user =
  JE.object 
    [ ("id", JE.string user.id)
    , ("name", JE.string user.name)
    , ("email", JE.string user.email)
    , ("token", JE.string user.token)
    , ("rank", JE.int user.rank)
    ]


