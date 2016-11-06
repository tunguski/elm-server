module TableModel exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


import BaseModel exposing (..)


type alias Table =
  { name : String
  , players : List String
  }


tableDecoder : Decoder Table 
tableDecoder =
  Json.object2 Table
    ("name" := string)
    ("players" := list string)


tableEncoder : Table -> Value
tableEncoder table =
  JE.object 
    [ ("name", JE.string table.name)
    , ("players", listToValue JE.string table.players)
    ]


encodeTable : Table -> String 
encodeTable table =
  JE.encode 0 <| tableEncoder table


