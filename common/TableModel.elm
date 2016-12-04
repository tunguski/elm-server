module TableModel exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


import BaseModel exposing (..)
import TichuModel exposing (Suit(..))


type alias Table =
  { name : String
  , players : List String
  }


type TableChange
  = Fold
  | Hand
  | DeclareTichu String
  | DeclareGrandTichu String


type alias Changes =
  List (Int TableChange)


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


decodeSuit : Decoder Suit
decodeSuit =
  let
    decodeToType string =
      case string of
        "Hearts" -> Result.Ok Hearts
        "Diamonds" -> Result.Ok Diamonds
        "Spades" -> Result.Ok Spades
        "Clubs" -> Result.Ok Clubs
        _ -> Result.Err ("Not valid pattern for decoder to Suit. Pattern: " ++ (toString string))
  in
    customDecoder string decodeToType


encodeSuit : Suit -> JE.Value
encodeSuit item =
  case item of
    Hearts -> JE.string "Hearts"
    Diamonds -> JE.string "Diamonds"
    Spades -> JE.string "Spades"
    Clubs -> JE.string "Clubs"


