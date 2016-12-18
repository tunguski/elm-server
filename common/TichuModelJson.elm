module TichuModelJson exposing (..)


import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String exposing (toInt)


import BaseModel exposing (..)
import UserModel exposing (..)
import TichuModel exposing (..)


gameDecoder : Decoder Game
gameDecoder =
  Json.object6 Game
    ("name" := string)
    ("users" := array (tuple2 (,) userDecoder int))
    ("round" := round)
    ("history" := list round)
    ("messages" := list message)
    ("log" := list gameUpdate)


round : Decoder Round
round =
  Json.object3 Round
    ("player" := array player)
    ("table" := list (list card))
    ("actualPlayer" := int)
    

card : Decoder Card
card =
  ("card" := string) `andThen` (\card ->
    case card of
      "NormalCard" ->
        tuple2 
          (\suit rank -> NormalCard suit rank)
          suit
          rank
      "MahJong" ->
        succeed MahJong
      "Dog" ->
        succeed Dog
      "Phoenix" ->
        succeed Phoenix
      "Dragon" ->
        succeed Dragon
      _ -> fail ("Not valid pattern for decoder to Card. Pattern: " ++ toString string)
  )


suit : Decoder Suit
suit =
  ("suit" := string) `andThen` (\string ->
    case string of
      "Hearts" -> succeed Hearts
      "Diamonds" -> succeed Diamonds
      "Spades" -> succeed Spades
      "Clubs" -> succeed Clubs
      _ -> fail ("Not valid pattern for decoder to Suit. Pattern: " ++ toString string)
  )

rank : Decoder Rank
rank =
  ("rank" := string) `andThen` (\string ->
    case string of
      "J" -> succeed J 
      "Q" -> succeed Q
      "K" -> succeed K
      "A" -> succeed A
      _ ->
        case toInt string of
          Ok i ->
            succeed (R i)
          Err err ->
            fail ("Not valid pattern for decoder to Rank. Pattern: " ++ string)
  )
    

player : Decoder Player
player =
  object7 Player
    ("hand" := list card)
    ("collected" := list card)
    ("selection" := list card)
    ("name" := string)
    ("score" := int)
    ("tichu" := bool)
    ("grandTichu" := bool)
    

message : Decoder Message
message =
  object2 Message
    ("type" := messageType)
    ("text" := string)


messageType : Decoder MessageType
messageType =
  string `andThen` (\string ->
    case string of
      "Error" -> succeed Error 
      "Warning" -> succeed Warning
      "Info" -> succeed Info
      "Success" -> succeed Success
      _ -> fail ("Not valid pattern for decoder to MessageType. Pattern: " ++ toString string)
  )
  
    

gameUpdate : Decoder UpdateGame
gameUpdate =
  fail "Not implemented yet"


gameEncoder : Game -> Value
gameEncoder game =
  JE.object 
    [ ("name", JE.string game.name)
    --, ("players", listToValue JE.string game.players)
    ]


encodeGame : Game -> String 
encodeGame game =
  JE.encode 0 <| gameEncoder game


encodeSuit : Suit -> Value
encodeSuit item =
  case item of
    Hearts -> JE.string "Hearts"
    Diamonds -> JE.string "Diamonds"
    Spades -> JE.string "Spades"
    Clubs -> JE.string "Clubs"


awaitingTableDecoder : Decoder AwaitingTable
awaitingTableDecoder =
  Json.object2 AwaitingTable
    ("name" := string)
    ("users" := list (tuple2 (,) string float))


awaitingTableEncoder : AwaitingTable -> Value
awaitingTableEncoder table =
  JE.object
    [ ("name", JE.string table.name)
    , ("users", JE.list 
        (List.map (\user -> JE.list [ JE.string <| fst user, JE.float <| snd user ]) 
        table.users))
    ]


encodeAwaitingTable awaitingTable =
  JE.encode 0 <| awaitingTableEncoder awaitingTable

