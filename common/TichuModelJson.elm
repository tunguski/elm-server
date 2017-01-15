module TichuModelJson exposing (..)

import Date exposing (Date)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String exposing (toInt)
import BaseModel exposing (..)
import UserModel exposing (..)
import TichuModel exposing (..)
import Tuple


gameDecoder : Decoder Game
gameDecoder =
    Json.map7 Game
        (field "name" string)
        (field "seed" (succeed 0))
        (field "users" <| list gameUser)
        (field "round" round)
        (field "history" <| list round)
        (field "messages" <| list message)
        (field "log" <| list gameUpdate)


round : Decoder Round
round =
    Json.map7 Round
        (field "players" <| list player)
        (field "table" <| list cardsDecoder)
        (field "actualPlayer" int)
        (field "tableHandOwner" (maybe int))
        (field "demand" (maybe rank))
        (field "demandCompleted" bool)
        (field "seed" (succeed 0))


gameUser : Decoder GameUser
gameUser =
    Json.map2 GameUser
        (field "name" string)
        (field "lastCheck" float)


cardsDecoder : Decoder (List Card)
cardsDecoder =
    list card


decodeCards =
    decodeString cardsDecoder


card : Decoder Card
card =
    (field "type" string)
        |> andThen
            (\card ->
                case card of
                    "NormalCard" ->
                        map2
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

                    _ ->
                        fail ("Not valid pattern for decoder to Card. Pattern: " ++ toString string)
            )


suit : Decoder Suit
suit =
    (field "suit" string)
        |> andThen
            (\string ->
                case string of
                    "Hearts" ->
                        succeed Hearts

                    "Diamonds" ->
                        succeed Diamonds

                    "Spades" ->
                        succeed Spades

                    "Clubs" ->
                        succeed Clubs

                    _ ->
                        fail ("Not valid pattern for decoder to Suit. Pattern: " ++ toString string)
            )


rank : Decoder Rank
rank =
    (field "rank" string)
        |> andThen
            (\string ->
                case string of
                    "J" ->
                        succeed J

                    "Q" ->
                        succeed Q

                    "K" ->
                        succeed K

                    "A" ->
                        succeed A

                    _ ->
                        case toInt string of
                            Ok i ->
                                succeed (R i)

                            Err err ->
                                fail ("Not valid pattern for decoder to Rank. Pattern: " ++ string)
            )


player : Decoder Player
player =
    map8 Player
        (field "hand" <| cardsDecoder)
        (field "cardsOnHand" <| int)
        (field "collected" <| cardsDecoder)
        (field "selection" <| cardsDecoder)
        (field "name" string)
        (field "score" int)
        (field "tichu" bool)
        (field "sawAllCards" bool)
    |> andThen (\p ->
        map2 p
            (field "grandTichu" bool)
            (field "exchange" (maybe
                (cardsDecoder
                |> andThen (\list ->
                    case list of
                        a :: b :: c :: t ->
                            succeed (a, b, c)
                        _ ->
                            fail "Could not parse list with three elements"
                ))))
    )


message : Decoder Message
message =
    map2 Message
        (field "type" messageType)
        (field "text" string)


messageType : Decoder MessageType
messageType =
    string
        |> andThen
            (\string ->
                case string of
                    "Error" ->
                        succeed Error

                    "Warning" ->
                        succeed Warning

                    "Info" ->
                        succeed Info

                    "Success" ->
                        succeed Success

                    _ ->
                        fail ("Not valid pattern for decoder to MessageType. Pattern: " ++ toString string)
            )


gameUpdate : Decoder UpdateGame
gameUpdate =
    fail "Not implemented yet"


gameEncoder : Game -> Value
gameEncoder game =
    JE.object
        [ ( "name", JE.string game.name )
        , ( "seed", JE.int game.seed )
        , ( "users"
          , JE.list (List.map gameUserEncoder game.users)
          )
        , ( "round", roundEncoder game.round )
        , ( "history", JE.list (List.map roundEncoder game.history) )
        , ( "messages", JE.list (List.map messageEncoder game.messages) )
        , ( "log", JE.list [] )
        ]


gameUserEncoder : GameUser -> Value
gameUserEncoder gameUser =
    JE.object
        [ ( "name", JE.string gameUser.name )
        , ( "lastCheck", JE.float gameUser.lastCheck )
        ]


messageEncoder : Message -> Value
messageEncoder message =
    JE.object
        []


cardEncoder : Card -> Value
cardEncoder card =
    case card of
        NormalCard suit rank ->
            JE.object
                [ ( "type", JE.string "NormalCard" )
                , ( "suit", JE.string <| toString suit )
                , ( "rank", rankEncoder rank )
                ]

        _ ->
            JE.object
                [ ( "type", JE.string (toString card) ) ]


encodeCards : Cards -> String
encodeCards cards =
    encode JE.list (List.map cardEncoder cards)


rankEncoder : Rank -> Value
rankEncoder rank =
    JE.string <|
        case rank of
            R value ->
                toString value
            _ ->
                toString rank


playerEncoder : Player -> Value
playerEncoder player =
    JE.object
        [ ( "hand", listToValue cardEncoder player.hand )
        , ( "cardsOnHand", JE.int player.cardsOnHand )
        , ( "collected", listToValue cardEncoder player.collected )
        , ( "selection", listToValue cardEncoder player.selection )
        , ( "name", JE.string player.name )
        , ( "score", JE.int player.score )
        , ( "tichu", JE.bool player.tichu )
        , ( "sawAllCards", JE.bool player.sawAllCards )
        , ( "grandTichu", JE.bool player.grandTichu )
        , ( "exchange", case player.exchange of
            Just (a, b, c) ->
                listToValue cardEncoder [a, b, c]
            Nothing ->
                JE.null
          )
        ]


roundEncoder : Round -> Value
roundEncoder round =
    JE.object
        [ ( "players", listToValue playerEncoder round.players )
        , ( "table", listToValue (List.map cardEncoder >> JE.list) round.table )
        , ( "actualPlayer", JE.int round.actualPlayer )
        , ( "tableHandOwner",
            case round.tableHandOwner of
                Just owner ->
                    JE.int owner
                Nothing ->
                    JE.null
          )
        , ( "demand",
            case round.demand of
                Just rank ->
                    rankEncoder rank
                Nothing ->
                    JE.null
          )
        , ( "demandCompleted", JE.bool round.demandCompleted )
        , ( "seed", JE.int round.seed )
        ]


encodeGame : Game -> String
encodeGame game =
    JE.encode 0 <| gameEncoder game


encodeSuit : Suit -> Value
encodeSuit item =
    case item of
        Hearts ->
            JE.string "Hearts"

        Diamonds ->
            JE.string "Diamonds"

        Spades ->
            JE.string "Spades"

        Clubs ->
            JE.string "Clubs"


awaitingTableDecoder : Decoder AwaitingTable
awaitingTableDecoder =
    Json.map4 AwaitingTable
        (field "name" string)
        (field "users" <|
            list
                (map3 AwaitingTableUser
                    (field "name" string)
                    (field "lastCheck" float)
                    (field "pressedStart" bool))
        )
        (field "test" bool)
        (field "seed" (succeed 0))


awaitingTableEncoder : AwaitingTable -> Value
awaitingTableEncoder table =
    JE.object
        [ ( "name", JE.string table.name )
        , ( "seed", JE.int table.seed )
        , ( "users"
          , JE.list
                (List.map (\user ->
                    JE.object
                        [ ("name", JE.string user.name)
                        , ("lastCheck", JE.float user.lastCheck)
                        , ("pressedStart", JE.bool user.pressedStart)
                        ]
                    ) table.users
                )
          )
        , ( "test", JE.bool table.test )
        ]


encodeAwaitingTable awaitingTable =
    JE.encode 0 <| awaitingTableEncoder awaitingTable


