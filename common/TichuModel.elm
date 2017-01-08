module TichuModel exposing (..)

import Time exposing (Time)
import Array exposing (Array)
import List exposing (..)
import Maybe exposing (andThen)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)


import UserModel exposing (User)


-----------------------------------------------------------------------------
-- MODEL
-----------------------------------------------------------------------------


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = R Int
    | J
    | Q
    | K
    | A


type Card
    = NormalCard Suit Rank
    | MahJong
    | Dog
    | Phoenix
    | Dragon


type alias Cards =
    List Card


allowedRanks : List Rank
allowedRanks =
    (map (\i -> R i) (List.range 2 10)) ++ [ J, Q, K, A ]


rankWeight : Rank -> Int
rankWeight rank =
    case rank of
        R i -> i
        J -> 11
        Q -> 12
        K -> 13
        A -> 14


cardWeight : Card -> Int
cardWeight card =
    case card of
        NormalCard suit rank -> rankWeight rank
        MahJong -> 1
        Dog -> 0
        Phoenix -> 15
        Dragon -> 16


cardOrder : Card -> Card -> Order
cardOrder a b =
    compare (cardWeight a) (cardWeight b)


allCards : List Card
allCards =
    append [ MahJong, Dog, Phoenix, Dragon ]
        (concatMap (\s -> map (NormalCard s) allowedRanks) [ Clubs, Diamonds, Hearts, Spades ])


type
    Combination
    -- lowest, length; (bomb)
    = StraightFlush Rank Int
      -- (bomb)
    | Four Rank
      -- three's rank
    | FullHouse Rank
      -- lowest, length
    | Straight Rank Int
    | Three Rank
      -- lowest, length
    | PairStairs Rank Int
    | Pair Rank
      -- card's power
    | SingleCard Int



-- a single card;


highCard : Cards -> Maybe Combination
highCard combination =
    case combination of
        [ a ] ->
            Just (SingleCard (cardWeight a))

        _ ->
            Nothing


pair : Cards -> Maybe Combination
pair combination =
    case combination of
        [ NormalCard a b, NormalCard c d ] ->
            if rankWeight b == rankWeight d then
                Just (Pair b)
            else
                Nothing

        _ ->
            Nothing



-- two or more "stairs" (consecutive pairs; for example, 55667788. Non-consecutive pairs may not be played);


pairStairs : Cards -> Maybe Combination
pairStairs combination =
    Maybe.map (\( r, i ) -> PairStairs r i) (extractPairStairs combination)


nextPairPower : ( Rank, Int ) -> Card -> ( Rank, Int )
nextPairPower ( r, i ) card =
    case card of
        NormalCard s r ->
            ( r, i + 1 )

        _ ->
            ( r, i )


calculatePairStraitPower : Card -> Card -> Maybe Card
calculatePairStraitPower a b =
    ((pair [ a, b ])
        |> andThen
            (\combination ->
                case combination of
                    Pair rank ->
                        case a of
                            NormalCard s r ->
                                if ((rankWeight r) == 0 || (rankWeight r) == (rankWeight rank) + 1) then
                                    Just a
                                else
                                    Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )
    )


extractPairStairs : Cards -> Maybe ( Rank, Int )
extractPairStairs combination =
    case combination of
        a :: b :: tail ->
            Maybe.map2 nextPairPower
                (extractPairStairs tail)
                (calculatePairStraitPower a b)

        _ ->
            Nothing



-- three of a kind;
-- straights of at least five cards in length, regardless of suit/color (so 56789TJQ is playable);
-- and full houses (three of a kind & a pair).
-- Four of a kind or a straight flush of at least five cards is a bomb


allowedCombination : Cards -> Cards -> Bool
allowedCombination table combination =
    False


parseTrick : Round -> String -> Maybe Combination
parseTrick round name =
    Nothing


cardInTrick : Maybe Rank -> Cards -> Bool
cardInTrick rank selection =
    case rank of
        Just r ->
            List.any (\card ->
                case card of
                    NormalCard suit cardRank ->
                        cardRank == r
                    _ ->
                        False
            ) selection
        Nothing ->
            True


bomb : Maybe Combination -> Bool
bomb combination =
    case combination of
        Just (StraightFlush r i) ->
            True
        Just (Four r) ->
            True
        _ ->
            False



type alias Player =
    { hand : List Card
    , cardsOnHand : Int
    , collected : List Card
    , selection : List Card
    , name : String
    , score : Int
    , tichu : Bool
    , sawAllCards : Bool
    , grandTichu : Bool
    , exchange : Maybe (Card, Card, Card)
    }


type alias Round =
    -- players in order, first has to play
    { players : Array Player
    -- hands on table
    , table : List Cards
    , actualPlayer : Int
    , demand : Maybe Rank
    , demandCompleted : Bool
    , seed : Int
    }


type MessageType
    = Error
    | Warning
    | Info
    | Success


type alias Message =
    { messageType : MessageType
    , text : String
    }


type alias GameUser =
    { name : String
    , lastCheck : Time
    }


type alias Game =
    { name : String
    , seed : Int
    , users : Array GameUser
    , round : Round
    , history : List Round
    , messages : List Message
    , log : List UpdateGame
    }


type alias AwaitingTableUser =
    { name : String
    , lastCheck : Time
    , pressedStart : Bool
    }


type alias AwaitingTable =
    { name : String
    , users : List AwaitingTableUser
    , test : Bool
    , seed : Int
    }


type UpdateGame
    = UpdatePlayer (Player -> Player)
    | UpdateRound (Round -> Round)


initGame : String -> Int -> List AwaitingTableUser -> Game
initGame name seed users =
    let
        gameUsers =
            Array.fromList <| List.map (\user ->
                GameUser user.name user.lastCheck
            ) users
    in
        { name = name
        , seed = seed
        , users = gameUsers
        , round = initRound seed gameUsers
        , history = []
        , messages = []
        , log = []
        }


initRound : Int -> Array GameUser -> Round
initRound seed users =
    case step (shuffle allCards) (initialSeed seed) of
        (cards, _) ->
            { players =
                Array.indexedMap (\i user ->
                    initPlayer cards user.name i
                ) users
            , table = []
            , actualPlayer = 0
            , demand = Nothing
            , demandCompleted = False
            , seed = seed
            }


initPlayer : List Card -> String -> Int -> Player
initPlayer cards name offset =
    { hand = {-sortWith cardOrder <|-}
        take 14 <| drop (offset * 14) cards
    , cardsOnHand = 14
    , collected = []
    , selection = []
    , name = name
    , score = 0
    , tichu = False
    , sawAllCards = False
    , grandTichu = False
    , exchange = Nothing
    }


