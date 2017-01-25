module TichuModel exposing (..)

import Array
import Time exposing (Time)
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


{-| Check does first combination is stronger than second
-}
isSameTrickAndStronger : Combination -> Combination -> Bool
isSameTrickAndStronger c1 c2 =
    case (c1, c2) of
        (SingleCard i1, SingleCard i2) -> i1 > i2
        (Pair r1, Pair r2) -> rankWeight r1 > rankWeight r2
        (PairStairs b1 i1, PairStairs b2 i2) -> i1 == i2 && rankWeight b1 > rankWeight b2
        (Three r1, Three r2) -> rankWeight r1 > rankWeight r2
        (Straight r1 i1, Straight r2 i2) -> i1 == i2 && rankWeight r1 > rankWeight r2
        (FullHouse r1, FullHouse r2) -> rankWeight r1 > rankWeight r2
        _ -> False


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
        [ NormalCard a b, Phoenix ] ->
            Just (Pair b)

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


nextPairPower : ( Rank, Int ) -> Rank -> Maybe ( Rank, Int )
nextPairPower ( r, i ) rank =
    if rankWeight rank + 1 == rankWeight r then
        Just (rank, i + 1)
    else
        Nothing


calculatePairStairsPower : Card -> Card -> Maybe Rank
calculatePairStairsPower a b =
    case pair [ a, b ] of
        Just (Pair rank) ->
            Just rank
        _ ->
            Nothing


extractPairStairs : Cards -> Maybe ( Rank, Int )
extractPairStairs combination =
    (case combination of
        a :: b :: [] ->
            case calculatePairStairsPower a b of
                Just rank ->
                    Just (rank, 1)
                _ ->
                    Nothing
        a :: b :: tail ->
            Maybe.map2 nextPairPower
                (extractPairStairs tail)
                (calculatePairStairsPower a b)
            |> (\m -> case m of
                    Just (Just v) ->
                        Just v
                    _ ->
                        Nothing
                )

        _ ->
            Nothing
    )


-- three of a kind;
three : Cards -> Maybe Combination
three cards =
    case cards of
        [ NormalCard s1 r1, NormalCard s2 r2, Phoenix ] ->
            if r1 == r2 then
                Just (Three r1)
            else
                Nothing

        [ NormalCard s1 r1, NormalCard s2 r2, NormalCard s3 r3 ] ->
            if r1 == r2 && r2 == r3 then
                Just (Three r1)
            else
                Nothing

        _ ->
            Nothing


-- straights of at least five cards in length, regardless of suit/color (so 56789TJQ is playable);
straight : Cards -> Maybe Combination
straight cards =
    Nothing


-- and full houses (three of a kind & a pair).
fullHouse : Cards -> Maybe Combination
fullHouse cards =
    case cards of
        a :: b :: c :: d :: e :: [] ->
            case (three [a, b, c], pair [d, e]) of
                (Just t, Just p) ->
                    case t of
                        Three r ->
                            Just <| FullHouse r
                        _ ->
                            Nothing
                _ ->
                    case (pair [a, b], three [c, d, e]) of
                        (Just p, Just t) ->
                            case t of
                                Three r ->
                                    Just <| FullHouse r
                                _ ->
                                    Nothing
                        _ ->
                            Nothing

        _ ->
            Nothing


-- Four of a kind or a straight flush of at least five cards is a bomb
four : Cards -> Maybe Combination
four cards =
    case cards of
        [ NormalCard s1 r1, NormalCard s2 r2, NormalCard s3 r3, NormalCard s4 r4 ] ->
            if r1 == r2 && r2 == r3 && r3 == r4 then
                Just (Four r1)
            else
                Nothing
        _ ->
            Nothing




{-| Is combination allowed to be placed on table. There may be previous trick on table.
-}
allowedCombination : Maybe Cards -> Cards -> Bool
allowedCombination table combination =
    (case parseTrick combination of
        Just trick ->
            case Maybe.andThen parseTrick table of
                Just trickOnTable ->
                    isSameTrickAndStronger trick trickOnTable
                Nothing ->
                    True
        Nothing ->
            False
    ) |> Debug.log "allowedCombination"


tryParse parser (combination, cards) =
    case combination of
        Just _ -> (combination, cards)
        Nothing -> (parser cards, cards)


parseTrick : Cards -> Maybe Combination
parseTrick cards =
    (Nothing, cards)
    |> tryParse highCard
    |> tryParse pair
    |> tryParse three
    |> tryParse four
    |> tryParse fullHouse
    |> tryParse straight
    |> tryParse pairStairs
    |> Tuple.first


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
    , collected : List (List Cards)
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
    { players : List Player
    -- hands on table
    , table : List Cards
    , actualPlayer : Int
    -- the owner of actually highest trick on table
    , tableHandOwner : Maybe Int
    , demand : Maybe Rank
    , demandCompleted : Bool
    , seed : Int
    , winner : Maybe Int
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
    , users : List GameUser
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
            List.map (\user ->
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


hasCard card player =
    List.member card player.hand


initRound : Int -> List GameUser -> Round
initRound seed users =
    case step (shuffle allCards) (initialSeed seed) of
        (cards, _) ->
            { players =
                List.indexedMap (\i user ->
                    initPlayer cards user.name i
                ) users
            , table = []
            , actualPlayer = 0
            , tableHandOwner = Nothing
            , demand = Nothing
            , demandCompleted = False
            , seed = seed
            , winner = Nothing
            }
    -- set actual player by seeking MahJong
    |> (\round ->
        { round | actualPlayer =
            List.indexedMap (,) round.players
            |> List.foldl (\(i, user) before ->
                case hasCard MahJong user of
                    True -> Just i
                    False -> before
            ) Nothing
            |> Maybe.withDefault 0
        }
    )


initPlayer : List Card -> String -> Int -> Player
initPlayer cards name offset =
    { hand = take 14 <| drop (offset * 14) cards
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


openDemand round =
    case round.demand of
        Just r -> not round.demandCompleted
        Nothing -> False


openDemandMatch round =
    case round.demand of
        Just r ->
            List.any (\card ->
                case card of
                    NormalCard suit rank ->
                        (not round.demandCompleted) && (rank == r)
                    _ ->
                        False
            ) (getActualPlayer round).hand
        Nothing ->
            False


defaultCrash text item =
    case item of
        Just value -> value
        Nothing -> Debug.crash text


getPlayer round name =
    List.filter (.name >> (==) name) round.players
    |> List.head
    |> defaultCrash ("Malformed state getPlayer: " ++ name ++ ": " ++ toString round)


modifyPlayer name function round =
    { round | players =
        List.map (\player ->
            case player.name == name of
                True -> function player
                False -> player
        ) round.players
    }


getActualPlayer : Round -> Player
getActualPlayer round =
    round.players
    |> List.drop round.actualPlayer
    |> List.head
    |> defaultCrash ("Malformed state getActualPlayer: " ++ toString round)


incActualPlayer r =
    { r | actualPlayer = (r.actualPlayer + 1) % 4 }
    |> maybeCollectCards
    |> (\round ->
        if List.isEmpty (getActualPlayer round).hand then
            incActualPlayer round
        else
            round
    )


playerIndex players player =
    let
        getPlayerIndex i players player =
            case players of
                [] -> Nothing
                h :: t ->
                    case h.name == player.name of
                        True -> Just i
                        False -> getPlayerIndex (i + 1) t player
    in
        getPlayerIndex 0 players player


setActualPlayer player round =
    case playerIndex round.players player of
        Just i -> { round | actualPlayer = (i + 1) % 4 }
        Nothing -> round


maybeIncActualPlayer round =
    case (getActualPlayer round).hand of
        [] -> incActualPlayer round
        _ -> round


maybeSetWinner hand actualPlayer round =
    case hand of
        [] ->
            case round.winner of
                Just _ ->
                    round
                Nothing ->
                    { round | winner = Just actualPlayer }
        _ ->
            round



removeCards cards hand =
    List.filter (\c -> not <| List.member c cards) hand


switchCard card i players =
    (Array.get (i % 4) players
     |> (\pl ->
         case pl of
             Just player ->
                Array.set (i % 4)
                    { player | hand = card :: player.hand }
                    players
             Nothing ->
                 players
    ))


setTableHandOwnerAsActualPlayer owner round =
    case playerIndex round.players owner of
        Just i ->
            { round | tableHandOwner = Just i }
        Nothing ->
            round


putCardsOnTable cards round =
    { round | table = cards :: round.table }


nextRound table =
    { table
    | round = initRound ((table.round.seed + 19) * 263) table.users
    , history = table.round :: table.history
    }


updatePlayerPlayingHand player param =
    modifyPlayer player.name
        (\player -> { player
            | hand = removeCards param.parsedCards player.hand
            , cardsOnHand = List.length (removeCards param.parsedCards player.hand)
        })


playHandAndUpdateRound table round player param =
    case param.parsedCards of
        Dog :: [] ->
            { table |
                round =
                    { round | actualPlayer = (round.actualPlayer + 2) % 4 }
                    |> setTableHandOwnerAsActualPlayer player
                    |> maybeIncActualPlayer
                    |> updatePlayerPlayingHand player param
                    |> maybeSetWinner (removeCards param.parsedCards player.hand) round.actualPlayer
                    |> (\round ->
                        let
                            player = getActualPlayer round
                        in
                            modifyPlayer
                                player.name
                                (\player -> { player
                                    | collected = [ [ [ Dog ] ] ] ++ player.collected
                                })
                                round
                    )
                    -- putCardsOnTable param.parsedCards
            }
        _ ->
            { table |
                round =
                    round
                    |> updatePlayerPlayingHand player param
                    |> setActualPlayer player
                    |> setTableHandOwnerAsActualPlayer player
                    |> maybeIncActualPlayer
                    |> maybeSetWinner (removeCards param.parsedCards player.hand) round.actualPlayer
                    |> putCardsOnTable param.parsedCards
            }
    |> maybeEndRound


maybeEndRound table =
    table.round.players
    |> List.filter (.hand >> List.isEmpty >> not)
    |> List.length
    |> (\activePlayers ->
        if (activePlayers == 1) then
            nextRound table
        else
            table
    )


modifyNthPlayer i modifier round =
    case List.drop i round.players |> List.head of
        Just player ->
            modifyPlayer player.name modifier round
        Nothing ->
            Debug.log ("Could not find nth player: " ++ toString i) round


maybeCollectCards round =
    case round.tableHandOwner of
        Just owner ->
            if owner == round.actualPlayer then
                collectCards owner round
            else
                round
        Nothing ->
            round


collectCards owner round =
    { round
    | tableHandOwner = Nothing
    , table = []
    }
    |> modifyNthPlayer owner
        (\player -> { player
            | collected = Debug.log "collected" <| round.table :: player.collected
        })


{-| Exchange cards between players
-}
exchangeCardsBetweenPlayers table =
    Array.fromList table.round.players
    |> (\p ->
        Array.indexedMap (,) p
        |> Array.foldr (\(i, player) players ->
            case player.exchange of
                Just (a, b, c) ->
                    switchCard c (i + 1) players
                    |> switchCard a (i + 2)
                    |> switchCard b (i + 3)
                Nothing ->
                    Debug.log "Cannot exchange if player did not declare cards" players
        ) p
    )
    |> (\players ->
        let
            round = table.round
        in
            { round
            | players = List.map (\player ->
                    { player | hand =
                        case player.exchange of
                            Just (a, b, c) ->
                                removeCards [a, b, c] player.hand
                                |> sortWith cardOrder
                            Nothing ->
                                Debug.log "Cannot remove if player did not declare cards" player.hand
                    }
                ) <| Array.toList players
            }
    )
    |> (\round -> { table | round = round })


