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
    | Straight Int Int
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
        (Straight r1 i1, Straight r2 i2) -> i1 == i2 && r1 > r2
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
    let
        parseStraight length last next =
            case next of
                (NormalCard _ r) :: t ->
                    case rankWeight r == last + 1 of
                        True -> parseStraight (length + 1) (last + 1) t
                        False -> Nothing
                Phoenix :: t ->
                    -- MahJong after Ace does not count
                    case last < 14 of
                        True -> parseStraight (length + 1) (last + 1) t
                        False -> Nothing
                [] ->
                    case length >= 5 of
                        True -> Just (Straight last length)
                        False -> Nothing
                _ ->
                    Nothing

    in
        case cards of
            (NormalCard _ r) :: t ->
                parseStraight 1 (rankWeight r) t
            MahJong :: t ->
                parseStraight 1 1 t
            Phoenix :: (NormalCard _ r) :: t ->
                parseStraight 2 (rankWeight r) t
            _ ->
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
    )


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
    , human : Bool
    }


type alias Game =
    { name : String
    , config : GameConfig
    , test : Bool
    , seed : Int
    , users : List GameUser
    , round : Round
    , history : List Round
    , messages : List Message
    , log : List UpdateGame
    , finished : Maybe (List Int)
    }


type GameType
    = Humans
    | HumansVsBots
    | Bots


type alias GameConfig =
    { gameType : GameType
    }


type alias AwaitingTableUser =
    { name : String
    , lastCheck : Time
    , pressedStart : Bool
    , human : Bool
    }


type alias AwaitingTable =
    { name : String
    , config : GameConfig
    , users : List AwaitingTableUser
    , test : Bool
    , seed : Int
    }


type UpdateGame
    = UpdatePlayer (Player -> Player)
    | UpdateRound (Round -> Round)


initGame : String -> GameConfig -> Bool -> Int -> List AwaitingTableUser -> Game
initGame name config test seed users =
    let
        gameUsers =
            List.map (\user ->
                GameUser user.name user.lastCheck user.human
            ) users
    in
        { name = name
        , config = config
        , test = test
        , seed = seed
        , users = gameUsers
        , round = initRound seed gameUsers
        , history = []
        , messages = []
        , log = []
        , finished = Nothing
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
                choose (hasCard MahJong user) (Just i) before
            ) Nothing
            |> Maybe.withDefault 0
        }
    )


initPlayer : List Card -> String -> Int -> Player
initPlayer cards name offset =
    { hand = take 14 <| drop (offset * 14) cards
    , cardsOnHand = 14
    , collected = []
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


getNthPlayer round index =
    List.drop (index % 4) round.players
    |> List.head
    |> defaultCrash ("Malformed state getNthPlayer: " ++ (toString index) ++ ": " ++ toString round)


getPlayer round name =
    List.filter (.name >> (==) name) round.players
    |> List.head
    |> defaultCrash ("Malformed state getPlayer: " ++ name ++ ": " ++ toString round)


modifyPlayer name function round =
    { round | players =
        List.map (\player ->
            choose (player.name == name) (function player) player
        ) round.players
    }


maybeModifyPlayer index function round =
    case index of
        Just i ->
            List.drop i round.players
            |> List.head
            |> (\player ->
                case player of
                    Just p ->
                        modifyPlayer p.name function round
                    _ ->
                        round
            )
        _ ->
            round


getActualPlayer : Round -> Player
getActualPlayer round =
    round.players
    |> List.drop round.actualPlayer
    |> List.head
    |> defaultCrash ("Malformed state getActualPlayer: " ++ toString round)


nextPlayerNumber i = (i + 1) % 4


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
        Just i -> { round | actualPlayer = nextPlayerNumber i }
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


giveLoosersCards : Round -> Round
giveLoosersCards round =
    round.players
    |> List.indexedMap (,)
    |> List.filter (Tuple.second >> .hand >> List.isEmpty >> not)
    |> List.head
    |> Maybe.map (\(i, looser) ->
        round
        |> maybeModifyPlayer round.winner
            (\player -> { player | collected = looser.collected ++ player.collected })
        |> maybeModifyPlayer (Just i)
            (\player -> { player | collected = [] })
        |> maybeModifyPlayer (Just (nextPlayerNumber i))
            (\player -> { player | collected = [ looser.hand ] :: player.collected })
    )
    |> Maybe.withDefault round


nextRound table =
    let
        history = (giveLoosersCards table.round) :: table.history
        ( t1, t2 ) = Debug.log "points:" <| calculateTeamPoints history
    in
        if t1 >= 1000 || t2 >= 1000 then
            { table
            | finished = Just [ t1, t2 ]
            , history = history
            }
            |> Debug.log "end of game!"
        else
            { table
            | round = initRound ((table.round.seed + 19) * 263) table.users
            , history = history
            }
            |> Debug.log "end of round!"


updatePlayerPlayingHand player param =
    modifyPlayer player.name
        (\player -> { player
            | hand = removeCards param.parsedCards player.hand
            , cardsOnHand = List.length (removeCards param.parsedCards player.hand)
        })


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


{-| Increment actual player

1. Add one to actulaPlayer index (cyclic modulo 4).
2. If actualPlayer == owner of hand
    - if Dragon is on top, do nothing more
    - if not, collect cards and if player has no more cards
      increment once again

-}
incActualPlayer r =
    { r | actualPlayer = nextPlayerNumber r.actualPlayer }
    |> maybeCollectCards
    |> (\(stop, round) ->
        if not stop && (List.isEmpty (getActualPlayer round).hand) then
            incActualPlayer round
        else
            round
    )


maybeCollectCards round =
    case round.tableHandOwner of
        Just owner ->
            if owner == round.actualPlayer then
                collectCards round
            else
                (False, round)
        Nothing ->
            (False, round)


giveDragonTo index round =
    { round
    | tableHandOwner = Nothing
    , table = []
    }
    |> (\r ->
        modifyNthPlayer index
            (\player -> { player | collected = round.table :: player.collected })
            r
    )


collectCards : Round -> (Bool, Round)
collectCards round =
    { round
    | tableHandOwner = Nothing
    , table = []
    }
    |> (\r ->
        case List.head round.table of
            Just (Dragon :: []) ->
                if List.isEmpty (getNthPlayer round (round.actualPlayer + 1)).hand then
                    (,) False <|
                    modifyNthPlayer ((round.actualPlayer + 3) % 4)
                        (\player -> { player | collected = round.table :: player.collected })
                        r
                else if List.isEmpty (getNthPlayer round (round.actualPlayer + 3)).hand then
                    (,) False <|
                    modifyNthPlayer ((round.actualPlayer + 1) % 4)
                        (\player -> { player | collected = round.table :: player.collected })
                        r
                else
                    -- player has to decide who gets the dragon
                    (True, round)
            _ ->
                (,) False <|
                modifyNthPlayer round.actualPlayer
                    (\player -> { player | collected = round.table :: player.collected })
                    r
    )


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


choose condition true false =
    if condition then true else false


calculatePlayersPoints : Round -> List Int
calculatePlayersPoints round =
    round.players
    |> List.map (\player ->
        player.collected
        |> List.concatMap identity
        |> List.concatMap identity
        |> List.map (\card ->
           case card of
               Dragon -> 25
               Phoenix -> -25
               NormalCard _ K -> 10
               NormalCard _ (R 10) -> 10
               NormalCard _ (R 5) -> 5
               _ -> 0
        )
        |> List.sum
        |> (\pointsFromCards ->
            if player.tichu || player.grandTichu then
                round.winner
                |> Maybe.andThen (\winner ->
                    List.drop winner round.players
                    |> List.head
                )
                |> Maybe.map (\winner ->
                    let
                        addon = choose player.tichu 100 200
                        won = choose (winner.name == player.name) identity negate
                    in
                        won addon + pointsFromCards
                )
                |> Maybe.withDefault pointsFromCards
            else
                pointsFromCards
        )
    )


calculateHistoryPoints : List Round -> List (List Int)
calculateHistoryPoints history =
    List.map calculatePlayersPoints history


calculateTeamPoints : List Round -> (Int, Int)
calculateTeamPoints history =
    let
        points = calculateHistoryPoints history
        summaries = List.foldl (\item sum ->
                case (item, sum) of
                    ([ i1, i2, i3, i4 ], [ s1, s2, s3, s4 ]) ->
                        [ s1 + i1, s2 + i2, s3 + i3, s4 + i4 ]
                    _ ->
                        sum
            ) [ 0, 0, 0, 0 ] points
    in
        case summaries of
            [ p1, p2, p3, p4 ] ->
                ( p1 + p3, p2 + p4 )
            _ ->
                ( 0, 0 )


