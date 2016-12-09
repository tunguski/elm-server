module TichuModel exposing (..)


import Dict exposing (Dict, empty, insert, update)
import List exposing (..)
import Maybe exposing (andThen)


-----------------------------------------------------------------------------
-- MODEL
-----------------------------------------------------------------------------

type Suit = Clubs | Diamonds | Hearts | Spades
type Rank = R Int | J | Q | K | A
type Card = NormalCard Suit Rank | MahJong | Dog | Phoenix | Dragon
type alias Cards = List Card


allowedRanks : List Rank
allowedRanks = (map (\i -> R i) [2..10]) ++ [ J, Q, K, A ]


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


type Combination
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
    [a] -> Just (SingleCard (cardWeight a))
    _ -> Nothing


pair : Cards -> Maybe Combination
pair combination =
  case combination of
    [NormalCard a b , NormalCard c d] ->
      if rankWeight b == rankWeight d
      then
        Just (Pair b)
      else
        Nothing
    _ -> Nothing


-- two or more "stairs" (consecutive pairs; for example, 55667788. Non-consecutive pairs may not be played); 
pairStairs : Cards -> Maybe Combination
pairStairs combination =
  Maybe.map (\(r, i) -> PairStairs r i) (extractPairStairs combination)


nextPairPower : (Rank, Int) -> Card -> (Rank, Int)
nextPairPower (r, i) card =
  case card of
    NormalCard s r -> (r, i + 1)
    _ -> (r, i)
    
    
calculatePairStraitPower : Card -> Card -> Maybe Card
calculatePairStraitPower a b =
  ((pair [a,b]) `andThen` (\combination ->
    case combination of
      Pair rank ->
        case a of
          NormalCard s r ->
            if ((rankWeight r) == 0 || (rankWeight r) == (rankWeight rank) + 1) then
              Just a
            else
              Nothing
          _ -> Nothing
      _ -> Nothing
  ))


extractPairStairs : Cards -> Maybe (Rank, Int)
extractPairStairs combination =
  case combination of
    a::b::tail ->
      Maybe.map2 nextPairPower
        (extractPairStairs tail)
        (calculatePairStraitPower a b)
    _ -> Nothing


-- three of a kind; 
-- straights of at least five cards in length, regardless of suit/color (so 56789TJQ is playable); 
-- and full houses (three of a kind & a pair). 
-- Four of a kind or a straight flush of at least five cards is a bomb
allowedCombination : Cards -> Cards -> Bool
allowedCombination table combination =
  False


type alias Player =
  { hand : List Card
  , collected : List Card
  , selection : List Card
  , name : String
  , score : Int
  , tichu : Bool
  , grandTichu : Bool
  }


type alias Round =
    -- players in order, first has to play
  { players : Dict Int Player
    -- hands on table
  , table : List Cards
  , actualPlayer : Int
  }


type MessageType = Error | Warning | Info | Success


type alias Message =
  { messageType : MessageType
  , text : String
  }


type alias Game =
  { round : Round
  , history : List Round
  , messages : List Message
  , log : List UpdateGame
  }


type UpdateGame
    = UpdatePlayer (Player -> Player)
    | UpdateRound (Round -> Round)


initialModel : Game
initialModel =
  { round = initRound allCards
  , history = []
  , messages = []
  , log = []
  }
  
  
initRound : List Card -> Round
initRound cards =
  { players = 
      insert 0 (initPlayer cards 0) <|
      insert 1 (initPlayer cards 1) <|
      insert 2 (initPlayer cards 2) <|
      insert 3 (initPlayer cards 3) <| empty
  , table = []
  , actualPlayer = 0
  }
  
  
initPlayer : List Card -> Int -> Player
initPlayer cards offset =
  { hand = sortWith cardOrder <| take 13 <| drop (offset * 13) cards
  , collected = []
  , selection = []
  , name = "test!"
  , score = 0
  , tichu = False
  , grandTichu = False
  }


