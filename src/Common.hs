module Common where

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List
import Data.Maybe (fromJust, fromMaybe)

data Card = Card { color::Color, value::Value } deriving (Read, Eq)

instance Show Card where
  show c = show (color c, value c)

instance Ord Card where
  compare c1 c2 = compare (color c1, value c1) (color c2, value c2)

instance Enum Card where
  toEnum n = let (v,s) = n `divMod` 5
             in Card (toEnum v) (toEnum s)

  fromEnum c = fromEnum (value c) * 5 + fromEnum (color c)

data Value = Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Plus2
           | Stop
           | ChDir
           | Plus4
           | ChCol
           | Dummy
           deriving (Read, Show, Eq, Ord, Enum)

type Hand = [ Card ]
type Deck = [ Card ]
type D_Stack = [ Card ]

data Player = HPlayer { name :: String,
                        hand :: Hand }
            | AiPlayer { name :: String,
                         hand :: Hand }
            | NoPlayer { name :: String } deriving (Show, Eq)

noPlayer :: Player
noPlayer = NoPlayer { name = "No Player" }

data State = State { players :: [ Player ],
                     e_players :: [ Player ],
                     deck :: Deck,
                     d_stack :: D_Stack,
                     cur_player :: Player }

data Action = UseCard
            | TakeFromDeck
            | EndTurn
            | AttackReverse
            | AttackSkip
            | AttackDraw2
            | AttackWildDraw4 deriving (Read, Show, Eq)

data Attack = NoAttack
            | Skip
            | Draw2
            | Draw4 deriving (Read, Show, Eq)

numberCards = zeroes ++ (nplicate 2 ncards) where
  zeroes = [ Card c Zero | c <- [Red .. Blue] ]
  ncards = [ Card c v | c <- [Red .. Blue], v <- [One .. Nine] ]

attackCards = (nplicate 2 acards) where
  acards = [ Card c v | c <- [Red .. Blue], v <- [Plus2 .. ChDir] ]

wildCards = (nplicate 4 blacks) where
  blacks = [ Card Black v | v <- [Plus4, ChCol] ]

fullDeck :: Deck
fullDeck = numberCards ++ attackCards ++ wildCards

-- Not regular cards, but used in the game loop, to represent change of color.
changeColCards = [ Card c v | c <- [Red .. Blue], v <- [Plus4, ChCol] ]

colorBlack c = Card { color = Black, value = value c }
colorizeWildcard col c = Card { color = col, value = (value c) }
whiteDummy = Card { color = White, value = Dummy }
noCard = whiteDummy

isNumberCard :: Card -> Bool
isNumberCard c = c `elem` numberCards

isAttackCard :: Card -> Bool
isAttackCard c = c `elem` attackCards

isWildcard :: Card -> Bool
isWildcard c = c `elem` wildCards

isChangeColCards :: Card -> Bool
isChangeColCards c = c `elem` changeColCards

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

getCards :: Deck -> Card -> [ Card ]
getCards d c = [ c' | c' <- d, c' == c ]

countCards :: Deck -> Card -> Int
countCards d c = length ([ c' | c' <- d, c' == c ])

countCardsByColor :: Color -> Hand -> Int
countCardsByColor col hand = length $ [ c | c <- hand, (color c) == col ]

takeCards :: [ Card ] -> Hand -> (Hand, Hand)
takeCards [ card ] from = ([ card ], f1 ++ (drop 1 f2)) where
  (f1, f2) = break (== card) from
takeCards (card:rest) from = (t1 ++ t2, remaining) where
  (t1, from') = takeCards [ card ] from
  (t2, remaining) = takeCards rest from'

clearHands :: [ Player ] -> [ Player ]
clearHands players = map (\p' -> p' { hand = [ ] }) players

blackenCards :: [ Card ]-> [ Card ]
blackenCards hand = map (\c -> if isChangeColCards c then colorBlack c else c) hand

colorInHand :: Color -> Hand -> Bool
colorInHand col hand = any (\c -> (color c) == col) hand

getCardWithColor :: Color -> Hand -> Maybe Card
getCardWithColor col hand = find (\c -> (color c) == col) hand

valueInHand :: Value -> Hand -> Bool
valueInHand val hand = any (\c -> (value c) == val) hand

getCardWithValue :: Value -> Hand -> Maybe Card
getCardWithValue val hand = find (\c -> (value c) == val) hand

wildcardInHand :: Hand -> Bool
wildcardInHand hand = any (\c -> isWildcard c) hand

getWildcard :: Hand -> Maybe Card
getWildcard hand = find (\c -> isWildcard c) hand
