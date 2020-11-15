module PatternMatching where

data Suit = Clubs | Diamonds | Hearts | Spades

data Color = Red | Black

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show)

data Card = Card {suit :: Suit, color :: Color, value :: Value}

-- pattern matching can be used to unpack an algebraic datatype and extract its fields
data Example = Example Int Int Int

example1 :: Example -> Int
example1 x = case x of
  Example a b c -> a + b + c

example2 :: Example -> Int
example2 (Example a b c) = a + b + c

-- can use a double pattern match to produce a function which determines which suit trumps another suit
-- value :: Value -> Integer
-- value card = case card of
--   Two -> 2
--   Three -> 3
--   Four -> 4
--   -- ...
--   Ace -> 1

suitBeats :: Suit -> Suit -> Bool
suitBeats Clubs Diamonds = True
suitBeats Clubs Hearts = True
suitBeats _ _ = False

beats :: Card -> Card -> Bool
beats (Card suit1 color1 value1) (Card suit2 color2 value2) = (suitBeats suit1 suit2) && (value1 > value2)

-- This is quite common in pattern matching definitions
-- which recursively tear down or build up data structures.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Pattern matching on lists is also an extremely common pattern.

addOne :: [Int] -> [Int]
addOne (x : xs) = (x + 1) : addOne xs
addOne [] = []

{-Guards-}

absolute :: Int -> Int
absolute n
  | n < 0 = (- n)
  | otherwise = n

absoluteJust :: Maybe Int -> Maybe Int
absoluteJust n = case n of
  Nothing -> Nothing
  Just n
    | n < 0 -> Just (- n)
    | otherwise -> Just n
