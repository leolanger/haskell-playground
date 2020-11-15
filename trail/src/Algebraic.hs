module Algebraic (Card, diamond, spade, heart, club, Value, Suit, Color) where

data Suit = Clubs | Diamonds | Hearts | Spades

data Color = Red | Black

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Show)

data Card = Card {suit :: Suit, color :: Color, value :: Value}

diamond :: Value -> Card
diamond = Card Diamonds Red

spade :: Value -> Card
spade = Card Spades Black

heart :: Value -> Card
heart = Card Hearts Red

club :: Value -> Card
club = Card Clubs Black

-- data List a = Nil | List a (List a)
data List a = Nil | a :+: (List a)

list :: List Integer
list = 1 :+: (2 :+: (3 :+: Nil))

-- list = List 1 (List 2 (List 3 Nil))
