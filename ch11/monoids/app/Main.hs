import qualified Data.Foldable as F

class Monoid' m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr Main.mappend Main.mempty

-- List

instance Monoid' [a] where
  mempty = []
  mappend = (++)

-- ghci> mempty :: [a]
-- []
-- ghci> ("one" `mappend` "two") `mappend` "tree"
-- "onetwotree"
-- ghci> "one" `mappend` ("two" `mappend` "tree")
-- "onetwotree"

-- Product and Sum

newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

-- ghci> getProduct $ Product 3 `mappend` Product 9
-- 27
-- ghci> getProduct $ Product 3 `mappend` mempty
-- 3

-- Any and All

newtype Any = Any {getAny :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

newtype All = All {getAll :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' All where
  mempty = All True
  All x `mappend` All y = All (x && y)

--The Ordering monoid

instance Monoid' Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT

-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y =
--   let a = length x `compare` length y
--       b = x `compare` y
--    in if a == EQ then b else a

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  (length x `compare` length y)
    `Main.mappend` (x `compare` y)

-- Maybe the monoid

instance Monoid' a => Monoid' (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `Main.mappend` m2)

-- ghci> Nothing `mappend` Just "andy"
-- Just "andy"
-- ghci> Just LT `mappend` Nothing
-- Just LT
-- ghci> Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance Monoid' (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x

-- ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- Just 9

-- Using monoids to fold data structures

-- ghci> F.foldl (+) 2 (Just 9)
-- 11
-- ghci> F.foldr (||) False (Just True)
-- True

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance F.Foldable Tree where
  foldMap f Empty = Prelude.mempty
  foldMap f (Node x l r) =
    F.foldMap f l
      `Prelude.mappend` f x
      `Prelude.mappend` F.foldMap f r

testTree :: Tree Integer
testTree =
  Node
    5
    ( Node
        3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    ( Node
        9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )

-- ghci> F.foldl (+) 0 testTree
-- 42
-- ghci> F.foldl (*) 1 testTree
-- 64800
