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

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing