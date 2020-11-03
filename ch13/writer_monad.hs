import Control.Monad.Writer
import Data.Monoid

applayLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applayLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- ghci> ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})
-- ghci> ("jerky", Sum 25) `applyLog` addDrink
-- ("whiskey",Sum {getSum = 124})
-- ghci> ("dogmeat", Sum 5) `applyLog` addDrink
-- ("beer",Sum {getSum = 35})

{-The Writer type-}

-- newtype Writer w a = Writer {runWriter :: (a, w)}

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- eg.
-- ghci> runWriter (return 3 :: Writer String Int)
-- (3,"")
-- ghci> runWriter (return 3 :: Writer (Sum Int) Int)
-- (3,Sum {getSum = 0})
-- ghci> runWriter (return 3 :: Writer (Product Int) Int)
-- (3,Product {getProduct = 1})

{-do notation with Writer-}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

--eg.
-- ghci> runWriter multWithLog
-- (15,["Got number: 3","Got number: 5"])

{-adding logging to programs-}

-- gcd' :: Int -> Int -> Int
-- gcd' a b
--   | b == 0 = a
--   | otherwise = gcd' b (a `mod` b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

--inefficient list construction

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

-- Difference lists

-- f `append` g = \xs -> f (g xs)

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

--eg.
-- ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
-- [1,2,3,4,1,2,3]

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcd'' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

{-Comparing Performance-}

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x -1)
  tell (toDiffList [show x])

-- it is faster than normal method
