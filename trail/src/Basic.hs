module Basic where

import Data.List
import Prelude hiding (($), (.))

{-operators and Sections-}
-- An operator is a function that can be applied using infix syntax or partially applied using a section.

{-Tuple-}

tuple2 :: (Integer, [Char])
tuple2 = (1, "foo")

fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

{-Where &^ Let Clause-}

-- A let binding is an expression and binds anywhere in its body

f :: Integer
f =
  let x = 1
      y = 2
   in (x + y)

-- A where binding is a toplevel syntax construct (i.e. not an expression) that binds variables at the end of a function.

f' :: Integer
f' = x + y
  where
    x = 1
    y = 1

{-Conditionals-}

absolute :: Int -> Int
absolute n =
  if n < 0
    then (- n)
    else n

-- If statements are just syntactic sugar for case expressions over boolean values.

absolute' :: Int -> Int
absolute' n = case (n < 0) of
  True -> (- n)
  False -> n

{-Function Composition-}

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- Haskell code will liberally use this operator to compose chains of functions.

example :: [Integer] -> [Integer]
example =
  sort
    . filter (< 100)
    . map (* 10)

-- flip function takes as its first argument a function of two argu­ments,
-- and reverses the order of these two arguments returning a new function.

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- operator $ is right associative and takes the entire expression
-- on the right hand side of the operator and applies it to function on the left.

infixr 0 $

($) :: (a -> b) -> a -> b
($) h x = (h x)

--

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

-- The on function takes a function b and yields the result of applying unary function u to two arguments x and y

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

-- λ : sortSize = sortBy (compare `on` length)
-- λ : sortSize [[1,2], [1,2,3], [1]]
-- [[1],[1,2],[1,2,3]]

{-List Comprehensions-}

-- [n*x | x <- [1,2,3,4,5], let n = 3, odd x]
--       ^^^^^^^^^^^^^^^^   ^^^^^^^^^  ^^^^^
--       Generator        Let binding   Guard

factorial :: Integer -> Integer
factorial n = product [1 .. n]

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [n | n <- xs, n `mod` p > 0]
