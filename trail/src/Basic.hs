module Basic where

import Data.List
import Prelude hiding (head, undefined, (!!), ($), (.))

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

{-Typeclasses-}

-- we can define an equality class which allows us to define an overloaded notion of equality depending on the
-- data structure provided.
class Equal a where
  equal :: a -> a -> Bool

-- we can define this typeclass over several different types.

instance Equal Bool where
  equal True True = True
  equal False False = True
  equal True False = False
  equal False True = False

instance Equal () where
  equal () () = True

data Ordering' = LT' | EQ' | GT'

instance Equal Ordering' where
  equal LT' LT' = True
  equal EQ' EQ' = True
  equal GT' GT' = True
  equal _ _ = False

-- An Equal instance for a more complex data structure relies upon the fact that the type of the elements
-- in the list must also have a notion of equality,

instance (Equal a) => Equal [a] where
  equal [] [] = True
  -- Empty lists are equal
  equal [] ys = False -- Lists of unequal size are not equal
  equal xs [] = False
  -- equal x y is only allowed here due to the constraint (Equal a)
  equal (x : xs) (y : ys) = equal x y && equal xs ys

instance (Equal a, Equal b) => Equal (a, b) where
  equal (x0, x1) (y0, y1) = equal x0 y0 && equal x1 y1

-- After the definition of a datatype you can
-- add a deriving clause which will generate the instances for this datatype automatically.

data List a
  = Cons a (List a)
  | Nil
  deriving (Eq, Ord, Show)

{-Bottoms-}

-- For instance, undefined is an easily called example of a bottom value.
-- This function has type a but lacks any type constraints in its type signature.

undefined :: a
undefined = error "Prelude.undefined"

-- mean :: Num a => Vector a -> a     -- Partially defined function
-- mean nums = (total / count)
--   where
--     total = undefined
--     count = undefined

-- addThreeNums :: Num a => a -> a -> a -> a
-- addThreeNums n m j = undefined     -- No function body declared at all

-- f :: a -> Complicated Type
-- f = undefined                      -- Write tomorrow, typecheck today! Arbitrarily complicated types

-- Another example of a bottom value comes from the evaluation of the error function

-- error :: String -> a

-- divByY :: (Num a, Eq a, Fractional a) => a -> a -> a
-- divByY _ 0 = error "Divide by zero error."
-- divByY dividend divisor = dividend / divisor

--eg.
fn :: t
fn = let x = x in x

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs Basic.!! (n -1)
