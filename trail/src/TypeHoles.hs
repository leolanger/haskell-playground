{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeHoles (a, b) where

-- type holes allow underscores as stand­ins for actual values

-- By placing an underscore on any value on the right hand­side
-- of a declaration, GHC will throw an error during type­checking.
-- The error message describes which values may legally fill the type hole.

-- head' :: a
-- head' = head _

-- const' :: _
-- const' x y = x

-- foo :: _a -> _a
-- foo _ = False

-- succ' :: _ => a -> a
-- succ' x = x + 1

{-Deferred Type Errors-}

-- Even though a is ill-typed, it is not used in the end,
-- so if all that we’re interested in is main it can be useful to be able to ignore the problems in a

a :: Int
a = 'a'

b :: Int
b = 'b'

main = print "b"
