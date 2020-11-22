{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Basic2 where

import Control.Exception

{-Exhaustiveness-}

-- Since the case of a Nothing was not defined in unsafe , we say that the pattern matching within that function is non­exhaustive.

unsafe :: Num a => Maybe a -> Maybe a
unsafe (Just x) = Just $ x + 1

-- such a function will halt from an incomplete match like Nothing.

-- Several flags exist that we can pass to the compiler to warn us about such patterns or forbid them entirely

-- ghc -c -Wall -Werror A.hs
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

boom :: Num a => Maybe a -> Maybe a
boom = \(Just a) -> Just (a + 1)
