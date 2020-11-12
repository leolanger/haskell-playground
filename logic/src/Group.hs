module Group () where

import Prelude hiding (Monoid, Semigroup, mappend, mconcat, mempty, (<>))

{-Semigroup-}
class Semigroup a where
  (<>) :: a -> a -> a

-- also required to be combinable
-- A <> B <> C = A <> (B <> C)

{-Monoid-}
class Semigroup m => Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mappend = (<>)
  mconcat :: [m] -> m
  mconcat = foldl mappend mempty

{-Group-}
class Monoid m => Group m where
  invert :: m -> m

-- should follow
-- invert m <> m = mempty
-- m <> (invert) m = mempty
