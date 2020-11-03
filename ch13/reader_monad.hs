import Control.Monad.Instances

-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w) w

-- addStuff :: Int -> Int
-- addStuff = do
--   a <- (* 2)
--   b <- (+ 10)
--   return (a + b)

addStuff :: Int -> Int
addStuff x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b

-- ghci> addStuff 3
-- 19
