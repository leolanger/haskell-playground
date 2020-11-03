{-liftM-}

-- eg.

-- ghci > liftM (* 3) (Just 8)
-- Just 24
-- ghci > fmap (* 3) (Just 8)
-- Just 24

import Control.Monad.Writer

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

{-ap-}

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

--eg.
-- ghci> Just (+3) <*> Just 4
-- Just 7
-- ghci> Just (+3) `ap` Just 4
-- Just 7

{-the join function-}

-- eg.
-- ghci> join (Just (Just 9))
-- Just 9
-- ghci> join (Just Nothing)
-- Nothing
-- ghci> join [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]

-- ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))
-- (1,"bbbaaa")
-- ghci> join (Right (Right 9)) :: Either String Int
-- Right 9
-- ghci> join (Right (Left "error")) :: Either String Int
-- Left "error"

join :: (Monad m) => m (m a) -> m a
join mm = do
  m <- mm
  m

{-filterM-}

-- filter :: (a -> Bool) -> [a] -> [a]
-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

-- eg.

-- ghci> filter (\x -> x < 4) [9,1,5,2,10,3]
-- [1,2,3]

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- ghci> powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

{-foldM-}

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

-- eg.
-- ghci> foldM binSmalls 0 [2,8,3,1]
-- Just 14
-- ghci> foldM binSmalls 0 [2,11,3,1]
-- Nothing

{-Composing monadic fungtions-}

-- eg.

-- ghci> let f = (+1) . (*100)
-- ghci> f 4
-- 401
-- ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))
-- ghci> Just 4 >>= g
-- Just 401
