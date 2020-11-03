-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- Maybe monad

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :")""
-- Just "smile :""
-- ghci> Nothing `applyMaybe` \x -> Just (x+1)
-- Nothing
-- ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing
-- Nothing

-- Monad type class

class Monad' m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x Main.>>= \_ -> y
  fail :: String -> m a
  fail msg = error msg

instance Monad' Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing

-- ghci> return "WHAT" :: Maybe String
-- Just "WHAT"
-- ghci> Just 9 >>= \x -> return (x*10)
-- Just 90
-- ghci> Nothing >>= \x -> return (x*10)
-- Nothing
