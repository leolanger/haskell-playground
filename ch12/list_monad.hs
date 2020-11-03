class Monad' m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x Main.>>= \_ -> y
  fail :: String -> m a
  fail msg = error msg

instance Monad' [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []

-- ghci> [3,4,5] >>= \x -> [x,-x]
-- [3,-3,4,-4,5,-5]
-- ghci> [] >>= \x -> ["bad","mad","rad"]
-- []
-- ghci> [1,2,3] >>= \x -> []
-- []

-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  Main.return (n, ch)

class Monad' m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = Main.return ()
guard False = mzero

-- ghci> guard (5 > 2) :: Maybe ()
-- Just ()
-- ghci> guard (1 > 2) :: Maybe ()
-- Nothing
-- ghci> guard (5 > 2) :: [()]
-- [()]
-- ghci> guard (1 > 2) :: [()]
-- []

-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]
-- ghci> guard (5 > 2) >> return "cool" :: [String]
-- ["cool"]
-- ghci> guard (1 > 2) >> return "cool" :: [String]
-- []

sevensOnly :: [Int]
sevensOnly = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  Main.return x