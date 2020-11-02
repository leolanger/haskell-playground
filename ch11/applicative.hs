-- ghci> :t fmap (++) (Just "hey")
-- fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
-- ghci> :t fmap compare (Just 'a')
-- fmap compare (Just 'a') :: Maybe (Char -> Ordering)
-- ghci> :t fmap compare "A LIST OF CHARS"
-- fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
-- ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]
-- fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]

import Control.Applicative (ZipList (ZipList))

class (Functor f) => Applicative' f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b

instance Applicative' Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
  f <$> x = fmap f x

-- ghci> pure (+3) <*> Just 9
-- Just 12
-- ghci> Just (++"hahah") <*> Nothing
-- Nothing
-- ghci> Nothing <*> Just "woot"
-- Nothing

-- ghci> pure (+) <*> Just 3 <*> Just 5
-- Just 8
-- ghci> pure (+) <*> Just 3 <*> Nothing
-- Nothing

-- ghci> (++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"

-- List

instance Applicative' [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
  f <$> x = fmap f x

-- ghci> [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
-- ghci> [(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]
-- ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- IO

instance Applicative' IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)
  a <$> b = fmap a b

-- myAction :: IO String
-- myAction = do
--     a <- getLine
--     b <- getLine
--     return $ a ++ b

myAction :: IO String
myAction = (++) Main.<$> getLine Main.<*> getLine

main = do
  a <- (++) Main.<$> getLine Main.<*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a

-- (->) r

instance Applicative' ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
  f <$> x = fmap f x

-- ghci> pure 3 "blah"
-- 3
-- ghci> :t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
-- ghci> (+) <$> (+3) <*> (*100) $ 5
-- 508

-- ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- [8.0,10.0,2.5]

-- ZipList

instance Applicative' ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
  fs <$> ZipList xs = fmap fs (ZipList xs)

-- ghci> (+) Main.<$> ZipList [1,2,3] Main.<*> ZipList [100,100,100]
-- ZipList {getZipList = [101,102,103]}ain.<*> ZipList [100,100,100]
-- ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
-- [101,102,103]
-- ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
-- [5,3,3,4]
-- ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]

-- function

liftA2 :: (Applicative' f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f Main.<$> a Main.<*> b

-- ghci> liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
-- ghci> (:) <$> Just 3 <*> Just [4]
-- Just [3,4]

sequenceA :: (Applicative' f) => [f a] -> f [a]
sequenceA [] = Main.pure []
sequenceA (x : xs) = (:) Main.<$> x Main.<*> Main.sequenceA xs

-- sequenceA = foldr (liftA2 (:)) (Main.pure [])

-- ghci> sequenceA [Just 3, Just 2, Just 1]
-- Just [3,2,1]
-- ghci> sequenceA [Just 3, Nothing, Just 1]
-- Nothing
-- ghci> sequenceA [(+3),(+2),(+1)] 3
-- [6,5,4]
-- ghci> sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
-- []

-- ghci> map (\f -> f 7) [(>4),(<10),odd]
-- [True,True,True]
-- ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
-- True
-- ghci> sequenceA [(>4),(<10),odd] 7
-- [True,True,True]
-- ghci> and $ sequenceA [(>4),(<10),odd] 7
-- True

-- ghci> sequenceA [[1,2],[3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
-- ghci> [[x,y] | x <- [1,2], y <- [3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
