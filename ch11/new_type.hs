-- data ZipList a = ZipList {getZipList :: [a]}

newtype ZipList a = ZipList {getZipList :: [a]}

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

-- ghci> CharList "this will be shown!"
-- CharList {getCharList = "this will be shown!"}
-- ghci> CharList "benny" == CharList "benny"
-- True
-- ghci> CharList "benny" == CharList "oisters"
-- False
-- ghci> getCharList $ CharList "this will be shown"
-- "this will be shown"

--

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- ghci> getPair $ fmap (*100) (Pair (2,3))
-- (200,3)
-- ghci> getPair $ fmap reverse (Pair ("london calling", 3))
-- ("gnillac nodnol",3)

--

-- data CoolBool = CoolBool { getCoolBool :: Bool }
-- ghci> helloMe undefined
-- "*** Exception: Prelude.undefined  "

newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- ghci > helloMe undefined
-- "hello"

--
