-- ghci> :t 'a'
-- 'a' :: Char

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- ghci> :t head
-- head :: [a] -> a

-- ghci> :t (==)
-- (==) :: (Eq a) => a -> a -> Bool

-- ghci> :t (>)
-- (>) :: (Ord a) => q -> q -> Bool

-- ghci> "Abrakadabra" `compare` "Zebra"
-- LT

-- ghci> [2, 3] `compare` [2, 3]
-- EQ

-- ghci> 5 `compare` 3
-- GT

-- ghci> show 3
-- "3"

-- ghci> read "5" - 2
-- 3

-- ghci> read "(3, 'a')" :: (Int, Char)
-- (3, 'a')

-- ghci> maxBound :: (Bool, Int, Char)
-- (True，2147483647，'\1114111')

-- ghci> :t 20
-- 20 :: (Num t) => t

-- ghci> :t (*)
-- (*) :: (Num a) => a -> a -> a
