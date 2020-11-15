module List () where

myList1 :: [Int]
myList1 = [1, 2, 3]

myList2 :: [Bool]
myList2 = [True, False]

-- [1, 2, 3] = 1 : 2 : 3 : []
-- [1, 2, 3] = 1 : (2 : (3 : []))
-- (:) :: a -> [a] -> [a]

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = length' xs + 1

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

powersOfTwo :: [Integer]
powersOfTwo = iterate' (2 *) 1

-- λ : take 15 powersOfTwo
-- [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384]