-- ghci> max 4 5
-- 5
-- ghci> (max 4) 5
-- 5

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- ghci> let multTwoWithNine = multThree 9
-- ghci> multTwoWithNine 2 3
-- 54
-- ghci> let multWithEighteen = multTwoWithNine 2
-- ghci> multWithEighteen 10
-- 180

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ghci> applyTwice (+3) 10
-- 16
-- ghci> applyTwice (++ " HAHA") "HEY"
-- "HEY HAHA HAHA"
-- ghci> applyTwice ("HAHA " ++) "HEY"
-- "HAHA HAHA HEY"
-- ghci> applyTwice (multThree 2 2) 9
-- 144
-- ghci> applyTwice (3:) [1]
-- [3,3,1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
-- [6,8,7,9]
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- flip' f y x = f x y

-- ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
-- [5,4,3,2,1]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- ghci> map (+3) [1,5,3,1,6]
-- [4,8,6,4,9]
-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]

addThree :: (Num b) => [b] -> [b]
addThree [] = []
addThree (x : xs) = (3 + x) : addThree xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- [5,6,4]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (>= x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 166650
-- ghci > sum (takeWhile (<10000) [x | x <- [y ^ 2 | y <- [1..]], odd x])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

-- ghci> let listOfFuns = map (*) [0..]
-- ghci> (listOfFuns !! 4) 5
-- 20

-- numLongChains :: Int
-- numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- [153.0,61.5,31.0,15.75,6.6]

-- ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- [3,8,9,8,7]

addThree' :: (Num a) => a -> a -> a -> a
addThree' x y z = x + y + z

-- addThree' :: (Num a) => a -> a -> a -> a
-- addThree' = \x -> \y -> \z -> x + y + z

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

fileter'' :: (a -> Bool) -> [a] -> [a]
fileter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- ghci> scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]
-- ghci> scanr (+) 0 [3,5,2,1]
-- [11,8,3,1,0]
-- ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- [3,4,5,5,7,9,9,9]
-- ghci> scanl (flip (:)) [] [3,2,1]
-- [[],[3],[2,3],[1,2,3]]

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- ghci> sqrtSums
-- 131

($) :: (a -> b) -> a -> b
f $ x = f x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]
-- ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]

-- ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- [-14,-15,-27]
-- ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- [-14,-15,-27]

-- sum (replicate 5 (max 6.7 8.9))
-- 44.5
-- (sum . replicate 5 . max 6.7) 8.9
-- 44.5

fn :: (RealFrac a, Integral b, Floating a) => a -> b
fn x = ceiling (negate (tan (cos (max 50 x))))

-- fn = ceiling . negate . tan . cos . max 50