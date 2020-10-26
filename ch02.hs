-- ghci> 5 /= 4
-- True

--ghci> min 9 10
--9

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- ghci> let lostNumbers = [4,8,15,16,23,48]
-- ghci> lostNumbers

-- ghci> [1,2,3,4] ++ [9,10,11,12]

-- ghci> "hello" ++ " " ++ "world"

--ghci> ['w', 'w'] ++ ['o','t']

--ghci> 'A':" SMALL CAT"

--ghci> 5:[1,2,3,4,5]

--ghci> "SteveBuscemi" !! 6

--ghci> head [5, 4, 3, 2, 1]
--ghci> tail [5, 4, 3, 2, 1]
--ghci> last [5, 4, 3, 2, 1]
--ghci> init [5, 4, 3, 2, 1]

--ghci> length [5, 4, 3, 2, 1]
--ghci> null [5, 3 ,2]
--ghci> reverse [5,4,3,2,1]
--ghci> take 3 [8,4,2,1]
--ghci> drop 3 [8,4,2,1]

--ghci> minimun [6,4,7,2,9]
--ghci> sum [5,2,1,2]
--ghci> product [6,2,1,2]

--ghci> 4 `elem` [3,4,5,6]

--ghci> take 10 (cycle [1,2,3])
--ghci> take 10 (repeat 5)

boomBangs :: Integral a => [a] -> [[Char]]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]
