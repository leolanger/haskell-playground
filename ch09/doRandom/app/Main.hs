import Control.Monad (when)
import Data.List
import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- ghci> threeCoins (mkStdGen 21)
-- (True,True,True)
-- ghci> threeCoins (mkStdGen 22)
-- (True,False,True)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

-- ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
-- [-1807975507,545074951,-1015194702,-1622477312,-502893664]
-- ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
-- [True,True,True,True,False]
-- ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
-- [7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n -1) newGen
   in (value : restOfList, finalGen)

-- ghci> randomR (1,6) (mkStdGen 359353)
-- (6,1494289578 40692)
-- ghci> randomR (1,6) (mkStdGen 35935335)
-- (3,1250031057 40692)

-- ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
-- "ndkxbvmomg"

-- main = do
--   gen <- getStdGen
--   putStr $ take 20 (randomRs ('a', 'z') gen)
--   gen2 <- getStdGen
--   putStr $ take 20 (randomRs ('a','z') gen2)

-- main = do
--   gen <- getStdGen
--   let randomChars = randomRs ('a', 'z') gen
--       (first20, rest) = splitAt 20 randomChars
--       (second20, _) = splitAt 20 rest
--   putStrLn first20
--   putStr second20

-- main = do
--   gen <- getStdGen
--   putStrLn $ take 20 (randomRs ('a', 'z') gen)
--   gen' <- newStdGen
--   putStr $ take 20 (randomRs ('a', 'z') gen')

-- main = do
--     gen <- getStdGen
--     askForNumber gen

-- askForNumber :: StdGen -> IO ()
-- askForNumber gen = do
--     let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--     putStr "Which number in the range from 1 to 10 am I thinking of? "
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString
--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--             askForNumber newGen

main = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main
