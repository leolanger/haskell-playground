import Control.Monad
import Data.Char

-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

-- main = do
--   contents <- getContents
--   putStr (map toUpper contents)

--

-- main = do
--   contents <- getContents
--   putStr (shortLinesOnly contents)

-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--   let allLines = lines input
--       shortLines = filter (\line -> length line < 10) allLines
--       result = unlines shortLines
--    in result

--main = interact $ unlines . filter ((< 10) . length) . lines

--
