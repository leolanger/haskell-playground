import Data.List
import System.Directory
import System.IO

-- main = do
--   todoItem <- getLine
--   appendFile "todo.txt" (todoItem ++ "\n")

main = do
  withFile
    "something.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

-- main = do
--   withFile
--     "something.txt"
--     ReadMode
--     ( \handle -> do
--         hSetBuffering handle $ BlockBuffering (Just 2048)
--         contents <- hGetContents handle
--         putStr contents
--     )
