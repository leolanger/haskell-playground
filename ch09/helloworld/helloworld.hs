-- main = putStrLn "Hello, world!"

-- main = do
--   putStrLn "Hello, what's your name?"
--   name <- getLine
--   putStrLn ("Hey " ++ name ++ ", you rock")

-- main = do
--   putStrLn "What's your first name?"
--   firstName <- getLine
--   putStrLn "What's your last name?"
--   lastName <- getLine
--   let bigFirstName = map toUpper firstName
--       bigLastName = map toUpper lastName
--   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how ar you?"

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   putStr "Hey, "
--   putStr "I'm "
--   putStrLn "Andy!"

-- main = do
--   putChar 't'
--   putChar 'e'
--   putChar 'h'
-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x : xs) = do
--   putChar x
--   putStr xs

-- main = do
--   print True
--   print 2
--   print "haha"
--   print 3.2
--   print [3, 4, 3]

import Control.Monad
import Data.Char

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

-- main = do
--   rs <- sequence [getLine, getLine, getLine]
--   print rs

-- ghci> sequence (map print [1,2,3,4,5])
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
-- ghci> mapM print [1,2,3]
-- 1
-- 2
-- 3
-- [(),(),()]
-- ghci> mapM_ print [1,2,3]
-- 1
-- 2
-- 3

-- main = forever $ do
--   putStr "Give me some input:"
--   l <- getLine
--   putStrLn $ map toUpper l

-- main = do
--   colors <-
--     forM
--       [1, 2, 3, 4]
--       ( \a -> do
--           putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--           color <- getLine
--           return color
--       )
--   putStrLn "The colors that you associatewith 1, 2, 3 and 4 are: "
--   mapM putStrLn colors
