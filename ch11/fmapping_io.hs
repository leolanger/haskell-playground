import Data.Char
import Data.List

-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- main = do
--   line <- getLine
--   let line' = reverse line
--   putStrLn $ "You said " ++ line' ++ " backwards!"
--   putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- main = do
--   line <- fmap reverse getLine
--   putStrLn $ "You said " ++ line ++ " backwards!"
--   putStrLn $ "Yes, you really said " ++ " backwards!"

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

-- instance Functor ((->) r) where
--   fmap f g = (\x -> f (g x))

-- instance Functor ((->) r) where
--     fmap = (.)

-- ghci> :t fmap (*3) (+100)
-- fmap (*3) (+100) :: (Num a) => a -> a
-- ghci> fmap (*3) (+100) 1
-- 303
-- ghci> (*3) `fmap` (+100) $ 1
-- 303
-- ghci> (*3) . (+100) $ 1
-- 303
-- ghci> fmap (show . (*3)) (*100) 1
-- "300"

-- ghci> :t fmap (*2)
-- fmap (*2) :: (Num a, Functor f) => f a -> f a
-- ghci> :t fmap (replicate 3)
-- fmap (replicate 3) :: (Functor f) => f a -> f [a]
