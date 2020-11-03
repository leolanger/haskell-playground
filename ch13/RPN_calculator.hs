import Control.Monad
import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x : y : ys) "*" = (x * y) : ys
foldingFunction (x : y : ys) "+" = (x + y) : ys
foldingFunction (x : y : ys) "-" = (y - x) : ys
foldingFunction xs numberString = read numberString : xs

-- safer

solveRPN' :: String -> Maybe Double
solveRPN' st = do
  [result] <- foldM foldingFunction' [] (words st)
  return result

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction' (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction' (x : y : ys) "-" = return ((y - x) : ys)
foldingFunction' xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing