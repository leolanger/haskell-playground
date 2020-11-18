import Debug.Trace
import Text.Printf

example1 :: Int
example1 = trace "impure print" 1

example2 :: Int
example2 = traceShow "tracing" 2

example3 :: [Int]
example3 = [trace "will not be called" 3]

main :: IO ()
main = do
  print example1
  print example2
  print $ length example3

-- impure print
-- 1
-- "tracing"
-- 2
-- 1

traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = Main.traceM . show

tracePrintfM :: (Monad m, PrintfArg a) => String -> a -> m ()
tracePrintfM s = Main.traceM . printf s
