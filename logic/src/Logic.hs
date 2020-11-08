module Logic
  ( someFunc,
    (\/),
    (/\),
    (-|),
    (.->),
    (<->),
    doTruthTable2,
    doTruthTable3,
    doConjunctiveParadigm2,
  )
where

import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

infixl 4 \/

(\/) :: Bool -> Bool -> Bool
x \/ y
  | x == False && y == False = False
  | otherwise = True

infixl 4 /\

(/\) :: Bool -> Bool -> Bool
x /\ y
  | x == True && y == True = True
  | otherwise = False

infixr 5 -|

(-|) :: Bool -> Bool
(-|) x = not x

infixl 2 <->

(<->) :: Bool -> Bool -> Bool
x <-> y
  | x == y = True
  | otherwise = False

infixl 3 .->

(.->) :: Bool -> Bool -> Bool
x .-> y
  | x == True && y == False = False
  | otherwise = True

doTruthTable2 :: (Bool -> Bool -> Bool) -> [(Bool, Bool)]
doTruthTable2 f = do
  p <- [True, False]
  q <- [True, False]
  guard (f p q == True)
  return (p, q)

doTruthTable3 :: (Bool -> Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
doTruthTable3 f = do
  p <- [True, False]
  q <- [True, False]
  r <- [True, False]
  guard (f p q r == True)
  return (p, q, r)

doConjunctiveParadigm2 :: (Bool -> Bool -> Bool) -> [[Char]]
doConjunctiveParadigm2 f =
  map funct (doTruthTable2 f)
  where
    funct (p, q)
      | p == True && q == True = ['p', '\\', '/', 'q']
      | p == True && q == False = ['p', '\\', '/', '-', '|', 'q']
      | p == False && q == True = ['-', '|', 'p', '\\', '/', 'q']
      | otherwise = ['-', '|', 'p', '\\', '/', '-', '|', 'q']