module Main where

import Logic (doConjunctiveParadigm2, doTruthTable2, doTruthTable3, (-|), (.->), (/\), (\/))

main :: IO ()
main = do
  print [(p, q) | p <- [True, False], q <- [True, False], (-|) (p /\ q .-> q) == True]
  print [(p, q) | p <- [True, False], q <- [True, False], ((-|) p .-> q) .-> ((-|) q \/ p) == True]
  print $ doTruthTable2 (\p q -> (((-|) (q .-> (-|) p) /\ (-|) p) == False))
  print $ doTruthTable3 (\p q r -> ((p .-> (p \/ q)) \/ r) == False)
  print $ doConjunctiveParadigm2 (\p q -> (((((-|) p .-> q) .-> ((-|) q \/ p) == True))))