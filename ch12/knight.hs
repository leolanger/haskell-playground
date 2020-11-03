import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c -2, r -1),
      (c -2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c -1, r -2),
      (c -1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight (c,r) = filter onBoard
--     [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
--     ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
--     ]
--     where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- ghci> moveKnight (6,2)
-- [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
-- ghci> moveKnight (8,1)
-- [(6,2),(7,3)]

in3 :: KnightPos -> [KnightPos]
-- in3 start = do
--   first <- moveKnight start
--   second <- moveKnight first
--   moveKnight second
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- ghci> (6,2) `canReachIn3` (6,1)
-- True