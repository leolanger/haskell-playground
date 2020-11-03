import Control.Monad.State (MonadState (get, put, state), State)
import System.Random

main = return ()

{-Stack and Stones-}

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackMainp :: Stack -> (Int, Stack)
stackMainp stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
   in pop newStack2

-- same

-- stackManip = do
--     push 3
--     a <- pop
--     pop

-- λ stackMainp [5,8,2,1]
-- (5,[8,2,1])

{-State Monad-}

-- newtype State s a = State {runState :: s -> (a, s)}

-- instance Monad (State s) where
--   return x = State $ \s -> (x, s)
-- (>>=) :: State s a -> (a -> State s b) -> State s b
--   (State h) >>= f = State $ \s ->
--     let (a, newState) = h s
--         (State g) = f a
--      in g newState

pop' :: State Stack Int
pop' = state $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a : xs)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  a <- pop'
  pop'

stackStuff :: State Stack ()
stackStuff = do
  a <- pop'
  if a == 5
    then push' 5
    else do
      push' 3
      push' 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip'
  if a == 100
    then stackStuff
    else return ()

-- MonadState

-- get = State $ \s -> (s,s)
-- put newState = State $ \s -> ((),newState)

stayStack :: State Stack ()
stayStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 3, 1]
    else put [9, 2, 1]

{-random and state monad-}

-- random :: (RandomGen g, Random a) => g -> (a, g)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)
