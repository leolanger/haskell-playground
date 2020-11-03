type Birds = Int

type Pole = (Birds, Birds)

-- landLeft :: Birds -> Pole -> Pole
-- landLeft n (left,right) = (left + n,right)
-- landRight :: Birds -> Pole -> Pole
-- landRight n (left,right) = (left,right + n)
-- (-:) :: t1 -> (t1 -> t2) -> t2
-- x -: f = f x
-- ghci > (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2
-- (3, 1)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- ghci> landLeft 10 (0,3)
-- Nothing
-- ghci > landRight 1 (0, 0) >>= landLeft 2
-- Just (2, 1)
-- ghci> Nothing >>= landLeft 2
-- Nothing
-- ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
-- Just (2,4)

(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n

-- ghci> Nothing >> Just 3
-- Nothing
-- ghci> Just 3 >> Just 4
-- Just 4
-- ghci> Just 3 >> Nothing
-- Nothing
-- ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- Nothing

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

-- ghci> routine
-- Just (3,2)

routine' :: Maybe Pole
routine' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second