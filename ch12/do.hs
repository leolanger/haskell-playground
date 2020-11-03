-- ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"

-- foo :: Maybe String
-- foo =
--   Just 3
--     >>= ( \x ->
--             Just "!"
--               >>= ( \y ->
--                       Just (show x ++ y)
--                   )
--         )

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- ghci> Just 9 >>= (\x -> Just (x > 8))
-- Just True
marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

justH :: Maybe Char
justH = do
  (x : xs) <- Just "hello"
  return x

-- ghci> Just "hello" >>= (\(x:xs) -> return x)
-- Just 'h'

fail :: (Monad m) => String -> m a
fail msg = error msg

wopwop :: Maybe Char
wopwop = do
  (x : xs) <- Just ""
  return x

-- ghci> wopwop
-- Nothing