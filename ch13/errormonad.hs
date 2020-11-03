import Control.Monad.Error

-- instance (Error e) => Monad (Either e) where
--     return x = Right x
--     Right x >>= f = f x
--     Left err >>= f = Left err
--     fail msg = Left (strMsg msg)

-- ghci> Left "boom" >>= \x -> return (x+1)
-- Left "boom"
-- ghci> Right 100 >>= \x -> Left "no way!"
-- Left "no way!"