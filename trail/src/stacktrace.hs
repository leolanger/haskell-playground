import Control.Exception

{-Stack Traces-}

f :: Show a1 => a1 -> a2
f x = g x

g :: Show a1 => a1 -> a2
g x = error (show x)

main :: IO (Either SomeException ())
main = try (evaluate (f ())) :: IO (Either SomeException ())

-- ghc -O0 -rtsopts=all -prof -auto-all --make stacktrace.hs
-- ./stacktrace +RTS -xc

-- *** Exception (reporting due to +RTS -xc): (THUNK_2_0), stack trace:

--   Main.g,
--   called from Main.f,
--   called from Main.main,
--   called from Main.CAF
--   --> evaluated by: Main.main

{-Printf Tracing-}
