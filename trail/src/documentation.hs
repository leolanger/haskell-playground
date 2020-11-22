-- | Documentation for f
f :: a -> a
f = ...

-- | Multiline documentation for the function
-- f with multiple arguments.
fmap :: Functor f
    => (a -> b) -- ^ function
    -> f a -- ^ input
    -> f b -- ^ output

data T a b
    = A a -- ^ Documentation for A
    | B b -- ^ Documentation for B
data R a b = R
    { f1 :: a -- ^ Documentation for the field f1
    , f2 :: b -- ^ Documentation for the field f2
    }

data T a b
    = A a -- ^ Documentation for 'A'
    | B b -- ^ Documentation for 'B'

-- | Here we use the "Data.Text" library and import
-- the 'Data.Text.pack' function.

-- | An example of a code block.
--
-- @
--      f x = f (f x)
-- @

-- | A similar code block example that uses bird tracks (i.e. '>')
-- > f x = f (f x)

-- | Example of an interactive shell session embedded within documentation
--
-- >>> factorial 5
-- 120

<url https://github.com/>