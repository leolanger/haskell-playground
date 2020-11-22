-- there are cases where the principal type cannot be inferred

{-Mutually Recursive BInding Groups-}

-- Inferred types
f :: Char -> Char
f x = const x g

g :: p -> Char
g y = f 'A'

-- Most general types
-- f :: a -> a
-- g :: a -> Char

{-Polymorphic recursion-}

-- the inferred type variable a in size spans two possible types ( a and (a,a) )
data Tree a = Leaf | Bin a (Tree (a, a))

size :: (Num p) => Tree (a, a) -> p
size Leaf = 0
size (Bin _ t) = 1 + 2 * size t

size' :: Tree a -> Int
size' Leaf = 0
size' (Bin _ t) = 1 + 2 * size t

{-Monomorphism Restriction-}

-- types inferred for functions without explicit type signatures
-- may be more specific than expected.

-- λ : :set +t
-- λ : 3
-- 3
-- it :: Num a => a
-- λ : default (Double)
-- λ : 3
-- 3.0
-- it :: Num a => a
