data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

-- 改变节点值

-- changeToP :: Tree Char -> Tree Char
-- changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

-- a better method

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L : ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R : ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

{-breaedCrumbs-}

type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> Maybe (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = Just (l, L : bs)
goLeft (Empty, _) = Nothing

goRight :: (Tree a, Breadcrumbs) -> Maybe (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = Just (r, R : bs)
goRight (Empty, _) = Nothing

-- eg.
-- ghci> goLeft (goRight (freeTree, []))
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- ghci> (freeTree, []) -: goRight -: goLeft
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

-- Going back up

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> Maybe (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft' (Empty, _) = Nothing

goRight' :: (Tree a, Breadcrumbs' a) -> Maybe (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight' (Empty, _) = Nothing

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r : bs) = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs' a)

-- manipulating trees under focus

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- eg.
-- ghci> let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[])))

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- back to the topMost

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

{-list-}

data List a = Empty' | Cons a (List a) deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x : xs, bs) = Just (xs, x : bs)
goForward ([], _) = Nothing

goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (xs, b : bs) = Just (b : xs, bs)
goBack (_, []) = Nothing