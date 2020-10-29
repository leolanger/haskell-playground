import qualified Data.Map as Map

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- ghci> :t Circle
-- Circle :: Float -> Float -> Float -> Shape
-- ghci> :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- ghci> surface $ Circle 10 20 10
-- 314.15927
-- ghci> surface $ Rectangle 0 0 100 100
-- 10000.0

-- ghci> surface (Rectangle (Point 0 0) (Point 100 100))
-- 10000.0
-- ghci> surface (Circle (Point 0 0) 24)
-- 1809.5574

-- ghci> Circle 10 20 5
-- Circle 10.0 20.0 5.0
-- ghci> Rectangle 50 230 60 90
-- Rectangle 50.0 230.0 60.0 90.0

-- ghci> map (Circle 10 20) [4,5,6,6]
-- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- ghci> nudge (Circle (Point 34 34) 10) 5 10
-- Circle (Point 39.0 44.0) 10.0

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- ghci> nudge (baseRect 40 100) 60 23
-- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

-- Recdord Syntax

-- data Person = Person String String Int Float String String deriving (Show)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- ghci> guy
-- Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor
-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- ghci> firstName guy
-- "Buddy"
-- ghci> height guy
-- 184.2
-- ghci> flavor guy
-- "Chocolate"

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- ghci> Car {company="Ford", model="Mustang", year=1967}
-- Car {company = "Ford", model = "Mustang", year = 1967}

-- Type parameters

-- data Maybe a = Nothing | Just a

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Car1 a b c = Car1
  { company1 :: a,
    model1 :: b,
    year1 :: c
  }
  deriving (Show)

tellCar' :: (Show a) => Car1 String String a -> String
tellCar' (Car1 {company1 = c, model1 = m, year1 = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

-- Derived instance

-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      } deriving (Eq, Show, Read)

-- read "Just 't'" :: Maybe Char

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type sysonyms

phoneBook :: [([Char], [Char])]
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- type PhoneBook = [(String, String)]

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- type AssocList k v = [(k, v)]

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

--Recursive data structures

data List a = Empty | Cons a (List a) deriving (Show, Read, Ord, Eq)

-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- ghci> Empty
-- Empty
-- ghci> 5 `Cons` Empty
-- Cons 5 Empty
-- ghci> 4 `Cons` (5 `Cons` Empty)
-- Cons 4 (Cons 5 Empty)
-- ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- Cons 3 (Cons 4 (Cons 5 Empty))

infixr 5 :-:

data Lists a = Emptys | a :-: (Lists a) deriving (Show, Read, Eq, Ord)

--

infixr 5 .++

(.++) :: Lists a -> Lists a -> Lists a
Emptys .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> let b = 6 :-: 7 :-: Empty
-- ghci> a .++ b
-- (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))

--

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Main.singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- ghci> let nums = [8,6,4,1,7,3,5]
-- ghci> let numsTree = foldr treeInsert EmptyTree nums
-- ghci> numsTree
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

-- ghci> 8 `treeElem` numsTree
-- True
-- ghci> 100 `treeElem` numsTree
-- False
-- ghci> 1 `treeElem` numsTree
-- True
-- ghci> 10 `treeElem` numsTree
-- False

--

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- class (Eq a) => Num a where
--    ...

-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

--

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal then yesResult else noResult

-- ghci> yesnoIf [] "YEAH!" "NO!"
-- "NO!"
-- ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
-- "YEAH!"
-- ghci> yesnoIf True "YEAH!" "NO!"
-- "YEAH!"
-- ghci> yesnoIf (Just 500) "YEAH!" "NO!"
-- "YEAH!"
-- ghci> yesnoIf Nothing "YEAH!" "NO!"
-- "NO!"

-- Functor typeclass

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Main.Functor [] where
  fmap = map

-- map :: (a -> b) -> [a] -> [b]
-- ghci> fmap (*2) [1..3]
-- [2,4,6]
-- ghci> map (*2) [1..3]
-- [2,4,6]

instance Main.Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

-- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
-- Just "Something serious. HEY GUYS IM INSIDE THE JUST"
-- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
-- Nothing
-- ghci> fmap (*2) (Just 200)
-- Just 400
-- ghci> fmap (*2) Nothing
-- Nothing

instance Main.Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (Main.fmap f leftsub) (Main.fmap f rightsub)

-- ghci> fmap (*2) EmptyTree
-- EmptyTree
-- ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
-- Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree

instance Main.Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

-- Kind

-- ghci> :k Int
-- Int :: *

-- ghci> :k Maybe
-- Maybe :: * -> *
-- ghci> :k Maybe Int
-- Maybe Int :: *

-- ghci> :k Either
-- Either :: * -> * -> *
-- ghci> :k Either String
-- Either String :: * -> *
-- ghci> :k Either String Int
-- Either String Int :: *

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

-- ghci> :t Frank {frankField = Just "HAHA"}
-- Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
-- ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
-- Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
-- ghci> :t Frank {frankField = "YES"}
-- Frank {frankField = "YES"} :: Frank Char []

instance Tofu Frank where
  tofu x = Frank x

-- ghci> tofu (Just 'a') :: Frank Char Maybe
-- Frank {frankField = Just 'a'}
-- ghci> tofu ["HELLO"] :: Frank [Char] []
-- Frank {frankField = ["HELLO"]}

data Barry t k p = Barry {yabba :: p, dabba :: t k}

-- ghci> :k Barry
-- Barry :: (* -> *) -> * -> * -> *

instance Main.Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}