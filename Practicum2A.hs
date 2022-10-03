{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           Jack Heuberger
VU-net id:      jhe309
Student number: 2779029
Discussed with: N/A
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        Wikipedia algorithm for Towers of Hanoi
-}

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = 1 : map (+1) naturals

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = 1 : 0 : zeroesandones

-- Exercise 3
threefolds :: [Integer]
threefolds = map (*3) (0 : naturals)

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif f (h:t) = if (f h) then (removeif f t) else (h : removeif f t)

nothreefolds :: [Integer]
nothreefolds = removeif (\x -> mod x 3 == 0) (0 : naturals)

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = map (*n) (0:naturals)

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n = removeif (\x -> mod x n == 0) (0 : naturals)

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n l = removeif (\x -> mod x n == 0) l

-- Exercise 8
natsaftertwo :: [Integer]
natsaftertwo = 2 : map (+1) natsaftertwo
primehelper :: [Integer] -> [Integer]
primehelper (h:t) = h : (primehelper (filter (\y -> mod y h /= 0) t))
eratosthenes :: [Integer]
eratosthenes = primehelper natsaftertwo

-- Exercise 9
fibonacci :: [Integer]
-- add each element in fibonacci to the last element of fibonacci, which happens infinitely
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0

{- Show usage here -}
-- churchnumeral creates a church numeral for a given integer
-- backtointeger turns a church numberal into its corresponding integer representation
-- examples:
  -- backtointeger (churchnumeral 5) == 5

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s ( s z )

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s z = x (y s) z

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger (f (churchnumeral m) (churchnumeral n))


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes Leaf = 0
numberofnodes (Node l _ r) = 1 + numberofnodes l + numberofnodes r

-- Exercise 2
height :: BinaryTree a -> Integer
height Leaf = 1
height (Node l _ r) = 1 + max (height l) (height r)

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes Leaf = 0
sumnodes (Node l a r) = a + sumnodes l + sumnodes r

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node l a r) = Node r a l

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten Leaf = []
flatten (Node l a r) = flatten l ++ [a] ++ flatten r

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f Leaf = Leaf
treemap f (Node l a r) = Node (treemap f l) (f a) (treemap f r)

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
-- true if x is the largest element
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan x Leaf = True
smallerthan x (Node l a r) = a < x && smallerthan x r

-- true if x is the smallest element
largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan x Leaf = True
largerthan x (Node l a r) = a > x && largerthan x l

-- Exercise 2
-- recursive calls, then make sure all elements in left subtree are smaller than current, and all elements in right subtree are greater than current
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree Leaf = True
isbinarysearchtree (Node l a r) = isbinarysearchtree l && isbinarysearchtree r && smallerthan a l && largerthan a r

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement x Leaf = False
iselement x (Node l a r) = x == a || iselement x l || iselement x r

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l a r) 
  | x == a = Node l a r
  | x > a = Node l a (insert x r)
  | x < a = Node (insert x l) a r

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree [] = Leaf
createbinarysearchtree (h:t) = Node (createbinarysearchtree (filter (< h) t)) h (createbinarysearchtree (filter (> h) t))

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove _ Leaf = Leaf
-- recursive calls until x == a
remove x (Node l a r)
  | x < a = Node (remove x l) a r
  | x > a = Node l a (remove x r)
-- handle cases with no or 1 child
remove _ (Node Leaf _ Leaf) = Leaf
remove _ (Node l _ Leaf) = l 
remove _ (Node Leaf _ r) = r
-- delete root node
remove x (Node l a r) = delete x (Node l a r)

delete :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
-- find minimum child in right subtree, bring that value to current node & delete that node
delete _ (Node l _ r) = let minr = minimum (flatten r); rnew = remove minr r in Node l minr rnew

----------------------------
-- Exercise Tower of Hanoi
----------------------------

-- comment out the following two lines in case you take a different approach
-- and in that case adapt the type of the function hanoi accordingly
type Rod = String
-- number of disks, from rod, to rod
type Move = (Integer, Rod, Rod)
-- num disks -> three rods
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi 1 src aux dest = [(1, src, dest)]
hanoi num src aux dest = hanoi (num-1) src dest aux ++ hanoi 1 src aux dest ++ hanoi (num-1) aux src dest
