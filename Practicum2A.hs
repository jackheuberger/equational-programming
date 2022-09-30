{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           Jack Heuberger
VU-net id:      jhe309
Student number: 2779029
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
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
numberofnodes = undefined

-- Exercise 2
height :: BinaryTree a -> Integer
height = undefined

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes = undefined

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror = undefined

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten = undefined

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap = undefined

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan = undefined

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan = undefined

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree = undefined

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement = undefined

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert = undefined

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree = undefined

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove = undefined


----------------------------
-- Exercise Tower of Hanoi
----------------------------

-- comment out the following two lines in case you take a different approach
-- and in that case adapt the type of the function hanoi accordingly
type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi = undefined

