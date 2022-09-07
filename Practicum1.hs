
module Practicum1 where

{-
Name:           Jack Heuberger
VU-net id:      vu\jhe309
Student number: 2779029
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        Resources on Canvas
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi = undefined

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending = undefined

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual = undefined

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent = undefined

-- Exercise 5
{-
   <Paste your answer here>
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial = undefined

-- Exercise 7
fib :: Integer -> Integer
fib = undefined

-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
strangeSummation = undefined

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList = undefined

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList = undefined

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend = undefined

-- Exercise 12
myreverse :: [a] -> [a]
myreverse = undefined

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember = undefined

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum = undefined

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range = undefined

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat = undefined

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert = undefined

insertionsort :: Ord a => [a] -> [a]
insertionsort = undefined

-- Exercise 18
minim :: Ord a => [a] -> a
minim = undefined

removeFirstOccurrence :: Eq t => t -> [t] -> [t]
removeFirstOccurrence = undefined

selectionsort :: Ord a => [a] -> [a]
selectionsort = undefined

-- Exercise 19
quicksort :: Ord a => [a] -> [a]
quicksort = undefined

-- Exercise 20
evensB :: [Integer] -> [Integer]
evensB = undefined

-- Exercise 22
mymap :: (a -> b) -> [a] -> [b]
mymap = undefined

-- Exercise 23
twice :: (a -> a) -> a -> a
twice = undefined

-- Exercise 24
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- Exercise 25
mylast :: [a] -> a
mylast = undefined

-- Exercise 26
mylastb :: [a] -> a
mylastb = undefined

-- Exercise 27
myinit, myinitb :: [a] -> [a]
myinit = undefined
myinitb = undefined

-- Exercise 28
mysecondconcat :: [[a]] -> [a]
mysecondconcat = undefined

mysecondreverse :: [a] -> [a]
mysecondreverse = undefined

-- Exercise 28
mythirdconcat :: [[a]] -> [a]
mythirdconcat = undefined

mythirdreverse :: [a] -> [a]
mythirdreverse = undefined


-- Exercise 29

prefix :: [a] -> [[a]]
prefix = undefined

