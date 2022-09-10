
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
                https://wiki.haskell.org/How_to_work_on_lists#Finding_.2F_searching
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi m n = if m >= n then m else n

--maxi 3 2 == 3
--maxi 2 3 == 3

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending a b c d = a < b && b < c && c < d

-- fourAscending 1 2 3 4 == True
-- fourAscending 1 2 4 3 == False
-- fourAscending 1 1 1 1 == False

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

-- fourEqual 1 1 1 1 == True
-- fourEqual 0 1 2 3 == False

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = a /= b && a /= c && a /= d && b /= c && b /= d && c /= d

-- fourDifferent 1 2 3 4 == True
-- fourDifferent 1 2 3 1 == False

-- Exercise 5
{-
   threeDifferent 1 2 1
   -- The function does not check to see that a and c are not equal, only that a/b and b/c are not equal
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- factorial 5 == 120
-- factorial 10 == 3628800

-- Exercise 7
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- fib 2 == 1
-- fib 6 == 8

-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
strangeSummation n = (8 * n) + 28

-- strangeSummation 50 == 428
-- strangeSummation 5 = 68

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
sumList [] = 0
sumList (h:t) = h + sumList t

-- sumList [1,2,3] == 6
-- sumList [] == 0
-- sumList [1,1,1,1,] == 4

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (h:t) = 2*h : doubleList t

-- doubleList [1,2,3] == [2,4,6]
-- doubleList [] = []
-- doubleList [-1, 0, 23] == [-2, 0, 46]

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend [] [] = []
myappend (h:t) [] = h:t
myappend [] (h:t) = h:t
myappend (h:t) (i:u) = h : myappend t (i:u)

-- myappend [1,2,3] [4,5,6] = [1,2,3,4,5,6]
-- myappend [] [1,2,3] = [1,2,3]
-- myappend [1,2,3] [] = [1,2,3]

-- Exercise 12
-- NOTE: use myappend 
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myappend (myreverse t) [h]

-- myreverse [1,2,3] == [3,2,1]
-- myreverse [] == []
-- myreverse [0] == [0]

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember a [] = False
mymember a (h:t) = (a == h) || mymember a t

-- mymember 1 [] == False
-- mymember 1 [2,3,4] == False
-- mymember 1 [3,2,1] == True

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum [] = 0
mysquaresum (h:t) = (h * h) + mysquaresum t

-- mysquaresum [5,4,3] == 50
-- mysquaresum [1] == 1
-- mysquaresum [] == 0

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range a b = if a > b then [] else (if a == b then [b] else a : range (a+1) b)

-- range 1 5 == [1,2,3,4,5]
-- range 1 1 == [1]
-- range 3 1 == []

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

-- myconcat [[1,2,3],[4,5,6]] == [1,2,3,4,5,6]
-- myconcat [] == []

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if x > h then h : insert x t else x : h : t

-- insert 3 [1,2,4] == [1,2,3,4]
-- insert 3 [4] == [3,4]
-- insert 5 [] = [5]

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (h:t) = insert h (insertionsort t) 

-- insertionsort [4,3,1,5] == [1,3,4,5]
-- insertionsort [1,4,-5] == [-5,1,4]
-- insertionsort [5] == [5]

-- Exercise 18
minim :: Ord a => [a] -> a
minim x = head (insertionsort x)

removeFirstOccurrence :: Eq t => t -> [t] -> [t]
removeFirstOccurrence x (h:t) = if x == h then t else h : removeFirstOccurrence x t

selectionsort :: Ord a => [a] -> [a]
selectionsort a = selectionsort (removeFirstOccurrence x a) where x = minim a

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

