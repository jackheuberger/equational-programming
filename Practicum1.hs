
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
minim [] = undefined
minim x = head (insertionsort x)

-- minim [5,3,1] == 1
-- minim [] == undefined
-- minim [1] == 1

removeFirstOccurrence :: Eq t => t -> [t] -> [t]
removeFirstOccurrence x [] = []
removeFirstOccurrence x (h:t) = if x == h then t else h : removeFirstOccurrence x t

-- removeFirstOccurrence 4 [5,4,4,1] == [5,4,1]
-- removeFirstOccurrence 4 [5,4,3,4,1] == [5,3,4,1]
-- removeFirstOccurrence 1 [1] == []

selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort a = x : selectionsort (removeFirstOccurrence x a) where x = minim a

-- selectionsort [3,2,1] == [1,2,3]
-- selectionsort [0] == [0]
-- selectionsort [5, -5] == [-5, 5]

-- Exercise 19
gt :: Ord a => a -> [a] -> [a]
gt x [] = []
gt x arry = filter (>x) arry

-- gt 3 [1,2,3] == []
-- gt 5 [4,5,6] == [6]
-- gt 1 [2,3,4] == [2,3,4]

lteq :: Ord a => a -> [a] -> [a]
lteq x [] = []
lteq x arry = filter (<=x) arry

-- lteq 5 [3,4,5] == [3,4,5]
-- lteq 6 [8,7,9] == []

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (h:t) = quicksort (lteq h t) ++ [h] ++ quicksort (gt h t)

-- quicksort [3,2,1] == [1,2,3]
-- quicksort [0] == [0]
-- quicksort [-3, -4, -5, -3] == [-5, -4, -3, -3]

-- Exercise 20
evensB :: [Integer] -> [Integer]
evensB arry = [x | x <- arry, even x]

-- evensB [1,2,3] == [2]
-- evensB [3,1,5] == []
-- evensB [] == []

-- Exercise 22
mymap :: (a -> b) -> [a] -> [b]
mymap func [] = []
mymap func (h:t) = func h : mymap func t

-- mymap (\x -> x + 1) [1,2,3] == [2,3,4]
-- mymap (even) [1,2,3,4] == [False, True, False, True]
-- mymap (\x -> x + 1) [] == []

-- Exercise 23
twice :: (a -> a) -> a -> a
twice func a = func (func a)

-- twice (\x -> x + 1) 1 == 3
-- twice (\x -> x^2) 5 == 625

-- Exercise 24
compose :: (b -> c) -> (a -> b) -> a -> c
compose a b x = a (b (x))

-- compose (\x -> x+1) (\x -> x^2) 3 == 10
-- compose (\x -> x^2) (\x -> x+1) 3 == 16

-- Exercise 25
mylast :: [a] -> a
mylast a = head (reverse a)

-- mylast [] -> Exception: Prelude.head: empty list
-- mylast [2,3,1] == [1]
-- mylast [[1,2,5],[58],[2,1,5,8,0]] == [2,1,5,8,0]

-- Exercise 26
mylastb :: [a] -> a
mylastb a = head (drop (length a - 1) a)

-- mylastb [] -> Exception: Prelude.head: empty list
-- mylastb [2,3,1] == [1]
-- mylastb [[1,2,5],[58],[2,1,5,8,0]] == [2,1,5,8,0]

-- Exercise 27
myinit, myinitb :: [a] -> [a]
myinit a = take (length a -1) a
myinitb a = reverse (tail (reverse a))

-- Tests hold for both functions
-- myinit [1,2,3] == [1,2]
-- myinit [1] == []

-- Exercise 28
mysecondconcat :: [[a]] -> [a]
mysecondconcat a = foldr (++) [] a

-- mysecondconcat [[]] == []
-- mysecondconcat [[1,2,3],[4,5,6]] == [1,2,3,4,5,6]

-- Given an element and an array, append the element to the end of the array
toend :: a -> [a] -> [a]
toend a b = b ++ [a]

-- toend 3 [1,2] == [1,2,3]
-- toend 1 [] == [1]

-- Reversing = sending each element to the end of the array from right to left
mysecondreverse :: [a] -> [a]
mysecondreverse a = foldr toend [] a

-- mysecondreverse [] == []
-- mysecondreverse [3,2,1] == [1,2,3]

-- Exercise 28
mythirdconcat :: [[a]] -> [a]
mythirdconcat a = foldl (++) [] a

-- mysecondconcat [[]] == []
-- mysecondconcat [[1,2,3],[4,5,6]] == [1,2,3,4,5,6]

tofront :: [a] -> a -> [a]
tofront a b = b : a

-- tofront [2,3] 1 == [1,2,3]
-- tofront [] 1 == [1]

-- Reversing = sending each element to the front of the array from left to right
mythirdreverse :: [a] -> [a]
mythirdreverse a = foldl tofront [] a

-- mythirdreverse [] == []
-- mmythirdreverse [3,2,1] == [1,2,3]

-- Exercise 29

prefix :: [a] -> [[a]]
prefix [] = [[]]
prefix (h:t) = [] : map (h:) (prefix t)

-- [] : prefix t -- leaves out first element from all permutations
-- Need to append the first element back to everything -- using map

-- prefix [1,2,3] == [[],[1],[1,2],[1,2,3]]
-- prefix [] == [[]]
-- prefix "jack" == ["","j","ja","jac","jack"]
