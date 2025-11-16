module Practicum1 where

{-
Name:           <Daniela Mackay>
VU-net id:      <dmt380>
Student number: <2663452>
Discussed with: 
Remarks:       <I'm really sorry I handed in late, I hope it can still be graded.> 
Sources:        <https://wiki.haskell.org/How_to_work_on_lists>
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi  x y | (x >= y) = x
          | (x < y) = y

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending w x y z = (w < x) && (x < y) && (y < z) 

{- 
fourAscending 1 2 3 0
False
fourAscending 1 2 3 4
True
fourAscending 0 0 3 4
False
-}

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual w x y z = (w == x) && (w == y) && (w == z)

{-
fourEqual 1 1 1 1
True
fourEqual 1 1 1 2
False
fourEqual 1 1 2 2
False
fourEqual 1 2 1 1
False
-}

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z = (w /= x) && (w /= y) && (w /= z) && (x /= y) && (x /= z) && (y /= z)

{-
fourDifferent 1 2 3 4
True
fourDifferent 1 2 3 1
False
fourDifferent 1 1 3 4
False
fourDifferent 1 2 1 4
False
fourDifferent 0 2 1 1
-}

-- Exercise 5
{-
  threeDifferent 1 2 1
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial x |(x == 0) = 1
            |(x /= 0) = x * factorial (x - 1)


{- Examples:
factorial 4
24
factorial 0
1
-}

-- Exercise 7
fib :: Integer -> Integer
fib n |(n == 0) = 0
      |(n == 1) = 1
      |(n > 1)  = fib(n-1) + fib(n-2)

{- Examples
fib 0
0
fib 1
1
fib 8
21
-}

-- Exercise 8
-- it is possible to define auxiliary functions

summation :: Integer -> Integer -> Integer
summation start end |(start == end) = end
                    |(start < end) = start + summation (start + 1) end

strangeSummation :: Integer -> Integer
strangeSummation n = summation n (n+7)

{- Examples
summation 1 8
36
strangeSummation 1
36
strangeSummation 5
68
-}

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
sumList x |(lengthList x == 0) = 0
          |(lengthList x > 0) = head x + sumList (tail x)

{- Examples
sumList [1, 2, 4]
7
sumList []
0
sumList [0, 3, -8]
-}

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList x = map (*2) x


{- Examples
doubleList []
[]
doubleList [1, 5, 7, -2]
[2,10,14,-4]
-}

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend a b  | (length a == 0)   = b
              | (length a > 0)    = (head a):(myappend (tail a) b)

{- Examples
myappend [4, 6] [1, 2]
[4,6,1,2]
myappend [4, 6] []
[4,6]
myappend [] [1, 2]
[1,2]
myappend [True, False] [True]
[True,False,True]
-}

-- Exercise 12
myreverse :: [a] -> [a]
myreverse x | (length x == 0)   = []
            | (length x > 0)    = (last x):(myreverse (init x))


{- Examples
myreverse []
[]
myreverse [1, 2, 3]
[3,2,1]
myreverse [False, True, True]
[True,True,False]
-}

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember x y = any (==x) y

{- Examples
mymember "hi" ["hi", "it's me","yes"]
True
mymember 7 []
False
mymember 7 [1,7,8]
True
mymember 7 [1,8]
False
-}

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum x = sum (map (^2) x)

{- Examples
mysquaresum [9,1]
82
mysquaresum []
0
-}

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range a b |(a > b) = []
          |(a <= b) = a:(range (a+1) b)

{- Examples
range 2 7
[2,3,4,5,6,7]
range 2 1
[]
-}

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat a|(length a == 0)  = []
          |(length a > 0)   = (head a ++ (myconcat (tail a)))

{- Examples
myconcat [[1,2],[7]]
[1,2,7]
myconcat [[1,2],[7], []]
[1,2,7]
myconcat [[1,2],[7], [8, 9 -1]]
[1,2,7,8,8]
myconcat [[True, False], [False], [], [True, True]]
[True,False,False,True,True]
-}

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x list = (filter (<x) list) ++ [x] ++ (filter (>=x) list)

{- Examples
insert 3 [1, 2, 3, 4, 5, 6]
[1,2,3,3,4,5,6]
insert 3 [1, 2, 5, 6]
[1,2,3,5,6]
insert 3 []
[3]
-}

insertionsort :: Ord a => [a] -> [a]
insertionsort x |(length x == 0)  = []
                |(length x > 0)   = insert (last x) (insertionsort (init x))


{- Examples
insertionsort []
[]
insertionsort [1, 5, 3, 9, 0, 2]
[0,1,2,3,5,9]
insertionsort [1,- 5, 3, 9, 0, -2]
[-5,-2,0,1,3,9]
-}

-- Exercise 18
minim :: Ord a => [a] -> a
minim x |(length x == 1)  = x!!0
        |(length x > 1)   = mini (head x) (minim (tail x))

mini :: Ord a => a -> a -> a
mini a b |(a >= b)  = b
         |(a < b)   = a

{- Examples
minim [1, 2, 3, 4]
1
minim [9,1, 2, 3, 4]
1
-}

removeFirstOccurrence :: Eq t => t -> [t] -> [t]
removeFirstOccurrence x list  |(list == []) = []
                              |(x == (head list))  = tail list
                              |(x /= (head list))  = [head list] ++ removeFirstOccurrence x (tail list)


{- Examples
removeFirstOccurrence 3 [1, 6, 4, 3, 5, 3, 2, 6, 3, 3]
[1,6,4,5,3,2,6,3,3]
removeFirstOccurrence 3 []
[]
-}

selectionsort :: Ord a => [a] -> [a]
selectionsort x |(length x == 0)  = []
                |(length x > 0)   = [minim x] ++ selectionsort (removeFirstOccurrence (minim x) x) 

{- Examples
selectionsort []
[]
selectionsort [-4, 5, 20, 0]
[-4,0,5,20]
selectionsort [2, 1, 7, 8, 4]
[1,2,4,7,8]
-}

-- Exercise 19
quicksort :: Ord a => [a] -> [a]
quicksort x |(length x == 0)  = []
            |(length x > 0)   = quicksort (filter (< (head x)) (tail x)) ++ [head x] ++ quicksort (filter (>= (head x)) (tail x))

{- Examples
quicksort ['a', 'b', 'b']
"abb"
quicksort ['a', 'b', 'b', 'a', 'c', 'b']
"aabbbc"
quicksort [-3, 0, 2, 2, 2, 1]
[-3,0,1,2,2,2]
quicksort []
[]
-}

-- Exercise 20
evensB :: [Integer] -> [Integer]
evensB xs = [x | x <- xs, mod x 2 == 0]

{- Examples
evensB [1, 2, 3, 4, 5, 6, 7, 0, -3, -4]
[2,4,6,0,-4]
evensB []
[]
evensB [1, 1, 1]
[]
-}

-- Exercise 21
mymap :: (a -> b) -> [a] -> [b]
mymap func xs |(length xs == 0) = []
              |(length xs > 0)  = [func (head xs)] ++ mymap (func) (tail xs)

{- Examples
mymap (*3) [1, 4, 6]
[3,12,18]
mymap (\x -> x+1) [1,2,3]
[2,3,4]
mymap (\x -> x+1) []
[]
-}

-- Exercise 22
twice :: (a -> a) -> a -> a
twice func b = func (func b)

{- Examples
twice (\x -> x+1) 3
5
twice (*2) 5
20
-}

-- Exercise 23
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x) 

{- Examples
compose (*2) (+7) 3
20
compose (*2) (\x -> x^2) 3
18
-}

-- Exercise 24
mylast :: [a] -> a
mylast xs = head (reverse xs)

{- Examples
mylast [1, 2, 3]
3
mylast [17, 0, 1, 2]
2
-}

-- Exercise 25
mylastb :: [a] -> a
mylastb xs = head (drop (length xs - 1) xs) 

{- Examples
mylastb [1, 2, 4, 5]
5
mylastb [1, 2]
2
-}

-- Exercise 26
myinit, myinitb :: [a] -> [a]
myinit xs = reverse (tail (reverse xs))

{- Examples
myinit [1, 2, 3, 4]
[1,2,3]
myinit ['a', 'c', 'c', 'b']
"acc"
-}

myinitb xs = take (length xs - 1) xs 

{- Examples
myinitb ['a', 'c', 'c', 'b']
"acc"
myinitb [1, 2, 3, 4]
[1,2,3]
-}

-- Exercise 27
mysecondconcat :: [[a]] -> [a]
mysecondconcat xss = foldr (++) [] xss 

{- Examples
mysecondconcat [['a', 'b'],['s'],['q','w','p']]
"absqwp"
mysecondconcat [[1, 2],[4],[3, 7, 8]]
[1,2,4,3,7,8]
mysecondconcat [["hi"], ["nice to meet you", "have a nice day"]]
["hi","nice to meet you","have a nice day"]
-}

mysecondreverse :: [a] -> [a]
mysecondreverse xs = foldr (\x y -> y ++ [x]) [] xs

{- Examples
mysecondreverse [1, 2, 3]
[3,2,1]
mysecondreverse [True, True, False]
[False,True,True]
mysecondreverse []
[]
-}

-- Exercise 28
mythirdconcat :: [[a]] -> [a]
mythirdconcat xss = foldl (++) [] xss 

{- Examples
mythirdconcat [[True, True, False], [False, False]]
[True,True,False,False,False]
mythirdconcat []
[]
mythirdconcat [[1, 2],[3],[6, 0]]
[1,2,3,6,0]
-}

mythirdreverse :: [a] -> [a]
mythirdreverse xs = foldl (\x y -> [y] ++ x) [] xs

{- Examples
mythirdreverse [1, 2, 3]
[3,2,1]
mythirdreverse [True, True, False, False, False, True]
[True,False,False,False,True,True]
mythirdreverse []
[]
-}

-- Exercise 29

prefix :: [a] -> [[a]]
prefix xs = foldl (\x y -> x ++ [(last x) ++[y]]) [[]] xs

{- Examples
 prefix [1, 2, 3]
[[],[1],[1,2],[1,2,3]]
prefix ['h', 'e', 'y']
["","h","he","hey"]
prefix []
[[]]
-}
