{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-    
Name:           <Daniela Mackay>
VU-net id:      <dmt380>
Student number: <2663452>
Discussed with: 
Remarks:        I haven't been able to finish. I thought I would hand in what I have to show that I am still participating and hope to be able to tak the resit of the assignments.
Sources:        
-}

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = iterate (+1) 1

naturals0 = 0 : map (+1) naturals0

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = cycle [0,1]

-- Exercise 3
threefolds :: [Integer]
threefolds = 0 : map (+3) threefolds

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif = undefined

nothreefolds :: [Integer]
nothreefolds =  filter (\x -> mod x 3/=0) naturals0

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = 0 : map (+n) (allnfolds n)

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n = filter (\x -> mod x n/=0) naturals0

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n l = filter (\x -> mod x n/=0) l

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = primes [2..]
               where
               primes (x:xs) = x : (allelementsexceptnfolds x xs)

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0: 1: zipWith (+) fibonacci (tail fibonacci)

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

{- 
churchnumeral 3 (/2) 20
2.5
churchnumeral 3 (+1) 4
7
backtointeger (churchnumeral 8)
8
churchnumeral 3 (\x -> x+1 ) 2
5
churchnumeral 3 (succ) 2
5
 -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4                                                         
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Exercise 5  
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6                                                         -----------------------------------------------------------
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = x (y s)

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x 

-- Exercise 7                                                          ----------------------------------------------------------
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = undefined


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes Leaf = 0
numberofnodes (Node left x right) = numberofnodes(left) + 1 + numberofnodes(right)


-- Exercise 2
height :: BinaryTree a -> Integer
height Leaf = 0
height (Node left x right)  | (height left >= height right) = 1 + height left
                            | (height left <  height right) = 1 + height right

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes Leaf = 0
sumnodes (Node left x right) = sumnodes left + x + sumnodes right

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node left x right) = Node (mirror right) x (mirror left)

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten Leaf = []
flatten (Node left x right) = (flatten left) ++ [x] ++ (flatten right)


-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f Leaf = Leaf
treemap f (Node left x right) = Node (treemap f left) (f x) (treemap f right) 

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan n Leaf = True
smallerthan n (Node left x right) = smallerthan n left && (x<n) && smallerthan n right

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan n Leaf = True
largerthan n (Node left x right) = largerthan n left && (x>n) && largerthan n right

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree Leaf = True
isbinarysearchtree (Node left x right) = smallerthan x left && largerthan x right && isbinarysearchtree left && isbinarysearchtree right

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement n Leaf = False
iselement n (Node left x right) = iselement n left || n == x || iselement n right

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert n Leaf = Node Leaf n Leaf
insert n (Node left x right) | (iselement n (Node left x right)) = Node left x right
                             | (n > x) = Node left x (insert n right)
                             | (n < x) = Node (insert n left) x right


-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree (x:xs) |(length xs == 0) = insert x Leaf
                              |(length xs >  0) = insert x (createbinarysearchtree xs)

-- Exercise 6                                                                                     ----------------------------------
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove n Leaf = Leaf
remove n (Node left x right)  | ((iselement n (Node left x right)) == False) = Node left x right
                              | (n == x) = removeroot (Node left x right)
                              | (n < x) = remove n left
                              | (n > x) = remove n right


removeroot :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
removeroot Leaf = Leaf
removeroot (Node left x right)  | (left == Leaf && right == Leaf) = Leaf
                                | (left == Leaf && right /= Leaf) = Leaf


----------------------------
-- Exercise Tower of Hanoi                                                                         
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi n a b c |(n == 0) = []
              |(n > 0)  = hanoi (n-1) a c b ++ [(n, a, c)] ++ hanoi (n-1) b a c


{-
The smallest disk is numbered as 1 and then as numbers increase so does the size.

-}
