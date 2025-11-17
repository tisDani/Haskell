{-# LANGUAGE RankNTypes #-}
module Practicum2A where

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- List of all Natural numbers
naturals :: [Integer]
naturals = iterate (+1) 1

-- List of all Natural numbers from 0

naturals0 = 0 : map (+1) naturals0

-- List of alternating 0s and 1s
zeroesandones :: [Integer]
zeroesandones = cycle [0,1]

-- List of all multiples of 3
threefolds :: [Integer]
threefolds = 0 : map (+3) threefolds

-- List of all Natural numbers that are not multiples of 3
nothreefolds :: [Integer]
nothreefolds =  filter (\x -> mod x 3/=0) naturals0

-- List of all multiples of n
allnfolds :: Integer -> [Integer]
allnfolds n = 0 : map (+n) (allnfolds n)

-- List of all Natural numbers that are not multiples of n
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n = filter (\x -> mod x n/=0) naturals0

-- List of all elements except multiples of n from a given list 
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n l = filter (\x -> mod x n/=0) l

-- List of all prime numbers using the Sieve of Eratosthenes
eratosthenes :: [Integer]
eratosthenes = primes [2..]
               where
               primes (x:xs) = x : primes (allelementsexceptnfolds x xs)

-- List of all Fibonacci numbers
fibonacci :: [Integer]
fibonacci = 0: 1: zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- We need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Convert a number to a Church Numeral and reverse the process
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0


-- Check if two Church Numerals are equal
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Calculate the successor of a Church Numeral
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Alternative definition of successor                                                        
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Apply a function on a Church Numeral and convert the result back to an integer  
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger (f (churchnumeral n)) 

-- Add, multiply and raise to a power with two Church Numerals
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = x (y s)

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x 

-- Apply a function with two Church Numerals and convert the result back to an integer
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger (f (churchnumeral m) (churchnumeral n))


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Count the number of nodes in a binary tree
numberofnodes :: BinaryTree a -> Integer
numberofnodes Leaf = 0
numberofnodes (Node left x right) = numberofnodes(left) + 1 + numberofnodes(right)


-- Calculate the height of a binary tree
height :: BinaryTree a -> Integer
height Leaf = 0
height (Node left x right)  | (height left >= height right) = 1 + height left
                            | (height left <  height right) = 1 + height right

-- Calculate the sum of all nodes in a binary tree
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes Leaf = 0
sumnodes (Node left x right) = sumnodes left + x + sumnodes right

-- Mirror a binary tree
mirror :: BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node left x right) = Node (mirror right) x (mirror left)

-- Flatten a binary tree to a list
flatten :: BinaryTree a -> [a]
flatten Leaf = []
flatten (Node left x right) = (flatten left) ++ [x] ++ (flatten right)


-- Make a binary tree from a list (inorder)
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f Leaf = Leaf
treemap f (Node left x right) = Node (treemap f left) (f x) (treemap f right) 

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Calculate if all elements in the tree are smaller/larger than a given value
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan n Leaf = True
smallerthan n (Node left x right) = smallerthan n left && (x<n) && smallerthan n right

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan n Leaf = True
largerthan n (Node left x right) = largerthan n left && (x>n) && largerthan n right

-- See if a binary tree is a binary search tree
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree Leaf = True
isbinarysearchtree (Node left x right) = smallerthan x left && largerthan x right && isbinarysearchtree left && isbinarysearchtree right

-- Check if an element is in a binary search tree
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement n Leaf = False
iselement n (Node left x right) = iselement n left || n == x || iselement n right

-- Insert an element in a binary search tree
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert n Leaf = Node Leaf n Leaf
insert n (Node left x right) | (iselement n (Node left x right)) = Node left x right
                             | (n > x) = Node left x (insert n right)
                             | (n < x) = Node (insert n left) x right

-- Create a binary search tree from a list
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree (x:xs) |(length xs == 0) = insert x Leaf
                              |(length xs >  0) = insert x (createbinarysearchtree xs)

-- Remove an element from a binary search tree    
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove n Leaf = Leaf
remove n (Node left x right)  | ((iselement n (Node left x right)) == False) = Node left x right
                              | (n == x) = removeroot (Node left x right)
                              | (n < x) = Node (remove n left) x right
                              | (n > x) = Node left x (remove n right)


-- Helper function to remove the root of a binary search tree
removeroot :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
removeroot Leaf = Leaf
removeroot (Node left x right)  | (left == Leaf && right == Leaf) = Leaf
                                | (left == Leaf && right /= Leaf)  
                                || (left /= Leaf && right /= Leaf) = Node left (leftmost right) (remove (leftmost right) right) 
                                | (left /= Leaf && right == Leaf) = Node (remove (rightmost left) left) (rightmost left) right


-- Helper functions to find the leftmost and rightmost elements of a binary search tree
leftmost :: (Ord a, Eq a) => BinaryTree a -> a
leftmost (Node left x right)  | (left == Leaf) = x
                              | (left /= Leaf) = leftmost left

rightmost :: (Ord a, Eq a) => BinaryTree a -> a
rightmost (Node left x right) | (right == Leaf) = x
                              | (right /= Leaf) = rightmost right

----------------------------
-- Tower of Hanoi                                                                         
----------------------------
-- Define types and function to solve the Tower of Hanoi problem. 
-- The smallest disk is numbered as 1 and then as numbers increase so does the size.
type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi n a b c |(n == 0) = []
              |(n > 0)  = hanoi (n-1) a c b ++ [(n, a, c)] ++ hanoi (n-1) b a c


