module Practicum1 where


-- Maximum of two integers
maxi :: Integer -> Integer -> Integer
maxi  x y | (x >= y) = x
          | (x < y) = y


-- Check if ascending
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending w x y z = (w < x) && (x < y) && (y < z) 


-- Check if all 4 are equal
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual w x y z = (w == x) && (w == y) && (w == z)


-- Check if all are different
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z = (w /= x) && (w /= y) && (w /= z) && (x /= y) && (x /= z) && (y /= z)



-- Calculate factorial
factorial :: Integer -> Integer
factorial x |(x == 0) = 1
            |(x /= 0) = x * factorial (x - 1)


-- Give the next Fibonacci number
fib :: Integer -> Integer
fib n |(n == 0) = 0
      |(n == 1) = 1
      |(n > 1)  = fib(n-1) + fib(n-2)


-- Add from start to end: summation 1 4 = 1 + 2 + 3 + 4 = 10

summation :: Integer -> Integer -> Integer
summation start end |(start == end) = end
                    |(start < end) = start + summation (start + 1) end

-- Use summation n n+7

strangeSummation :: Integer -> Integer
strangeSummation n = summation n (n+7)


-- Length of a list
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

-- Length of a list alternative solution using case expression

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

-- Sum of a list

sumList :: [Integer] -> Integer
sumList x |(lengthList x == 0) = 0
          |(lengthList x > 0) = head x + sumList (tail x)


-- Double each element in a list
doubleList :: [Integer] -> [Integer]
doubleList x = map (*2) x


-- Append a list to another
myappend :: [a] -> [a] -> [a]
myappend a b  | (length a == 0)   = b
              | (length a > 0)    = (head a):(myappend (tail a) b)


-- Reverse a list
myreverse :: [a] -> [a]
myreverse x | (length x == 0)   = []
            | (length x > 0)    = (last x):(myreverse (init x))


-- Check if an element is a member of a list
mymember :: (Eq a) => a -> [a] -> Bool
mymember x y = any (==x) y


-- Add the squares of a list
mysquaresum :: [Integer] -> Integer
mysquaresum x = sum (map (^2) x)


-- Generate a range of integers from a to b
range :: Integer -> Integer -> [Integer]
range a b |(a > b) = []
          |(a <= b) = a:(range (a+1) b)


-- Concatenate a list of lists
myconcat :: [[a]] -> [a]
myconcat a|(length a == 0)  = []
          |(length a > 0)   = (head a ++ (myconcat (tail a)))


-- Insert an element into a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x list = (filter (<x) list) ++ [x] ++ (filter (>=x) list)


-- Sort a list using insertion sort
insertionsort :: Ord a => [a] -> [a]
insertionsort x |(length x == 0)  = []
                |(length x > 0)   = insert (last x) (insertionsort (init x))



-- Minimum element of a list
minim :: Ord a => [a] -> a
minim x |(length x == 1)  = x!!0
        |(length x > 1)   = mini (head x) (minim (tail x))

mini :: Ord a => a -> a -> a
mini a b |(a >= b)  = b
         |(a < b)   = a


-- Remove the first occurrence of an element from a list
removeFirstOccurrence :: Eq t => t -> [t] -> [t]
removeFirstOccurrence x list  |(list == []) = []
                              |(x == (head list))  = tail list
                              |(x /= (head list))  = [head list] ++ removeFirstOccurrence x (tail list)


-- Sort a list using selection sort
selectionsort :: Ord a => [a] -> [a]
selectionsort x |(length x == 0)  = []
                |(length x > 0)   = [minim x] ++ selectionsort (removeFirstOccurrence (minim x) x) 


-- Sort a list using quicksort
quicksort :: Ord a => [a] -> [a]
quicksort x |(length x == 0)  = []
            |(length x > 0)   = quicksort (filter (< (head x)) (tail x)) ++ [head x] ++ quicksort (filter (>= (head x)) (tail x))


-- Make a list of even numbers from a list
evensB :: [Integer] -> [Integer]
evensB xs = [x | x <- xs, mod x 2 == 0]


-- Map a function over a list
mymap :: (a -> b) -> [a] -> [b]
mymap func xs |(length xs == 0) = []
              |(length xs > 0)  = [func (head xs)] ++ mymap (func) (tail xs)


-- Do a function twice
twice :: (a -> a) -> a -> a
twice func b = func (func b)


-- Compose two functions
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x) 


-- Return the last element of a list
mylast :: [a] -> a
mylast xs = head (reverse xs)


-- Return the last element of a list alternative solution
mylastb :: [a] -> a
mylastb xs = head (drop (length xs - 1) xs) 


-- Return all but the last element of a list
myinit, myinitb :: [a] -> [a]
myinit xs = reverse (tail (reverse xs))


myinitb xs = take (length xs - 1) xs 


-- Concatenate a list of lists using foldr
mysecondconcat :: [[a]] -> [a]
mysecondconcat xss = foldr (++) [] xss 

-- Reverse a list using foldr

mysecondreverse :: [a] -> [a]
mysecondreverse xs = foldr (\x y -> y ++ [x]) [] xs


-- Concatenate a list of lists using foldl
mythirdconcat :: [[a]] -> [a]
mythirdconcat xss = foldl (++) [] xss 


-- Reverse a list using foldl
mythirdreverse :: [a] -> [a]
mythirdreverse xs = foldl (\x y -> [y] ++ x) [] xs


-- Generate all prefixes of a list
prefix :: [a] -> [[a]]
prefix xs = foldl (\x y -> x ++ [(last x) ++[y]]) [[]] xs

