module Part2 where

{- $setup
>>> import Test.QuickCheck (polyQuickCheck)
-}

-- * Task 1

{- | Returns whether the second argument is divisible by the first argument.
>>> isDivisibleBy 3 10
False
>>> isDivisibleBy 5 10
True
>>> isDivisibleBy 10 10
True
-}
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x y = error "Please implement isDivisibleBy"

{- | Gets the multiples of 3 or 5 from 1 up to but not including the first argument.
>>> multiplesOf3Or5UpTo 15
[3,5,6,9,10,12]
>>> multiplesOf3Or5UpTo 4
[3]
>>> multiplesOf3Or5UpTo 2
[]
>>> multiplesOf3Or5UpTo 0
[]
-}
multiplesOf3Or5UpTo :: Int -> [Int]
multiplesOf3Or5UpTo n = error "Please implement multiplesOf3Or5UpTo"

{- | The solution to the Euler Problem 1: the sum of all the multiples of 3 or 5 below 1000.
https://projecteuler.net/problem=1
>>> eulerProblem1
233168
-}
eulerProblem1 :: Int
eulerProblem1 = error "Please implement eulerProblem1"

-- * Task 2

{- | An infinite list of the fibonacci numbers (starting with 0).
>>> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
-}
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{- | The sum of the even fibonacci numbers that do not exceed 4 million.

Hint: the takeWhile function might be useful:
takeWhile :: (a -> Bool) -> [a] -> [a]

It takes the elements from the list while some condition is true.
takeWhile even [2, 4, 2, 5, 6, 1] == [2, 4, 2]
takeWhile even [5, 6, 2, 4] == []

>>> eulerProblem2
4613732
-}
eulerProblem2 :: Int
eulerProblem2 = error "Please implement eulerProblem2"

-- * Task 3: Bonus

{- | Like multiplesOf3Or5UpTo but for any arbitrary list of divisors.
prop> multiplesOfUpTo [3, 5] n == multiplesOf3Or5UpTo n
>>> multiplesOfUpTo [3, 5] 20
[3,5,6,9,10,12,15,18]
>>> multiplesOfUpTo [2] 7
[2,4,6]
>>> multiplesOfUpTo [2, 4] 5
[2,4]
>>> multiplesOfUpTo [3, 4] 10
[3,4,6,8,9]
>>> multiplesOfUpTo [] 20
[]
-}
multiplesOfUpTo :: [Int] -> Int -> [Int]
multiplesOfUpTo xs n = error "Please implement multiplesOfUpTo"

{- | Generalised Euler problem 1. generalEulerProblem1 divisors n is the sum of all the numbers
divisible by at least one of the divisors up to but not including n.
>>> eulerProblem1 == generalEulerProblem1 [3, 5] 1000
True
>>> generalEulerProblem1 [2, 3, 5] 20
122
>>> generalEulerProblem1 [] 20
0
>>> generalEulerProblem1 [1] 11
55
>>> generalEulerProblem1 [3, 5, 7] 1000
271066
-}
generalEulerProblem1 :: [Int] -> Int -> Int
generalEulerProblem1 xs n = error "Please implement generalEulerProblem1"
