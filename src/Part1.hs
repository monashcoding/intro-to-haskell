module Part1 where

{- |
Multiples a number by 2.
>>> double 3
6
>>> double 5
10
>>> double 0
0
-}
double :: Int -> Int
double x = error "Please implement double"

{- | Adds two numbers.
>>> add 1 2
3
>>> add 3 4
7
-}
add :: Int -> Int -> Int
add x y = error "Please implement add"

{- | Finds the factorial of a number. n! = 1 × 2 × 3 × … × n
>>> factorial 0
1
>>> factorial 1
1
>>> factorial 3
6
>>> factorial 5
120
-}
factorial :: Int -> Int
factorial n = error "Please implement factorial"

{- | Finds the nth fibonacci number. This has been partially done for you and
you just need to implement the helper auxiliary function.
The first fibonacci number should be at 0 (nthFibonacciNumber 0). See the below
test cases for how this function should work.
>>> nthFibonacciNumber 0
0
>>> nthFibonacciNumber 1
1
>>> nthFibonacciNumber 2
1
>>> nthFibonacciNumber 3
2
>>> nthFibonacciNumber 4
3
>>> nthFibonacciNumber 5
5
>>> nthFibonacciNumber 6
8
-}
nthFibonacciNumber :: Int -> Int
nthFibonacciNumber = helper 0 1
 where
  helper :: Int -> Int -> Int -> Int
  helper a b n = error "Please implement the nthFibonacciNumber helper"
