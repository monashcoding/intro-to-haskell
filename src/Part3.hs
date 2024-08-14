{-# LANGUAGE ImportQualifiedPost #-}

module Part3 where

import Data.List (find)
import Data.Map.Strict qualified as M

-- * Task 1

{-
Maybe is defined like this:
data Maybe a = Nothing | Just a
-}

{- | 'safeDivide a b' divides 'a' by 'b'. If the divisor is 0, it returns
'Nothing'.
>>> safeDivide 10 5
Just 2.0
>>> safeDivide 10 3
Just 3.3333333333333335
>>> safeDivide 1 0
Nothing
-}
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

{- | Adds two integers together, returning 'Nothing' if either (or both) of them
are Nothing.
>>> addMaybes (Just 1) (Just 2)
Just 3
>>> addMaybes (Just 1) Nothing
Nothing
>>> addMaybes Nothing (Just 2)
Nothing
>>> addMaybes Nothing Nothing
Nothing
-}
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _ = Nothing

-- addMaybes = liftA2 (+)

-- * Task 2

{-
find :: (a -> Bool) -> [a] -> Maybe a
-}

{- | Gets the first even number from a list of integers. If there are no even
numbers in the list, it returns Nothing.
Hint: use the find and even functions
find :: (Int -> Bool) -> [Int] -> Maybe Int
even :: Int -> Bool

>>> findFirstEvenNumber [1, 2, 3, 4]
Just 2
>>> findFirstEvenNumber [1, 3, 5]
Nothing
>>> findFirstEvenNumber []
Nothing
-}
findFirstEvenNumber :: [Int] -> Maybe Int
findFirstEvenNumber xs = find even xs

-- Because all functions in Haskell are curried, this could be simplified (the technical term is
-- eta-reduced) to:
-- findFirstEvenNumber = find even

{- | Returns a string about the first even number in the list.
>>> evenNumberMessage [1, 2, 3, 4]
"The first even number is 2"
>>> evenNumberMessage [1, 3, 5]
"There are no even numbers"
>>> evenNumberMessage []
"There are no even numbers"
-}
evenNumberMessage :: [Int] -> String
evenNumberMessage xs = case findFirstEvenNumber xs of
  Just x -> "The first even number is " <> show x
  Nothing -> "There are no even numbers"

-- If you're curious, there's a function called maybe:
-- evenNumberMessage = maybe "There are no even numbers" (("The first even number is " <>) . show) . findFirstEvenNumber

-- * Task 3

-- | A map of student names to their grades.
grades :: M.Map String Double
grades =
  M.fromList
    [ ("Alice", 83.6)
    , ("Bob", 64.3)
    , ("Charlie", 93.5)
    , ("David", 79.4)
    ]

{- | Gets the grade for a student, or 'Nothing' if the student cannot be found.
>>> studentGrade "Alice"
Just 83.6
>>> studentGrade "Eve"
Nothing
-}
studentGrade :: String -> Maybe Double
studentGrade student = M.lookup student grades

{- | Calculates the average grade of 2 students, returning 'Nothing' if either
student's grade cannot be found.
>>> averageGrade "Alice" "Charlie"
Just 88.55
>>> averageGrade "Alice" "Eve"
Nothing
>>> averageGrade "Eve" "Frank"
Nothing
-}
averageGrade :: String -> String -> Maybe Double
averageGrade student1 student2 = case studentGrade student1 of
  Nothing -> Nothing
  Just grade1 -> case studentGrade student2 of
    Nothing -> Nothing
    Just grade2 -> Just ((grade1 + grade2) / 2)

-- Alternatively, using a tuple:
-- averageGrade2 student1 student2 = case (studentGrade student1, studentGrade student2) of
--   (Just grade1, Just grade2) -> Just ((grade1 + grade2) / 2)
--   _ -> Nothing

-- * Task 4

data Expression
  = Number Double
  | Negate Expression
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Power Expression Expression
  -- the Eq means we can check if two expressions are equal, and the Show means
  -- we can convert an expression to a string (useful for debugging)
  deriving (Eq, Show)

{- | Recursively evaluates an expression.
>>> evaluateExpression $ Number 1
1.0
>>> evaluateExpression $ Add (Number 1) (Number 2)
3.0
>>> evaluateExpression $ Subtract (Number 1) (Number 2)
-1.0
>>> evaluateExpression $ Power (Number 2) (Number 3)
8.0
>>> evaluateExpression $ Multiply (Add (Number 1) (Power (Number 2) (Number 3))) (Negate (Divide (Number 3) (Number 4))) -- (1 + 2 ^ 3) * -(3 / 4)
-6.75
-}
evaluateExpression :: Expression -> Double
evaluateExpression (Number n) = n
evaluateExpression (Negate e) = -evaluateExpression e
evaluateExpression (Add a b) = evaluateExpression a + evaluateExpression b
evaluateExpression (Subtract a b) = evaluateExpression a - evaluateExpression b
evaluateExpression (Multiply a b) = evaluateExpression a * evaluateExpression b
evaluateExpression (Divide a b) = evaluateExpression a / evaluateExpression b
evaluateExpression (Power a b) = evaluateExpression a ** evaluateExpression b
