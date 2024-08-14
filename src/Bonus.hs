module Bonus where

import Part3 (safeDivide, studentGrade)

{- $setup
>>> import Test.QuickCheck (polyQuickCheck)
>>> import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
>>> :{
  fromList = foldr Cons Empty
  toList Empty = []
  toList (Cons x xs) = x : toList xs
  instance Arbitrary a => Arbitrary (MyList a) where
    arbitrary = fromList <$> arbitrary
:}
-}

data MyList a = Empty | Cons a (MyList a) deriving (Eq, Show)

-- * Task 1

{- | Applies a function to each element of a list.
prop> myMap id xs == xs
>>> toList $ myMap (+1) $ fromList []
[]
>>> toList $ myMap (+1) $ fromList [1, 2, 3]
[2,3,4]
-}
myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- * Task 2

{- | Returns a new list with only the elements satisfying a predicate.
prop> myFilter (const True) xs == xs
prop> myFilter (const False) xs == Empty
>>> toList $ myFilter (> 5) $ fromList [1, 3, 6, 4, 8, 1, 8, 3, 9]
[6,8,8,9]
>>> toList $ myFilter (> 5) $ fromList []
[]
-}
myFilter :: (a -> Bool) -> MyList a -> MyList a
myFilter _ Empty = Empty
myFilter p (Cons x xs)
  | p x = Cons x (myFilter p xs)
  | otherwise = myFilter p xs

-- * Task 3

{- | Finds the first element of a list satisfying a predicate.
prop> myFind (const True) xs == case xs of { Empty -> Nothing; Cons x _ -> Just x }
>>> myFind (> 5) $ fromList [1, 6, 3, 8]
Just 6
>>> myFind (> 5) $ fromList [1, 3]
Nothing
>>> myFind (> 5) $ fromList []
Nothing
>>> myFind (> 5) $ fromList [8]
Just 8
-}
myFind :: (a -> Bool) -> MyList a -> Maybe a
myFind _ Empty = Nothing
myFind f (Cons x xs)
  | f x = Just x
  | otherwise = myFind f xs

-- * Task 4

{- | Applies a function to each element of a list (from right to left) and an accumulator value,
starting with an initial accumulator value. The function returns a new accumulator value, and the
final accumulator value is returned.
prop> myFoldr (+) 0 xs == sum (toList xs)
prop> myFoldr (*) 1 xs == product (toList xs)
prop> myFoldr (:) [] xs == toList xs
>>> myFoldr (+) 0 $ fromList [1, 2, 3, 4]
10
>>> myFoldr (*) 1 $ fromList [1, 2, 3, 4]
24
>>> myFoldr (flip (-)) 15 $ fromList [1, 2, 3, 4]
5
>>> myFoldr (\x acc -> if x > 5 then x : acc else acc) [] $ fromList [1, 3, 6, 4, 8, 1, 8, 3, 9]
[6,8,8,9]
>>> myFoldr (:) [] $ fromList [1,2,3,4]
[1,2,3,4]
-}
myFoldr :: (a -> b -> b) -> b -> MyList a -> b
myFoldr _ z Empty = z
myFoldr f z (Cons x xs) = f x (myFoldr f z xs)

{- | Applies a function to each element of a list (from left to right) and an accumulator value,
starting with an initial accumulator value. The function returns a new accumulator value, and the
final accumulator value is returned.
prop> myFoldl (+) 0 xs == sum (toList xs)
prop> myFoldl (*) 1 xs == product (toList xs)
prop> myFoldl (flip (:)) [] xs == reverse (toList xs)
>>> myFoldl (\acc x -> if x > 5 then x : acc else acc) [] $ fromList [1, 3, 6, 4, 8, 1, 8, 3, 9]
[9,8,8,6]
>>> myFoldl (flip (:)) [] $ fromList [1,2,3,4]
[4,3,2,1]
-}
myFoldl :: (b -> a -> b) -> b -> MyList a -> b
myFoldl _ z Empty = z
myFoldl f z (Cons x xs) = myFoldl f (f z x) xs

-- * Task 5

-- Implement these using myFoldr.

{- | Applies a function to each element of a list.
prop> myMap2 id xs == xs
>>> toList $ myMap2 (+1) $ fromList []
[]
>>> toList $ myMap2 (+1) $ fromList [1, 2, 3]
[2,3,4]
-}
myMap2 :: (a -> b) -> MyList a -> MyList b
myMap2 f = myFoldr (Cons . f) Empty

{- | Returns a new list with only the elements satisfying a predicate.
prop> myFilter2 (const True) xs == xs
prop> myFilter2 (const False) xs == Empty
>>> toList $ myFilter2 (> 5) $ fromList [1, 3, 6, 4, 8, 1, 8, 3, 9]
[6,8,8,9]
>>> toList $ myFilter2 (> 5) $ fromList []
[]
-}
myFilter2 :: (a -> Bool) -> MyList a -> MyList a
myFilter2 p = myFoldr (\x acc -> if p x then Cons x acc else acc) Empty

{- | Finds the first element of a list satisfying a predicate.
prop> myFind2 (const True) xs == case xs of { Empty -> Nothing; Cons x _ -> Just x }
>>> myFind2 (> 5) $ fromList [1, 6, 3, 8]
Just 6
>>> myFind2 (> 5) $ fromList [1, 3]
Nothing
>>> myFind2 (> 5) $ fromList []
Nothing
>>> myFind2 (> 5) $ fromList [8]
Just 8
-}
myFind2 :: (a -> Bool) -> MyList a -> Maybe a
myFind2 p = myFoldr (\x acc -> if p x then Just x else acc) Nothing

{- This one could be implemented more efficiently using foldl:
myFind2 p = myFoldl f Nothing
 where
  f Nothing x
    | p x       = Just x
    | otherwise = Nothing
  f acc _ = acc
Of course, the real Haskell implementation exits once it has found an element matching the
predicate.
-}

-- * Task 6

-- Remember averageGrade from part 3. Can you reimplement this for any number
-- of students?
-- The safeDivide and studentGrade functions from part 3 have been imported at the
-- top of the file, so you can use these functions here.
--
-- As a reminder:
-- safeDivide :: Double -> Double -> Maybe Double
-- studentGrade :: String -> Maybe Double

{- | Sums everything in the list, returning 'Nothing' if any of the elements are
'Nothing'.
>>> sumMaybes []
Just 0.0
>>> sumMaybes [Just 1]
Just 1.0
>>> sumMaybes [Just 1.5, Just 2]
Just 3.5
>>> sumMaybes [Just 1, Nothing]
Nothing
-}
sumMaybes :: [Maybe Double] -> Maybe Double
sumMaybes [] = Just 0
sumMaybes (Nothing : _) = Nothing
sumMaybes (Just x : xs) = (+ x) <$> sumMaybes xs

{- | Calculates the average grade of 2 students, returning 'Nothing' if any of
the students' grades cannot be found. This function should also return 'Nothing'
if there are no students in the list (as we can't divide by 0).
Hint: you can convert an Int to a Double with fromIntegral
>>> averageGrade ["Alice", "Charlie"]
Just 88.55
>>> averageGrade ["Alice"]
Just 83.6
>>> averageGrade ["Alice", "Bob", "Charlie", "David"]
Just 80.19999999999999
>>> averageGrade ["Alice", "Eve"]
Nothing
>>> averageGrade []
Nothing
-}
averageGrade :: [String] -> Maybe Double
averageGrade students = case sumMaybes (map studentGrade students) of
  Nothing -> Nothing
  Just total -> safeDivide total (fromIntegral (length students))

-- averageGrade students = (`safeDivide` fromIntegral (length students)) . sum =<< traverse studentGrade students
