module Lib
  ( penultimate
  , findK
  , isPalindrome
  , duplicate
  , ziplike
  , splitAtIndex
  , dropK
  , slice
  , insertElem
  , rotate
  ) where

penultimate :: [a] -> a
penultimate l = last (init l)

findK :: (Eq a, Eq b, Num b, Ord b) => b -> [a] -> a
findK k l
  | k > 0 && l == [] = error "index longer than length"
  | k < 0 = error "positive indicies only"
  | k == 0 = head l
  | otherwise = findK (k - 1) (tail l)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "isPalindrome cannot handle an empty list"
isPalindrome l = isPalindrome' l

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [_] = True
isPalindrome' l   = head l == (last l) && isPalindrome' (tail (init l))

duplicate :: [a] -> [a]
duplicate []     = []
duplicate (x:xs) = x : x : (duplicate xs)

ziplike :: [a] -> [b] -> [(a, b)]
ziplike [] [] = []
ziplike a _   = []
ziplike _ b   = []
ziplike a b   = (head a, head b) : ziplike (tail a) (tail b)

splitAtIndex :: Int -> [a] -> [a]
splitAtIndex k l
  | k < 0 = error "splitAtIndex cannot have a negative index"
  | k == 0 = tail l
  | otherwise = splitAtIndex (k - 1) (tail l)

dropK :: Int -> [a] -> [a]
dropK k l | k < 0 = error "dropK cannot have a negative index"
  | k > 0 && ((null l) /= True) = error "dropK cannot have index greater than length"

slice :: Int -> Int -> [a] -> [a]
slice = undefined

insertElem :: a -> Int -> [k]
insertElem a k = undefined

rotate :: Int -> [a] -> [a]
rotate = undefined

