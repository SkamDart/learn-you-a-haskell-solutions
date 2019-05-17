module Lib
(
  penultimate
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
findK k l | k > 0 && l == [] = error "index longer than length"
          | k < 0            = error "positive indicies only"
          | k == 0           = head l
          | otherwise        = findK (k - 1) (tail l)

isPalindrome :: (Eq a) => [a] -> [a] -> Bool
isPalindrome [] = error "isPalindrome cannot handle an empty list"

isPalindrome' [_] = True
isPalindrome' l   = (head l == (last l)) && isPalindrome' tail (init l)

duplicate :: [a] -> [a]
duplicate = undefined

ziplike :: [a] -> [b] -> [(a, b)]
ziplike = undefined

splitAtIndex :: int -> [a] -> [a]
splitAtIndex = undefined

dropK :: int -> [a] -> [a]
dropK = undefined

slice :: int -> int -> [a] -> [a]
slice = undefined

insertElem :: a -> int -> [k]
insertElem = undefined

rotate :: Int -> [a] -> [a]
rotate = undefined

