module Lib
(
    englishDigit
    , divTuple
    , threeZeroList
) where


englishDigit :: Int -> String
englishDigit x | x > -1 && x < 10 = show x
               | otherwise        = "Unknown"

divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x / y

threeZeroList :: [Int] -> Bool
threeZeroList [0, 0, 0] = True
threeZeroList _          = False
