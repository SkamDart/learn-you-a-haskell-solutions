module (


) where

power :: Int -> Int -> Int
power x y = power' x y 1
    where power' x 0 acc = acc
          power' x y acc = power' x (y - 1) (acc * x)


fib :: (Num a, Eq a) => a -> [a]
fib x = x


stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign = undefined
