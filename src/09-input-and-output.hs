import System.Random

main = do
    command <- getLine
    putStrLn command

echo :: String
echo s = putStrLn isInfixOf "-n" s

lottery :: StdGen -> [Int]
lottery gen = undefined
