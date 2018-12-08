module Day1Part2 where

import qualified Data.IntSet as IS

fileName :: String
fileName = "day-1-input.txt"

readIntList :: String -> [Int]
readIntList file = map (read . noPlus) (lines file)
    where
        noPlus :: String -> String
        noPlus ('+':xs) = xs
        noPlus xs = xs

part2 :: [Int] -> Int
part2 l = go IS.empty (scanl (+) 0 $ cycle l)
    where 
        go s (x:xs)
            | IS.member x s = x
            | otherwise = go (IS.insert x s) xs

main :: IO ()
main = do
    input <- readFile fileName
    print $ part2 $ readIntList input