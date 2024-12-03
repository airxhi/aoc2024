

module Day2 (day2, isIncreasing, isDecreasing, increasing, decreasing) where
import Data.List.Split ( splitOn )
import Data.List (sort, group)
import qualified Data.Bifunctor
import qualified Data.Map as Map
import GHC.Base (ap)


day2 :: String -> IO ()
day2 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2


part1 :: [[Int]] -> Int
part1 xs = length $ filter isSafe xs

part2 :: [[Int]] -> Int
part2 xs = length $ filter test xs


test :: [Int] -> Bool
test xs = isSafe xs || or (tryDeletus isSafe xs)

isSafe :: [Int] -> Bool
isSafe x = isIncreasing x || isDecreasing x

tryDeletus :: ([a] -> b) -> [a] -> [b]
tryDeletus f xs = map (f . removeAt xs) [0..length xs - 1]

removeAt :: [a] -> Int -> [a]
removeAt xs idx = take idx xs ++ drop (idx + 1) xs

compareAdjacent :: (Int -> Int -> Bool) -> [Int] -> Bool
compareAdjacent = (. ap zip tail) . all . uncurry

isIncreasing :: [Int] -> Bool
isIncreasing = compareAdjacent increasing

isDecreasing :: [Int] -> Bool
isDecreasing = compareAdjacent decreasing

increasing :: Int -> Int -> Bool
increasing a b = b > a && b <= a + 3

decreasing :: Int -> Int -> Bool
decreasing a b = b < a && b >= a - 3

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . filter (/= "") . splitOn "\n"
