module Day1 (day1) where


import Data.List.Split ( splitOn )
import Data.List (sort, group)
import qualified Data.Bifunctor
import qualified Data.Map as Map


day1 :: String -> IO ()
day1 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2

part1 :: ([Int], [Int]) -> Int
part1 = sum . getAbsDiffs

-- Calculate frequencies of b, then map a to \x -> x * freq(b)[x]
part2 :: ([Int], [Int]) -> Int
part2 (a,b) = let f = freq b
    in sum $ map (\x -> x * lookupMap x f) a

lookupMap :: Int -> Map.Map Int Int -> Int
lookupMap = Map.findWithDefault 0

getAbsDiffs :: Num c => ([c], [c]) -> [c]
getAbsDiffs (a,b) = zipWith (\ x y -> abs (x - y)) a b

freq :: (Ord k, Num a) => [k] -> Map.Map k a
freq xs = Map.fromListWith (+) [(x, 1) | x <- xs]

extractTuple :: [b] -> (b, b)
extractTuple xs = (head xs, xs !! 1)

parseInput :: String -> ([Int], [Int])
parseInput = Data.Bifunctor.bimap sort sort . unzip . map (extractTuple . map read . words) . filter (/= "") . splitOn "\n"