module Day3 (day3) where

import qualified Data.Bifunctor
import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~))

day3 :: String -> IO ()
day3 inp = do
  print $ part1 inp
  print $ part2 inp

data Op = Mul Int Int | Do | Don't

part1 :: String -> Int
part1 = calculate r1

part2 :: String -> Int
part2 = calculate r2

calculate :: (String -> [[String]]) -> String -> Int
calculate f s = snd . foldl operate (True, 0) $ extractOperations f s

operate :: (Bool, Int) -> Op -> (Bool, Int)
operate (enabled, acc) op = case op of
  Do -> (True, acc)
  Don't -> (False, acc)
  Mul a b -> (if enabled then (enabled, acc + a * b) else (enabled, acc))

extractOperations :: (String -> [[String]]) -> String -> [Op]
extractOperations f xs = map findOp $ f xs

findOp :: [String] -> Op
findOp [_, m, a, b, d, _]
  | m /= "" = Mul (read a) (read b)
  | d /= "" = Do
  | otherwise = Don't

r1 :: String -> [[String]]
r1 xs = xs =~ "(mul)\\(([0-9]+),([0-9]+)\\)()()"

r2 :: String -> [[String]]
r2 xs = xs =~ "(mul)\\(([0-9]+),([0-9]+)\\)|(do\\(\\))|(don't\\(\\))"
