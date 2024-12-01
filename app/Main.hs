module Main (main) where

import Day1

run :: (String -> IO()) -> String -> IO ()
run f fileName =
    do
        contents <- readFile fileName
        f contents

main :: IO ()
main = 
    do
        run day1 "inputs/1.txt"