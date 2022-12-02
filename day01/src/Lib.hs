module Lib (day01) where

import Data.List (sort)
import System.Environment (lookupEnv)

type Input = [Int]

day01 :: IO ()
day01 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 input
    _ -> solve1 input

solve1 :: [Int] -> Int
solve1 = maximum

solve2 :: [Int] -> Int
solve2 = sum . take 3 . reverse . sort

readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"

parseInput :: String -> [Int]
parseInput = map sum . chunked . lines

chunked :: [String] -> [[Int]]
chunked = foldl chunkAndParse [[]]

chunkAndParse :: [[Int]] -> String -> [[Int]]
chunkAndParse chunks str = if str == "" then chunks ++ [[]] else init chunks ++ [last chunks ++ [read str]]