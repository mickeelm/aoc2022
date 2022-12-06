module Lib (day06) where

import Data.List (nub)
import System.Environment (lookupEnv)

type Input = String

day06 :: IO ()
day06 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 input
    _ -> solve1 input

readInput :: IO Input
readInput = readFile "input.txt"

solve1 :: Input -> Int
solve1 = markerIndex 0 4

solve2 :: Input -> Int
solve2 = markerIndex 0 14

markerIndex :: Int -> Int -> String -> Int
markerIndex idx dist str = if unique $ take dist str then idx + dist else markerIndex (idx + 1) dist $ tail str

unique :: String -> Bool
unique x = nub x == x
