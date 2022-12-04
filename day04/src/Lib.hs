module Lib (day04) where

import System.Environment (lookupEnv)
import Text.Regex.TDFA

type Input = [((Int, Int), (Int, Int))]

day04 :: IO ()
day04 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 input
    _ -> solve1 input

readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"

parseInput :: String -> Input
parseInput = map (tupled . split) . lines

tupled :: [String] -> ((Int, Int), (Int, Int))
tupled [a, b, c, d] = ((read a, read b), (read c, read d))

split :: String -> [String]
split x = getAllTextMatches (x =~ "[0-9]+")


solve1 :: Input -> Int
solve1 = length . filter contains

solve2 :: Input -> Int
solve2 = length . filter overlaps

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((a, b), (c, d)) = (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a, b), (c, d)) =  a < c && b >= c || a == c || a > c && d >= a