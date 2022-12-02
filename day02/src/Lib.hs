module Lib (day02) where

import Data.List (sort)
import System.Environment (lookupEnv)

type Input = [(Char, Char)]

day02 :: IO ()
day02 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 input
    _ -> solve1 input

solve1 :: Input -> Int
solve1 = sum . map score

solve2 :: Input -> Int
solve2 = sum . map (score . strategy)

readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"

parseInput :: String -> Input
parseInput = map tupled . lines

tupled :: String -> (Char, Char)
tupled [x,_,y] = (x,y)

strategy :: (Char, Char) -> (Char, Char)
strategy ('A','X') = ('A','Z')
strategy ('A','Y') = ('A','X')
strategy ('A','Z') = ('A','Y')
strategy ('B','X') = ('B','X')
strategy ('B','Y') = ('B','Y')
strategy ('B','Z') = ('B','Z')
strategy ('C','X') = ('C','Y')
strategy ('C','Y') = ('C','Z')
strategy ('C','Z') = ('C','X')
 
score :: (Char, Char) -> Int
score ('A','X') = 4
score ('A','Y') = 8
score ('A','Z') = 3
score ('B','X') = 1
score ('B','Y') = 5
score ('B','Z') = 9
score ('C','X') = 7
score ('C','Y') = 2
score ('C','Z') = 6    