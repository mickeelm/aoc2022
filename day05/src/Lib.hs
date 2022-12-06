module Lib (day05) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex)
import Data.Maybe
import System.Environment (lookupEnv)
import Text.Regex.TDFA

type Input = ([String], [(Int, Int, Int)])

day05 :: IO ()
day05 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 input
    _ -> solve1 input

-- PARSING

readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"

parseInput :: String -> Input
parseInput = organizeInput . splitInput . lines

splitInput :: [String] -> ([String], [String])
splitInput x = splitAt (blankIndex x) x

blankIndex :: [String] -> Int
blankIndex = fromJust . elemIndex ""

organizeInput :: ([String], [String]) -> Input
organizeInput (crates, moves) = (organizeCrates (reverse $ init crates), map (organizeMove . parseMove) $ tail moves)

organizeMove :: [String] -> (Int, Int, Int)
organizeMove [a, b, c] = (read a, read b, read c)

parseMove :: String -> [String]
parseMove x = getAllTextMatches (x =~ "[0-9]+")

organizeCrates :: [String] -> [String]
organizeCrates x = map (dropWhileEnd isSpace) $ mergeLayers $ map parseLayer x

mergeLayers :: [[String]] -> [String]
mergeLayers [x] = x
mergeLayers (x : xs) = zipWith (++) x $ mergeLayers xs

parseLayer :: String -> [String]
parseLayer raw = [[raw !! stackPos x] | x <- [1 .. numberOfStacks raw]]

stackPos :: Int -> Int
stackPos x = 1 + (4 * (x - 1))

numberOfStacks :: String -> Int
numberOfStacks x = (length x `div` 4) + 1

-- OH MY GOD, ACTUAL SOLUTION

solve1 :: Input -> String
solve1 = topCrates . uncurry operateCrateMover9000

solve2 :: Input -> String
solve2 = topCrates . uncurry operateCrateMover9001

topCrates :: [String] -> String
topCrates [x] = [last x]
topCrates (x : xs) = last x : topCrates xs

operateCrateMover9000 :: [String] -> [(Int, Int, Int)] -> [String]
operateCrateMover9000 = foldl moveSeparately

operateCrateMover9001 :: [String] -> [(Int, Int, Int)] -> [String]
operateCrateMover9001 = foldl move

moveSeparately :: [String] -> (Int, Int, Int) -> [String]
moveSeparately stacks (1, from, to) = move stacks (1, from, to)
moveSeparately stacks (noOfCrates, from, to) = moveSeparately (move stacks (1, from, to)) (noOfCrates -1, from, to)

move :: [String] -> (Int, Int, Int) -> [String]
move stacks (noOfCrates, from, to) =
  let fromStack = stacks !! (from -1)
      toStack = stacks !! (to -1)
      (newAtFrom, cratesToMove) = splitAt (length fromStack - noOfCrates) fromStack
      newAtTo = toStack ++ cratesToMove
   in replaceStack to newAtTo $ replaceStack from newAtFrom stacks

replaceStack :: Int -> String -> [String] -> [String]
replaceStack 1 updated stacks = updated : tail stacks
replaceStack pos updated stacks =
  let split = splitAt pos stacks
   in init (fst split) ++ [updated] ++ snd split
