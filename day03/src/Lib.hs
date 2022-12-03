module Lib (day03) where

import Data.Char (ord)
import System.Environment (lookupEnv)

type Input = [String]

day03 :: IO ()
day03 = do
  input <- readInput
  part <- lookupEnv "part"
  print $ case part of
    Just "part2" -> solve2 $ groupThree input
    _ -> solve1 $ map splitInHalf input

readInput :: IO Input
readInput = lines <$> readFile "input.txt"

splitInHalf :: String -> (String, String)
splitInHalf str = splitAt (div (length str) 2) str

groupThree :: [String] -> [(String, String, String)]
groupThree [x, y, z] = [(x, y, z)]
groupThree (x : y : z : xs) = [(x, y, z)] ++ groupThree xs


solve1 :: [(String, String)] -> Int
solve1 = sum . map (priority . duplicate)

solve2 :: [(String, String, String)] -> Int
solve2 = sum . map (priority . common)

duplicate :: (String, String) -> Char
duplicate ([x], _) = x
duplicate (x : xs, y) = if elem x y then x else duplicate (xs, y)

common :: (String, String, String) -> Char
common ([x], y, z) = x
common (x : xs, y, z) = if contains x y z then x else common (xs, y, z)

contains :: Char -> String -> String -> Bool
contains x y z = elem x y && elem x z

priority :: Char -> Int
priority x = if elem x ['A' .. 'Z'] then ord x - 38 else ord x - 96