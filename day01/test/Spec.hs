import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lib

main :: IO ()
main = defaultMain [
    testGroup "Day 01" [
      testCase "Example input part 1" testExamplePt1,
      testCase "Example input part 2" testExamplePt2,
      testCase "Real input part 1" testRealPt1,
      testCase "Real input part 2" testRealPt2
    ]
  ]

testExamplePt1 :: Assertion
testExamplePt1 = readExampleInput >>= \input -> assertEqual "files not equal" 24000 (solve1 input)

testExamplePt2 :: Assertion
testExamplePt2 = readExampleInput >>= \input -> assertEqual "files not equal" 45000 (solve2 input)

testRealPt1 :: Assertion
testRealPt1 = readRealInput >>= \input -> assertEqual "files not equal" 71934 (solve1 input)

testRealPt2 :: Assertion
testRealPt2 = readRealInput >>= \input -> assertEqual "files not equal" 211447 (solve2 input)

readRealInput :: IO String
readRealInput = readFile "input.txt"

readExampleInput :: IO String
readExampleInput = readFile "example_input.txt"