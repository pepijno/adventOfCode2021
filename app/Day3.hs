module Main where

import Data.Char
import Data.Function
import Data.List
import Lib

binaryToInt :: [Int] -> Int
binaryToInt = foldl ((+) . (* 2)) 0

binaryToInt2 :: [Bool] -> Int
binaryToInt2 = foldl ((. fromEnum) . (+) . (* 2)) 0

toBools :: String -> [Bool]
toBools = map (== '1')

solve1 :: [String] -> Int
solve1 xs = combine $ map most $ transpose $ asBools xs
  where
    asBools = map toBools
    most = (>) <$> (* 2) . count <*> length
    count = length . filter id
    combine = ((*) `on` binaryToInt2) <*> map not

part2 xs i
  | l == 1 = xs
  | 2 * s >= l = filter (\x -> (x !! i) == 1) xs
  | otherwise = filter (\x -> (x !! i) == 0) xs
  where
    l = length xs
    s = sum ((transpose xs) !! i)

part3 xs i
  | l == 1 = xs
  | 2 * s >= l = filter (\x -> (x !! i) == 0) xs
  | otherwise = filter (\x -> (x !! i) == 1) xs
  where
    l = length xs
    s = sum ((transpose xs) !! i)

-- solve2 :: [String] -> Int
solve2 xs = (fst p) * (snd p)
  where
    l = length xs
    numbers = map (map (\x -> ord x - 48)) $ xs
    p =
      ( binaryToInt $ head $ foldl part2 numbers [0 .. 15],
        binaryToInt $ head $ foldl part3 numbers [0 .. 15]
      )

main :: IO ()
main = mainWrapper "day3" solve1 solve2
