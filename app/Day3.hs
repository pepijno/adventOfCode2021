module Main where

import Data.List
import Data.Char
import Lib

toBinary n [] = n
toBinary n (l:xs) = toBinary (2 * n + l) xs

solve1 :: [String] -> Int
solve1 xs = (fst part) * (snd part)
  where
    l = length xs
    numbers = map sum . transpose . map (map (\x -> ord x - 48)) $ xs
    part = (toBinary 0 $ map (\x -> if x < (l `div` 2) then 0 else 1) numbers, toBinary 0 $ map (\x -> if x < (l `div` 2) then 1 else 0) numbers)

part2 xs i
  | l == 1 = xs
  | 2 * s >= l = filter (\x -> (x!!i) == 1) xs
  | otherwise = filter (\x -> (x!!i) == 0) xs
  where
    l = length xs
    s = sum ((transpose xs) !! i)

part3 xs i
  | l == 1 = xs
  | 2 * s >= l = filter (\x -> (x!!i) == 0) xs
  | otherwise = filter (\x -> (x!!i) == 1) xs
  where
    l = length xs
    s = sum ((transpose xs) !! i)

-- solve2 :: [String] -> Int
solve2 xs = (fst p) * (snd p)
  where
    l = length xs
    numbers = map (map (\x -> ord x - 48)) $ xs
    p = (toBinary 0 $ head $ foldl part2 numbers [0..15], toBinary 0 $ head $ foldl part3 numbers [0..15])

main :: IO()
main = mainWrapper "day3" solve1 solve2
