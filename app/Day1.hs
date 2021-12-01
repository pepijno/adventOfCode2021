module Main where

import Lib

solve1 :: [String] -> Int
solve1 xs = length $ filter (>0) $ zipWith (-) (tail ys) ys
  where
    ys = map read xs

sumTriples :: [Int] -> [Int]
sumTriples [a, b, c] = [a + b + c]
sumTriples (x:y:z:xs) = (x + y + z):(sumTriples (y:z:xs))

solve2 :: [String] -> Int
solve2 xs = length $ filter (>0) $ zipWith (-) (tail zs) zs
  where
    ys = map read xs
    zs = sumTriples ys

main :: IO ()
main = mainWrapper "day1" solve1 solve2
