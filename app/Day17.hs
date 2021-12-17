module Main where

import Lib
import Grid
import Parser

data Trench = Trench Coord Coord deriving (Show, Eq)

parseTrench :: Parser Trench
parseTrench = do
  string "target area: x="
  x1 <- integer
  string ".."
  x2 <- integer
  string ", y="
  y1 <- integer
  string ".."
  y2 <- integer
  return (Trench (x1, x2) (y1, y2))

steps :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
steps (x, y) (vx, vy) = (x, y) : steps (x + vx, y + vy) (max (vx - 1) 0, vy - 1)

path :: Trench -> (Int, Int) -> [(Int, Int)]
path (Trench _ (y1, _)) = takeWhile ((>= y1) . snd) . steps (0, 0)

isInTrench :: Trench -> (Int, Int) -> Bool
isInTrench (Trench (x1, x2) (y1, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

allPaths :: Trench -> [[(Int, Int)]]
allPaths trench@(Trench (_, maxX) (minY, _)) = [p | vx <- [1..maxX], vy <- [minY..negate minY], let p = path trench (vx, vy), any (isInTrench trench) p]

solve1 :: [String] -> Int
solve1 xs = minY * (minY + 1) `div` 2
  where
    (Trench _ (minY, _)) = unsafeParse parseTrench $ head xs

solve2 :: [String] -> Int
solve2 = length . allPaths . unsafeParse parseTrench . head

main :: IO()
main = mainWrapper "day17" solve1 solve2
