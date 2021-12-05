{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Lib
import Parser

data Line = Line (Int, Int) (Int, Int) deriving (Show, Eq)

parseLine :: Parser Line
parseLine = do
  n1 <- integer
  char ','
  n2 <- integer
  string " -> "
  n3 <- integer
  char ','
  n4 <- integer
  pure (Line (n1, n2) (n3, n4))

isStraightLine :: Line -> Bool
isStraightLine (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

range :: (Enum a, Num a, Ord a) => a -> a -> [a]
range a b
  | a < b = [a..b]
  | otherwise = [a,(a-1)..b]

linePoints :: Line -> [(Int, Int)]
linePoints (Line (x1, y1) (x2, y2))
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = zip (range x1 x2) (range y1 y2)

solve1 :: [String] -> Int
solve1 = M.size . M.filter (>1) . M.fromListWith (+) . map ((,1)) . concatMap linePoints . filter isStraightLine . map (unsafeParse parseLine)

solve2 :: [String] -> Int
solve2 = M.size . M.filter (>1) . M.fromListWith (+) . map ((,1)) . concatMap (linePoints . unsafeParse parseLine)

main :: IO()
main = mainWrapper "day5" solve1 solve2
