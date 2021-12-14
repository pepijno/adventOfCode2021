module Main where

import qualified Data.Sequence as S
import Lib
import Parser

parseFish' :: Parser [Int]
parseFish' = sepBy integer (char ',')

parseFish :: [String] -> [Int]
parseFish = head . map (unsafeParse parseFish')

countFish :: [Int] -> S.Seq Int
countFish fish = S.fromList [length $ filter (== x) fish | x <- [0..8]]

stepFish :: S.Seq Int -> S.Seq Int
stepFish fish = S.fromList
  [ case i of
    6 -> S.index fish 7 + S.index fish 0
    8 -> S.index fish 0
    _ -> S.index fish (i + 1)
  | i <- [0..8]]

fishAtDay :: Int -> S.Seq Int -> Int
fishAtDay day = sum . nSteps day stepFish

solve1 :: [String] -> Int
solve1 = fishAtDay 80 . countFish . parseFish

solve2 :: [String] -> Int
solve2 = fishAtDay 256 . countFish . parseFish

main :: IO()
main = mainWrapper "day6" solve1 solve2
