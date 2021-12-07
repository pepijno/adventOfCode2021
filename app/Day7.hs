module Main where

import Lib
import Parser

parseNumbers :: Parser [Int]
parseNumbers = sepBy integer (char ',')

calcFuel :: (Int -> Int) -> [Int] -> Int
calcFuel costFunction xs = minimum $ map totalFuel [minimum xs..maximum xs]
  where
    totalFuel x = sum $ map (costFunction . abs . (-) x) xs

solve1 :: [String] -> Int
solve1 = calcFuel id . head . map (unsafeParse parseNumbers)

solve2 :: [String] -> Int
solve2 = flip div 2 . calcFuel ((+1) >>= (*)) . head . map (unsafeParse parseNumbers)

main :: IO()
main = mainWrapper "day7" solve1 solve2
