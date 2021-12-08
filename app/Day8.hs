module Main where

import Lib
import Parser
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Arrow

type Line = ([String], [String])

parseLine :: Parser Line
parseLine = do
  inputs <- sepBy stringLiteral (char ' ')
  string " | "
  outputs <- sepBy stringLiteral (char ' ')
  return (inputs, outputs)

is1478 :: [a] -> Bool
is1478 x = l == 2 || l == 4 || l == 3 || l == 7
  where
    l = length x

solve1 :: [String] -> Int
solve1 = length . filter is1478 . concatMap (snd . unsafeParse parseLine)

solve :: ([String], [String]) -> Int
solve (input, target) = foldl ((+) . (10*)) 0 $ map ((M.!) searchable . S.toList . S.fromList) target
  where
    searchable = M.fromList $ zip (map S.toList [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]) [0..]
    sorted = map S.fromList $ sortOn length input
    [s1, s7, s4, _, _, _, _, _, _, s8] = sorted
    Just s5 = find (\s -> S.size s == 5 && (S.\\) s4 s1 `S.isSubsetOf` s) sorted
    s9 = S.union s1 s5
    [s2, s3, _] = sortOn (S.size . S.intersection s5) $ filter ((==5) . S.size) sorted
    [s6, s0] = sortOn (S.size . S.intersection s1) $ filter (\x -> x /= s9 && S.size x == 6) sorted

solve2 :: [String] -> Int
solve2 = sum . map (solve . unsafeParse parseLine)

main :: IO()
main = mainWrapper "day8" solve1 solve2
