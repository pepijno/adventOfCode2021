module Main where

import Lib
import Parser
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char

parseLine :: Parser [(String, [String])]
parseLine = do
  from <- stringLiteral
  char '-'
  to <- stringLiteral
  return [(from, [to]), (to, [from])]

isLargeCave :: String -> Bool
isLargeCave = all isUpper

findRoute :: M.Map String [String] -> S.Set String -> Bool -> String -> [[String]]
findRoute _ _ _ [] = []
findRoute routes had visitedTwice from
  | from == "end"                         = [[from]]
  | from == "start" && S.member from had  = []
  | isLargeCave from                      = map (from:) $ concatMap (findRoute routes had visitedTwice) neighbours
  | S.member from had && visitedTwice     = []
  | S.member from had && not visitedTwice = map (from:) $ concatMap (findRoute routes (S.insert from had) True) neighbours
  | otherwise                             = map (from:) $ concatMap (findRoute routes (S.insert from had) visitedTwice) neighbours
  where
    neighbours = routes M.! from

findRoute1 :: M.Map String [String] -> [[String]]
findRoute1 conns = findRoute conns S.empty True "start"

solve1 :: [String] -> Int
solve1 = length . findRoute1 . M.fromListWith (++) . concatMap (unsafeParse parseLine)

findRoute2 :: M.Map String [String] -> [[String]]
findRoute2 conns = findRoute conns S.empty False "start"

solve2 :: [String] -> Int
solve2 = length . findRoute2 . M.fromListWith (++) . concatMap (unsafeParse parseLine)

main :: IO()
main = mainWrapper "day12" solve1 solve2
