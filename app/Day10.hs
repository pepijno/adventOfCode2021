module Main where

import Lib
import Data.List
import Data.Maybe

errorChar :: String -> Maybe Char
errorChar = errorChar' []
  where
    errorChar' s (c:t) | c `elem` "([{<" = errorChar' (c:s) t
    errorChar' ('(':s) (')':t) = errorChar' s t
    errorChar' ('[':s) (']':t) = errorChar' s t
    errorChar' ('<':s) ('>':t) = errorChar' s t
    errorChar' ('{':s) ('}':t) = errorChar' s t
    errorChar' _ (')':_) = Just ')'
    errorChar' _ (']':_) = Just ']'
    errorChar' _ ('}':_) = Just '}'
    errorChar' _ ('>':_) = Just '>'
    errorChar' _ _ = Nothing

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore _ = 0

solve1 :: [String] -> Int
solve1 = sum . mapMaybe (fmap errorScore . errorChar)

completion :: String -> Maybe String
completion = completion' []
  where
    completion' s [] = Just s
    completion' s (c:t) | c `elem` "([{<" = completion' (c:s) t
    completion' ('(':s) (')':t) = completion' s t
    completion' ('[':s) (']':t) = completion' s t
    completion' ('<':s) ('>':t) = completion' s t
    completion' ('{':s) ('}':t) = completion' s t
    completion' _ _ = Nothing

charScore :: Char -> Int
charScore '(' = 1
charScore '[' = 2
charScore '{' = 3
charScore '<' = 4
charScore _ = 0

totalScore :: String -> Int
totalScore = foldl (\x -> (+) (5*x) . charScore) 0

-- solve2 :: [String] -> Int
solve2 = middle . sort . mapMaybe (fmap totalScore . completion)
  where
    middle l = l !! (length l `div` 2)

main :: IO()
main = mainWrapper "day10" solve1 solve2
