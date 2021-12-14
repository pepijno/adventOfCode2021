{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Parser
import qualified Data.Map.Strict as M

type Rules = M.Map (Char, Char) Char
type Polymer = M.Map (Char, Char) Int

parseRule :: Parser ((Char, Char), Char)
parseRule = do
  a <- anyChar
  b <- anyChar
  string " -> "
  c <- anyChar
  return ((a, b), c)

parsePolymer :: String -> Polymer
parsePolymer xs = M.fromListWith (+) $ zipWith (curry (,1)) xs (tail xs)

parseAll :: [String] -> (Polymer, Rules)
parseAll xs = (parsePolymer start, M.fromList $ map (unsafeParse parseRule) rules)
  where
    [[start], rules] = groupPairs xs

doStep :: Rules -> Polymer -> Polymer
doStep rules = M.foldlWithKey f M.empty
  where
    f pols x@(a, b) c = let n = rules M.! x
                         in M.insertWith (+) (n, b) c $ M.insertWith (+) (a, n) c pols

countChars :: Polymer -> [Int]
countChars pol = M.elems $ M.map (flip div 2 . (+1)) $ M.foldlWithKey f M.empty pol
  where
    f pols (a, b) n = M.insertWith (+) a n $ M.insertWith (+) b n pols

getResult :: Polymer -> Int
getResult = ((-) <$> maximum <*> minimum) . countChars

solve1 :: [String] -> Int
solve1 xs = getResult pol
  where
    (polymer, rules) = parseAll xs
    pol = nSteps 10 (doStep rules) polymer

solve2 :: [String] -> Int
solve2 xs = getResult pol
  where
    (polymer, rules) = parseAll xs
    pol = nSteps 40 (doStep rules) polymer

main :: IO()
main = mainWrapper "day14" solve1 solve2
