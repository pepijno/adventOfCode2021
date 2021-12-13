module Main where

import Lib
import Parser
import qualified Data.Set as S
import Control.Arrow

data Axis = X | Y deriving (Show, Eq)
data Fold = Fold Axis Int deriving (Show, Eq)

parseDot :: Parser (Int, Int)
parseDot = (,) <$> integer <* char ',' <*> integer

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  axis <- anyChar
  char '='
  i <- integer
  return $ Fold (if axis == 'x' then X else Y) i

doFold :: S.Set (Int, Int) -> Fold -> S.Set (Int, Int)
doFold dots foldAlong = case foldAlong of
  Fold X x -> S.map (folding x . fst &&& snd) dots
  Fold Y y -> S.map (fst &&& folding y . snd) dots
  where
    folding v d = v - abs (v - d)

solve1 :: [String] -> Int
solve1 xs = S.size $ doFold dots $ head folds
  where
    folds = map (unsafeParse parseFold) $ flip (!!) 1 $ groupPairs xs
    dots = S.fromList $ map (unsafeParse parseDot) $ head $ groupPairs xs

printDots :: S.Set (Int, Int) -> String
printDots dots = unlines $ map (\y -> concatMap (\x -> if S.member (x, y) dots then "#" else ".") [0..maxX]) [0..maxY]
  where
    maxX = S.findMax $ S.map fst dots
    maxY = S.findMax $ S.map snd dots

solve2 :: [String] -> String
solve2 xs = printDots $ foldl doFold dots folds
  where
    folds = map (unsafeParse parseFold) $ flip (!!) 1 $ groupPairs xs
    dots = S.fromList $ map (unsafeParse parseDot) $ head $ groupPairs xs

main :: IO()
main = mainWrapper "day13" solve1 solve2
