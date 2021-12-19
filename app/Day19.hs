{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Parser
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Ord
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Point = (Int, Int, Int)
data Axis = X | Y | Z deriving (Show, Eq, Ord)

(!+) (a, b, c) (x, y, z) = (a + x, b + y, c + z)
(!-) (a, b, c) (x, y, z) = (a - x, b - y, c - z)

parsePoint :: Parser Point
parsePoint = do
  x <- integer
  char ','
  y <- integer
  char ','
  z <- integer
  return (x, y ,z)

rotatePoint :: Axis -> Point -> Point
rotatePoint X (x, y, z) = (x, -z, y)
rotatePoint Y (x, y, z) = (z, y, -x)
rotatePoint Z (x, y, z) = (y, -x, z)

orientationsPoint :: Point -> [Point]
orientationsPoint p = map (foldl (flip rotatePoint) p) rotations
  where
    rotations = [ [], [X], [Y], [Z], [X, X], [X, Y], [X, Z], [Y, X], [Y, Y], [Z, Y], [Z, Z], [X, X, X], [X, X, Y], [X, X, Z], [X, Y, X], [X, Y, Y], [X, Z, Z], [Y, X, X], [Y, Y, Y], [Z, Z, Z], [X, X, X, Y], [X, X, Y, X], [X, Y, X, X], [X, Y, Y, Y]]

orientations :: [Point] -> [[Point]]
orientations ps = transpose $ map orientationsPoint ps

overlap :: [Point] -> [Point] -> [Point]
overlap ps1 ps2 = M.keys . M.filter (>=12) . M.fromListWith (+) . map (,1) $ (!-) <$> ps1 <*> ps2

triangulate :: [([Point], Point)] -> [[Point]] -> [[Point]] -> [([Point], Point)]
triangulate result _ [] = result
triangulate result (ref:refs) scanners = triangulate (found ++ result) (map fst found ++ refs) notFound
  where
    (found, notFound) = partitionEithers
          [ maybe (Right scanner) Left . s $ triangulate2 ref scanner
          | scanner <- scanners
          ]
    s f = case f of
      x : _ -> Just x
      [] -> Nothing
triangulate _ _ _ = []

triangulate2 :: [Point] -> [Point] -> [([Point], Point)]
triangulate2 a b = [(map (pos !+) o, pos) | o <- orientations b, pos <- overlap a o]

manhattan :: Point -> Point -> Int
manhattan (a, b, c) (x, y, z) = abs (a - x) + abs (b - y) + abs (c - z)

solve1 :: [String] -> Int
solve1 xs = let (s:ss) = map (map (unsafeParse parsePoint) . tail) $ groupPairs xs
             in length $ nub $ concatMap fst $ triangulate [(s, (0, 0, 0))] [s] ss

solve2 :: [String] -> Int
solve2 xs = let (s:ss) = map (map (unsafeParse parsePoint) . tail) $ groupPairs xs
                points = map snd $ triangulate [(s, (0, 0, 0))] [s] ss
             in maximum [manhattan x y | x <- points, y <- points]

main :: IO()
main = mainWrapper "day19" solve1 solve2
