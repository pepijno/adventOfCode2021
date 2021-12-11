{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Grid
import Data.Char
import qualified Data.Map.Strict as M

increaseEnergy :: M.Map Coord Int -> M.Map Coord Int
increaseEnergy = M.map (+1)

doFlash :: M.Map Coord Int -> M.Map Coord Int
doFlash = M.map (const 0) . M.filter (>=10)

flashNeighbours :: M.Map Coord Int -> M.Map Coord Int -> [Coord]
flashNeighbours all grid = concatMap (neighbours dir8 all) $ M.keys grid

ripple :: [Coord] -> M.Map Coord Int -> M.Map Coord Int
ripple n = M.mapWithKey f
  where
    f p x
      | x /= 0 = x + count (==p) n
      | otherwise = x

drain :: M.Map Coord Int -> M.Map Coord Int
drain = M.map f
  where
    f x
      | x >= 10 = 0
      | otherwise = x

doStep :: M.Map Coord Int -> M.Map Coord Int
doStep grid = snd $ until (M.null . fst) m (f'', r')
  where
    l' = increaseEnergy grid
    f' = flashNeighbours grid $ doFlash l'
    f'' = M.fromList $ map (, 0) f'
    r' = ripple f' (drain l')
    m (_, r) = let f = doFlash r
               in  (f, ripple (flashNeighbours grid f) (drain r))

solve1 :: [String] -> Int
solve1 xs = fst $ flip (!!) 100 $ iterate step (0, grid)
  where
    grid = parseGrid digitToInt xs
    step (s, p) = let p' = doStep p
                   in (s + count (==0) (map snd $ M.toList p'), p')

solve2 :: [String] -> Int
solve2 xs = length $ takeWhile ((/=0) . sum . M.elems) $ iterate doStep grid
  where
    grid = parseGrid digitToInt xs

main :: IO()
main = mainWrapper "day11" solve1 solve2
