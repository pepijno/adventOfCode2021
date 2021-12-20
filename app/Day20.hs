module Main where

import Lib
import Grid
import qualified Data.Map.Strict as M

scannedArea :: Bool -> Grid Char -> Coord -> String
scannedArea onOff grid pos = [
  M.findWithDefault (if onOff then '#' else '.') (pos !+ (-1, -1)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ (-1,  0)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ (-1,  1)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 0, -1)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 0,  0)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 0,  1)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 1, -1)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 1,  0)) grid,
  M.findWithDefault (if onOff then '#' else '.') (pos !+ ( 1,  1)) grid
  ]

scanToInt :: String -> Int
scanToInt = foldl ((+) . (2*)) 0 . map toInt
  where
    toInt '.' = 0
    toInt _ = 1

padGrid :: Bool -> Grid Char -> Grid Char
padGrid onOff grid = M.union incGrid $ M.fromList [((x, y), if onOff then '#' else '.') | x <- [0..(1 + maxX)], y <- [0..(1 + maxY)], not (1 <= x && x <= maxX && 1 <= y && y <= maxY)]
  where
    incGrid = M.mapKeys (!+ (1, 1)) grid
    maxX = maximum $ map fst $ M.keys incGrid
    maxY = maximum $ map snd $ M.keys incGrid

lookupAlgo :: String -> Int -> Char
lookupAlgo algo index = algo !! index

transform :: Bool -> String -> Grid Char -> Grid Char
transform onOff algo grid = M.mapWithKey (\k _ -> lookupAlgo algo $ scanToInt $ scannedArea onOff bordered k) bordered
  where bordered = padGrid onOff grid
 
steps :: Int -> Bool -> String -> Grid Char -> Grid Char
steps 0 _     _    grid = grid
steps n onOff algo grid = steps (n - 1) (not onOff) algo nextStep
  where nextStep = transform onOff algo grid

solve1 :: [String] -> Int
solve1 xs = M.size $ M.filter (=='#') $ steps 2 False a $ parseGrid id image
  where
    [[a], image] = groupPairs xs

solve2 :: [String] -> Int
solve2 xs = M.size $ M.filter (=='#') $ steps 50 False a $ parseGrid id image
  where
    [[a], image] = groupPairs xs

main :: IO()
main = mainWrapper "day20" solve1 solve2
