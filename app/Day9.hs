module Main where

import Grid
import Lib
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

isLowest :: Grid Int -> Coord -> Bool
isLowest grid x = all ((>v) . (M.!) grid) $ neighbours dir4 grid x
  where
    v = grid M.! x

lowestPoints :: Grid Int -> Grid Int
lowestPoints grid = M.filterWithKey (\a b -> isLowest grid a) grid

solve1 :: [String] -> Int
solve1 = sum . map (+1) . M.elems . lowestPoints . parseGrid digitToInt

findBasin :: Grid Int -> Coord -> S.Set Coord
findBasin grid coord = findBasin' grid S.empty [coord]
  where
    findBasin' grid had [] = had
    findBasin' grid had (x:xs)
      | S.member x had = findBasin' grid had xs
      | val == 9 = findBasin' grid had xs
      | otherwise = findBasin' grid (S.insert x had) (xs ++ neighbours dir4 grid x)
      where
        val = grid M.! x

solve2 :: [String] -> Int
solve2 xs = product $ take 3 $ sortBy (flip compare) $ map (S.size . findBasin grid) $ M.keys $ lowestPoints grid
  where
    grid = parseGrid digitToInt xs

main :: IO()
main = mainWrapper "day9" solve1 solve2
