module Main where

import Lib
import Grid
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import qualified Data.Map.Strict as M

manhattan :: Coord -> Coord -> Int
manhattan (a, b) (x, y) = abs (a - x) + abs (b - y)

astar :: Grid Int -> Coord -> Int
astar gr maxCoord = astar' maxCoord gr (PQ.singleton (manhattan (0, 0) maxCoord) ((0,0), 0)) S.empty (M.singleton (0,0)0)
  where
    astar' goal grid queue seen gscore
      | node == goal = gcost
      | S.member node seen = astar' goal grid queue' seen gscore
      | otherwise = astar' goal grid queue'' seen' gscore'
      where
        (node, gcost) = snd . PQ.findMin $ queue
        queue' = PQ.deleteMin queue
        seen' = S.insert node seen
        successorsAndCosts n gc = mapSnd (gc +) $ map (\x -> (x, grid M.! x)) $ neighbours dir4 grid n
        successors = filter (\(s, g) -> not (S.member s seen') && (not (s `M.member` gscore) || g < (fromJust . M.lookup s $ gscore))) $ successorsAndCosts node gcost
        queue'' = foldl (\q (s, g) -> PQ.insert (g + manhattan s goal) (s, g) q) queue' successors
        gscore' = foldl (\m (s, g) -> M.insert s g m) gscore successors

solve1 :: [String] -> Int
solve1 xs = astar gr maxCoord
  where
    gr = parseGrid digitToInt xs
    maxCoord = (length xs - 1, length xs - 1)

moveGrid :: Grid Int -> Coord -> Grid Int
moveGrid grid (a, b) = M.mapKeys (\(x, y) -> (x + 100 * a, y + 100 * b)) $ M.map (\x -> 1 + ((x + tot) `mod` 9)) grid
  where
    tot = a + b - 1

solve2 :: [String] -> Int
solve2 xs = astar gr maxCoord
  where
    grid = parseGrid digitToInt xs
    gr = foldl (\x y -> M.union (moveGrid grid y) x) M.empty [(a, b) | a <- [0..4], b <- [0..4]]
    maxCoord = (5 * length xs - 1, 5 * length xs - 1)

main :: IO()
main = mainWrapper "day15" solve1 solve2
