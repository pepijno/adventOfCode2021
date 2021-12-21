{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Parser
import Control.Monad
import qualified Data.Map.Strict as M

data Player = Player {
  position :: Int,
  score ::Int
  } deriving (Show, Ord, Eq)
data Side = Player1 | Player2 deriving (Show, Ord, Eq)
data Game = Game {
  currentPlayer :: Player,
  otherPlayer :: Player,
  lastRoll :: Int
  } deriving (Show, Ord, Eq)

parseLine :: Parser Player
parseLine = do 
  string "Player "
  integer
  string " starting position: "
  pos <- integer
  return $ Player pos 0

createGame :: Player -> Player -> Game
createGame p1 p2 = Game p1 p2 0

updatePlayer :: Player -> Int -> Player
updatePlayer (Player p s) r = let p' = updatePos p r
                               in Player p' (s + p')
  where
    updatePos p r = (p - 1 + r) `mod` 10 + 1

step :: Game -> Game
step (Game p1 p2 lastRoll) = Game p2 (updatePlayer p1 rolls) (lastRoll + 3)
  where
    rolls = 3 * lastRoll + 6

isOver :: Game -> Bool
isOver = (>=1000) . score . otherPlayer

answer :: Game -> Int
answer (Game (Player _ s1) (Player _ s2) lastRoll) = min s1 s2 * lastRoll

solve1 :: [String] -> Int
solve1 xs = answer $ until isOver step $ createGame p1 p2
  where
    [p1, p2] = map (unsafeParse parseLine) xs

allPosibilities :: Game -> M.Map Game Int
allPosibilities (Game p1 p2 _) = M.fromListWith (+) [ (Game p2 (updatePlayer p1 r) 0, 1) | r <- map sum $ replicateM 3 [1, 2, 3] ]

step2 :: (M.Map Game Int, (Int, Int)) -> (M.Map Game Int, (Int, Int))
step2 (m, (w1, w2)) = (newMapFiltered, (w2, w1 + wins))
  where
    newMap = M.foldlWithKey (\prev g v -> M.unionWith (+) prev $ M.map (*v) $ allPosibilities g) M.empty m
    newMapFiltered = M.filterWithKey (\(Game _ (Player _ s2) _) _ -> s2 < 21) newMap
    wins = sum $ M.elems $ M.filterWithKey (\(Game _ (Player _ s2) _) _ -> s2 >= 21) newMap

solve2 :: [String] -> Int
solve2 xs = uncurry max $ snd $ until (M.null . fst) step2 (gameAsList, (0, 0))
  where
    [p1, p2] = map (unsafeParse parseLine) xs
    gameAsList = M.fromList [(createGame p1 p2, 1)]

main :: IO()
main = mainWrapper "day21" solve1 solve2
