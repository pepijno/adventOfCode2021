module Main where

import Data.Function
import Data.List.Split
import Data.List
import Parser
import Lib

data BingoNumber = NotDrawn Int | Drawn Int deriving (Show, Ord, Eq)
type BingoRow = [BingoNumber]
type BingoBoard = [BingoRow]

parseNumbers :: [String] -> [Int]
parseNumbers xs = (map read . splitOn "," . head $ head $ pairs)
  where
    pairs = groupPairs xs

parseNumberRow :: Parser BingoRow
parseNumberRow = do
  many whiteSpace
  n1 <- NotDrawn <$> natural
  many whiteSpace
  n2 <- NotDrawn <$> natural
  many whiteSpace
  n3 <- NotDrawn <$> natural
  many whiteSpace
  n4 <- NotDrawn <$> natural
  many whiteSpace
  n5 <- NotDrawn <$> natural
  return [n1, n2, n3, n4, n5]

parseBoards :: [String] -> [BingoBoard]
parseBoards xs = map (map (unsafeParse parseNumberRow)) $ tail $ pairs
  where
    pairs = groupPairs xs

drawNumber :: BingoBoard -> Int -> BingoBoard
drawNumber board num = map (map replace) board
  where
    replace n = case n of
      NotDrawn x
        | x == num -> Drawn num
        | otherwise -> NotDrawn x
      x -> x

isDrawn :: BingoNumber -> Bool
isDrawn (Drawn _) = True
isDrawn (NotDrawn _) = False

hasWon :: BingoBoard -> Bool
hasWon = ((||) `on` (any hasRowWon)) <$> id <*> transpose
  where
    hasRowWon = all isDrawn

totalUnDrawn :: BingoBoard -> Int
totalUnDrawn board = sum [n | (NotDrawn n) <- concat board]

drawUntilWon :: [Int] -> [BingoBoard] -> (Int, [Int], BingoBoard, [BingoBoard])
drawUntilWon (n:ns) boards = case wonBoards of
  (board:_) -> (n, ns, board, lostBoards)
  [] -> drawUntilWon ns boards'
  where
    boards' = map (flip drawNumber n) boards
    wonBoards = filter hasWon boards'
    lostBoards = filter (not . flip elem wonBoards) boards'

firstWonBoard :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
firstWonBoard ns boards = (n, board)
  where
    (n, _, board, _) = drawUntilWon ns boards

getResult :: (Int, BingoBoard) -> Int
getResult (n, board) = n * (totalUnDrawn board)

solve1 :: [String] -> Int
solve1 xs = getResult $ firstWonBoard numbers boards
  where
    numbers = parseNumbers xs
    boards = parseBoards xs

lastWonBoard :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
lastWonBoard ns boards
  | null boards' = (wonNumber, wonBoard)
  | otherwise = lastWonBoard ns' boards'
  where
    (wonNumber, ns', wonBoard, boards') = drawUntilWon ns boards

solve2 :: [String] -> Int
solve2 xs = getResult $ lastWonBoard numbers boards
  where
    numbers = parseNumbers xs
    boards = parseBoards xs

main :: IO()
main = mainWrapper "day4" solve1 solve2
