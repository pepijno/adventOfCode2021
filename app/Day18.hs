module Main where

import Lib
import Parser

data Snail = Leaf Int | Node Snail Snail deriving (Eq, Show)
data Split = HasSplit Snail | NotSplit deriving (Eq, Show)
data Explode = HasExploded | NotExploded | Add (Int, Int) | CarryLeft Int | CarryRight Int deriving (Eq, Show)

parseNumber :: Parser Snail
parseNumber = leaf <|> node
  where
    leaf = Leaf <$> integer
    node = Node <$> (char '[' *> parseNumber <* char ',') <*> parseNumber <* char ']'

add :: Snail -> Snail -> Snail
add = Node

splitSnail :: Snail -> Split
splitSnail (Leaf n)
  | n >= 10 = HasSplit $ Node (Leaf $ n `div` 2) (Leaf $ (n + 1) `div` 2)
  | otherwise = NotSplit
splitSnail (Node i j) = case splitSnail i of
  HasSplit s -> HasSplit $ Node s j
  NotSplit -> case splitSnail j of
    HasSplit s -> HasSplit $ Node i s
    NotSplit -> NotSplit

explodeSnail :: Int -> Snail -> (Snail, Explode)
explodeSnail _ (Leaf l) = (Leaf l, NotExploded)
explodeSnail 4 (Node (Leaf i) (Leaf j)) = (Leaf 0, Add (i, j))
explodeSnail n (Node i j) = case explodeSnail (n + 1) i of
  (k, Add (x, y)) -> (Node k $ addLeft y j, CarryLeft x)
  (k, CarryLeft l) -> (Node k j, CarryLeft l)
  (k, CarryRight r) -> (Node k $ addLeft r j, HasExploded)
  (k, HasExploded) -> (Node k j, HasExploded)
  (_, NotExploded) -> case explodeSnail (n + 1) j of
    (k, Add (x, y)) -> (Node (addRight i x) k, CarryRight y)
    (k, CarryLeft l) -> (Node (addRight i l) k, HasExploded)
    (k, CarryRight r) -> (Node i k, CarryRight r)
    (k, HasExploded) -> (Node i k, HasExploded)
    (_, NotExploded) -> (Node i j, NotExploded)

addLeft :: Int -> Snail -> Snail
addLeft a (Leaf b) = Leaf (a + b)
addLeft a (Node b c) = Node (addLeft a b) c

addRight :: Snail -> Int -> Snail
addRight (Leaf b) a = Leaf (a + b)
addRight (Node b c) a = Node b (addRight c a)

reduceSnail :: Snail -> Snail
reduceSnail s = case explodeSnail 0 s of
  (n, NotExploded) -> case splitSnail n of
    HasSplit m -> reduceSnail m
    NotSplit -> n
  (n, _) -> reduceSnail n

explodeUntil :: Snail -> Snail
explodeUntil s = fst $ until ((==NotExploded) . snd) (explodeSnail 0 . fst) (s, HasExploded)

magnitude :: Snail -> Int
magnitude (Leaf l) = l
magnitude (Node i j) = 3 * magnitude i + 2 * magnitude j

solve1 :: [String] -> Int
solve1 = magnitude . foldl1 (\x -> reduceSnail . add x) . map (unsafeParse parseNumber)

solve2 :: [String] -> Int
solve2 xs = maximum [ magnitude $ reduceSnail (add i j) | i <- numbers, j <- numbers, i /= j]
  where
    numbers = map (unsafeParse parseNumber) xs

main :: IO()
main = mainWrapper "day18" solve1 solve2
