module Main where

import Lib
import Parser
import Data.Char

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show, Eq)

hexToBin :: Char -> [Char]
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin _ = "0000"

binToInt :: [Char] -> Int
binToInt = foldl (\x y -> 2 * x + y) 0 . map digitToInt

parsePacket :: Parser Packet
parsePacket = do
  version <- binToInt <$> takeN 3 anyChar
  typeID <- binToInt <$> takeN 3 anyChar
  case typeID of
    4 -> do
      n <- binToInt <$> parseNumber
      return (Literal version n)
    _ -> do
      lengthID <- anyChar
      case lengthID of
        '1' -> do
          n <- binToInt <$> takeN 11 anyChar
          packets <- takeN n parsePacket
          return (Operator version typeID packets)
        '0' -> do
          l <- binToInt <$> takeN 15 anyChar
          toParse <- takeN l anyChar
          return (Operator version typeID (unsafeParse (many1 parsePacket) toParse))

parseNumber :: Parser String
parseNumber = do
  header <- anyChar
  case header of
    '1' -> do
      bits <- takeN 4 anyChar
      rest <- parseNumber
      return $ bits ++ rest
    '0' -> takeN 4 anyChar

addVersions :: Packet -> Int
addVersions (Literal version _) = version
addVersions (Operator version _ ps) = version + sum (map addVersions ps)

evaluate :: Packet -> Int
evaluate (Literal _ n) = n
evaluate (Operator _ 0 ps) = sum $ map evaluate ps
evaluate (Operator _ 1 ps) = product $ map evaluate ps
evaluate (Operator _ 2 ps) = minimum $ map evaluate ps
evaluate (Operator _ 3 ps) = maximum $ map evaluate ps
evaluate (Operator _ 5 ps) = (\[a, b] -> if a > b then 1 else 0) $ take 2 $ map evaluate ps
evaluate (Operator _ 6 ps) = (\[a, b] -> if a < b then 1 else 0) $ take 2 $ map evaluate ps
evaluate (Operator _ 7 ps) = (\[a, b] -> if a == b then 1 else 0) $ take 2 $ map evaluate ps
evaluate Operator {} = 0

solve1 :: [String] -> Int
solve1 = addVersions . unsafeParse parsePacket . concatMap hexToBin . head

solve2 :: [String] -> Int
solve2 = evaluate . unsafeParse parsePacket . concatMap hexToBin . head

main :: IO()
main = mainWrapper "day16" solve1 solve2
