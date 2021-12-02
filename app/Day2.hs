module Main where

import Lib
import Parser

data Instruction = Forward Int | Down Int | Up Int deriving (Eq, Show)

parseInstruction :: Parser Instruction
parseInstruction = Forward <$> match "forward" <|> Down <$> match "down" <|> Up <$> match "up"
  where
    match s = string s *> whiteSpace *> integer

parseAll :: [String] -> [Instruction]
parseAll = map (unsafeParse parseInstruction)

applyInstruction :: (Int, Int) -> Instruction -> (Int, Int)
applyInstruction (pos, depth) instruction = case instruction of
  Forward x -> (pos + x, depth)
  Down x -> (pos, depth + x)
  Up x -> (pos, depth - x)

mult :: (Int, Int) -> Int
mult (x, y) = x * y

solve1 :: [String] -> Int
solve1 = mult . foldl applyInstruction (0, 0) . parseAll

applyInstructionAim :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
applyInstructionAim (pos, depth, aim) instruction = case instruction of
  Forward x -> (pos + x, depth + aim * x, aim)
  Down x -> (pos, depth, aim + x)
  Up x -> (pos, depth, aim -x)

mult2 :: (Int, Int, Int) -> Int
mult2 (x, y, _) = x * y

solve2 :: [String] -> Int
solve2 = mult2 . foldl applyInstructionAim (0, 0, 0) . parseAll

main :: IO()
main = mainWrapper "day2" solve1 solve2
