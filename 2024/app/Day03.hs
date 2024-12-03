module Day03 where

import           Data.Maybe  (catMaybes)
import           Handy
import           Text.Parsec (anyChar, char, choice, digit, many1, string, try)

data Instruction = Mul Int Int | Enable | Disable

parser :: Parser [Instruction]
parser = catMaybes <$> many1 (choice [ Just <$> try mul
                                     , Just <$> try enable
                                     , Just <$> try disable
                                     , Nothing <$ anyChar
                                     ])
    where mul = Mul <$> (string "mul(" *> val <* char ',')
                    <*> val <* char ')'
          enable = Enable <$ string "do()"
          disable = Disable <$ string "don't()"
          val = read <$> many1 digit

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 3
    pure $ foldr (\i acc -> case i of
                    Mul a b -> acc + a * b
                    _       -> acc) 0 input

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 3
    pure $ snd $ foldl (\(on, acc) i -> case i of
                        Mul a b -> if on then (on, acc + a * b) else (on, acc)
                        Enable  -> (True, acc)
                        Disable -> (False, acc)) (True, 0) input
