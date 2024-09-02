module Day01 where

import Text.Parsec (Parsec, char, many, parse, (<|>))

floors :: Parsec String () [Int]
floors = many $ char '(' *> pure 1 <|> char ')' *> pure (-1)

part1 :: String -> Int
part1 = either (error . show) sum . parse floors "input"

part2 :: String -> Int
part2 = either (error . show) basement . parse floors "input"
 where
  basement = length . takeWhile (/= -1) . scanl (+) 0