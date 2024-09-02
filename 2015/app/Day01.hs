module Day01 where

import Text.Parsec (Parsec, many1, parse, string, (<|>))

floors :: Parsec String () [Int]
floors = many1 (string "(" *> pure 1 <|> string ")" *> pure (-1))

part1 :: String -> Int
part1 = either (error . show) sum . parse floors "input"

part2 :: String -> Int
part2 = either (error . show) basement . parse floors "input"
  where
    basement = (1 +) . length . takeWhile (/= -1) . scanl1 (+)