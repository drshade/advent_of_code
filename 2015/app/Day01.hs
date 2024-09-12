module Day01 where

import Text.Parsec (Parsec, char, getState, many, modifyState, parse, runParser, (<|>))

floors :: Parsec String () [Int]
floors = many $ char '(' *> pure 1 <|> char ')' *> pure (-1)

part1 :: String -> Int
part1 = either (error . show) sum . parse floors "input"

part2 :: String -> Int
part2 = either (error . show) basement . parse floors "input"
  where
    basement = length . takeWhile (/= -1) . scanl (+) 0

----- A totally different approach now using Parsec's stateful parser (i.e. calculating the floor number while parsing)

part1parser :: Parsec String Int Int
part1parser =
    getState <* (many $ char '(' *> modifyState succ <|> char ')' *> modifyState pred)

part1' :: String -> Int
part1' = either (error . show) id . runParser part1parser 0 "input"
