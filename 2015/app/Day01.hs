module Day01 where

import Control.Monad (guard)
import Data.Functor (($>)) -- thanks Damien!
import Text.Parsec

-- Super simple parse the floors as +1 for '(' and -1 for ')'

floors :: Parsec String () [Int]
floors = many $ char '(' $> 1 <|> char ')' $> -1

part1 :: String -> Int
part1 = either (error . show) sum . parse floors "input"

part2 :: String -> Int
part2 = either (error . show) basement . parse floors "input"
 where
  basement = length . takeWhile (/= -1) . scanl (+) 0

-- A totally different approach now using Parsec's stateful parser
-- (i.e. calculating the floor number while parsing, and stopping if we
-- hit the basement)

part1parser :: Parsec String Int Int
part1parser =
  (many $ char '(' *> modifyState succ <|> char ')' *> modifyState pred) *> getState

part1' :: String -> Int
part1' = either (error . show) id . runParser part1parser 0 "input"

part2parser :: Parsec String Int Int
part2parser =
  walk_floor `manyTill` basement *> (pred . sourceColumn <$> getPosition)
 where
  walk_floor :: Parsec String Int ()
  walk_floor = char '(' *> modifyState succ <|> char ')' *> modifyState pred
  basement :: Parsec String Int ()
  basement = do
    level <- getState
    guard (level < 0) <|> fail "not at basement"

-- too dirty to write this?
--
-- basement' :: Parsec String Int ()
-- basement' = (getState >>= guard . (< 0)) <|> fail "not at basement"

part2' :: String -> Int
part2' = either (error . show) id . runParser part2parser 0 "input"