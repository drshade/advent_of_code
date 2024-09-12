module Day06 where

import Data.Vector.Unboxed (Vector, generate, sum, (!), (//))
import Text.Parsec (Parsec, char, choice, digit, many1, newline, optional, runParser, string, try)
import Prelude hiding (sum)

data Coord = Coord Int Int
data Action
    = Toggle Coord Coord
    | TurnOff Coord Coord
    | TurnOn Coord Coord

parseCoord :: Parsec String () Coord
parseCoord = do
    start <- read <$> many1 digit
    _ <- char ','
    end <- read <$> many1 digit
    pure $ Coord start end

parseActions :: Parsec String () [Action]
parseActions =
    many1 $
        choice
            [ try $ string "toggle " *> (go Toggle)
            , try $ string "turn on " *> (go TurnOn)
            , try $ string "turn off " *> (go TurnOff)
            ]
  where
    go action =
        action
            <$> (parseCoord <* string " through ")
            <*> (parseCoord <* optional newline)

processActions :: Vector Int -> [Action] -> Vector Int
processActions vec [] = vec
processActions vec (action : actions) =
    processActions (single action) actions
  where
    single (TurnOn start end) = modify start end (const 1)
    single (TurnOff start end) = modify start end (const 0)
    single (Toggle start end) = modify start end (\x -> if x == 0 then 1 else 0)
    modify (Coord sx sy) (Coord ex ey) f =
        vec // [(x * 1000 + y, f $ vec ! (x * 1000 + y)) | x <- [sx .. ex], y <- [sy .. ey]]

part1 :: IO Int
part1 = do
    input <- readFile "data/Day06_input.txt"
    let parse = either (error . show) id . runParser parseActions () "input"
        emptyVec = generate (1000 * 1000) $ const 0
     in pure $ sum $ processActions emptyVec $ parse input
