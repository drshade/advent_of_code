module Day06 where

import Data.Vector.Unboxed (Vector, filter, generate, length, sum, (!), (//))
import Text.Parsec (Parsec, char, choice, digit, many1, newline, optional, runParser, string, try)
import Prelude hiding (filter, length, sum)

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

processActions :: Vector Bool -> [Action] -> Vector Bool
processActions vec [] = vec
processActions vec (action : actions) =
    processActions (single action) actions
  where
    single (TurnOn start end) = modify start end (const True)
    single (TurnOff start end) = modify start end (const False)
    single (Toggle start end) = modify start end not
    modify (Coord sx sy) (Coord ex ey) f =
        vec // [(pos x y, f $ vec ! pos x y) | x <- [sx .. ex], y <- [sy .. ey]]
      where
        pos x y = x * 1000 + y

processActions' :: Vector Int -> [Action] -> Vector Int
processActions' vec [] = vec
processActions' vec (action : actions) =
    processActions' (single action) actions
  where
    single (TurnOn start end) = modify start end (+ 1)
    single (TurnOff start end) = modify start end (\v -> if v == 0 then 0 else v - 1)
    single (Toggle start end) = modify start end (+ 2)
    modify (Coord sx sy) (Coord ex ey) f =
        vec // [(pos x y, f $ vec ! pos x y) | x <- [sx .. ex], y <- [sy .. ey]]
      where
        pos x y = x * 1000 + y

part1 :: IO Int
part1 = do
    input <- readFile "data/Day06_input.txt"
    let parse = either (error . show) id . runParser parseActions () "input"
        emptyVec = generate (1000 * 1000) $ const False
     in pure $ length $ filter ((==) True) $ processActions emptyVec $ parse input

part2 :: IO Int
part2 = do
    input <- readFile "data/Day06_input.txt"
    let parse = either (error . show) id . runParser parseActions () "input"
        emptyVec = generate (1000 * 1000) $ const 0
     in pure $ sum $ processActions' emptyVec $ parse input
