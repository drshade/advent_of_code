module Day03 where

import Control.Arrow ((***))
import Data.Functor (($>))
import Data.List (partition)
import Data.Map (Map, empty, fromList, insertWith)
import Text.Parsec (char, choice, many1, parse)
import Text.Parsec.String (Parser, parseFromFile)

stepsParser :: Parser [(Int, Int)]
stepsParser =
    many1 $
        choice
            [ (char '^' $> (0, 1))
            , (char 'v' $> (0, -1))
            , (char '<' $> (-1, 0))
            , (char '>' $> (1, 0))
            ]

-- my cool add-two-houses-together function
(+-+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) +-+ (x2, y2) = (x1 + x2, y1 + y2)

part1 :: IO Int
part1 = do
    steps <- either (error . show) id <$> parseFromFile stepsParser "data/Day03_input.txt"
    let route = scanl (+-+) (0, 0) steps
    -- Use a Map to keep track of house visits
    let visitedHouses = foldl (\m k -> insertWith (+) k (1 :: Int) m) empty route
    pure $ length visitedHouses

part2 :: IO Int
part2 = do
    steps <- either (error . show) id <$> parseFromFile stepsParser "data/Day03_input.txt"
    -- Nasty partitioning!
    -- splitting the input into two lists by whether index is odd, and then unpack back to original elements yuck
    -- originally I did this: (\(r, s) -> (snd <$> r, snd <$> s)) $ partition (odd . fst) $ zip [0 :: Int ..] steps
    -- but then wanted to find a nicer way and used arrows :)
    let (robo, og) = ((snd <$>) *** (snd <$>)) $ partition (odd . fst) $ zip [0 :: Int ..] steps
    let (roboRoute, ogRoute) = (scanl (+-+) (0, 0) robo, scanl (+-+) (0, 0) og)
    let visitedHouses = foldl (\m k -> insertWith (+) k (1 :: Int) m) empty (roboRoute <> ogRoute)
    pure $ (length $ visitedHouses)
