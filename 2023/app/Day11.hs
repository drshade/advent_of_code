module Day11 where

import           AoC
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Handy               (WhichPuzzleInput (..), combinations,
                                      get_puzzle_input, unique)
import           Parsing             (run_parser, run_parser_with_state)
import           Text.Parsec         (Parsec, anyChar, char, choice, digit,
                                      getPosition, getState, letter, many1,
                                      newline, optional, sepBy, setState,
                                      sourceColumn, sourceLine, string, try,
                                      (<|>))

type Position = (Int, Int)

type Galaxy = Map.Map Position ()

parse_galaxy :: Parsec String () Galaxy
parse_galaxy = do
  galaxies <-
    many1 $ do
      pos <-
        (,) <$> (sourceColumn <$> getPosition) <*> (sourceLine <$> getPosition)
      (char '.' *> pure Nothing) <|> (char '#' *> pure (Just pos)) <|>
        (char '\n' *> pure Nothing)
  pure $ Map.fromList $ (, ()) <$> catMaybes galaxies

dimensions :: Galaxy -> (Int, Int, Int, Int)
dimensions galaxy =
  let xs = fst <$> Map.keys galaxy
      ys = snd <$> Map.keys galaxy
   in (minimum xs, maximum xs, minimum ys, maximum ys)

expand :: Int -> Galaxy -> Galaxy
expand size galaxy =
  let (min_x, max_x, min_y, max_y) = dimensions galaxy
      row y = catMaybes [Map.lookup (x, y) galaxy | x <- [min_x .. max_x]]
      col x = catMaybes [Map.lookup (x, y) galaxy | y <- [min_y .. max_y]]
      empty_cols =
        snd <$>
        filter ((==) True . fst) [(col x == [], x) | x <- [min_x .. max_x]]
      empty_rows =
        snd <$>
        filter ((==) True . fst) [(row y == [], y) | y <- [min_y .. max_y]]
      project (x, y) =
        ( x + (size * length (filter (< x) empty_cols))
        , y + (size * length (filter (< y) empty_rows)))
   in Map.mapKeys project galaxy

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

solve1 :: Galaxy -> Int
solve1 galaxy =
  let expanded = expand 1 galaxy
   in foldr (\(a:b:[]) total -> total + distance a b) 0 <$> combinations 2 $
      Map.keys expanded

solve2 :: Galaxy -> Int
solve2 galaxy =
  let expanded = expand 999999 galaxy
   in foldr (\(a:b:[]) total -> total + distance a b) 0 <$> combinations 2 $
      Map.keys expanded

solve :: IO (Solution Int)
solve = do
  input <- run_parser parse_galaxy <$> get_puzzle_input Mine 2023 11
  solution_1 <- pure $ solve1 input
  solution_2 <- pure $ solve2 input
  pure $
    SolvedTwo
      2023
      11
      solution_1
      (Revealed 9233514)
      solution_2
      (Revealed 363293506944)
