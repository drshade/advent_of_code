{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module AoC_2022_Day_12 where

import           Algorithm.Search (dijkstra)
import           Control.Monad    (join)
import           Data.Char        (ord)
import           Data.List.Extra  (find, (!?))
import           Data.Maybe       (catMaybes)
import           Utils            (read_data, trim, (<$->))

test :: String
test = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

type Offset = (Int, Int)

data Description
  = Start
  | Finish
  | Height Int
  deriving (Show, Eq, Ord)

type Position = (Description, Int, Int)

type Grid = [[Char]]

type State = (Grid, Position)

-- We swap the start & end positions so that we're solving from the end
-- to the beginning rather than the other way around
-- This is just so we can nail the second question easier :)
-- 'a' is the heightest (25) and 'z' is the lowest (0)
block :: Char -> Description
block 'E' = Start
block 'S' = Finish
block c   = Height $ (ord 'z' - ord 'a') - (ord c - ord 'a')

fetch :: Grid -> (Int, Int) -> Maybe Position
fetch grid (x, y) = (\b -> (block b, x, y)) <$> (!? y) <$-> (grid !? x)

look :: Grid -> Position -> Offset -> Maybe Position
look grid (_, x, y) (ox, oy) = fetch grid (x + ox, y + oy)

dimensions :: Grid -> (Int, Int)
dimensions grid = (length grid, length $ grid !! 0)

-- Find the starting point
start :: Grid -> Maybe State
start grid =
  let (xlen, ylen) = dimensions grid
      every =
        (\p -> (grid, p)) <$>
        (catMaybes $ [fetch grid (x, y) | x <- [0 .. xlen], y <- [0 .. ylen]])
   in find (\(_, (b, _, _)) -> b == Start) every

-- The rules of traversal from one point to another
walkable :: Description -> Description -> Bool
walkable Start (Height 0)      = True
walkable (Height 25) Finish    = True
walkable (Height s) (Height e) = e - s <= 1
walkable _ _                   = False

neighbours :: State -> [State]
neighbours (grid, pos@(cur_block, _, _)) =
  let x = catMaybes $ look grid pos <$> [(-1, 0), (1, 0), (0, -1), (0, 1)]
   in (\p -> (grid, p)) <$>
      (filter (\(new_block, _, _) -> walkable cur_block new_block) x)

solve :: Grid -> Description -> Maybe (Int, [State])
solve grid endblock =
  let cost_function = const $ const 1
      end_function = (\(_, (block', _, _)) -> block' == endblock)
   in case start grid of
        Nothing     -> Nothing
        Just start' -> dijkstra neighbours cost_function end_function start'

run :: IO ()
run = do
  grid <- read_data (lines) "data/AoC_2022_Day_12"
  case solve grid Finish of
    Nothing -> putStrLn "Day 12 Q1 => Failed"
    Just (steps, state) -> do
      putStrLn $ "Day12 Q1 => " <> show steps
  case solve grid $ Height 25 of
    Nothing -> putStrLn "Day 12 Q2 => Failed"
    Just (steps, state) -> do
      putStrLn $ "Day12 Q2 => " <> show steps
  pure ()
