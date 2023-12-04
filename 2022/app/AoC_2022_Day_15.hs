{-# LANGUAGE ScopedTypeVariables #-}

module AoC_2022_Day_15 where

import           AoC
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import           Data.Function       (on)
import           Data.List           as List (elem, filter, find, foldr,
                                              intercalate, length, maximum,
                                              minimum, minimumBy, sortBy)
import           Data.Map            (Map)
import           Data.Map            as Map (empty, insert, keys, lookup)
import           Data.Maybe          (fromMaybe)
import           Text.Parsec         (Parsec, char, digit, many1, newline,
                                      optionMaybe, optional, runParser, string)
import           Utils               (chunk, range, read_data, takeUntilM,
                                      (<$$>))

test :: String
test =
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

type Position = (Int, Int)

parser :: Parsec String () [(Position, Position)]
parser =
  let neg_value = do
        sign <- optionMaybe $ char '-'
        value :: Int <- read <$> many1 digit
        pure $ maybe value (const $ value * (-1)) sign
      entry = do
        s_x <- string "Sensor at x=" *> neg_value <* string ", y="
        s_y <- neg_value <* string ": closest beacon is at x="
        b_x <- neg_value <* string ", y="
        b_y <- neg_value <* (optional newline)
        return ((s_x, s_y), (b_x, b_y))
   in many1 $ entry

dimensions :: [(Position, Position)] -> (Int, Int, Int, Int)
dimensions coords =
  let all_flat = join $ (\e -> [fst e, snd e]) <$> coords
      min_x = List.minimum $ (fst <$> all_flat)
      min_y = List.minimum $ (snd <$> all_flat)
      max_x = List.maximum $ (fst <$> all_flat)
      max_y = List.maximum $ (snd <$> all_flat)
   in (min_x, min_y, max_x, max_y)

manhattan_distance :: Position -> Position -> Int
manhattan_distance (x, y) (x', y') =
  (if x > x'
     then x - x'
     else x' - x) +
  (if y > y'
     then y - y'
     else y' - y)

closest_sensors :: [(Position, Position)] -> Position -> [(Position, Position)]
closest_sensors coords to_pos =
  List.sortBy (compare `on` manhattan_distance to_pos . fst) (coords)

closest_sensor :: [(Position, Position)] -> Position -> Position
closest_sensor coords to_pos =
  List.minimumBy
    (\a b -> compare (manhattan_distance a to_pos) (manhattan_distance b to_pos))
    (fst <$> coords)

is_sensor :: [(Position, Position)] -> Position -> Bool
is_sensor coords pos = pos `List.elem` (fst <$> coords)

is_beacon :: [(Position, Position)] -> Position -> Bool
is_beacon coords pos = pos `List.elem` (snd <$> coords)

is_blocked :: [(Position, Position)] -> Position -> Bool
is_blocked coords pos =
  let closest = closest_sensors coords pos
      y =
        List.filter
          (\(s, b) -> manhattan_distance pos s <= manhattan_distance b s)
          closest
   in length y > 0

print_sim :: [(Position, Position)] -> String
print_sim coords =
  let (min_x, min_y, max_x, max_y) = dimensions coords
      all_xy = [(x, y) | y <- [min_y .. max_y], x <- [min_x .. max_x]]
      print pos =
        if is_sensor coords pos
          then 'S'
          else if is_beacon coords pos
                 then 'B'
                 else if is_blocked coords pos
                        then '.'
                        else '_'
      a = chunk (max_x - min_x + 1) all_xy
      b :: [String]
      b = (print) <$$> a
      z = intercalate "\n" $ b
   in z

-- for each position (which is not a beacon or sensor):
-- for each sensor:
-- am I closer to the sensor than the that sensors closest beacon?
run :: IO ()
run = do
  input <- run_parser parser <$> get_puzzle_input Example1 2022 15
  putStrLn $ show input
  putStrLn $ show $ closest_sensors input (0, 0)
  let dim@(min_x, min_y, max_x, max_y) = dimensions input
  putStrLn $ show dim
  putStrLn $ print_sim input
  let scanline =
        List.filter
          (\p ->
             not (is_beacon input p) &&
             not (is_sensor input p) && (is_blocked input p))
          [(x, 2000000) | x <- [min_x - 100000 .. max_x + 100000]]
  putStrLn $ "Day15 Q1 => " <> (show $ length scanline)
-- 4062165 was too low
-- 4062164 was too low
