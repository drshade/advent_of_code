{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AoC_2022_Day_03 where

import           Data.List       (elemIndex)
import           Data.List.Split (chunksOf)
import           Data.Set        as Set (elemAt, fromList, intersection)
import           Utils           (read_data)

score :: Char -> Int
score x =
  let rank = ['a' .. 'z'] ++ ['A' .. 'Z']
   in maybe 0 ((+) 1) (elemIndex x rank)
   --          ^^^^^ because it's 1 indexed

calc :: Int -> [String] -> Int
calc acc (rucksack:xs) =
  let (first:second:_) =
        Set.fromList <$> chunksOf (length rucksack `div` 2) rucksack
      priority = score $ elemAt 0 $ first `intersection` second
   in calc (acc + priority) xs
calc acc _ = acc

calc' :: Int -> [String] -> Int
calc' acc group@(_:_:_:xs) =
  let (x:y:z:_) = Set.fromList <$> group
      priority = score $ elemAt 0 $ x `intersection` y `intersection` z
   in calc' (acc + priority) xs
calc' acc _ = acc

run :: IO ()
run = do
  real_vals <- read_data words "data/AoC_2022_Day_03"
  putStrLn $ "Day3 Q1  => " ++ show (calc 0 real_vals)
  putStrLn $ "Day3 Q2  => " ++ show (calc' 0 real_vals)
