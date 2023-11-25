module AoC_2022_Day_01 where

import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           System.IO       (IOMode (ReadMode), hGetContents, openFile)
import           Utils           (read_data)

read_vals_from :: String -> IO [String]
read_vals_from filename = do
  contents <- openFile filename ReadMode >>= hGetContents
  pure $ splitOn "\n" contents

build :: Int -> [Int] -> [String] -> [Int]
build av ao ("":vs) = build 0 (av : ao) vs
build av ao (v:vs)  = build (av + (read v)) ao vs
build _ ao _        = ao

measure_top_3 :: [String] -> Int
measure_top_3 s = sum $ take 3 $ reverse $ sort $ build 0 [] s

measure_max :: [String] -> Int
measure_max s = foldr max 0 $ build 0 [] s

run :: IO ()
run = do
  real_vals <- read_data (splitOn "\n") "data/AoC_2022_Day_01"
  putStrLn $ "Day1 Q1  => " ++ show (measure_max real_vals)
  putStrLn $ "Day1 Q2  => " ++ show (measure_top_3 real_vals)
