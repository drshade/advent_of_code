module AoC_2022_Day_06 where

import           Data.Set (fromList)
import           Utils    (read_data)

carrier :: Int -> Int -> String -> Int
carrier req pos str
  | length (fromList $ take req str) == req = pos + req
  | otherwise = carrier req (pos + 1) $ drop 1 str

run :: IO ()
run = do
  real_vals <- read_data id "data/AoC_2022_Day_06"
  putStrLn $ "Day6 Q1  => " ++ (show $ carrier 4 0 real_vals)
  putStrLn $ "     Q2  => " ++ (show $ carrier 14 0 real_vals)
