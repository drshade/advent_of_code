module AoC_2021_Day_01 where

import           System.IO (IOMode (ReadMode), hGetContents, openFile)

type IncreasedCount = Int

test_vals :: [Int]
test_vals = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

read_vals_from :: String -> IO [Int]
read_vals_from filename = do
  contents <- openFile filename ReadMode >>= hGetContents
  pure $ read <$> words contents

measure :: IncreasedCount -> [Int] -> IncreasedCount
measure m (c:ns@(n:_))
  | n > c = measure (m + 1) ns
  | otherwise = measure m ns
measure m _ = m

measure_window3 :: IncreasedCount -> [Int] -> IncreasedCount
measure_window3 m (c:ns@(x:y:z:_))
  | x + y + z > c + x + y = measure_window3 (m + 1) ns
  | otherwise = measure_window3 m ns
measure_window3 m _ = m

run :: IO ()
run = do
  real_vals <- read_vals_from "data/AoC_2021_Day_01"
  putStrLn $ "Day1 Q1 => " ++ show (measure 0 real_vals)
  putStrLn $ "Day1 Q2 => " ++ show (measure_window3 0 real_vals)
