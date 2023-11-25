module AoC_2022_Day_02 where

import           Data.Char (ord)
import           Utils     (read_data)

calc :: Int -> [String] -> Int
calc a ("A":"X":xs) = calc (a + 1 + 3) xs -- 1 1 => 4
calc a ("B":"Y":xs) = calc (a + 2 + 3) xs -- 2 2 => 5
calc a ("C":"Z":xs) = calc (a + 3 + 3) xs -- 3 3 => 6
calc a ("A":"Y":xs) = calc (a + 2 + 6) xs -- 1 2 => 8
calc a ("A":"Z":xs) = calc (a + 3 + 0) xs -- 1 3 => 3
calc a ("B":"X":xs) = calc (a + 1 + 0) xs -- 2 1 => 1
calc a ("B":"Z":xs) = calc (a + 3 + 6) xs -- 2 3 => 9
calc a ("C":"X":xs) = calc (a + 1 + 6) xs -- 3 1 => 7
calc a ("C":"Y":xs) = calc (a + 2 + 0) xs -- 3 2 => 2
calc a _            = a

calc' :: Int -> [String] -> Int
calc' a ("A":"X":xs) = calc' (a + 3 + 0) xs
calc' a ("B":"Y":xs) = calc' (a + 2 + 3) xs
calc' a ("C":"Z":xs) = calc' (a + 1 + 6) xs
calc' a ("A":"Y":xs) = calc' (a + 1 + 3) xs
calc' a ("A":"Z":xs) = calc' (a + 2 + 6) xs
calc' a ("B":"X":xs) = calc' (a + 1 + 0) xs
calc' a ("B":"Z":xs) = calc' (a + 3 + 6) xs
calc' a ("C":"X":xs) = calc' (a + 2 + 0) xs
calc' a ("C":"Y":xs) = calc' (a + 3 + 3) xs
calc' a _            = a

calc'a :: Int -> [String] -> Int
calc'a acc (o:p:xs) =
  let op = (ord $ o !! 0) - ord 'A'
      pl = (ord $ p !! 0) - ord 'X'
   -- Mike Magic (tm)
   in calc'a (acc + (3 * ((1 - op + pl) `mod` 3)) + pl + 1) xs
calc'a acc _ = acc

run :: IO ()
run = do
  real_vals <- read_data words "data/AoC_2022_Day_02"
  putStrLn $ "Day2 Q1  => " ++ show (calc 0 real_vals)
  putStrLn $ "Day2 Q1a => " ++ show (calc'a 0 real_vals)
  putStrLn $ "Day2 Q2  => " ++ show (calc' 0 real_vals)
