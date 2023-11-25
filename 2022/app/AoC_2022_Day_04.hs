{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AoC_2022_Day_04 where

import           Data.List.Split (splitOn)
import           Data.Set        as Set (Set, disjoint, fromList, isSubsetOf)
import           Utils           (read_data)

split :: String -> (Set Int, Set Int)
split x =
  let ((s1:e1:_):(s2:e2:_):_) = (\g -> read <$> splitOn "-" g) <$> splitOn "," x
   in (Set.fromList [s1 .. e1], Set.fromList [s2 .. e2])

calc :: Int -> [String] -> Int
calc a (x:xs) =
  let (s1, s2) = split x
   in if s1 `isSubsetOf` s2 || s2 `isSubsetOf` s1
        then calc (a + 1) xs
        else calc a xs
calc a _ = a

calc' :: Int -> [String] -> Int
calc' a (x:xs) =
  let (s1, s2) = split x
   in if s1 `disjoint` s2
        then calc' a xs
        else calc' (a + 1) xs
calc' a _ = a

run :: IO ()
run = do
  real_vals <- read_data words "data/AoC_2022_Day_04"
  putStrLn $ "Day4 Q1  => " ++ show (calc 0 real_vals)
  putStrLn $ "Day4 Q2  => " ++ show (calc' 0 real_vals)
