module Main where

import Day01 qualified (solve)
import Day02 qualified (solve)
import Day03 qualified (solve)
import Day04 qualified (solve)
import Day05 qualified (solve)
import Day06 qualified (solve)
import Day07 qualified (solve)
import Day08 qualified (solve)
import Day09 qualified (solve)
import Day10 qualified (solve)
import Day11 qualified (solve)
import Day12 qualified (solve)
import Day13 qualified (solve)
import Day14 qualified (solve)
import Day15 qualified (solve)
import Day16 qualified (solve)
import Day17 qualified (solve)
import Day18 qualified (solve)
import Day19 qualified (solve)
import Day20 qualified (solve)
import Day21 qualified (solve)
import Day22 qualified (solve)
import Day23 qualified (solve)
import Day24 qualified (solve)
import Day25 qualified (solve)

full :: IO ()
full = do
  d01 <- Day01.solve
  putStrLn $ show d01
  d02 <- Day02.solve
  putStrLn $ show d02
  d03 <- Day03.solve
  putStrLn $ show d03
  d04 <- Day04.solve
  putStrLn $ show d04
  d05 <- Day05.solve
  putStrLn $ show d05
  d06 <- Day06.solve
  putStrLn $ show d06
  d07 <- Day07.solve
  putStrLn $ show d07
  d08 <- Day08.solve
  putStrLn $ show d08
  d09 <- Day09.solve
  putStrLn $ show d09
  d10 <- Day10.solve
  putStrLn $ show d10
  d11 <- Day11.solve
  putStrLn $ show d11
  d12 <- Day12.solve
  putStrLn $ show d12

-- d13 <- Day13.solve
-- putStrLn $ show d13
-- d14 <- Day14.solve
-- putStrLn $ show d14
-- d15 <- Day15.solve
-- putStrLn $ show d15
-- d16 <- Day16.solve
-- putStrLn $ show d16
-- d17 <- Day17.solve
-- putStrLn $ show d17
-- d18 <- Day18.solve
-- putStrLn $ show d18
-- d19 <- Day19.solve
-- putStrLn $ show d19
-- d20 <- Day20.solve
-- putStrLn $ show d20
-- d21 <- Day21.solve
-- putStrLn $ show d21
-- d22 <- Day22.solve
-- putStrLn $ show d22
-- d23 <- Day23.solve
-- putStrLn $ show d23
-- d24 <- Day24.solve
-- putStrLn $ show d24
-- d25 <- Day25.solve
-- putStrLn $ show d25

one :: IO ()
one = do
  d <- Day12.solve
  putStrLn $ show d

wait :: IO ()
wait = putStrLn "Waiting for next day... 😿"

-- Run with (with nice reloading etc)
--   $ ghcid -r --height 20
--
main :: IO ()
main = full
