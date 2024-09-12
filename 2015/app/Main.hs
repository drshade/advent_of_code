module Main where

import Day06 qualified as Day06 (part1)

main :: IO ()
main = do
    v <- Day06.part1
    putStrLn $ show v
