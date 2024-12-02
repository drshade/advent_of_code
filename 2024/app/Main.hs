module Main where

import           Day01

main :: IO ()
main = part1 >>= putStrLn . show
