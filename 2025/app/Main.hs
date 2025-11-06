module Main where

import           Control.Monad (void)
import qualified Day01         as D01

main :: IO ()
main = do
    r <- D01.part1
    putStrLn $ show r
