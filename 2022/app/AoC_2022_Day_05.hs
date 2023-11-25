{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AoC_2022_Day_05 where

import           Control.Monad   (join)
import           Data.List.Split (splitOn)
import           Utils           (dropLast, read_data, takeLast, trim)

type Stack = [String]

type Move = (Int, Int, Int)

parse_stacks :: String -> Stack
parse_stacks input = [trim [snipe x y | y <- [0 .. 7]] | x <- [0 .. 8]]
  where
    snipe x y =
      (join $ drop 1 $ reverse $ splitOn "\n" input) !! (1 + (x * 4) + (y * 35))

parse_moves :: String -> [Move]
parse_moves input = grab <$> splitOn " " <$> splitOn "\n" input
  where
    grab (_:count:_:from:_:to:_) = (read count, read from - 1, read to - 1)

move :: Stack -> Move -> Stack
move stack (0, _, _) = stack
move stack (count, from, to) = move (remove $ add stack) (count - 1, from, to)
  where
    add :: Stack -> Stack
    add s = take to s ++ (s !! to ++ [last $ s !! from]) : drop (to + 1) s
    remove s = take from s ++ (init $ s !! from) : drop (from + 1) s

move' :: Stack -> Move -> Stack
move' stack (count, from, to) = remove $ add stack
  where
    add s =
      take to s ++ (s !! to ++ (takeLast count $ s !! from)) : drop (to + 1) s
    remove s = take from s ++ (dropLast count $ s !! from) : drop (from + 1) s

run :: IO ()
run = do
  real_vals <- read_data id "data/AoC_2022_Day_05"
  let (stacks:moves:_) = splitOn "\n\n" real_vals
  putStrLn $
    "Day5 Q1  => " ++
    (last <$> foldl move (parse_stacks stacks) (parse_moves moves))
  putStrLn $
    "     Q2  => " ++
    (last <$> foldl move' (parse_stacks stacks) (parse_moves moves))
