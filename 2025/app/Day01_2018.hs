{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day01_2018 where

import           Control.Monad        (foldM)
import           Data.Functor         (($>))
import qualified Data.Set             as Set
import           Handy
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' [Int]
input = some $ (*) <$> (char '-' $> (-1) <|> char '+' $> 1)
                    <*> (read <$> some digitChar <* optional newline)

part1 :: IO Int
part1 = sum . parse' input <$> puzzle Main 2018 1

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2018 1
    -- Naughty on matching only Left, but we know it must terminate
    -- Leverage Either & foldM to build a terminating fold mechanism :) Magical
    let Left (answer, _) = foldM (\(acc', seen) val ->
                                    let freq = acc' + val
                                    in if freq `Set.member` seen
                                        then Left (freq, seen)
                                        else Right (freq, Set.insert freq seen)
                                 ) (0, Set.empty) $ cycle values
    pure answer
