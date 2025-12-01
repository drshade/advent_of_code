{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day01_2018 where

import           Data.Set             (empty, insert, member)
import           Handy
import           Prelude              hiding (empty, insert, some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

input :: Parser' [Int]
input = some $ (*) <$> (char '-' $> (-1) <|> char '+' $> 1)
                   <*> (num <* optional newline)

part1 :: IO Int
part1 = sum . parse' input <$> puzzle Main 2018 1

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2018 1
    -- Naughty on matching only Left, but we know it must terminate
    -- Leverage Either & foldM to build a terminating fold mechanism :) Magical
    let Left answer = foldM (\(acc', seen) val ->
                                let freq = acc' + val
                                 in if freq `member` seen
                                    then Left freq
                                    else Right (freq, insert freq seen)
                            ) (0, empty) $ cycle values
    pure answer

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- (>>=) :: Either a t -> (t -> Either a b) -> Either a b
-- Right a >>= f = (f a)
-- Left b >>= f = Left b
-- pure = Right

-- result :: Either String Int
-- result = do
--     x <- Right 5
--     y <- Right 7
--     z <- Left "fail"
--     pure (x+y+z)

-- print result
