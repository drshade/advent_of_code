
module Day06 where

import           Handy
import           Prelude              hiding (some, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Row = [Int]
type Oper a = (a -> a -> a)

input :: Parser' ([Row], [Oper Int])
input = do
    row <- some ((optional hspace *> num `sepBy1` hspace) <* newline)
    ops <- choice [char '*' $> ((*) @Int), char '+' $> ((+) @Int)] `sepEndBy` hspace
    pure (row, ops)

type Col = [String]

columninput :: Parser' ([Col], [Oper Int])
columninput = do
    rows <- some (some (digitChar <|> char ' ') <* newline)
    ops <- choice [char '*' $> ((*) @Int), char '+' $> ((+) @Int)] `sepEndBy` hspace
    pure (columnise ' ' rows, ops)
    where
        columnise :: Char -> [String] -> [Col]
        columnise delim rows = -- where there is a common char in all inputs, thats the common index to split on
            let len = minimum $ length <$> rows
                common = filter (\idx -> all (\str -> str !! idx == delim) rows) [0..len - 1]
                -- Convert to ranges between delimiters, skipping the delim itself (+1)
                ranges = zip (0 : fmap (+1) common) (common ++ [len])
                split str = [take (e - s) (drop s str) | (s, e) <- ranges]
             in transpose $ split <$> rows -- finally transpose after splitting

part1 :: IO Int
part1 = do
    (rows, opers) <- parse' input <$> puzzle Main 2025 6
    pure $ sum $ uncurry foldr1 <$> zip opers (transpose rows)

part2 :: IO Int
part2 = do
    (rows, opers) <- parse' columninput <$> puzzle Main 2025 6
    -- horrible horrible horrible, but my brain is fried
    pure $ sum ((\(oper, vals) -> foldr1 oper (read <$> transpose vals)) <$> zip opers rows)
