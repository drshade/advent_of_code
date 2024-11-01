module Day02 where

import Text.Parsec (char, digit, many, many1, newline, optional)
import Text.Parsec.String (Parser, parseFromFile)

data Box = Box Int Int Int

boxesParser :: Parser [Box]
boxesParser =
    let number = read <$> many1 digit
     in many $ Box <$> number <* char 'x' <*> number <* char 'x' <*> number <* optional newline

wrappingPaper :: Box -> Int
wrappingPaper (Box l w h) = (2 * l * w) + (2 * w * h) + (2 * h * l) + extra
  where
    extra = min (l * w) $ min (w * h) (h * l)

ribbon :: Box -> Int
ribbon (Box l w h) = smallestPerimeter + (l * w * h)
  where
    smallestPerimeter = 2 * min (l + w) (min (w + h) (h + l))

part1 :: IO Int
part1 = do
    boxes <- either (error . show) id <$> parseFromFile boxesParser "data/Day02_input.txt"
    pure $ sum $ wrappingPaper <$> boxes

part2 :: IO Int
part2 = do
    boxes <- either (error . show) id <$> parseFromFile boxesParser "data/Day02_input.txt"
    pure $ sum $ ribbon <$> boxes