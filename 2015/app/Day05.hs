module Day05 where

import Control.Applicative.Combinators
import Control.Monad (foldM)
import Control.Monad.State (State, get, modify, runState)
import Data.Functor (void, ($>))
import Data.Void (Void)
import Text.Megaparsec (ParsecT, lookAhead, runParserT)
import Text.Megaparsec.Char (letterChar, string)

type Parser = ParsecT Void String (State Int)

-- It contains at least three vowels (aeiou only), like aei, xazegov, or
-- aeiouaeiouaeiou.
rule1 :: Parser Bool
rule1 = do
  void $ many $ do
    c <- letterChar
    if c `elem` "aeiou"
      then void $ modify (+ 1)
      else pure ()
  get >>= pure . (>= 3)

-- It contains at least one letter that appears twice in a row, like xx,
-- abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
-- Use lookAhead to not consume input for the second character
rule2 :: Parser Bool
rule2 = do
  p <- many $ (==) <$> (Just <$> letterChar) <*> (lookAhead $ optional letterChar)
  pure $ any (== True) p

-- It does not contain the strings ab, cd, pq, or xy, even if they are part of
-- one of the other requirements.
rule3 :: Parser Bool
rule3 = do
  x <-
    many $
      choice
        [ string "ab" $> False,
          string "cd" $> False,
          string "pq" $> False,
          string "xy" $> False,
          letterChar $> True
        ]
  pure $ all (== True) x

nice :: Parser Bool
nice = foldM (\a b -> b >>= pure . (&& a)) True (lookAhead <$> [rule1, rule2, rule3])

run :: Parser a -> String -> a
run p i = either (error . show) id $ fst $ runState (runParserT p "input" i) 0

part1 :: IO ()
part1 = do
  input <- readFile "data/Day05_input.txt"
  let r1 = length $ filter (== True) $ run nice <$> lines input
  putStrLn $ show r1
