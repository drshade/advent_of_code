{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AoC_2022_Day_13 where

import           Data.List   (findIndex, intercalate, sort)
import           Text.Parsec
import           Utils       (read_data, (<$->))

scalar_parser :: Parsec String () Element
scalar_parser = Scalar <$> read <$> many1 digit

vector_parser :: Parsec String () Element
vector_parser =
  Vector <$> between (char '[') (char ']') (element_parser `sepBy` char ',')

element_parser :: Parsec String () Element
element_parser = vector_parser <|> scalar_parser

signal_pair_parser :: Parsec String () [(Element, Element)]
signal_pair_parser =
  many $ do
    first <- element_parser <* newline
    second <- element_parser <* (optional newline) <* (optional newline)
    return (first, second)

data Element
  = Scalar Int
  | Vector [Element]
  deriving (Eq)

instance Show Element where
  show (Scalar v) = "" ++ show v ++ ""
  show (Vector v) = "[" ++ (intercalate "," $ show <$> v) ++ "]"

instance Ord Element where
  compare :: Element -> Element -> Ordering
  compare (Scalar a) (Scalar b)     = compare a b
  compare a@(Vector _) b@(Scalar _) = compare a $ Vector [b]
  compare a@(Scalar _) b@(Vector _) = compare (Vector [a]) b
  compare (Vector a) (Vector b)     = a `compare` b

test :: String
test =
  "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"

run :: IO ()
run = do
  real <- read_data (id) "data/AoC_2022_Day_13"
  case runParser signal_pair_parser () "(input)" real of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right signals -> do
      let processed =
            (\(i, (l, r)) -> (i, l `compare` r)) <$> zip [1 :: Int ..] signals
      putStrLn $
        "Day13 Q1 => " <>
        (show $ sum $ fst <$> filter (\(_, c) -> c == LT) processed)
      let p1 = Vector [Vector [Scalar 2]]
          p2 = Vector [Vector [Scalar 6]]
          every = sort $ p1 : p2 : (\(f, s) -> [f, s]) <$-> signals
          decoder_key :: Maybe Int
          decoder_key = do
            x1 <- findIndex (== p1) every
            x2 <- findIndex (== p2) every
            pure $ (x1 + 1) * (x2 + 1)
      putStrLn $ "Day13 Q2 => " <> show decoder_key
