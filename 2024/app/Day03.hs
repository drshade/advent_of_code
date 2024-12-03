module Day03 where
import           Control.Applicative ((<|>))
import           Data.Functor        (($>))
import           Data.Maybe          (catMaybes)
import           Handy
import           Text.Parsec         (anyChar, char, choice, digit, many, many1,
                                      string, try)

data Instruction
    = Mul Int Int
    | Enable
    | Disable
    deriving (Show)

parser :: Parser [Instruction]
parser = catMaybes <$> many1 (choice [ Just <$> try mul
                                     , Just <$> try enable
                                     , Just <$> try disable
                                     , anyChar $> Nothing
                                     ])
    where mul = Mul <$> (string "mul(" *> val <* char ',')
                    <*> val <* char ')'
          enable = Enable <$ string "do()"
          disable = Disable <$ string "don't()"
          val = read <$> many1 digit

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 3
    let result = foldr (\i acc -> case i of
                            Mul a b -> acc + a * b
                            _       -> acc) 0 input
    pure $ result

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 3
    let result = foldl (\(on, acc) i -> case i of
                        Mul a b -> if on then (on, acc + a * b) else (on, acc)
                        Enable  -> (True, acc)
                        Disable -> (False, acc)) (True, 0) input
    pure $ snd result
