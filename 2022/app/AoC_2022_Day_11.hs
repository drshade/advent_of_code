module AoC_2022_Day_11 where

import           Data.List   (sort)
import           Data.Map    (Map, adjust, alter, empty, insert, keys, toList,
                              (!))
import           Text.Parsec
import           Utils       (read_data)

data Operation =
  Operation (Integer -> Integer)

data Condition =
  Condition Integer (Integer -> Int -> Int -> Int)

type Monkey = ([Integer], Operation, Condition, Int, Int)

type Monkeys = Map Int Monkey

root_parser :: Parsec String () Monkeys
root_parser = do
  ms <-
    many $ do
      monkey_number <- monkey_number_parser <* newline
      starting_items <- items_parser <* newline
      operation <- operation_parser' <* newline
      condition <- condition_parser <* newline
      if_true <- outcome_parser <* newline
      if_false <- outcome_parser <* newline
      _ <- optional newline
      return
        ( monkey_number
        , (starting_items, operation, condition, if_true, if_false))
  return $ foldr (\(m, md) v -> insert m md v) empty ms

outcome_parser :: Parsec String () Int
outcome_parser = do
  read <$>
    (string "    If " *> (string "true" <|> string "false") *>
     (string ": throw to monkey ") *>
     many digit)

condition_parser :: Parsec String () Condition
condition_parser = do
  val <- string "  Test: divisible by " >> many digit
  return $
    Condition
      (read val)
      (\v t f ->
         if v `mod` (read val) == 0
           then t
           else f)

-- For part 1
operation_parser :: Parsec String () Operation
operation_parser = do
  operator <-
    string "  Operation: new = old " >> (char '+' <|> char '*') <* (char ' ')
  choice $
    [ do _ <- string "old"
         case operator of
           '*' -> return $ Operation (\n -> (n * n) `div` 3)
           '+' -> return $ Operation (\n -> (n + n) `div` 3)
           _   -> unexpected "Need + or * for operator"
    , do val <- many digit
         case operator of
           '*' -> return $ Operation (\n -> (n * (read val)) `div` 3)
           '+' -> return $ Operation (\n -> (n + (read val)) `div` 3)
           _   -> unexpected "Need + or * for operator"
    ]

-- For part 2
operation_parser' :: Parsec String () Operation
operation_parser' = do
  operator <-
    string "  Operation: new = old " *> (char '+' <|> char '*') <* (char ' ')
  choice $
    [ do _ <- string "old"
         case operator of
           '*' -> return $ Operation (\n -> (n * n))
           '+' -> return $ Operation (\n -> (n + n))
           _   -> unexpected "Need + or * for operator"
    , do val <- many digit
         case operator of
           '*' -> return $ Operation (\n -> n * (read val))
           '+' -> return $ Operation (\n -> n + (read val))
           _   -> unexpected "Need + or * for operator"
    ]

items_parser :: Parsec String () [Integer]
items_parser = do
  _ <- string "  Starting items:"
  many $ do read <$> (char ' ' *> many digit <* (optional $ char ','))

monkey_number_parser :: Parsec String () Int
monkey_number_parser = do
  string "Monkey " *> (read <$> many digit) <* (char ':')

run_round :: Integer -> Monkeys -> Map Int Int -> (Monkeys, Map Int Int)
run_round lcm' monkeys results =
  foldr
    (\mn (monkeys', acc') ->
       let exec ((worry:is), Operation op, Condition multiple cond, t, f) m acc =
             let new_worry = op worry `mod` lcm'
              in exec
                   (is, Operation op, Condition multiple cond, t, f)
                   (move mn new_worry (cond new_worry t f) m)
                   (inc mn acc)
           exec _ m ac = (m, ac)
        in exec (monkeys' ! mn) monkeys' acc')
    (monkeys, results)
    (reverse $ keys monkeys)

move :: Int -> Integer -> Int -> Monkeys -> Monkeys
move from_mn item to_mn monkeys =
  adjust (\(items, o, c, t, f) -> (items ++ [item], o, c, t, f)) to_mn $
  adjust (\(items, o, c, t, f) -> (tail items, o, c, t, f)) from_mn monkeys

inc :: Int -> Map Int Int -> Map Int Int
inc mn monkeys = alter (\mc -> Just $ (maybe 1 (+ 1) mc)) mn monkeys

run :: IO ()
run = do
  real <- read_data (id) "data/AoC_2022_Day_11"
  case runParser root_parser () "(input)" real of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right monkeys -> do
      let lcm' =
            product $
            (\(_, _, Condition multiple _, _, _) -> multiple) <$> monkeys
      let result =
            toList $
            snd $
            foldr
              (\_ (monk, acc) -> run_round lcm' monk acc)
              (monkeys, empty)
              [1 :: Int .. 10000]
      putStrLn $
        "Day11 Q2  => " ++
        (show $ product $ take 2 $ reverse $ sort $ snd <$> result)
