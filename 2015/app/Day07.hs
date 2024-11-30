{-# LANGUAGE TypeFamilies #-}

module Day07 where

import Control.Applicative ((<|>))
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.MemoTrie (HasTrie (..), Reg, enumerateGeneric, memoFix, trieGeneric, untrieGeneric, (:->:))
import Data.Word (Word16)
import GHC.Generics (Generic)
import Text.Parsec (Parsec, choice, digit, letter, many1, newline, optional, runParser, string, try)
import Prelude hiding (lookup)

type Wire = String
type Value = Word16

data Signal = FromWire Wire | FromValue Value deriving (Generic)
data Assignment = Assign Wire Expression
data Expression
  = Load Signal
  | And Signal Signal
  | Or Signal Signal
  | Lshift Signal Signal
  | Rshift Signal Signal
  | Not Signal

type Parser a = Parsec String () a

parseAssignments :: Parser [Assignment]
parseAssignments = do
  many1 $ (choice $ try <$> allTheParsers) <* optional newline
 where
  allTheParsers =
    [ (\s t -> Assign t $ Load s) <$> signal <* string " -> " <*> name
    , (\s1 s2 t -> Assign t $ And s1 s2) <$> signal <* string " AND " <*> signal <* string " -> " <*> name
    , (\s1 s2 t -> Assign t $ Or s1 s2) <$> signal <* string " OR " <*> signal <* string " -> " <*> name
    , -- Lshift & Rshift technically never show a shift from anything other than values... but we support this?
      (\s v t -> Assign t $ Lshift s v) <$> signal <* string " LSHIFT " <*> signal <* string " -> " <*> name
    , (\s v t -> Assign t $ Rshift s v) <$> signal <* string " RSHIFT " <*> signal <* string " -> " <*> name
    , (\s t -> Assign t $ Not s) <$> (string "NOT " *> signal <* string " -> ") <*> name
    ]
  signal = FromWire <$> name <|> FromValue <$> value
  name = many1 letter
  value = read <$> many1 digit

-- Memoize the eval function

type Memo f = f -> f

eval :: [Assignment] -> Memo (Signal -> Value)
eval _ _ (FromValue v) = v
eval as recurse (FromWire signal) =
  let (Assign _ rootExpr) = findAssignment signal
   in compute rootExpr
 where
  findAssignment :: Wire -> Assignment
  findAssignment wire =
    fromMaybe (error $ "can't find wire -> " <> wire) $
      find
        ( \case
            Assign wire' _ | wire == wire' -> True
            _ -> False
        )
        as
  compute :: Expression -> Value
  compute = \case
    Load signal' -> recurse signal'
    And signal1 signal2 -> (recurse signal1) .&. (recurse signal2)
    Or signal1 signal2 -> (recurse signal1) .|. (recurse signal2)
    Lshift signal1 amount -> shiftL (recurse signal1) (fromIntegral $ recurse amount)
    Rshift signal1 amount -> shiftR (recurse signal1) (fromIntegral $ recurse amount)
    Not signal1 -> complement (recurse signal1)

-- Use memoFix to create a new memoized function
eval'memo :: [Assignment] -> Signal -> Value
eval'memo assignments = memoFix (eval assignments)

run :: Parser a -> String -> a
run parser input = either (error . show) id $ runParser parser () "input" input

part1 :: IO Value
part1 = do
  input <- readFile "data/Day07_input.txt"
  pure $ eval'memo (run parseAssignments input) (FromWire "a")

part2 :: IO Value
part2 = do
  input <- readFile "data/Day07_input.txt"
  let procs = run parseAssignments input
  -- Get Wire a's value
  let value_a = eval'memo procs (FromWire "a")
  -- Set b's value to it (at the start so it gets hit first... hehe)
  pure $ eval'memo (Assign ("b") (Load $ FromValue value_a) : procs) (FromWire "a")

-- Need this for memoizing Signals
instance HasTrie Signal where
  newtype Signal :->: b = SignalTrie {unSignalTrie :: Reg Signal :->: b}
  trie = trieGeneric SignalTrie
  untrie = untrieGeneric unSignalTrie
  enumerate = enumerateGeneric unSignalTrie
