{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Day12 where

import qualified AoC                         as AoC
import           Control.Monad               (join)
import           Control.Monad.State         (State, get, put, runState)
import           Control.Parallel.Strategies (evalBuffer, parBuffer, parMap,
                                              rpar, rseq, runEval)
import           Data.List                   (groupBy)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import           Data.MemoTrie
import qualified Data.Set                    as Set
import           Debug.Trace                 (trace)
import           GHC.Generics                (Generic)
import           Handy                       (WhichPuzzleInput (..),
                                              get_puzzle_input, unique)
import           Parsing                     (run_parser, run_parser_with_state)
import           Text.Parsec                 (Parsec, anyChar, char, choice,
                                              digit, getState, letter, many1,
                                              newline, optional, sepBy,
                                              setState, string, try, (<|>))

data Spring
  = Unknown
  | Damaged
  | Operational
  deriving (Show, Eq, Enum, Bounded, Generic)

instance HasTrie Spring where
  newtype (Spring :->: b) = SpringTrie{unSpringTrie ::
                                     Reg Spring :->: b}
  trie = trieGeneric SpringTrie
  untrie = untrieGeneric unSpringTrie
  enumerate = enumerateGeneric unSpringTrie

parse_spring :: Parsec String () Spring
parse_spring = do
  choice
    [ char '.' *> pure Operational
    , char '#' *> pure Damaged
    , char '?' *> pure Unknown
    ]

parse_damaged_order :: Parsec String () [Int]
parse_damaged_order = (read <$> many1 digit) `sepBy` char ','

parse_entry :: Parsec String () ([Spring], [Int])
parse_entry = do
  springs <- many1 parse_spring <* char ' '
  damaged_orders <- parse_damaged_order
  _ <- newline
  pure (springs, damaged_orders)

-- ???.### 1,1,3
-- .??.### 1,1,3
--
-- Only looks at whether damaged groups match the specification (Unknown will not work)
valid_permutation :: [Int] -> [Spring] -> Bool
valid_permutation damaged springs =
  let damaged_groups =
        length <$> (filter (\grp -> head grp == Damaged) $ groupBy (==) springs)
   in damaged == damaged_groups

valid_permutation' = memo2 valid_permutation

data Tree
  = Leaf
  | Fork Spring Tree Spring Tree
  | Branch Spring Tree
  deriving (Show)

perm :: [Spring] -> [Int] -> Tree
perm springs (0:cs) = perm springs cs --
perm (Unknown:rest) (c:cs) =
  Fork Operational (perm rest (c : cs)) Damaged (perm rest (c - 1 : cs))
perm (a:rest) cs = Branch a (perm rest cs)
perm [] cs = Leaf

perm2 :: [Spring] -> [Int] -> [[Spring]]
perm2 springs counts =
  let go path springs (0:cs) = go path springs cs
      go path (Unknown:rest) (c:cs) =
        go (Operational : path) rest (c : cs) ++
        go (Damaged : path) rest (c - 1 : cs)
      go path (a:rest) cs = go (a : path) rest cs
      go path _ _ = [reverse path]
   in go [] springs counts

perm2' = memo2 perm2

perm3 :: [Spring] -> [Int] -> Int
perm3 springs counts =
  let go path springs (0:cs) = go path springs cs
      go path (Unknown:rest) (c:cs) =
        go (Operational : path) rest (c : cs) +
        go (Damaged : path) rest (c - 1 : cs)
      go path (a:rest) cs = go (a : path) rest cs
      go path _ _ =
        let x =
              if (valid_permutation counts $ reverse path)
                then 1
                else 0
         in x
      gomemo2 :: [Spring] -> [Int] -> Int
      gomemo2 = memo2 (go [])
      gomemo :: ([Spring], [Spring], [Int]) -> Int
      gomemo =
        memoFix
          (\rec (path', springs', counts') ->
             case (path', springs', counts') of
               (path'', springs'', (0:cs)) -> rec (path'', springs'', cs)
               (path'', (Unknown:rest), (c:cs)) ->
                 rec ((Operational : path''), rest, (c : cs)) +
                 rec ((Damaged : path''), rest, (c - 1 : cs))
               (path'', (a:rest), cs) -> rec ((a : path''), rest, cs)
               (path'', _, _) ->
                 if (valid_permutation counts $ reverse path'')
                   then 1
                   else 0)
   -- in gomemo2 springs counts
   in go [] springs counts

flatten :: Tree -> [[Spring]]
flatten tree = go [] tree
  where
    go path Leaf = [reverse path] -- If it's a leaf, add the path to the result
    go path (Branch spring subtree) = go (spring : path) subtree
    go path (Fork spring leftTree rightSpring rightTree) =
      go (spring : path) leftTree ++ go (rightSpring : path) rightTree

-- permute :: [Spring] -> [Int] -> Int
-- -- ?. [0] -> .
-- permute (Unknown:Operational:rest) (0:[]) = 0 + permute rest cs
-- -- ? [1] -> #
-- permute (Unknown:rest) (c:cs)             = 1 + permute rest (c - 1 : cs)
-- permute ()
permute springs (0:cs) = permute springs cs
permute (a:rest) cs    = permute rest cs
permute [] cs          = 0

explode :: Int -> ([Spring], [Int]) -> ([Spring], [Int])
explode times (springs, damages) =
  (join $ replicate times springs, join $ replicate times damages)

solve1 :: ([([Spring], [Int])]) -> Int
solve1 input =
  let count :: [Spring] -> [Int] -> Int
      count springs damaged = perm3 springs damaged
      x = parMap rseq (\(s, d) -> count s d) input
   in sum $ x

solve2 :: ([([Spring], [Int])]) -> Int
solve2 input =
  let exploded = explode 5 <$> input
      count :: [Spring] -> [Int] -> Int
      count springs damaged = perm3 springs damaged
      x = parMap (rpar) (\(s, d) -> count s d) exploded
      y = runEval $ parBuffer 4 rpar (map (\(s, d) -> count s d) exploded)
   in 0 --sum $ y -- sum $ (\(s, d) -> count s d) <$> exploded

solve :: IO (AoC.Solution Int)
solve = do
  input <- run_parser (many1 parse_entry) <$> get_puzzle_input Mine 2023 12
  solution_1 <- pure $ solve1 input
  solution_2 <- pure $ solve2 input
  pure $
    AoC.SolvedTwo
      2023
      12
      solution_1
      (AoC.Revealed 6852)
      solution_2
      (AoC.Unknown)
