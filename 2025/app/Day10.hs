module Day10 where

import           Data.Bits            (Bits, complementBit, setBit)
import           Data.Graph.AStar     (aStar)
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as Set
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Mask = HashSet Int

input :: Parser' [(HashSet Int, [HashSet Int], [Int])]
input = some $ do
    lights <- catMaybes
                        <$> between (char '[') (char ']')
                                    (some $ choice  [ Nothing <$ char '.'
                                                    , (Just <$> (\b -> b - 1) <$> sourcex) <* char '#' ] )
    char ' '
    buttons <- between (char '(') (char ')') (num `sepBy` char ',') `endBy` char ' '
    jolts <- between (char '{') (char '}') (num `sepBy` char ',')
    optional newline
    pure (Set.fromList lights, Set.fromList <$> buttons, jolts)

setXor :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
setXor a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

part1 :: IO ()
part1 = do
    values <- parse' input <$> puzzle Main 2025 10
    -- print values
    let x = mapMaybe (\((target :: HashSet Int), (buttons :: [HashSet Int]), _) ->
                aStar
                    (\value -> Set.fromList $ fmap (setXor value) buttons) -- (node -> HashSet node) - get valid neighbors
                    (\from to -> 1)       -- (node -> node -> Int) - actual cost between nodes
                    (const 1)  -- (node -> Int) - estimated cost to goal (e.g. manhattan distance)
                    (== target)   -- (node -> Bool) - check if we've reached the goal
                    Set.empty      -- starting node
                ) values
    -- print x
    print $ sum $ length <$> x
    pure ()

part2 :: IO ()
part2 = do
    values <- parse' input <$> puzzle Main 2025 10
    pure ()
