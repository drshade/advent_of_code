module Day10 where

import           Algorithm.Search     (bfs)
import qualified Data.HashSet         as Set
import           Data.SBV             hiding (some)
import           Handy
import           Prelude              hiding (Rep, branch, some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Mask = HashSet Int

input :: Parser' [(HashSet Int, [HashSet Int], [Int])]
input = some $ do
    lights <- catMaybes <$> between (char '[') (char ']')
                                    (some $ choice  [ Nothing <$ char '.'
                                                    , (Just . (\b -> b - 1) <$> sourcex) <* char '#' ] )
    buttons <- char ' ' *> between (char '(') (char ')') (num `sepBy` char ',') `endBy` char ' '
    jolts <- between (char '{') (char '}') (num `sepBy` char ',') <* optional newline
    pure (Set.fromList lights, Set.fromList <$> buttons, jolts)

setXor :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
setXor a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

part1 :: IO ()
part1 = do
    values <- parse' input <$> puzzle Main 2025 10
    let x = mapMaybe (\(target, buttons, _) ->
                bfs (\value -> Set.fromList $ fmap (setXor value) buttons)  -- (node -> HashSet node) - get valid neighbors
                    (== target)                                             -- (node -> Bool) - check if we've reached the goal
                    Set.empty                                               -- starting node
                ) values
    print $ sum $ length <$> x
    pure ()

-- Claude wrote this. I have no real idea of what it's doing.
-- Its using SBV library (and z3 backend) to solve the linear equations
leastPushes :: (HashSet Int, [HashSet Int], [Int]) -> IO (Maybe Int)
leastPushes (_, buttons, targets) = do
    let numButtons = length buttons

    result <- optimize Lexicographic $ do
        xs <- mapM (\i -> sInteger ("x" ++ show i)) [0..numButtons-1]
        mapM_ (\x -> constrain $ x .>= 0) xs
        forM_ (zip [0..] targets) $ \(ci, target) -> do
            let terms = [xs !! bi | (bi, btn) <- zip [0..] buttons, ci `Set.member` btn]
            constrain $ sum terms .== fromIntegral target
        minimize "total" $ sum xs

    pure $ case result of
        LexicographicResult res ->
            case getModelValue "total" res of
                Just n  -> Just (fromInteger n)
                Nothing -> Nothing
        _ -> Nothing

part2 :: IO ()
part2 = do
    values <- parse' input <$> puzzle Main 2025 10
    results <- mapM leastPushes values
    print $ sum $ catMaybes results
