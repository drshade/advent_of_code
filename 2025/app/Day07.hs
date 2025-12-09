module Day07 where

import           Control.Monad.Trans.Writer.Strict (runWriterT)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Day07Vis
import           Handy
import           Prelude                           hiding (cast, some)
import           Text.Megaparsec                   hiding (State)
import           Text.Megaparsec.Char

type Start = XY
type Splitter = XY

input :: Parser' (Start, Set.Set Splitter)
input = -- First line contains the starting point, next lines just a Set of positions of the splitters
    (,) <$> (some (char '.') *> xy <* char 'S' <* some (char '.') <* newline)
        <*> (Set.fromList . catMaybes <$> some (choice [ Just <$> xy <* char '^', Nothing <$ char '.' ] <* optional newline))

maxdepth :: Set.Set XY -> Int
maxdepth s = snd $ maximumBy (compare `on` snd) $ Set.toList s

-- DFS with State (to keep count of number of times we split)
totalSplits :: Set Splitter -> XY -> Visualise (State (Set XY)) Int
totalSplits splitters xy@(x, y) = do
    tell [xy]
    counted <- get
    if y > maxdepth splitters || xy `Set.member` counted
        then pure 0
        else if xy `Set.member` splitters then do
            modify (Set.insert xy)
            lb <- totalSplits splitters (x-1, y+1)
            rb <- totalSplits splitters (x+1, y+1)
            pure $ 1 + lb + rb -- count this as 1 split event (doesn't matter whether left or right are dupes)
        else do
            modify (Set.insert xy)
            totalSplits splitters (x, y+1)

-- DFS with State (keep total number of paths, memoized as a Map)
totalPaths :: Set Splitter -> XY -> Visualise (State (Map.Map XY Int)) Int
totalPaths splitters xy@(x, y) = do
    tell [xy]
    memo <- get
    case Map.lookup xy memo of
        Just a -> pure a
        Nothing -> do
            count <- if y > maxdepth splitters
                        then pure 1
                        else
                            if xy `Set.member` splitters
                                then do
                                    lpaths <- totalPaths splitters (x-1, y+1)
                                    rpaths <- totalPaths splitters (x+1, y+1)
                                    pure $ lpaths + rpaths
                                else totalPaths splitters (x, y+1)
            modify (Map.insert xy count)
            pure count

part1 :: IO Int
part1 = do
    (start, splitters) <- parse' input <$> puzzle Main 2025 7
    let solution = runWriterT $ totalSplits splitters start
    let (answer, vis) = evalState solution Set.empty
    displayVis splitters vis
    pure answer

part2 :: IO Int
part2 = do
    (start, splitters) <- parse' input <$> puzzle Main 2025 7
    let solution = runWriterT $ totalPaths splitters start
    let (answer, vis) = evalState solution Map.empty
    displayVis splitters vis
    pure answer
