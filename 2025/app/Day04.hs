{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day04 where

import qualified Data.Set             as Set
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' (Set.Set XY)
input = Set.fromList . catMaybes <$> some (choice [ (Just <$> xy) <* char '@'
                                                  , Nothing <$ char '.'
                                                  ] <* optional newline)

neighbours :: Set.Set XY -> XY -> [XY]
neighbours grid (px,py) = filter (`Set.member` grid) [(px+x,py+y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)]

part1 :: IO Int
part1 = do
    grid <- parse' input <$> puzzle Main 2025 4
    pure $ length $ filter ((< 4) . length . neighbours grid) (Set.toList grid)

part2 :: IO Int
part2 = do
    grid <- parse' input <$> puzzle Main 2025 4
    let Left answer = foldM (\(cnt, grid') _ -> -- continuously remove valid positions until there are zero
            case Set.filter ((< 4) . length . neighbours grid') grid' of
                    n | null n -> Left cnt
                    n          -> Right (cnt + length n, Set.difference grid' n)
         ) (0, grid) grid
    pure answer
