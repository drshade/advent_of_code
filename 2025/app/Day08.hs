{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day08 where

import qualified Data.Set             as Set
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

type XYZ = (Int,Int,Int)

input :: Parser' (Set.Set XYZ)
input = Set.fromList <$> some ((,,) <$> (num <* char ',')
                                    <*> (num <* char ',')
                                    <*> (num <* optional newline))

sortedPairs :: Set.Set XYZ -> [(Int, XYZ, XYZ)]
sortedPairs points = sortOn (\(d,_,_) -> d) [(distance p q, p, q) | p <- Set.toList points, q <- Set.toList points, p < q]
    where distance (px,py,pz) (qx,qy,qz) = (px-qx)^2 + (py-qy)^2 + (pz-qz)^2

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 8
    let clusters = foldl (\(clusters :: Set.Set (Set.Set XYZ)) (_, p1, p2) ->
                            case (find (p1 `Set.member`) clusters, find (p2 `Set.member`) clusters) of
                                    -- mergey mergey sets and points together
                                    (Nothing, Nothing) -> Set.fromList [p1, p2] `Set.insert` clusters
                                    (Just s1, Nothing) -> (p2 `Set.insert` s1) `Set.insert` (s1 `Set.delete` clusters)
                                    (Nothing, Just s2) -> (p1 `Set.insert` s2) `Set.insert` (s2 `Set.delete` clusters)
                                    (Just s1, Just s2) -> (s1 `Set.union` s2) `Set.insert` (s1 `Set.delete` (s2 `Set.delete` clusters))
                         ) Set.empty (take 1000 $ sortedPairs values)
    pure $ product $ take 3 $ reverse $ sort (Set.size <$> Set.toList clusters)

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 8
    let Left (x1, x2) = -- Keep folding until we hit the condition of there being a single cluster
            foldM (\(clusters :: Set.Set (Set.Set XYZ)) (_, p1@(x1,_,_),p2@(x2,_,_)) ->
                let newclusters = case (find (p1 `Set.member`) clusters, find (p2 `Set.member`) clusters) of
                        (Nothing, Nothing) -> Set.fromList [p1, p2] `Set.insert` clusters
                        (Just s1, Nothing) -> (p2 `Set.insert` s1) `Set.insert` (s1 `Set.delete` clusters)
                        (Nothing, Just s2) -> (p1 `Set.insert` s2) `Set.insert` (s2 `Set.delete` clusters)
                        (Just s1, Just s2) -> (s1 `Set.union` s2) `Set.insert` (s1 `Set.delete` (s2 `Set.delete` clusters))
                   -- Horrible! Detect there being a single cluster holding all the elements of the input
                in if Set.elemAt 0 newclusters == values then Left (x1, x2) else Right newclusters
            ) Set.empty $ sortedPairs values
    pure $ x1 * x2
