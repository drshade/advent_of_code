{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 where

import           Handy
import           Prelude              hiding (loop, some)
import           Raylib.Core          (clearBackground, windowShouldClose)
import           Raylib.Core.Shapes   (drawLine, drawRectangle)
import           Raylib.Types         (Color (..))
import           Raylib.Util          (drawing, withWindow)
import           Raylib.Util.Colors   (black)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' [XY]
input = some $ (,) <$> num <* char ','
                   <*> num <* optional newline

area :: (XY, XY) -> Int -- +1 because inclusive of bounds...
area ((ax,ay), (bx,by)) = (abs (bx - ax) + 1) * (abs (by - ay) + 1)

part1 :: IO Int
part1 = do
    pts <- parse' input <$> puzzle Main 2025 9
    pure $ maximum $ area <$> [(a,b) | a <- pts, b <- pts]

draw :: [XY] -> [(XY, XY)] -> IO ()
draw pts candidates = do
    let fps = 20
        (screenX, screenY) = (1000, 1000)
        (minX, minY, maxX, maxY) = ( minimum $ fst <$> pts, minimum $ snd <$> pts
                                   , maximum $ fst <$> pts, maximum $ snd <$> pts )
        toScreen :: XY -> XY -- convert challenge coord to screen coord
        toScreen (x,y) = ( (x - minX) * screenX `div` (maxX - minX + 1) + 1, (y - minY) * screenY `div` (maxY - minY + 1) + 1)
    withWindow screenX screenY "AoC 2025" fps (\_ -> do
        let loop ((p1, p2) : rest) = do
                shouldClose <- windowShouldClose
                unless shouldClose $ do
                    drawing $ do
                        clearBackground black
                        let screenPts = toScreen <$> pts ++ [head pts] -- wrap around
                        foldM_ (\(lx,ly) (x, y) -> do
                            drawRectangle (x - 2) (y - 2) (round 4) (round 4) (Color 0 150 0 200)
                            drawLine lx ly x y (Color 150 0 0 200)
                            pure (x,y)
                            ) (head screenPts) (tail screenPts ++ [head screenPts])

                        let (rx, ry)  = (minimum $ fst <$> [toScreen p1, toScreen p2], minimum $ snd <$> [toScreen p1, toScreen p2])
                        let (w, h)    = (maximum (fst <$> [toScreen p1, toScreen p2]) - rx, maximum (snd <$> [toScreen p1, toScreen p2]) - ry)
                        drawRectangle rx ry w h (Color 150 150 0 200)
                    loop rest
        loop (cycle candidates)
        )

-- Is p inside the rect?
inside :: XY -> (XY, XY) -> Bool
inside (px,py) ((x1,y1), (x2,y2)) = px > min x1 x2 && px < max x1 x2 && py > min y1 y2 && py < max y1 y2

part2 :: IO Int
part2 = do
    pts <- parse' input <$> puzzle Main 2025 9

    -- from my data, these are the two weird points: (94800,50143) and (94800,48628)
    -- I assume these must be at least one corner of the solution

    -- all points above our first point and all the points below our second point ;)
    let pairs = [(a,b) | let a = (94800,48628), b <- pts, snd b < snd a]
             ++ [(a,b) | let a = (94800,50143), b <- pts, snd b > snd a]

    -- Only consider pairs which form empty rectangles (no points from the polygon inside it)
    let candidates = filter (\(rect :: (XY,XY)) -> not $ any (`inside` rect) pts) pairs
    let (maxArea, _rect) = maximumBy (compare `on` fst) $ (\rect -> (area rect, rect)) <$> candidates
    draw pts candidates
    pure maxArea
