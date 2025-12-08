module Day07Viz where

import           Control.Monad.Trans.Writer.Strict (runWriterT)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Day07                             as Day07
import           Handy
import           Raylib.Core
import           Raylib.Core.Shapes
import           Raylib.Core.Text
import           Raylib.Core.Textures              (drawTexturePro,
                                                    loadRenderTexture,
                                                    unloadRenderTexture)
import           Raylib.Types
import           Raylib.Util
import           Raylib.Util.Colors

type Visualise m = WriterT [XY] m

-- DFS with State (to keep count of number of times we split)
totalSplits :: Set Day07.Splitter -> XY -> WriterT [XY] (State (Set XY)) Int
totalSplits splitters xy@(x, y) = do
    tell [xy]
    counted <- get
    if y > Day07.maxdepth splitters || xy `Set.member` counted
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
totalPaths :: Set Day07.Splitter -> XY -> WriterT [XY] (State (Map.Map XY Int)) Int
totalPaths splitters xy@(x, y) = do
    tell [xy]
    memo <- get
    case Map.lookup xy memo of
        Just a -> pure a
        Nothing -> do
            count <- if y > Day07.maxdepth splitters
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


runVis :: Set.Set XY -> [XY] -> IO ()
runVis splitters trace = do
    let width = 800
        height = 600

        allXs = map fst trace ++ map fst (Set.toList splitters)
        allYs = map snd trace ++ map snd (Set.toList splitters)
        minX = minimum allXs
        maxX = maximum allXs
        minY = minimum allYs
        maxY = maximum allYs

        gridW = maxX - minX + 1
        gridH = maxY - minY + 1

        padding = 80
        cellSize = min (fromIntegral (width - padding) / fromIntegral gridW)
                       (fromIntegral (height - padding) / fromIntegral gridH)

        offsetX = fromIntegral (padding `div` 2) - fromIntegral minX * cellSize
        offsetY = fromIntegral (padding `div` 2) - fromIntegral minY * cellSize

        toScreenX x = round (fromIntegral x * cellSize + offsetX)
        toScreenY y = round (fromIntegral y * cellSize + offsetY)

    withWindow width height "AoC 2025 Day 7 - DFS" 500 $ \window -> do
        buffer <- loadRenderTexture width height

        -- Draw static splitters to buffer once
        textureMode buffer $ do
            clearBackground black
            forM_ (Set.toList splitters) $ \(x, y) ->
                drawRectangle (toScreenX x) (toScreenY y)
                    (round cellSize) (round cellSize)
                    yellow

        let loop remaining step = do
                shouldClose <- windowShouldClose
                unless shouldClose $ do
                    let (remaining', current) = case remaining of
                            []     -> ([], Nothing)
                            (x:xs) -> (xs, Just x)

                    -- Draw only the new cell to buffer
                    forM_ current $ \(x, y) ->
                        textureMode buffer $
                            drawRectangle (toScreenX x) (toScreenY y)
                                (round cellSize) (round cellSize)
                                (Color 0 150 0 200)

                    -- Blit buffer to screen
                    drawing $ do
                        clearBackground black
                        -- Render texture is flipped vertically
                        drawTexturePro
                            (renderTexture'texture buffer)
                            (Rectangle 0 (fromIntegral height) (fromIntegral width) (fromIntegral (-height)))
                            (Rectangle 0 0 (fromIntegral width) (fromIntegral height))
                            (Vector2 0 0)
                            0
                            white

                        -- Current position highlight (on top)
                        forM_ current $ \(x, y) ->
                            drawRectangle (toScreenX x) (toScreenY y)
                                (round cellSize) (round cellSize)
                                red

                        drawText ("Step: " <> show step) 10 10 20 white

                    loop remaining' (step + 1)

        loop trace 0
        unloadRenderTexture buffer window

-- Entry point
visualize :: IO Int
visualize = do
    (start, splitters) <- parse' Day07.input <$> puzzle Main 2025 7
    let w = runWriterT $ totalPaths splitters start
    let (result, path) = evalState w Map.empty
    print $ "Result is: " <> show result
    print $ "Path is  : " <> (show $ length path)
    runVis splitters path
    pure result
