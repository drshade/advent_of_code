module Day07Vis where

import qualified Data.Set             as Set
import           Handy
import           Raylib.Core
import           Raylib.Core.Shapes
import           Raylib.Core.Text
import           Raylib.Core.Textures (drawTexturePro, loadRenderTexture,
                                       unloadRenderTexture)
import           Raylib.Types
import           Raylib.Util
import           Raylib.Util.Colors

type Visualise = WriterT [XY]

displayVis :: Set.Set XY -> [XY] -> IO ()
displayVis splitters trace = do
    let fps = 1000
        (screenX, screenY) = (1000, 1000)
        (minX, minY, maxX, maxY) = ( minimum $ fst <$> trace, minimum $ snd <$> trace
                                   , maximum $ fst <$> trace, maximum $ snd <$> trace )

        cellSize = min (fromIntegral (screenX - 80) / fromIntegral (maxX - minX + 1))
                       (fromIntegral (screenY - 80) / fromIntegral (maxY - minY + 1))

        offsetX = fromIntegral 40 - fromIntegral minX * cellSize
        offsetY = fromIntegral 40 - fromIntegral minY * cellSize

        toScreenX x = round (fromIntegral x * cellSize + offsetX)
        toScreenY y = round (fromIntegral y * cellSize + offsetY)

    withWindow screenX screenY "AoC 2025" fps (\window -> do
        buffer <- loadRenderTexture screenX screenY

        -- Draw static splitters to buffer once
        textureMode buffer $ do
            clearBackground black
            forM_ (Set.toList splitters) $ \(x, y) ->
                drawRectangle (toScreenX x) (toScreenY y) (round 2) (round 2) yellow

        let loop remaining step = do
                shouldClose <- windowShouldClose
                unless shouldClose $ do
                    let (batch, remaining') = splitAt 1 remaining
                        current = if null batch then Nothing else Just (last batch)

                    -- Draw all cells in batch to buffer
                    forM_ batch $ \(x, y) ->
                        textureMode buffer $ do
                            -- let (x, y) = toScreen (x,y)
                            drawRectangle (toScreenX x) (toScreenY y) (round 2) (round 2) (Color 0 150 0 200)

                    -- Blit buffer to screen
                    drawing $ do
                        clearBackground black
                        -- Render texture is flipped vertically
                        drawTexturePro
                            (renderTexture'texture buffer)
                            (Rectangle 0 (fromIntegral screenY) (fromIntegral screenX) (fromIntegral (-screenY)))
                            (Rectangle 0 0 (fromIntegral screenX) (fromIntegral screenY))
                            (Vector2 0 0)
                            0
                            white

                        drawText ("Step: " <> show step) 10 10 20 white

                    loop remaining' (step + 1)

        loop trace 0
        unloadRenderTexture buffer window) `catch` (\(e :: SomeException) -> putStrLn $ "Failed to boot visualisation... Repl? " <> displayException e)
