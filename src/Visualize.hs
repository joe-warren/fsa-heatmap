module Visualize
( visualize
) where

import Codec.Picture
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Word (Word8)

lat :: (Double, Double, Int) -> Double
lat (l, _, _) = l

lon :: (Double, Double, Int) -> Double
lon (_, l, _) = l

score :: (Double, Double, Int) -> Int
score (_, _, s) = s

toImage :: [(Double, Double, Int)]-> Image PixelRGB8
toImage d = 
    let minX = minimum (lat <$> d)
        maxX = maximum (lat <$> d) 
        minY = minimum (lon <$> d)
        maxY = maximum (lon <$> d)
        dX = maxX - minX
        dY = maxY - minY
        w = 600
        h = dY * w / dX
        bucket :: (Double, Double, Int) -> (Int, Int)
        bucket p = let x = floor ((lat p - minX) * w / dX)
                       y = floor (h - (lon p - minY) * w / dX )
                    in (x, y)

        white = PixelRGB8 255 255 255 
        m :: M.Map (Int, Int) [Int]
        m = M.fromListWith (<>) ((bucket &&& (pure . score)) <$> d)

        avg scores = fromIntegral (sum scores) / fromIntegral (length scores) 
        maxR = maximum $ length <$> m
        maxV = maximum (avg <$> m)
        minV = minimum (avg <$> m)
        dV = maxV - minV

        pxValue x y =
            case M.lookup (x, y) m of 
                Nothing -> white
                Just scores ->  
                    let bb = (avg scores - minV) / dV
                        blueness = bb ** 2 
                        c = (fromIntegral (length scores) / fromIntegral maxR) 
                        colourness = 1 - (1-c) ** 4
                        toI :: Double -> Word8
                        toI = floor . (*255)
                        g = toI (1-colourness)
                        r = toI ((1 - blueness) * colourness + (1 - colourness))
                        b = toI ((blueness) * colourness + (1 - colourness))
                    in PixelRGB8 r g b
    in generateImage pxValue (floor w) (floor h)


visualize :: String -> [(Double, Double, Int)] -> IO ()
visualize filepath d = writePng filepath (toImage d)