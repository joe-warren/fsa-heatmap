module Visualize
( visualize
, visualizeAnimated
) where

import Codec.Picture
import Codec.Picture.Gif
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Word (Word8)
import Data.Foldable (toList)

lat :: (Double, Double, Int) -> Double
lat (l, _, _) = l

lon :: (Double, Double, Int) -> Double
lon (_, l, _) = l

score :: (Double, Double, Int) -> Int
score (_, _, s) = s

toI :: Double -> Word8
toI = floor . (*255)

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
                        colourness = (1 - (1-c) ** 2) * 0.9 + 0.1
                        g = toI (1-colourness)
                        r = toI ((1 - blueness) * colourness + (1 - colourness))
                        b = toI ((blueness) * colourness + (1 - colourness))
                    in PixelRGB8 r g b
    in generateImage pxValue (floor w) (floor h)


visualize :: String -> [(Double, Double, Int)] -> IO ()
visualize filepath d = writePng filepath (toImage d)

hwb :: Double -> Double -> Double -> PixelRGB8
hwb h w b = 
    let v = 1-b
        i :: Int
        i = floor h
        f = h - fromIntegral i
        f' = if odd i then 1 - f else f
        n = w + f' * (v - w) 
    in case i of    
        1 -> PixelRGB8 (toI n) (toI v) (toI w)
        2 -> PixelRGB8 (toI w) (toI v) (toI n)
        3 -> PixelRGB8 (toI w) (toI n) (toI v)
        4 -> PixelRGB8 (toI n) (toI w) (toI v)
        5 -> PixelRGB8 (toI v) (toI w) (toI n)
        _ -> PixelRGB8 (toI v) (toI n) (toI w)

toImageSequence :: [(Double, Double, Int)] -> GifEncode
toImageSequence d = 
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

        m :: M.Map (Int, Int) [Int]
        m = M.fromListWith (<>) ((bucket &&& (pure . score)) <$> d)

        maxV = maximum $ do
            v <- toList m
            i <- [0..6]
            return . fromIntegral . length $ filter (== i) v

        frame :: Int -> GifFrame
        frame i = 
            let 
                pxValue x y =
                    case M.lookup (x, y) m of 
                        Nothing -> 0
                        Just scores ->  
                            let v = fromIntegral . length . filter (== i) $ scores
                                v' = (v / maxV) ** 0.2
                            in toI v'
                paleteValue x _ = 
                    let f = (fromIntegral x) / 255
                    in hwb (fromIntegral i * 5 / 6) (1-f) (if f == 0 then 0 else 0.1)
                palette = generateImage paleteValue 256 1
                in GifFrame 0 0 (Just palette) Nothing 100 DisposalAny (generateImage pxValue (floor w) (floor h))
    in GifEncode (floor w) (floor h) Nothing Nothing LoopingForever (frame <$> [0..5]) 

visualizeAnimated :: String -> [(Double, Double, Int)] -> IO ()
visualizeAnimated s d = either error id $ writeComplexGifImage s (toImageSequence d)
