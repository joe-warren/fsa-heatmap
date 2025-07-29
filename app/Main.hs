module Main (main) where

import DataFile (readData)
import System.Environment (getArgs)
import Visualize (visualize, visualizeAnimated)

isLondon :: (Double, Double, Int) -> Bool
isLondon (lon, lat, _) =
    (lat > 51.42808038005524) 
    && (lon > -0.1680498230542445)
    && (lat < 51.6083933397537)
    && (lon < 0.047826606254220506)

main :: IO ()
main = do
    a <- getArgs 
    res <- foldMap readData a
    visualize "map.png" res
    visualizeAnimated "map.gif" res
    visualize "london.png" (filter isLondon res)
    visualizeAnimated "london.gif" (filter isLondon res)
    return ()