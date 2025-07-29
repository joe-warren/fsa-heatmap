module Main (main) where

import DataFile (readData)
import System.Environment (getArgs)
import Visualize (visualize)

main :: IO ()
main = do
    a <- getArgs 
    res <- foldMap readData a
    visualize "map.png" res
    return ()