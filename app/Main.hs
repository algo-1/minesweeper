module Main (main) where

import Lib

main :: IO ()
main = do
    let puzzle = generateNewPuzzle 16 16 40 (8, 8)
    runGame puzzle
