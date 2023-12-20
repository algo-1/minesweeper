module Main (main) where

import Lib

main :: IO ()
main = do
    let puzzle = generateNewPuzzle 10 8 20 (5, 7)
    runGame puzzle
