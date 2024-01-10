module Main (main) where

import Lib

main :: IO ()
main = do
    -- display all closed squares
    -- user clicks square
    -- open square recursively that the user clicks & update display
    let puzzle = generateNewPuzzle 16 16 40 (16, 1)
    runGame puzzle
