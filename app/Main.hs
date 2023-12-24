module Main (main) where

import Lib

main :: IO ()
main = do
    -- display all closed squares
    -- user clicks square
    -- open square recursively that the user clicks & update display
    let puzzle = generateNewPuzzle 4 4 4 (4, 1) -- tiny board for testing (4x4 & 4 mines) start play is set to bottom left for better chance of more information?
    runGame puzzle
