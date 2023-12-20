{-# LANGUAGE RecordWildCards #-}

module Lib (
    generateNewPuzzle,
    runGame,
) where

-- I would prefer to use qualified imports but there is a bug with the formatter that changes "import qualified <module> as <name> to "import <module> qualified as <name>" and this gives compilation errors. Relevant github issue --> https://github.com/haskell/haskell-language-server/issues/3439
import Data.Array (Array, array, bounds, range, (!), (//))
import Data.List (nub)
import Data.Set (Set, fromList, member)
import System.Random

data State = Open | Closed
    deriving (Show)
data Square = Square
    { state :: State
    , isMine :: Bool
    , isFlagged :: Bool
    , neighbourMinesCount :: Int
    }
    deriving (Show)

type Board = Array (Int, Int) Square

-- board n by m, number of mines, fully opened board labelled with num of mine neighbours (0-8) / mine (-1) and game
-- above scrapped in favour of more expressive types
newtype Puzzle = Puzzle
    { currentBoard :: IO Board
    }

defaultSquare :: Square
defaultSquare = Square{state = Closed, isMine = False, isFlagged = False, neighbourMinesCount = 0}

-- select n distinct random indices from the range that is not equal to the excluded position
randomIndices :: Int -> (Int, Int) -> ((Int, Int), (Int, Int)) -> IO [(Int, Int)]
randomIndices n excludedPos idxRange = take n . nub . filter (/= excludedPos) . randomRs idxRange <$> newStdGen

allocateMines :: Board -> [(Int, Int)] -> Board
allocateMines emptyBoard randomIdxs =
    emptyBoard // [(idx, (emptyBoard ! idx){isMine = True}) | idx <- randomIdxs]

-- we only count the number of neighbour mines for non-mine squares
setNeighbourMinesCount :: Set (Int, Int) -> Board -> Board
setNeighbourMinesCount mines board =
    board // [(idx, (board ! idx){neighbourMinesCount = getCount idx}) | idx <- range $ bounds board, not $ idx `member` mines]
  where
    coords = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)]
    getCount (i, j) = sum [if (x + i, y + j) `member` mines then 1 else 0 | (x, y) <- coords, inBound (x + i, y + j)]
    inBound (a, b) =
        let (lower, upper) = bounds board
         in (a >= fst lower && b >= snd lower) && (a <= fst upper && b <= snd upper)

createGameBoard :: Int -> Int -> Int -> (Int, Int) -> IO Board
createGameBoard x y numMines excludedPos =
    let emptyBoard = createBoard x y defaultSquare
     in do
            mineIdxs <- randomIndices numMines excludedPos ((1, 1), (x, y))
            let mines = fromList mineIdxs
            return $ setNeighbourMinesCount mines $ allocateMines emptyBoard mineIdxs

createBoard :: Int -> Int -> Square -> Board
createBoard x y value = array ((1, 1), (x, y)) [((i, j), value) | i <- [1 .. x], j <- [1 .. y]]

generateNewPuzzle :: Int -> Int -> Int -> (Int, Int) -> Puzzle
generateNewPuzzle x y numMines excludedPos = Puzzle{currentBoard = createGameBoard x y numMines excludedPos}

runGame :: Puzzle -> IO ()
runGame (Puzzle{..}) = do
    putStrLn "Welcome to minesweeper!"
    board <- currentBoard
    print board
