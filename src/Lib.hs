{-# LANGUAGE RecordWildCards #-}

module Lib (
    generateNewPuzzle,
    runGame,
) where

-- I would prefer to use qualified imports but there is a bug with the formatter that changes "import qualified <module> as <name> to "import <module> qualified as <name>" and this gives compilation errors. Relevant github issue --> https://github.com/haskell/haskell-language-server/issues/3439

import Data.Array (Array, array, bounds, range, (!), (//))
import Data.List (foldl', nub)
import Data.Set (Set, empty, fromList, insert, member)
import System.Random
import Text.Read (readMaybe)

data State = Open | Closed
    deriving (Show, Eq)
data Square = Square
    { state :: State
    , isFlagged :: Bool
    , neighbourMinesCount :: Int
    }
    deriving (Show)

type Index = (Int, Int)

type Board = Array Index Square

data Puzzle = Puzzle
    { board :: Board
    , mines :: Set Index
    }

isMine :: Set Index -> Index -> Bool
isMine mines idx = idx `member` mines

printBoard :: Board -> IO ()
printBoard board = do
    let (_, (iMax, jMax)) = bounds board
    mapM_
        ( \i -> do
            mapM_
                ( \j -> do
                    let square = board ! (i, j)
                    let count = neighbourMinesCount square
                    putStr (if state square == Open then (if count /= 0 then show count ++ " " else "  ") else "O ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

printBoardDebug :: Board -> IO ()
printBoardDebug board = do
    let (_, (iMax, jMax)) = bounds board
    mapM_
        ( \i -> do
            mapM_
                ( \j -> do
                    let square = board ! (i, j)
                    let count = neighbourMinesCount square
                    putStr $ show count ++ " "
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

defaultSquare :: Square
defaultSquare = Square{state = Closed, isFlagged = False, neighbourMinesCount = 0}

-- select n distinct random indices from the range that are not equal to the excluded position
randomIndices :: Int -> Index -> (Index, Index) -> IO [Index]
randomIndices n excludedPos idxRange = take n . nub . filter (/= excludedPos) . randomRs idxRange <$> newStdGen

-- we only count the number of neighbour mines for non-mine squares
setNeighbourMinesCount :: Set Index -> Board -> Board
setNeighbourMinesCount mines board =
    board // [(idx, (board ! idx){neighbourMinesCount = getCount idx}) | idx <- range $ bounds board, not $ idx `member` mines]
  where
    coords = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)]
    getCount (i, j) = sum [if isMine mines (x + i, y + j) then 1 else 0 | (x, y) <- coords, inBound (x + i, y + j) board]

createBoard :: Int -> Int -> Square -> Board
createBoard x y value = array ((1, 1), (x, y)) [((i, j), value) | i <- [1 .. x], j <- [1 .. y]]

-- generates new puzzle where squares that have no mine neighbour on the board are opened recursively from the excludedPos,
generateNewPuzzle :: Int -> Int -> Int -> Index -> IO Puzzle
generateNewPuzzle x y numMines excludedPos =
    let emptyBoard = createBoard x y defaultSquare
     in do
            mineIdxs <- randomIndices numMines excludedPos ((1, 1), (x, y))
            let mines = fromList mineIdxs
            return $ Puzzle (openSquare excludedPos mines empty $ setNeighbourMinesCount mines emptyBoard) mines

hasNoMineNeighbours :: Index -> Board -> Bool
hasNoMineNeighbours idx board = neighbourMinesCount (board ! idx) == 0

inBound :: Index -> Board -> Bool
inBound (a, b) board =
    let (lower, upper) = bounds board
     in (a >= fst lower && b >= snd lower) && (a <= fst upper && b <= snd upper)

-- before calling this method check if index is a mine or flag and take appropriate action
-- assuming just a regular open index, recursively open all spots with no mine neighbours, if a tile has a mnine neighbour return but leave opened and display the neighbour mines count.
-- TODO: remove unnecessary part of commentary after implementation & testing?
openSquare :: Index -> Set Index -> Set Index -> Board -> Board
openSquare (i, j) mines visited board =
    let new_board = board // [((i, j), (board ! (i, j)){state = Open})]
     in let new_visited = insert (i, j) visited
         in let coords = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)] :: [Index]
             in let neighbours = [(x + i, y + j) | (x, y) <- coords, inBound (x + i, y + j) new_board && not (isMine mines (x + i, y + j)) && not ((x + i, y + j) `member` new_visited)]
                 in if hasNoMineNeighbours (i, j) new_board
                        then foldl' (f new_visited) new_board neighbours
                        else new_board
  where
    f visited' board' idx = openSquare idx mines visited' board'

readTuple :: IO (Int, Int)
readTuple = do
    putStrLn "Enter the first integer:"
    xStr <- getLine
    let mx = readMaybe xStr :: Maybe Int

    putStrLn "Enter the second integer:"
    yStr <- getLine
    let my = readMaybe yStr :: Maybe Int

    case (mx, my) of
        (Just x, Just y) -> return (x, y)
        _ -> do
            putStrLn "Invalid input. Please enter valid integers."
            readTuple

gameWon :: Board -> Set Index -> Bool
gameWon board mines = all (== Open) [state (board ! idx) | idx <- range $ bounds board, not $ isMine mines idx]

loop :: Board -> Set Index -> IO ()
loop board mines = do
    idx <- readTuple
    if idx == (-1, -1)
        then return ()
        else
            if not $ isMine mines idx
                then do
                    let new_board = openSquare idx mines empty board
                    printBoard new_board
                    if gameWon new_board mines
                        then do
                            print "Congratulations! You won!"
                            return ()
                        else do
                            putStrLn ""
                            loop new_board mines
                else do
                    print "Game over! you picked a mine!"
                    return ()

runGame :: IO Puzzle -> IO ()
runGame puzzleIO = do
    Puzzle{..} <- puzzleIO
    putStrLn "Welcome to Minesweeper!"
    putStrLn "O means the square is not opened.\nBlank means no neighbour mines.\nNumbers 1-8 indicates how many neighbouring mines."
    print "Debug board"
    printBoardDebug board
    putStrLn ""
    print "Board after first click!"
    printBoard board
    putStrLn ""
    print "Mines Below"
    print mines
    putStrLn ""
    loop board mines
