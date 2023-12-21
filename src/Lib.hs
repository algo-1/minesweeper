{-# LANGUAGE RecordWildCards #-}

module Lib (
    generateNewPuzzle,
    runGame,
) where

-- I would prefer to use qualified imports but there is a bug with the formatter that changes "import qualified <module> as <name> to "import <module> qualified as <name>" and this gives compilation errors. Relevant github issue --> https://github.com/haskell/haskell-language-server/issues/3439

import Data.Array (Array, array, bounds, range, (!), (//))
import Data.List (foldl', nub)
import Data.Set (Set, fromList, member)
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

data Move = OpenSquare | ToggleFlag
    deriving (Show, Read)

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
                    case state square of
                        Open -> putStr (if count /= 0 then show count ++ " " else "  ")
                        Closed -> putStr (if isFlagged square then "F " else "O ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

printBoardDebug :: Board -> Set Index -> IO ()
printBoardDebug board mines = do
    let (_, (iMax, jMax)) = bounds board
    mapM_
        ( \i -> do
            mapM_
                ( \j -> do
                    let square = board ! (i, j)
                    let count = neighbourMinesCount square
                    (if isMine mines (i, j) then putStr "X " else putStr $ show count ++ " ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

defaultSquare :: Square
defaultSquare = Square{state = Closed, isFlagged = False, neighbourMinesCount = 0}

-- the 8 directions
coords :: [Index]
coords = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)]

-- select n distinct random indices from the range that are not equal to the excluded position
-- provided that the range is large enough, if not all indices in the range will be returned.
randomIndices :: Int -> Index -> (Index, Index) -> IO [Index]
randomIndices n excludedPos idxRange = take n . nub . filter (/= excludedPos) . randomRs idxRange <$> newStdGen

-- we only count the number of neighbour mines for non-mine squares
setNeighbourMinesCount :: Set Index -> Board -> Board
setNeighbourMinesCount mines board =
    board // [(idx, (board ! idx){neighbourMinesCount = getCount idx}) | idx <- range $ bounds board, not $ idx `member` mines]
  where
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
            return $ Puzzle (openSquare excludedPos mines $ setNeighbourMinesCount mines emptyBoard) mines

hasNoMineNeighbours :: Index -> Board -> Bool
hasNoMineNeighbours idx board = neighbourMinesCount (board ! idx) == 0

inBound :: Index -> Board -> Bool
inBound (a, b) board =
    let (lower, upper) = bounds board
     in (a >= fst lower && b >= snd lower) && (a <= fst upper && b <= snd upper)

-- before calling this method check if index is a mine or flag and take appropriate action
-- assuming just a regular open index, recursively open all spots with no mine neighbours, if a tile has a mnine neighbour return but leave opened and display the neighbour mines count.
-- TODO: remove unnecessary part of commentary after implementation & testing?
openSquare :: Index -> Set Index -> Board -> Board
openSquare (i, j) mines board =
    let new_board = board // [((i, j), (board ! (i, j)){state = Open})]
     in let neighbours = [(x + i, y + j) | (x, y) <- coords, inBound (x + i, y + j) new_board && not (isMine mines (x + i, y + j)) && state (new_board ! (x + i, y + j)) == Closed]
         in if hasNoMineNeighbours (i, j) new_board
                then foldl' f new_board neighbours
                else new_board
  where
    f board' idx = openSquare idx mines board'

-- only works for closed squares
toggleFlagSquare :: Index -> Board -> Board
toggleFlagSquare idx board
    | state (board ! idx) == Closed = board // [(idx, (board ! idx){isFlagged = not $ isFlagged (board ! idx)})]
    | otherwise = board

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

readMove :: IO Move
readMove = do
    putStrLn "Enter the type of Move (OpenSquare | ToggleFlag):"
    xStr <- getLine
    let move = readMaybe xStr :: Maybe Move

    case move of
        (Just m) -> return m
        _ -> do
            putStrLn "Invalid input. Please enter a valid move."
            readMove

gameWon :: Board -> Set Index -> Bool
gameWon board mines = all (== Open) [state (board ! idx) | idx <- range $ bounds board, not $ isMine mines idx]

play :: Move -> Board -> Set Index -> Index -> Board
play OpenSquare board mines idx = openSquare idx mines board
play ToggleFlag board _ idx = toggleFlagSquare idx board

loop :: Board -> Set Index -> IO ()
loop board mines = do
    move <- readMove
    idx <- readTuple
    if idx == (-1, -1)
        then do
            print "You have successfully quit the game."
            return ()
        else case move of
            OpenSquare ->
                -- do not do anything if user tries to open a square that is open or flagged
                ( if isFlagged (board ! idx) || state (board ! idx) == Open
                    then do
                        printBoard board
                        putStrLn ""
                        loop board mines
                    else
                        ( if not $ isMine mines idx
                            then do
                                let new_board = play move board mines idx
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
                                printBoardDebug board mines
                                return ()
                        )
                )
            ToggleFlag -> do
                let new_board = play move board mines idx
                printBoard new_board
                putStrLn ""
                loop new_board mines

runGame :: IO Puzzle -> IO ()
runGame puzzleIO = do
    Puzzle{..} <- puzzleIO
    putStrLn "Welcome to Minesweeper!"
    putStrLn "O means the square is not opened.\nBlank means no neighbour mines.\nNumbers 1-8 indicates how many neighbouring mines."
    print "Debug board"
    printBoardDebug board mines
    putStrLn ""
    print "Board after first click!"
    printBoard board
    putStrLn ""
    print "Mines Below"
    print mines
    putStrLn ""
    loop board mines
