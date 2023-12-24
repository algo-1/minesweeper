{-# LANGUAGE RecordWildCards #-}

module Lib (
    generateNewPuzzle,
    runGame,
) where

-- I would prefer to use qualified imports but there is a bug with the formatter that changes "import qualified <module> as <name> to "import <module> qualified as <name>" and this gives compilation errors. Relevant github issue --> https://github.com/haskell/haskell-language-server/issues/3439

import Data.Array (Array, array, bounds, range, (!), (//))
import Data.HashSet (HashSet, fromList, member)
import Data.List (foldl', nub)
import System.Random
import Text.Read (readMaybe)

data State = Open | Closed
    deriving (Show, Eq)
data Square = Square
    { state :: State
    , isFlagged :: Bool
    , neighbourMinesCount :: Int
    , isSolverFlagged :: Bool
    }
    deriving (Show)

type Index = (Int, Int)

type Board = Array Index Square

data Puzzle = Puzzle
    { board :: Board
    , mines :: HashSet Index
    }

data Move = OpenSquare | ToggleFlag
    deriving (Show, Read)

isMine :: HashSet Index -> Index -> Bool
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
                        Closed -> putStr (if isFlagged square || isSolverFlagged square then "F " else "O ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

printBoardDebug :: Board -> HashSet Index -> IO ()
printBoardDebug board mines = do
    let (_, (iMax, jMax)) = bounds board
    mapM_
        ( \i -> do
            mapM_
                ( \j -> do
                    let square = board ! (i, j)
                    let count = neighbourMinesCount square
                    case state square of
                        Open -> putStr (if count /= 0 then show count ++ " " else "  ")
                        Closed -> (if isMine mines (i, j) then putStr "X " else putStr $ show count ++ " ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

defaultSquare :: Square
defaultSquare = Square{state = Closed, isFlagged = False, neighbourMinesCount = -1, isSolverFlagged = False}

-- the 8 directions
coords :: [Index]
coords = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, 1), (1, 1), (-1, -1), (1, -1)]

-- select n distinct random indices from the range that are not equal to the excluded position
-- provided that the range is large enough, if not all indices in the range will be returned.
randomIndices :: Int -> Index -> (Index, Index) -> IO [Index]
randomIndices n excludedPos idxRange = take n . nub . filter (/= excludedPos) . randomRs idxRange <$> newStdGen

-- we only count the number of neighbour mines for non-mine squares
setNeighbourMinesCount :: HashSet Index -> Board -> Board
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
openSquare :: Index -> HashSet Index -> Board -> Board
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

gameWon :: Board -> HashSet Index -> Bool
gameWon board mines = all (== Open) [state (board ! idx) | idx <- range $ bounds board, not $ isMine mines idx]

closedNeighbours :: Board -> Index -> [Index]
closedNeighbours board (i, j) = [(x + i, y + j) | (x, y) <- coords, inBound (x + i, y + j) board && state (board ! (x + i, y + j)) == Closed]

unflaggedClosedNeighbours :: Board -> Index -> [Index]
unflaggedClosedNeighbours board idx = filter (\closedNeighIdx -> not $ isSolverFlagged (board ! closedNeighIdx)) (closedNeighbours board idx)

isNumberedSquare :: Board -> Index -> Bool
isNumberedSquare board idx = state (board ! idx) == Open && neighbourMinesCount (board ! idx) > 0

-- return one index from the trivially open indices, where a trivially open index (x, y) is one where
--  there exists a neighbouring index (i, j) that has a neighbouring mine count == the number of its neighbours that are flagged
triviallyOpen :: Board -> Maybe Index
triviallyOpen board =
    if not (null xs) then Just (head xs) else Nothing
  where
    xs = concatMap (unflaggedClosedNeighbours board) allUnflaggedClosedNeighboursSafe
    allUnflaggedClosedNeighboursSafe = [idx | idx <- range $ bounds board, isNumberedSquare board idx && neighbourMinesCount (board ! idx) == numFlaggedNeighbours idx]
    numFlaggedNeighbours (i, j) = sum $ map (\idx -> if isSolverFlagged (board ! idx) then 1 else 0) (closedNeighbours board (i, j))

-- flag all closed neighbours of (i, j) where number of neighbouring mines of (i, j) == number of closedNeighbours
trivialFlags :: Board -> Board
trivialFlags board = board // [(idx, (board ! idx){isSolverFlagged = True}) | idx <- concatMap (closedNeighbours board) flagNeighbours]
  where
    flagNeighbours = [idx | idx <- range $ bounds board, isNumberedSquare board idx && neighbourMinesCount (board ! idx) == length (closedNeighbours board idx)]

-- returns the index of the first cloosed square it finds that is definitely not a mine or the flagged board
-- note flagged here is set using isSolverFlagged no number of neighbours because we cannot trust the flags on the board because the user can flag a non-mine before triggering the solver
findTrivialSafeSquare :: Board -> (Maybe Index, Board)
findTrivialSafeSquare board =
    let flagged_board = trivialFlags board in (triviallyOpen flagged_board, flagged_board)

-- used when the solver does not find an unambigous play
-- TODO: could be helpful to use info about number of remaining mines cite https://dash.harvard.edu/bitstream/handle/1/14398552/BECERRA-SENIORTHESIS-2015.pdf, be carefula about the correct probabilities
-- solver only accesses neighbour count of open squares as it would be cheating to use that information from closed squares
-- TODO: returns index of least "dangerous" square based on probabilities, corner, edge, inner heuristic ?
probabilisticSolver :: Board -> StdGen -> Index
probabilisticSolver board g =
    let potentialSafeSquares = [idx | idx <- range $ bounds board, state (board ! idx) == Closed && not (isSolverFlagged (board ! idx))]
        (list_idx, _) = randomR (0, length potentialSafeSquares - 1) g
     in potentialSafeSquares !! list_idx

-- csp Solver
-- TODO: use csp, efficient algorithm & reasonable heuristics to get a safe square, if still cannot, use probabilities from solutions
-- TODO: retain useful info in board for next play? more record fields? / csp data type ++ ?
cspSolver :: Board -> Index
cspSolver = undefined

solver :: Board -> StdGen -> (Index, Board)
solver board g = case findTrivialSafeSquare board of
    (Just idx, flagged_board) -> (idx, flagged_board)
    (Nothing, flagged_board) -> (probabilisticSolver flagged_board g, flagged_board)

play :: Move -> Board -> HashSet Index -> Index -> Board
play OpenSquare board mines idx = openSquare idx mines board
play ToggleFlag board _ idx = toggleFlagSquare idx board

loop :: Board -> HashSet Index -> IO ()
loop board mines = do
    -- move <- readMove -- from user via cli
    -- idx <- readTuple -- from user via cli
    let move = OpenSquare -- solver only opens square
    g <- newStdGen
    let (idx, flagged_board) = solver board g
    print "flagged board"
    printBoard flagged_board
    print [(index, isSolverFlagged (flagged_board ! index)) | index <- range $ bounds flagged_board, state (flagged_board ! index) == Closed && not (isSolverFlagged (flagged_board ! index))] -- potentially safe squares
    print "triviallyOpen"
    print $ triviallyOpen flagged_board
    print "probabilistic solver"
    print $ probabilisticSolver flagged_board g
    putStrLn ""
    print idx
    putStrLn ""
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
    if gameWon board mines
        then do
            print "Congratulations! You won with your first move!"
        else loop board mines
