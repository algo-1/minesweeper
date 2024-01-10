{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lib (
    generateNewPuzzle,
    runGame,
)
where

import Control.Monad (replicateM)
import Data.Array (Array, array, bounds, range, (!), (//))
import Data.Foldable (find, maximumBy)
import Data.Function (on)
import Data.HashSet (HashSet, fromList, member)
import Data.List (foldl', nub, sortBy, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Random
import Text.Read (readMaybe)

data State = Open | Closed
    deriving (Show, Eq)

data Square = Square
    { state :: State
    , isFlagged :: Bool
    , neighbourMinesCount :: Int
    , isSolverFlagged :: Bool
    , isSolverSafe :: Bool
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

type IndexVarValue = Int -- 0 or 1 to be precise but extra work to pattern match on types, easier to add assignment of int directly

type SumVarValue = [IndexVarValue]

-- IndexVar index is the potential mine / safe square, SumVar index is the index of a numbered square involved in a k-ary constraint. These are just convenient unique variable names.
data Variable = IndexVar Index | SumVar Index
    deriving (Eq, Show)

-- IndexVar is arbitrarily less than SumVar for Map sake
instance Ord Variable where
    compare (IndexVar _) (SumVar _) = LT
    compare (SumVar _) (IndexVar _) = GT
    compare (IndexVar i1) (IndexVar i2) = compare i1 i2
    compare (SumVar s1) (SumVar s2) = compare s1 s2

type Value = Either IndexVarValue SumVarValue

-- Unary|Binary, dclares what variable it applies to in order that the function needs,
-- Unary also has Int that specifies the length of values in its domain. For example 4 means the domain is [[0, 0, 0, 0], ... [1, 1, 1, 1]]
-- has function that takes the assignment to the variables in order specified
-- function returns True is constraint is preserved and False otherswise
data Constraint = Unary Variable Int (SumVarValue -> Bool) | Binary Variable Variable (Value -> Value -> Bool)
instance Show Constraint where
    show (Unary var idx _) = "Unary constraint on " ++ show var ++ "Num index vars = " ++ show idx
    show (Binary var1 var2 _) = "Binary constraint between " ++ show var1 ++ " and " ++ show var2 ++ " with function"

type CSPSolution = [(Variable, Value)]

type Domains = Map Variable [Value]

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

printBoardDebug2 :: Board -> HashSet Index -> IO ()
printBoardDebug2 board mines = do
    let (_, (iMax, jMax)) = bounds board
    mapM_
        ( \i -> do
            mapM_
                ( \j -> do
                    let square = board ! (i, j)
                    let count = neighbourMinesCount square
                    let safe = if isSolverSafe square then "Y" else "N"
                    case state square of
                        Open -> putStr (if count /= 0 then show count ++ " " else "  ")
                        Closed -> (if isMine mines (i, j) then putStr "X " else putStr $ safe ++ " ")
                )
                [1 .. jMax]
            putStrLn ""
        )
        [1 .. iMax]

defaultSquare :: Square
defaultSquare = Square{state = Closed, isFlagged = False, neighbourMinesCount = -1, isSolverFlagged = False, isSolverSafe = False}

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

-- gives variables in order according to number of constraints involved in (most -> least)
-- binarization of constraints happen here
-- variables are closed unflagged squares that are neighbours of numbered squares
getVariablesAndConstraints :: Board -> ([Variable], [Constraint])
getVariablesAndConstraints board =
    (indexVariables, constraints)
  where
    constraints = getConstraints
    indexVariables = map IndexVar $ concatMap (unflaggedClosedNeighbours board) numberedSquares
    numberedSquares = [idx | idx <- range $ bounds board, isNumberedSquare board idx]
    getConstraints =
        [Binary (IndexVar idx1) (IndexVar idx2) (getBinaryFunc idx) | (idx1, idx2, idx) <- twoUnflaggedClosedNeighbours] ++ [Unary (SumVar idx) (length candidates) (getUnaryFunc idx) | (candidates, idx) <- kUnflaggedClosedNeighbours] ++ concatMap binarize kUnflaggedClosedNeighbours
      where
        getBinaryFunc idx (Left a) (Left b) = (a + b) == neighbourMinesCount (board ! idx) - length (filter (isSolverFlagged . (!) board) (closedNeighbours board idx))
        getBinaryFunc _ _ _ = True

        twoUnflaggedClosedNeighbours =
            let candidates = unflaggedClosedNeighbours board
             in [(candidates idx !! 0, candidates idx !! 1, idx) | idx <- numberedSquares, length (candidates idx) == 2]

        kUnflaggedClosedNeighbours =
            let candidates = unflaggedClosedNeighbours board
             in [(candidates idx, idx) | idx <- numberedSquares, length (candidates idx) > 2]

        getUnaryFunc idx value = sum value == (neighbourMinesCount (board ! idx) - length (filter (isSolverFlagged . (!) board) (closedNeighbours board idx)))

        binarize (candidates, idx) = [Binary (IndexVar (candidates !! idx1)) (SumVar idx) (getBinaryFunc' idx1) | idx1 <- [0 .. length candidates - 1]]

        getBinaryFunc' idx (Left indexVarValue) (Right sumVarValue) = sumVarValue !! idx == indexVarValue
        getBinaryFunc' idx (Right sumVarValue) (Left indexVarValue) = sumVarValue !! idx == indexVarValue
        getBinaryFunc' _ _ _ = True

getSafestSquare :: [CSPSolution] -> Board -> Index
getSafestSquare solutions board = fromMaybe getLowestProbabilityMine f
  where
    f = find (\idx -> isSolverSafe (board ! idx) && state (board ! idx) == Closed) (range $ bounds board)
    computeProbability :: Index -> Double
    computeProbability idx = fromIntegral (length (filter (\sol -> (IndexVar idx, Left 1) `elem` sol) solutions)) / fromIntegral (length solutions)
    getLowestProbabilityMine =
        let closedSquares = filter (\idx -> state (board ! idx) == Closed) (range $ bounds board)
            probabilities = map (\idx -> (idx, computeProbability idx)) closedSquares
            (lowestIdx, _) = foldl1 (\acc@(_, prob1) (idx2, prob2) -> if prob1 < prob2 then acc else (idx2, prob2)) probabilities
         in lowestIdx

backtrack :: Board -> [Variable] -> [Constraint] -> Domains -> CSPSolution -> [CSPSolution] -> [CSPSolution]
backtrack board variables constraints domains assignment solutions
    | allVariablesAssigned assignment variables = [assignment]
    | otherwise =
        let selectedVar = selectVariable domains constraints [var | (var, _) <- assignment]
            unassignedVariables = [var | var <- variables, var `notElem` map fst assignment]
            orderedValues = lcvHeuristic selectedVar unassignedVariables domains board
         in tryValues selectedVar orderedValues assignment solutions
  where
    tryValues :: Variable -> [Value] -> CSPSolution -> [CSPSolution] -> [CSPSolution]
    tryValues _ [] _ sols = sols
    tryValues var (value : remainingValues) asnmt sols =
        if checkValue var value asnmt
            then
                let newAssignment = (var, value) : asnmt
                    (inferenceResult, updatedDomains) = ac3 constraints domains
                 in if inferenceResult
                        then
                            let
                                res = backtrack board variables constraints updatedDomains newAssignment sols
                             in
                                sols ++ res
                        else tryValues var remainingValues asnmt sols
            else tryValues var remainingValues asnmt sols

    checkValue :: Variable -> Value -> CSPSolution -> Bool
    checkValue var value sol =
        let ff = flip checkConstraint ((var, value) : sol)
         in all ff constraints

satisfiesConstraint :: Value -> Value -> (Value -> Value -> Bool) -> Bool
satisfiesConstraint x y func = func x y || func y x

-- perform revise for the (X, C) arc of (X C Y)return updated domain of X
revise :: Variable -> [Value] -> [Value] -> (Value -> Value -> Bool) -> (Variable, [Value])
revise a xDomain yDomain constraint = (a, filter (\x -> any (\y -> satisfiesConstraint x y constraint) yDomain) xDomain)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z

ac3 :: [Constraint] -> Domains -> (Bool, Domains)
ac3 constraints domains =
    let arcs =
            [(a, domains Map.! a, domains Map.! b, f) | (Binary a b f) <- constraints]
                ++ [(b, domains Map.! b, domains Map.! a, f) | (Binary a b f) <- constraints]
     in ac3Helper arcs domains constraints

ac3Helper :: [(Variable, [Value], [Value], Value -> Value -> Bool)] -> Domains -> [Constraint] -> (Bool, Domains)
ac3Helper arcs domains constraints =
    if not (null arcs)
        then
            let
                -- Step 1: Remove the first arc from arcs
                (currentVar, xDomain, yDomain, constraint) : remainingArcs = arcs

                -- Step 2: Revise the domain of the current arc
                (revisedDomainVar, revisedXDomain) = revise currentVar xDomain yDomain constraint

                -- Step 3: Update the domains with the revised domain
                updatedDomains = Map.insert revisedDomainVar revisedXDomain domains

                -- Step 4: Update arcs based on the revised domain
                updatedArcs = remainingArcs ++ [(a, updatedDomains Map.! a, updatedDomains Map.! b, f) | (Binary a b f) <- constraints, b == revisedDomainVar]
             in
                if null revisedXDomain
                    then (False, updatedDomains) -- If domain becomes empty, return False
                    else ac3Helper updatedArcs updatedDomains constraints
        else (True, domains) -- If all arcs are processed without empty domains, return True

mrvHeuristic :: Domains -> [Variable] -> [Variable]
mrvHeuristic domain assignedVars =
    let
        newMap = foldr Map.delete domain assignedVars
        lengths = Map.map length newMap
        minLength = minimum (Map.elems lengths)
     in
        Map.keys (Map.filter (== minLength) lengths)

degreeHeuristic :: [Variable] -> [Constraint] -> Variable
degreeHeuristic vars constraints =
    fst $ maximumBy (compare `on` snd) $ Map.toList varCounts
  where
    varCounts = countVariables vars constraints

countVariables :: [Variable] -> [Constraint] -> Map Variable Int
countVariables vars = foldr countConstraint initialCounts
  where
    initialCounts = Map.fromList $ map (,0) vars

    countConstraint (Unary v _ _) = updateCount v
    countConstraint (Binary v1 v2 _) = updateCount v1 . updateCount v2

    updateCount = Map.adjust (+ 1)

selectVariable :: Domains -> [Constraint] -> [Variable] -> Variable
selectVariable domain constraints assignedVars =
    let candidates = mrvHeuristic domain assignedVars
     in degreeHeuristic candidates constraints

isNeighbor :: Board -> Variable -> Variable -> Bool
isNeighbor board (IndexVar idx) (IndexVar potentialNeighIdx) = potentialNeighIdx `elem` unflaggedClosedNeighbours board idx
isNeighbor _ _ _ = False

lcvHeuristic :: Variable -> [Variable] -> Domains -> Board -> [Value]
lcvHeuristic var unassignedVariables domains board =
    let neighboringVars = filter (\v -> isNeighbor board var v && notElem v unassignedVariables) unassignedVariables
        valueCounts = Map.fromList [(value, countExcludedValues value neighboringVars domains) | value <- domainOf var domains]
        sortedValues = sortBy (comparing snd) $ Map.toList valueCounts
     in map fst sortedValues

countExcludedValues :: Value -> [Variable] -> Domains -> Int
countExcludedValues value variables domains =
    length $ filter (\var -> value `notElem` domainOf var domains) variables

domainOf :: Variable -> Domains -> [Value]
domainOf = Map.findWithDefault []

checkConstraint :: Constraint -> CSPSolution -> Bool
checkConstraint (Binary x y f) assignment =
    let valX = lookup x assignment
        valY = lookup y assignment
     in case (valX, valY) of
            (Just a, Just b) -> f a b || f b a
            _ -> True
checkConstraint _ _ = True -- Only binary constriants are passed into the csp

allVariablesAssigned :: CSPSolution -> [Variable] -> Bool
allVariablesAssigned assignment variables = length assignment == length variables

-- csp Solver
-- assumes that findtrivialSafe square has been tried in the process trivialFlags has been called too.
-- variables - all closed squares not marked as a flag -- domain - {0, 1} - 0 - Safe 1 - Mine
-- constraints
-- sum of variables = num_mines
-- all open squares with numbers must have the number = sum of adjacent variables + adjacent flags
-- break n-ary constraints into unary and binary
-- use backtracking with arc consistency to find all solutions
cspSolver :: Board -> (Index, Board, [CSPSolution], Map Variable [Either IndexVarValue SumVarValue], [Variable])
cspSolver board =
    let (indexVariables, constraints) = getVariablesAndConstraints board
        unaryConstraints =
            filter
                ( \case
                    (Unary{}) -> True
                    _ -> False
                )
                constraints
        binaryConstraints =
            filter
                ( \case
                    (Unary{}) -> False
                    _ -> True
                )
                constraints

        getSumVariableDomains (Unary (SumVar idx) numIndexVariables func) = (SumVar idx, map Right $ filter func (replicateM numIndexVariables [0, 1]))
        getSumVariableDomains _ = undefined -- not used
        sumVariablesWithDomains = Map.fromList (map getSumVariableDomains unaryConstraints)
        indexVariablesWithDomains = Map.fromList (map (,[Left 0, Left 1]) indexVariables)

        domains = Map.union indexVariablesWithDomains sumVariablesWithDomains

        variables = indexVariables ++ Map.keys sumVariablesWithDomains

        solutions = nub $ map filterIndexVars (backtrack board variables binaryConstraints domains [] [])

        filterIndexVars sol = [(var, val) | (var, val) <- sol, isIndexVar var]

        isIndexVar (IndexVar _) = True
        isIndexVar _ = False

        flaggedBoard = board // [(idx, (board ! idx){isSolverFlagged = True}) | idx <- range $ bounds board, filterMines (IndexVar idx)]
        filterMines idx = not (null solutions) && all ((== Just (Left 1)) . lookup idx) solutions -- solver flag all squares that are mines in all solutions
        updatedBoard = board // [(idx, (flaggedBoard ! idx){isSolverSafe = True}) | idx <- range $ bounds board, filterSafe (IndexVar idx)]
        filterSafe idx = not (null solutions) && all ((== Just (Left 0)) . lookup idx) solutions -- solver mark all squares that are safe in all solutions
        (a, b) = (getSafestSquare solutions updatedBoard, updatedBoard)
     in (a, b, solutions, domains, variables)

solver :: Board -> StdGen -> (Index, Board, [CSPSolution], Map Variable [Either IndexVarValue SumVarValue], [Variable])
solver board g = case findTrivialSafeSquare board of
    (Just idx, flagged_board) -> (idx, flagged_board, [], Map.empty, [])
    (Nothing, flagged_board) ->
        case find (\idx -> isSolverSafe (board ! idx) && state (board ! idx) == Closed) (range $ bounds board) of
            (Just idx) -> (idx, flagged_board, [], Map.empty, [])
            Nothing -> cspSolver flagged_board

-- (probabilisticSolver flagged_board g, flagged_board)

play :: Move -> Board -> HashSet Index -> Index -> Board
play OpenSquare board mines idx = openSquare idx mines board
play ToggleFlag board _ idx = toggleFlagSquare idx board

loop :: Board -> HashSet Index -> IO ()
loop board mines = do
    -- print "solver safe debug"
    -- printBoardDebug2 board mines
    -- move <- readMove -- from user via cli
    -- idx <- readTuple -- from user via cli
    let move = OpenSquare -- solver only opens square
    g <- newStdGen
    -- print "solversafe find?"
    -- print $ find (\idx -> isSolverSafe (board ! idx) && state (board ! idx) == Closed) (range $ bounds board)
    -- print $ isSolverSafe (board ! (1, 1))
    -- print $ getVariablesAndConstraints board

    print "numbered squares"
    let numberedSquares = [idx | idx <- range $ bounds board, isNumberedSquare board idx] -- numbered squares
    let twoUnflaggedClosedNeighbours =
            let candidates = unflaggedClosedNeighbours board
             in [(candidates idx !! 0, candidates idx !! 1, idx) | idx <- numberedSquares, length (candidates idx) == 2]

    print numberedSquares
    print "binary constraints"
    print twoUnflaggedClosedNeighbours
    let (idx, board', sols, domains, variables) = solver board g

    print "domains"
    print domains

    print "variables"
    print variables
    -- print "solver safe debug"
    -- printBoardDebug2 board' mines
    print "csp solutions"
    print $ take 5 sols

    let safeIdxs = [idx | indx <- range $ bounds board, filterSafe (IndexVar indx)]
        filterSafe indexx = not (null sols) && all ((== Just (Left 0)) . lookup indexx) sols

    print "safe squares"
    print safeIdxs

    print "flagged board"
    printBoard board'
    print "flagged indices"
    print [(index, isSolverFlagged (board' ! index)) | index <- range $ bounds board', state (board' ! index) == Closed && isSolverFlagged (board' ! index)] -- solverflagged
    -- print "triviallyOpen"
    -- print $ triviallyOpen flagged_board
    -- print "probabilistic solver"
    -- print $ probabilisticSolver flagged_board g
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
                ( if isFlagged (board' ! idx) || state (board' ! idx) == Open
                    then do
                        printBoard board'
                        putStrLn ""
                        loop board' mines
                    else
                        ( if not $ isMine mines idx
                            then do
                                let new_board = play move board' mines idx
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
                                printBoardDebug board' mines
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
