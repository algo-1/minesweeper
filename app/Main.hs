{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (unless)
import Data.Array (bounds, (!))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IORef
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Lib
import System.Random

-- cli version
-- main :: IO ()
-- main = do
--     -- display all closed squares
--     -- user clicks square
--     -- open square recursively that the user clicks & update display
--     let puzzle = generateNewPuzzle 16 16 40 (16, 1)
--     runGame puzzle

-- gui version

-- Game settings (Intermediate)
canvasSize :: Int
canvasSize = 480

-- board is n x m, threepenny canvas is j x i
n :: Int
n = 16

m :: Int
m = 16

aiStartPos :: Index
aiStartPos = (16, 1)

numMines :: Int
numMines = 40

main :: IO ()
main = do
    gen <- newStdGen
    let emptyBoard = createBoard n m defaultSquare
    startGUI defaultConfig (setup gen emptyBoard)

setup :: StdGen -> Board -> Window -> UI ()
setup g emptyBoard window = do
    return window # set title "Minesweeper"

    canvas <-
        UI.canvas
            # set UI.width canvasSize
            # set UI.height canvasSize
            # set
                UI.style
                [("border", "solid black 1px"), ("background", "#000")]

    openModeButton <- UI.button #+ [string "Open Mode"]
    flagModeButton <- UI.button #+ [string "Flag Mode"]
    aiMoveButton <- UI.button #+ [string "AI Move"]
    newGameButton <- UI.button #+ [string "Start New Game"]
    playUntilCompletionButton <- UI.button #+ [string "AI Move till End of Game"]

    currentBoard <- liftIO $ newIORef emptyBoard
    currentMode <- liftIO $ newIORef 0
    mines <- liftIO $ newIORef HashSet.empty
    count <- liftIO $ newIORef 1
    gameOver <- liftIO $ newIORef False

    drawGame emptyBoard ONGOING canvas gameOver

    getBody window
        #+ [ column [element canvas]
           , element openModeButton
           , element flagModeButton
           , element aiMoveButton
           , element newGameButton
           , element playUntilCompletionButton
           ]

    on UI.click openModeButton $ \_ -> do
        liftIO $ writeIORef currentMode 0

    on UI.click flagModeButton $ \_ -> do
        liftIO $ writeIORef currentMode 1

    on UI.click aiMoveButton $ \_ -> do
        gameOver' <- liftIO $ readIORef gameOver
        unless gameOver' $ do
            count' <- liftIO $ readIORef count
            if count' == 1
                then do
                    -- generate new puzzle + play on first click
                    (Puzzle newBoard newMines) <- liftIO $ generateNewPuzzle m n numMines aiStartPos
                    -- store new board and mines
                    liftIO $ writeIORef currentBoard newBoard
                    liftIO $ writeIORef mines newMines

                    liftIO $ writeIORef count (count' + 1)
                    drawGame newBoard (if gameWon newBoard newMines then WON else ONGOING) canvas gameOver
                else do
                    board <- liftIO $ readIORef currentBoard
                    mines' <- liftIO $ readIORef mines
                    let (idx, solverBoard, _, _, _) = solver board g
                        (board', status) = play' OpenSquare solverBoard mines' idx
                    liftIO $ writeIORef currentBoard board'
                    liftIO $ writeIORef count (count' + 1)
                    drawGame board' status canvas gameOver
    on UI.click newGameButton $ \_ -> do
        canvas # UI.clearCanvas
        -- reset board
        liftIO $ writeIORef currentBoard emptyBoard
        -- reset mines
        liftIO $ writeIORef mines HashSet.empty
        -- reset count
        liftIO $ writeIORef count 1
        -- reset mode
        liftIO $ writeIORef currentMode 0
        -- reset gameOver
        liftIO $ writeIORef gameOver False
        do
            drawGame emptyBoard ONGOING canvas gameOver

    on UI.mousedown canvas $ \(x, y) -> do
        gameOver' <- liftIO $ readIORef gameOver
        unless gameOver' $ do
            count' <- liftIO $ readIORef count
            let idx = (((floor y) `div` (canvasSize `div` n)) + 1, ((floor x) `div` (canvasSize `div` m)) + 1)

            if count' == 1
                then do
                    -- Generate new puzzle + play on first click
                    (Puzzle newBoard newMines) <- liftIO $ generateNewPuzzle m n numMines idx
                    -- Store new board and mines
                    liftIO $ writeIORef currentBoard newBoard
                    liftIO $ writeIORef mines newMines
                    liftIO $ writeIORef count (count' + 1)
                else do
                    mode <- liftIO $ readIORef currentMode
                    mines' <- liftIO $ readIORef mines
                    board <- liftIO $ readIORef currentBoard

                    case mode of
                        0 -> do
                            let (board', status) = play' OpenSquare board mines' idx
                            liftIO $ writeIORef currentBoard board'
                            liftIO $ writeIORef count (count' + 1)
                            drawGame board' status canvas gameOver
                        1 -> do
                            let (board', status) = play' ToggleFlag board mines' idx
                            liftIO $ writeIORef currentBoard board'
                            liftIO $ writeIORef count (count' + 1)
                            drawGame board' status canvas gameOver

    on UI.click playUntilCompletionButton $ \_ -> do
        f canvas count currentBoard mines gameOver g

f :: Element -> IORef Int -> IORef Board -> IORef (HashSet Index) -> IORef Bool -> StdGen -> UI ()
f canvas count currentBoard mines gameOver g = do
    gameOver' <- liftIO $ readIORef gameOver
    unless gameOver' $ do
        count' <- liftIO $ readIORef count
        if count' == 1
            then do
                -- generate new puzzle + play on the first click
                (Puzzle newBoard newMines) <- liftIO $ generateNewPuzzle m n numMines aiStartPos
                -- store new board and mines
                liftIO $ writeIORef currentBoard newBoard
                liftIO $ writeIORef mines newMines

                liftIO $ writeIORef count (count' + 1)
                drawGame newBoard (if gameWon newBoard newMines then WON else ONGOING) canvas gameOver
            else do
                board <- liftIO $ readIORef currentBoard
                mines' <- liftIO $ readIORef mines
                let (idx, solverBoard, _, _, _) = solver board g
                    (board', status) = play' OpenSquare solverBoard mines' idx
                liftIO $ writeIORef currentBoard board'
                liftIO $ writeIORef count (count' + 1)
                drawGame board' status canvas gameOver
        f canvas count currentBoard mines gameOver g

-- Draws the game
drawGame :: Board -> Status -> Element -> IORef Bool -> UI ()
drawGame arr status canvas gameOver = do
    drawBoard arr
    case status of
        LOST -> do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (fromIntegral canvasSize / 4, fromIntegral canvasSize / 4) (fromIntegral canvasSize / 1.5) (fromIntegral canvasSize / 2)
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "YOU LOST" ((fromIntegral canvasSize / 1.75), (fromIntegral canvasSize / 2))
            liftIO $ writeIORef gameOver True
        WON -> do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (fromIntegral canvasSize / 4, fromIntegral canvasSize / 4) (fromIntegral canvasSize / 1.5) (fromIntegral canvasSize / 2)
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "YOU LOST" ((fromIntegral canvasSize / 1.75), (fromIntegral canvasSize / 2))
            liftIO $ writeIORef gameOver True
        _ -> return ()
    return ()
  where
    ((li, lj), (ui, uj)) = bounds arr
    drawBoard b =
        mapM_
            ( \i -> do
                mapM_
                    ( \j -> do
                        let square = b ! (i, j)
                        drawSquare (i - 1, j - 1) square canvas -- because the array is 1 indexed
                    )
                    [1 .. uj]
            )
            [1 .. ui]

-- Draws a square on the canvas
-- red - you flagged a mine
-- green - AI flags a mine
-- if you flagged a mine and AI flags it, AI flag takes precedence

drawSquare :: Index -> Square -> Element -> UI ()
drawSquare
    (i, j)
    ( Square
            { state = Closed
            , isFlagged = False
            , neighbourMinesCount = _
            , isSolverFlagged = False
            , isSolverSafe = False
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "gray")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
drawSquare
    (i, j)
    ( Square
            { state = Closed
            , isFlagged = False
            , neighbourMinesCount = _
            , isSolverFlagged = True
            , isSolverSafe = False
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "red")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # set' UI.textAlign (UI.Center)
        canvas # set' UI.textFont "32px sans-serif"
        canvas
            # UI.fillText
                ("*")
                ( fromIntegral
                    ((j * (canvasSize `div` n)) + ((canvasSize `div` n) `div` 2))
                , fromIntegral
                    ((i * (canvasSize `div` m)) + ((canvasSize `div` m) - 3))
                )
drawSquare
    (i, j)
    ( Square
            { state = Closed
            , isFlagged = True
            , neighbourMinesCount = _
            , isSolverFlagged = True
            , isSolverSafe = False
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "green")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # set' UI.textAlign (UI.Center)
        canvas # set' UI.textFont "32px sans-serif"
        canvas
            # UI.fillText
                ("*")
                ( fromIntegral
                    ((j * (canvasSize `div` n)) + ((canvasSize `div` n) `div` 2))
                , fromIntegral
                    ((i * (canvasSize `div` m)) + ((canvasSize `div` m) - 6))
                )
drawSquare
    (i, j)
    ( Square
            { state = Closed
            , isFlagged = True
            , neighbourMinesCount = _
            , isSolverFlagged = False
            , isSolverSafe = False
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "red")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
drawSquare
    (i, j)
    ( Square
            { state = Closed
            , isFlagged = False
            , neighbourMinesCount = _
            , isSolverFlagged = False
            , isSolverSafe = False
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
drawSquare
    (i, j)
    ( Square
            { state = Open
            , isFlagged = _
            , neighbourMinesCount = count
            , isSolverFlagged = _
            , isSolverSafe = _
            }
        )
    canvas = do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas
            # UI.fillRect
                ( fromIntegral ((j * (canvasSize `div` n) + 3))
                , fromIntegral ((i * (canvasSize `div` m) + 3))
                )
                (fromIntegral (canvasSize `div` n - 6))
                (fromIntegral (canvasSize `div` m - 6))
        case count of
            1 -> canvas # set' UI.fillStyle (UI.htmlColor "blue")
            2 -> canvas # set' UI.fillStyle (UI.htmlColor "green")
            3 -> canvas # set' UI.fillStyle (UI.htmlColor "red")
            4 -> canvas # set' UI.fillStyle (UI.htmlColor "darkblue")
            5 -> canvas # set' UI.fillStyle (UI.htmlColor "brown")
            6 -> canvas # set' UI.fillStyle (UI.htmlColor "cyan")
            7 -> canvas # set' UI.fillStyle (UI.htmlColor "black")
            8 -> canvas # set' UI.fillStyle (UI.htmlColor "grey")
            _ -> canvas # set' UI.fillStyle (UI.htmlColor "black")

        canvas # set' UI.textAlign (UI.Center)
        canvas # set' UI.textFont "24px sans-serif"
        canvas
            # UI.fillText
                (show count)
                ( fromIntegral
                    ((j * (canvasSize `div` n)) + ((canvasSize `div` n) `div` 2))
                , fromIntegral
                    ((i * (canvasSize `div` m)) + ((canvasSize `div` m) - 8))
                )
