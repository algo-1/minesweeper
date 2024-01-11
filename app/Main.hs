{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Array (bounds, (!))
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

    drawGame emptyBoard ONGOING canvas

    getBody window
        #+ [ column [element canvas]
           , element openModeButton
           , element flagModeButton
           , element aiMoveButton
           , element newGameButton
           ]

    currentBoard <- liftIO $ newIORef emptyBoard
    prevIndex <- liftIO $ newIORef (-1, -1)
    currentMode <- liftIO $ newIORef 0
    mines <- liftIO $ newIORef HashSet.empty
    count <- liftIO $ newIORef 1

    on UI.click openModeButton $ \_ -> do
        liftIO $ writeIORef currentMode 0

    on UI.click flagModeButton $ \_ -> do
        liftIO $ writeIORef currentMode 1

    on UI.click aiMoveButton $ \_ -> do
        prevIndex' <- liftIO $ readIORef prevIndex
        count' <- liftIO $ readIORef count
        if count' == 2
            then do
                (Puzzle newBoard newMines) <- liftIO $ generateNewPuzzle n m numMines prevIndex'
                liftIO $ writeIORef currentBoard newBoard
                liftIO $ writeIORef mines newMines
            else return ()

        board <- liftIO $ readIORef currentBoard
        mines' <- liftIO $ readIORef mines
        let (idx, solverBoard, _, _, _) = solver board g
            (board', status) = if count' > 1 then play' OpenSquare solverBoard mines' idx else play' OpenSquare board mines' aiStartPos
        liftIO $ writeIORef currentBoard board'
        liftIO $ writeIORef count (count' + 1)
        liftIO $ writeIORef prevIndex idx
        drawGame board' status canvas

    on UI.click newGameButton $ \_ -> do
        canvas # UI.clearCanvas
        -- reset board
        liftIO $ writeIORef currentBoard emptyBoard
        -- reset mines
        liftIO $ writeIORef mines HashSet.empty
        -- reset prevIndex
        liftIO $ writeIORef prevIndex (-1, -1)
        -- reset count
        liftIO $ writeIORef count 1
        -- reset mode
        liftIO $ writeIORef currentMode 0
        do
            drawGame emptyBoard ONGOING canvas

    on UI.mousedown canvas $ \(x, y) -> do
        prevIndex' <- liftIO $ readIORef prevIndex
        count' <- liftIO $ readIORef count
        if count' == 2
            then do
                (Puzzle newBoard newMines) <- liftIO $ generateNewPuzzle n m numMines prevIndex'
                liftIO $ writeIORef currentBoard newBoard
                liftIO $ writeIORef mines newMines
            else return ()
        mode <- liftIO $ readIORef currentMode
        mines' <- liftIO $ readIORef mines
        board <- liftIO $ readIORef currentBoard

        let idx = (((floor y) `div` (canvasSize `div` n)) + 1, ((floor x) `div` (canvasSize `div` m)) + 1)

        case mode of
            0 -> do
                let (board', status) = play' OpenSquare board mines' idx
                liftIO $ writeIORef currentBoard board'
                liftIO $ writeIORef count (count' + 1)
                liftIO $ printBoard board'
                liftIO $ print (show count')
                liftIO $ writeIORef prevIndex idx
                drawGame board' status canvas
            1 -> do
                let (board', status) = play' ToggleFlag board mines' idx
                liftIO $ writeIORef currentBoard board'
                liftIO $ writeIORef count (count' + 1)
                liftIO $ writeIORef prevIndex idx
                drawGame board' status canvas

-- Draws the game
drawGame :: Board -> Status -> Element -> UI ()
drawGame arr status canvas = do
    drawBoard arr
    case status of
        LOST -> do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (fromIntegral canvasSize / 4, fromIntegral canvasSize / 4) (fromIntegral canvasSize / 1.5) (fromIntegral canvasSize / 2)
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "YOU LOST" ((fromIntegral canvasSize / 1.75), (fromIntegral canvasSize / 2))
        WON -> do
            canvas # set' UI.fillStyle (UI.htmlColor "black")
            canvas # UI.fillRect (fromIntegral canvasSize / 3, fromIntegral canvasSize / 3) (fromIntegral canvasSize / 1.5) (fromIntegral canvasSize / 1.5)
            canvas # set' UI.fillStyle (UI.htmlColor "white")
            canvas # set' UI.textAlign UI.Center
            canvas # set' UI.textFont "52px sans-serif"
            canvas # UI.fillText "YOU WON!" ((fromIntegral canvasSize / 1.75), (fromIntegral canvasSize / 2.5))
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
                    ((i * (canvasSize `div` m)) + ((canvasSize `div` m) - 10))
                )
