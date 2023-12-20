module HaskellRank.HaskellRankTicTacToe
    ( main
    ) where

import Data.Array

import Data.Array
import Data.Foldable ( asum )

import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss
import Graphics.Gloss.Data.Color



import Data.Array

import Graphics.Gloss



data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))


isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

winner :: Board -> Maybe Player
winner board = asum $ map full $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          diags = [[board ! (i,i) | i <- [0..n-1]]
                  ,[board ! (i,j) | i <- [0..n-1], let j = n-1-i ]]

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game


window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)


boardGridColor = makeColorI 255 255 255 255
playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = greyN 0.5

boardAsRunningPicture board =
    pictures [ color playerXColor $ xCellsOfBoard board
             , color playerOColor $ oCellsOfBoard board
             , color boardGridColor $ boardGrid
             ]

outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Just PlayerX) xCell

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Just PlayerO) oCell

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture board =
    pictures [ xCellsOfBoard board
             , oCellsOfBoard board
             , boardGrid
             ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)