module HaskellRank.HaskellRankTicTacToe
    ( main
    ) where

import Graphics.Gloss
import Data.Array

backgroundColor  = makeColor 255 255 255 255

n=3
screenWidth = 640
screenHeight = 480

cellWidth = fromIntegral screenWidth / fromIntegral n
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty]),
                                     gamePlayer = PlayerX,
                                     gameState  = Running }
                                     where indexRange = ((0,0), (n-1,n-1))

data Cell = Empty | Full Player deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board,
                                    gamePlayer :: Player,
                                    gameState :: State } deriving (Eq,Show)

data Player = PlayerX | PlayerO deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq,Show)

boardAsRunningPicture board =
    pictures [ color playerXColor $ xCellsOfBoard board
             , color playerOColor $ oCellsOfBoard board
             , color boardGridColor $ boardGrid
             ]


outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
 where
    x = fromIntegral column * cellWidth + cellWidth * 0.5
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
cellsOfBoard board cell cellPicture = pictures $ map (snapPictureToCell cellPicture.fst)  $ filter  (\(_, e) -> e == cell) $ assocs board


xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Full PlayerX) xCell
 

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Full PlayerO) oCell

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



playerXColor = makeColor 255 50 50 255
playerOColor = makeColor 50 100 255 255
boardGridColor = makeColor 255 255 0 255

tieColor =  greyN 0.5


boardAsPicture board =
    pictures [ xCellsOfBoard board
             , oCellsOfBoard board
             , boardGrid
             ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)


gameAsPicture :: Game -> Picture
gameAsPicture game = 
    --translate (fromIntegral screenWidth * (-0.5))
      --                         (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

transformGame _ game = game

window = InWindow "Functional" (640, 480) (100,100)



main :: IO ()
main = do
    play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
