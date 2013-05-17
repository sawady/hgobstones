module HGobstones(module Prelude) where

import Graphics.Blank
import qualified Data.Map as Map
import Prelude hiding (Integer, repeat)
import Control.Monad (forM_)

data Color  = Azul  | Negro | Rojo | Verde deriving (Show, Eq, Enum)
data Dir    = Norte | Este  | Sur  | Oeste deriving (Show, Eq, Enum)

data Cell  = Cell {
    _nroDeAzul  :: Int,
    _nroDeNegro :: Int,
    _nroDeRojo  :: Int,
    _nroDeVerde :: Int
} deriving (Show)

newCell :: Cell
newCell = Cell 0 0 0 0

data Board = Board {
    _cells     :: Map.Map (Int,Int) Cell,
    _current   :: (Int,Int)
} deriving (Show)

cellSize :: Float
cellSize  = 80

cellCount :: Int
cellCount = 9

boardSize :: Float
boardSize  = cellSize * fromIntegral cellCount

setCells :: Map.Map (Int,Int) Cell
setCells = foldr (\key m -> Map.insert key newCell m) Map.empty indexes 
         where indexes = zip l l
               l       = [0..cellCount-1]

newBoard :: Board
newBoard = Board {
  _cells   = setCells,
  _current = (0,0)
}

mover :: Dir -> Board -> Board
mover = mover'

mover' :: Dir -> Board -> Board
mover' Norte b = b { _current = let (x,y) = _current b in (x,y+1) }

-- hayBolitas :: Color -> Board -> Bool
-- nroBolitas :: Color -> Board -> Int

puedeMover :: Dir -> Board -> Bool
puedeMover Norte = (< cellCount - 1) . snd . _current
puedeMover Sur   = (> 0) . snd . _current
puedeMover Este  = (< cellCount - 1) . fst . _current
puedeMover Oeste = (> 0) . fst . _current

opuesto :: Dir -> Dir
opuesto d = head . tail . tail . dropWhile (/= d) $ cycle dirs

minColor   :: Color
minColor = Azul

maxColor   :: Color
maxColor = Verde

minDir     :: Dir
minDir = Norte

maxDir     :: Dir
maxDir = Oeste

colors     :: [Color]
colors = [minColor .. maxColor]

dirs       :: [Dir]
dirs = [minDir .. maxDir]

type Pos = (Float,Float)

data Figure = Circle Float
            | Line Pos Float
            | Rect Float Float
            | Text String Int
            
toCanvasColor :: Color -> String
toCanvasColor Azul  = "blue"
toCanvasColor Negro = "black"
toCanvasColor Rojo  = "red"
toCanvasColor Verde = "green"

-- foreach = forM_

repeat :: Int -> (Board -> Board) -> Board -> Board
repeat i c b = iterate c b !! i

onCanvas :: Canvas () -> Canvas ()
onCanvas d = do
    save()
    d
    restore()

draw :: Pos -> Color -> Figure -> Canvas ()
draw (x,y) c fig = onCanvas $ drawSpecific (x,y) c fig

drawSpecific :: Pos -> Color -> Figure -> Canvas ()
drawSpecific (x,y) c (Circle r) = do
    translate(x,y)
    arc(0, 0, r, 0, pi * 2, False)
    fillStyle (toCanvasColor c)
    fill()
    
drawSpecific (x,y) c (Line (w,h) g) = do
    beginPath()
    moveTo(x,y)
    lineTo(w,h)
    closePath()
    lineWidth g
    strokeStyle (toCanvasColor c)
    stroke() 
    
drawSpecific (x,y) c (Rect w h) = do
    translate(x,y)
    fillStyle (toCanvasColor c)
    fillRect(0,0,w,h)
    
drawSpecific (x,y) c (Text s n) = do
    font(show n ++ "pt Calibri")
    fillStyle (toCanvasColor c)
    fillText(s,x,y)
    
drawBoom :: String -> Canvas ()
drawBoom s = do
    draw (cellSize,cellSize)    Rojo  $ Rect boardSize boardSize
    draw (cellSize,cellSize+72)   Negro $ Text "BOOM!" 72
    draw (cellSize,cellSize+72*3) Negro $ Text "BOOM!" 72
    
drawBoard :: Either String Board -> Canvas ()
drawBoard eb = case eb of
    Left  s -> drawBoom s
    Right b -> do 
                drawBoard'                
    
drawBoard' :: Canvas ()
drawBoard' = onCanvas $ do
    translate(cellSize,cellSize)
    drawRows
    drawColumns
    
drawRows :: Canvas ()
drawRows = forM_ [0..cellCount] drawRow

drawRow :: Int -> Canvas ()
drawRow n = draw (0,offset) Negro $ Line (boardSize, offset) 1
            where offset = fromIntegral n * cellSize

drawColumns :: Canvas ()
drawColumns = do 
                translate (boardSize,0)
                rotate (pi / 2)
                drawRows                                

canvas :: Canvas () -> IO ()
canvas = blankCanvas 3000 . flip send

main :: IO ()
main = canvas $ do
    drawBoard'
