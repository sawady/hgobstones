module HGobstonesDraw 
    (
      module Canvas,
      drawBoard
    ) where

import qualified Data.Map as Map
import Canvas
import HGobstonesLang
import Control.Monad (forM_, when)
import Control.Lens

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- Board Drawing ---------------------------------------------------------------

cellSize :: Float
cellSize  = 80

boardSize :: Float
boardSize  = cellSize * fromIntegral cellCount

toCanvasColor :: Color -> CanvasColor
toCanvasColor Azul  = "blue"
toCanvasColor Negro = "black"
toCanvasColor Rojo  = "red"
toCanvasColor Verde = "green"

grabberColor :: String
grabberColor = "GoldenRod"

drawBoard :: Board -> Canvas ()
drawBoard b = do
                drawGrid
                drawCells b
                drawGrabber b
                
drawCells :: Board -> Canvas ()    
drawCells b = do
    let cellAssocs = Map.toList (view cells b)
    -- drawCell (0,0) (Cell 1 1 1 1)
    forM_ cellAssocs (uncurry drawCell)
    
    
drawCell :: (Int,Int) -> Cell -> Canvas ()
drawCell (x,y) c = do
    let pos = (cellSize + fromIntegral x * cellSize, cellSize + fromIntegral (cellCount - 1 - y) * cellSize)
    forM_ colores $ 
        \col -> do 
                  let cantCol = view (nroBolitasDe col) c
                  when (cantCol > 0) (drawCellColor pos cantCol col)
        
colorFontSize :: Int        
colorFontSize = 12
    
drawCellColor :: Pos -> Int -> Color -> Canvas ()
drawCellColor pos n c = do
    let posCol = posColor pos c
    let figcol = toCanvasColor c
    -- drawCircle posCol figcol 16 
    drawText posCol figcol (show n) colorFontSize
    

posColor :: Pos -> Color -> (Float,Float)

posColor (x,y) Azul  = ( x + (cellSize / 2) + fromIntegral colorFontSize
                       , y + cellSize - fromIntegral colorFontSize
                       )
                        
posColor (x,y) Negro = ( x + fromIntegral colorFontSize / 2
                       , y + cellSize - fromIntegral colorFontSize
                       )
                       
posColor (x,y) Rojo  = ( x + (cellSize / 2) + fromIntegral colorFontSize
                       , y + fromIntegral colorFontSize + fromIntegral colorFontSize / 2
                       )
                       
posColor (x,y) Verde = ( x + fromIntegral colorFontSize / 2
                       , y + fromIntegral colorFontSize + fromIntegral colorFontSize / 2
                       )

drawGrabber :: Board -> Canvas ()
drawGrabber b = onCanvas $ do
    let (x, y) = view current b
    translate(cellSize + fromIntegral x * cellSize , cellSize + fromIntegral (cellCount - 1 - y) * cellSize)
    lineWidth 2
    strokeStyle grabberColor
    strokeRect(0,0,cellSize,cellSize)
    
drawGrid :: Canvas ()
drawGrid = onCanvas $ do
    translate(cellSize,cellSize)
    drawRows
    drawColumns
    
drawRows :: Canvas ()
drawRows = forM_ [0..cellCount] drawRow

drawRow :: Int -> Canvas ()
drawRow n = drawLine (0,offset) "black" boardSize offset
            where offset = fromIntegral n * cellSize

drawColumns :: Canvas ()
drawColumns = do 
                translate (boardSize,0)
                rotate (pi / 2)
                drawRows
                
drawBoom :: String -> Canvas ()
drawBoom s = do
    drawRect (cellSize,cellSize)      "red"   boardSize boardSize
    drawText (cellSize,cellSize+72)   "black" "BOOM!"   72
    drawText (cellSize,cellSize+72*3) "black" s         48
