module Canvas
    (
      Pos, 
      CanvasColor,
      onCanvas, 
      drawCircle, 
      drawLine, 
      drawRect, 
      drawText,
      module Graphics.Blank
    ) where
    
import Graphics.Blank

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------                

-- Canvas ---------------------------------------------------------------------------

type Pos         = (Float,Float)
type CanvasColor = String
            
onCanvas :: Canvas () -> Canvas ()
onCanvas d = do
    save()
    d
    restore()

drawCircle :: Pos -> CanvasColor -> Float -> Canvas ()
drawCircle (x,y) c r = onCanvas $ do
    translate(x,y)
    arc(0, 0, r, 0, pi * 2, False)
    fillStyle c
    fill()

drawLine :: Pos -> CanvasColor -> Float -> Float -> Canvas ()
drawLine (x,y) c w h = onCanvas $ do
    beginPath()
    moveTo(x,y)
    lineTo(w,h)
    closePath()
    lineWidth 1
    strokeStyle c
    stroke()

drawRect :: Pos -> CanvasColor -> Float -> Float -> Canvas ()
drawRect (x,y) c w h = onCanvas $ do
    translate(x,y)
    fillStyle c
    fillRect(0,0,w,h)
    
drawText :: Pos -> CanvasColor -> String -> Int -> Canvas ()
drawText (x,y) c s n = onCanvas $ do
    font(show n ++ "pt Calibri")
    fillStyle c
    fillText(s,x,y)