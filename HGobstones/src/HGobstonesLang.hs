{-# LANGUAGE TemplateHaskell #-}
module HGobstonesLang where
    
import qualified Data.Map as Map    
import Control.Monad.State
import Control.Lens.TH
import Control.Lens
import Data.Maybe (fromJust)
import Prelude hiding (repeat)

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- Control Structures ---------------------------------------------------------------

foreach :: Monad m => [a] -> (a -> m ()) -> m ()
foreach = forM_

repeat :: Monad m => Int -> m () -> m ()
repeat i c = foreach [1..i] (const c)

while :: Monad m => m Bool -> m () -> m ()
while mb c = do
    b <- mb
    when b $ c >> while mb c
    
on :: MonadState t m => (t -> Bool) -> m () -> m ()
on p f = do
  b <- get
  when (p b)
       f    

infixr 0 !
(!) :: (a -> b) -> a -> b
(!) = ($)

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- Data Structures ---------------------------------------------------------------

type List a = [a]
              
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- Gobstones ------------------------------------------------------------------------

type Program = IO ()

type Procedure a = State a ()

data Color  = Azul  | Negro | Rojo | Verde deriving (Show, Eq, Enum)
data Dir    = Norte | Este  | Sur  | Oeste deriving (Show, Eq, Enum)

data Cell  = Cell {
    _nroDeAzul  :: Int,
    _nroDeNegro :: Int,
    _nroDeRojo  :: Int,
    _nroDeVerde :: Int
} deriving (Show)

data Board = Board {
    _cells     :: Map.Map (Int,Int) Cell,
    _current   :: (Int,Int)
} deriving (Show)

makeLenses ''Cell
makeLenses ''Board

newCell :: Cell
newCell = Cell 1 1 1 1

cellCount :: Int
cellCount = 9

setCells :: Map.Map (Int,Int) Cell
setCells = foldr (\key m -> Map.insert key newCell m) Map.empty indexes 
         where indexes = [(x,y) | x <- l, y <- l]
               l       = [0..cellCount-1]

newBoard :: Board
newBoard = Board {
  _cells   = setCells,
  _current = (0,0)
}

poner :: Color -> Procedure Board
poner c = modify (ponerEnTablero c)

ponerEnTablero :: Color -> Board -> Board
ponerEnTablero c b = over cells (Map.adjust (ponerCelda c) (view current b)) b

ponerCelda :: Color -> Cell -> Cell
ponerCelda c = over (nroBolitasDe c) (+1)

mover :: Dir -> Procedure Board
mover d = on (puedeMover d) (modify (moverCabezal d))
    
moverCabezal :: Dir -> Board -> Board
moverCabezal d = over current (moverCoordenada d)

moverCoordenada :: Dir -> (Int,Int) -> (Int,Int)
moverCoordenada Norte = \(x,y) -> (x,y+1)
moverCoordenada Sur   = \(x,y) -> (x,y-1)
moverCoordenada Este  = \(x,y) -> (x+1,y)
moverCoordenada Oeste = \(x,y) -> (x-1,y)

hayBolitas :: Color -> Board -> Bool
hayBolitas c = (> 0) . nroBolitas c

nroBolitas :: Color -> Board -> Int
nroBolitas c b =  view (nroBolitasDe c) . fromJust $ Map.lookup (_current b) (_cells b)

nroBolitasDe :: Functor f => Color -> (Int -> f Int) -> Cell -> f Cell
nroBolitasDe Azul  = nroDeAzul
nroBolitasDe Negro = nroDeNegro
nroBolitasDe Rojo  = nroDeRojo
nroBolitasDe Verde = nroDeVerde

puedeMover :: Dir -> Board -> Bool
puedeMover Norte = (< cellCount - 1) . snd . _current
puedeMover Sur   = (> 0) . snd . _current
puedeMover Este  = (< cellCount - 1) . fst . _current
puedeMover Oeste = (> 0) . fst . _current

opuesto :: Dir -> Dir
opuesto d = head . tail . tail . dropWhile (/= d) $ cycle direcciones

minColor :: Color
minColor = Azul

maxColor :: Color
maxColor = Verde

minDir :: Dir
minDir = Norte

maxDir :: Dir
maxDir = Oeste

colores :: [Color]
colores = [minColor .. maxColor]

direcciones :: [Dir]
direcciones = [minDir .. maxDir]