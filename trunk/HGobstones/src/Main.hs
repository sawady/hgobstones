module Main where

import Prelude()
import HGobstones

ponerDeTodosLosColores :: Procedure Board
ponerDeTodosLosColores = foreach colores poner

ponerDeTodosLosColoresN :: Int -> Procedure Board
ponerDeTodosLosColoresN n = repeat n ponerDeTodosLosColores

ponerN :: Int -> Color -> Procedure Board
ponerN n c = repeat n (poner c)

myMain :: Procedure Board
myMain = do
    foreach colores poner
    mover Este
    ponerDeTodosLosColoresN 100

main :: Program
main = runGobstones myMain