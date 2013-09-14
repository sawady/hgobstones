module HGobstonesApp
    (
      module HGobstonesLang,
      module HGobstonesDraw,
      runGobstones
    ) where

import HGobstonesLang
import HGobstonesDraw
import Control.Monad.State

runCanvas :: Canvas () -> Program
runCanvas = blankCanvas 3000 . flip send

runGobstones :: Procedure Board -> Program
runGobstones p = runCanvas $
    drawBoard (execState p newBoard)