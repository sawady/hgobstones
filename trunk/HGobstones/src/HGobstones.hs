module HGobstones
    (
      module Prelude,
      List,
      foreach,
      repeat,
      while,
      (!),
      Board,
      Program,
      Procedure,
      Color(..),
      Dir(..),
      poner,
      -- sacar,
      mover,
      nroBolitas,
      hayBolitas,
      puedeMover,
      opuesto,
      minColor,
      maxColor,
      minDir,
      maxDir,
      colores,
      direcciones,
      -- runCanvas,
      runGobstones
    ) where


import Prelude hiding (
    Integer, 
    repeat,
    Double 
    )

import HGobstonesApp