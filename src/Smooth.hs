{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Smooth where

import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import qualified Linear as L

smoothStencil =
  [stencil2|  1  4  7 4  1
              4 16 26 16 4
              7 26 41 16 7
              4 16 26 16 4
              1  4  7 4  1 |]

smooth :: (Source r Double) => Array r DIM2 Double -> Array D DIM2 Double
smooth array = R.map (/273) $ mapStencil2 BoundClamp smoothStencil array