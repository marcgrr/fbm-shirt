{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Gradient (gradient) where

import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import qualified Linear as L

gradientXStencil =
  [stencil2|  0  0  0
              1  0 -1
              0  0  0 |]

gradientYStencil =
  [stencil2|  0  1  0
              0  0  0
              0 -1  0 |]

gradient :: (Source r Double) => Array r DIM2 Double -> Array D DIM2 (L.V2 Double)
gradient array = R.zipWith L.V2 gradientX gradientY
  where
    gradientY = mapStencil2 BoundClamp gradientXStencil array
    gradientX = mapStencil2 BoundClamp gradientYStencil array