module Main where

import qualified Data.Array.Repa as R
import qualified Graphics.Gloss.Raster.Array as GA
import qualified Linear as L

import qualified Perlin as P
import qualified Fractal as F

fbmConfiguration :: F.FBMConfiguration
fbmConfiguration = F.FBMConfiguration
  { F.getBasis = P.perlin
  , F.getOctaves = 8
  , F.getLacunarity = 2
  , F.getH = 0.6
  }

fbm = F.fbm fbmConfiguration

fractalFunction (R.Z R.:. xi R.:. yi) = GA.rgbI' valuei valuei valuei where
  x = (fromIntegral xi) / 500
  y = (fromIntegral yi) / 500
  value = fbm (L.V2 x y)
  valuei = floor $ 200 * value

main :: IO ()
main = do
  let sizeX = 1000
  let sizeY = 1000

  let shape = R.Z R.:. sizeX R.:. sizeY
  let array _ = R.fromFunction shape fractalFunction

  GA.animateArray
    (GA.InWindow "Testing" (sizeX, sizeY) (10, 10))
    (1, 1)
    array