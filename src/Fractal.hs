module Fractal where

import Linear as L

data FBMConfiguration = FBMConfiguration
  { getBasis :: L.V2 Double -> Double
  , getOctaves :: Int
  , getLacunarity :: Double
  , getH :: Double
  }

-- todo: figure out the range of this function!!
fbm :: FBMConfiguration -> L.V2 Double -> Double
fbm conf = fbm' (getBasis conf) coeffs (getLacunarity conf) where
  coeffs = map (\octave -> (fromIntegral octave) ** (-(getH conf))) [1..(getOctaves conf)]

fbm' :: (L.V2 Double -> Double) -> [Double] -> Double -> L.V2 Double -> Double
fbm' _ [] _ _ = 0
fbm' basis (coeff : coeffs) lacunarity p =
  coeff * amplitude + amplitude * (fbm' basis coeffs lacunarity (lacunarity L.*^ p))
  where amplitude = basis p