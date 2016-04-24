module Perlin where

import qualified Data.Array as A
import qualified Data.Bits as B
import qualified Linear as L

-- Perlin noise. Range [0, 1]. (?? check if the range is true!)
perlin :: L.V2 Double -> Double
perlin p = let
  va = integerPart p
  vb = va L.^+^ (L.V2 1 0)
  vc = va L.^+^ (L.V2 0 1)
  vd = va L.^+^ (L.V2 1 1)

  oa = fractionalPart p
  ob = oa L.^-^ (L.V2 1 0)
  oc = oa L.^-^ (L.V2 0 1)
  od = oa L.^-^ (L.V2 1 1)

  ua = vertexUnitVector va
  ub = vertexUnitVector vb
  uc = vertexUnitVector vc
  ud = vertexUnitVector vd

  na = L.dot oa ua
  nb = L.dot ob ub
  nc = L.dot oc uc
  nd = L.dot od ud

  L.V2 dx dy = oa
  sdx = smooth dx
  sdy = smooth dy

  nu = interpolate na nb sdx
  nv = interpolate nc nd sdx

  in (1 + (interpolate nu nv sdy)) / 2

integerPart :: L.V2 Double -> L.V2 Int
integerPart (L.V2 x y) = L.V2 (floor x) (floor y)

fractionalPart :: L.V2 Double -> L.V2 Double
fractionalPart (L.V2 x y) = L.V2 (x - (fromIntegral $ floor x)) (y - (fromIntegral $ floor y))

-- Linear interpolation.
-- interpolate a b 0 = a
-- interpolate a b 1 = b
interpolate :: Double -> Double -> Double -> Double
interpolate a b t = a * (1 - t) + b * t

-- [0, 1] -> [0, 1]
smooth :: Double -> Double
smooth t = t * t * t * (t * (t * 6 - 15) + 10)

-- A pseudorandom unit vector.
vertexUnitVector :: L.V2 Int -> L.V2 Double
vertexUnitVector (L.V2 x y) = L.V2 (cos theta) (sin theta) where theta = pi * vertexNoise x y

-- A pseudorandom number from -1 to 1.
vertexNoise :: Int -> Int -> Double
vertexNoise x y =
  1.0 - (fromIntegral ((s * (s * s * 15731 + 789221) + 1376312589) B..&. 0x7fffffff)) / 1073741824.0
  where
    n = x + y * 131764
    s = B.xor n (B.shift n 13)
