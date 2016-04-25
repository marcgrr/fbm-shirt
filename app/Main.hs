{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Array.Repa as R
import qualified Graphics.Gloss as GG
import qualified Graphics.Gloss.Raster.Array as GA
import qualified Linear as L

import qualified Perlin as P
import qualified Fractal as F
import qualified Repas as RS
import qualified Water as W
import qualified Gradient as G
import qualified Smooth as S

-- TODOs:
-- height map display contours
-- height map correct scaling
-- height map "lakes"
-- rivers that interact with the height map and lakes
-- forests that interact with the height map
-- city placement that interacts with the height map and rivers and lakes
-- building placement that interacts with the city placement
-- road, railway placement that interacts with the height map and city placement


fbmConfiguration :: F.FBMConfiguration
fbmConfiguration = F.FBMConfiguration
  { F.getBasis = P.perlin
  , F.getOctaves = 20
  , F.getLacunarity = 2
  , F.getH = 0.8
  }

fbm = F.fbm fbmConfiguration

heightToColor h = GA.rgbI' valuei valuei valuei where
  valuei = floor $ 256 * h

waterColor q = GA.rgbI 0 0 valuei where
  valuei = floor $ 200 * q

renderWater waterState = R.map waterColor (W.getStateQuantity waterState)

lerpFrames a b t = R.zipWith (\p q -> GG.mixColors (1 - t) t p q) a b

terrainAndWaterColor :: Double -> Double -> GA.Color
terrainAndWaterColor h w = GA.rgbI 0 hvalue wvalue
  where
    hvalue = floor $ 256 * (1 - w) * h
    wvalue = floor $ 256 * w

renderWorld :: R.Array R.U R.DIM2 Double -> W.WaterState -> IO (R.Array R.D R.DIM2 GA.Color)
renderWorld terrain waterState = do
  let water = S.smooth $ R.map (\w -> if w > 10 then 1 else w / 10) $ W.getStateQuantity waterState
  return $ R.zipWith terrainAndWaterColor terrain water

stepWorld :: R.Array R.U R.DIM2 Double -> Float -> W.WaterState -> IO W.WaterState
stepWorld terrain _ initial = do
  putStrLn "Starting Step"
  totalQuantity <- W.totalQuantity initial
  print totalQuantity

  maxv <- RS.max $ R.map (\v -> L.norm v) $ W.getStateVelocity initial
  print maxv

  maxw <- RS.max $ W.getStateQuantity initial
  print maxw

  W.stepWater terrain initial

main :: IO ()
main = do
  let screenScale = 2
  let sizeX = 400
  let sizeY = 400

  let shape = R.Z R.:. sizeX R.:. sizeY
  let scale = 100
  let heightMap = RS.toRepa shape fbm scale
  scaledHeightMap <- RS.scale01 heightMap >>= R.computeP :: IO (R.Array R.U R.DIM2 Double)

  maxa <- RS.max scaledHeightMap
  mina <- RS.min scaledHeightMap

  print maxa
  print mina

  let gradient = (G.gradient scaledHeightMap)
  computedGradient <- R.computeP gradient :: IO (R.Array R.U R.DIM2 (L.V2 Double))
  let gradientX = R.map (\(L.V2 x _) -> x) computedGradient
  gradientXScaled <- RS.scale01 gradientX
  computedGradientXScaled <- R.computeP gradientXScaled :: IO (R.Array R.U R.DIM2 Double)

  GA.playArrayIO
    (GA.InWindow "Pretty Pictures!!" (screenScale * sizeX, screenScale * sizeY) (10, 10))
    (screenScale, screenScale)
    1
    (W.uniformStillWater shape (R.Z R.:. 150 R.:. 150) (R.Z R.:. 250 R.:. 250))
    (\world -> renderWorld computedGradientXScaled world)
    (\_ world -> return world)
    (stepWorld scaledHeightMap)