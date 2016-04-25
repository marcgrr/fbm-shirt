{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Water where

import qualified Data.Array.Repa as R
import qualified Linear as L

import qualified Gradient as G
import qualified Smooth as S

data WaterState = WaterState
  { getStateQuantity :: R.Array R.U R.DIM2 Double
  , getStateVelocity :: R.Array R.U R.DIM2 (L.V2 Double)
  }

totalQuantity :: (Monad m) => WaterState -> m Double
totalQuantity state = R.sumAllP $ getStateQuantity state

uniformStillWater :: R.DIM2 -> R.DIM2 -> R.DIM2 -> WaterState
uniformStillWater shape lb ub = WaterState
  { getStateQuantity = R.fromListUnboxed shape $ map (\i -> if R.inShapeRange lb ub (R.fromIndex shape i) then 10 else 0) [1..(R.size shape)]
  , getStateVelocity = R.fromListUnboxed shape $ map (\_ -> L.V2 0 0) [1..(R.size shape)]
  }

stepWater :: forall m. (Monad m) => R.Array R.U R.DIM2 Double -> WaterState -> m WaterState
stepWater terrain initial = do
  let addPerStep = 0.001
  let shape = R.extent $ getStateQuantity initial
  quantityAndVelocity <- R.computeP $ R.traverse2 (getStateQuantity initial) (getStateVelocity initial) (\sh _ -> sh) (accumulateFromNeighbors shape)  :: m (R.Array R.U R.DIM2 (Double, L.V2 Double))
  quantity <- R.computeP $ R.map (\(q, _) -> q + addPerStep) quantityAndVelocity
  velocity <- R.computeP $ R.map (\(q, v) -> v L.^* (if addPerStep == 0 then 1 else (q / (q + addPerStep)))) quantityAndVelocity :: m (R.Array R.U R.DIM2 (L.V2 Double))
  let terrainPlusWater = R.zipWith (\t w -> t + w / 10) terrain quantity
  let gradient = G.gradient $ S.smooth terrainPlusWater
  let velocity' = R.zipWith (\v g -> (v L.^+^ g)) velocity gradient
  velocity'' <- R.computeP $ R.map (\v -> v L.^-^ (0.05 L.*^ v)) velocity'
  return WaterState
    { getStateQuantity = quantity
    , getStateVelocity = velocity''
    }

accumulationRadius :: Int
accumulationRadius = 2

accumulateFromNeighbors :: R.DIM2 -> (R.DIM2 -> Double) -> (R.DIM2 -> (L.V2 Double)) -> R.DIM2 -> (Double, L.V2 Double)
accumulateFromNeighbors (R.Z R.:. w R.:. h) getQuantity getVelocity p@(R.Z R.:. x R.:. y) =
  (quantity, velocity)
  where
    xFroms = [(max 0 (x - accumulationRadius))..(min (w - 1) (x + accumulationRadius))]
    yFroms = [(max 0 (y - accumulationRadius))..(min (h - 1) (y + accumulationRadius))]
    quantitiesAndVelocities = [ f xFrom yFrom | xFrom <- xFroms, yFrom <- yFroms ]
    quantities = map (\(q, _) -> q) quantitiesAndVelocities
    quantity = sum quantities
    velocity = if quantity > 1e-3 then (L.sumV (map (\(q, v) -> q L.*^ v) quantitiesAndVelocities)) L.^/ quantity else L.V2 0 0
    f xFrom yFrom =
      let
        speed = 1
        pFrom = R.Z R.:. xFrom R.:. yFrom
        quantity = getQuantity pFrom
        velocity@(L.V2 vx vy) = getVelocity pFrom
        xTo = (fromIntegral xFrom) + speed * vx
        yTo = (fromIntegral yFrom) + speed * vy
        xTo' = clamp (fromIntegral (xFrom - accumulationRadius)) (fromIntegral (xFrom + accumulationRadius)) xTo
        yTo' = clamp (fromIntegral (yFrom - accumulationRadius)) (fromIntegral (yFrom + accumulationRadius)) yTo
      in
        (quantity * squareIntersectionArea xTo' yTo' (fromIntegral x) (fromIntegral y), velocity)

clamp :: Double -> Double -> Double -> Double
clamp small big value
  | value < small = small
  | value > big = big
  | otherwise = value

squareIntersectionArea :: Double -> Double -> Double -> Double -> Double
squareIntersectionArea x1 y1 x2 y2 =
  (intervalIntersectionLength x1 x2) * (intervalIntersectionLength y1 y2)

intervalIntersectionLength :: Double -> Double -> Double
intervalIntersectionLength x1 x2
  | right1 < left2 = 0
  | right1 < right2 = right1 - left2
  | left1 < right2 = right2 - left1
  | otherwise = 0
  where
    left1 = x1 - 0.5
    right1 = x1 + 0.5
    left2 = x2 - 0.5
    right2 = x2 + 0.5

{-|

getGradientDestinations :: (Monad m) => R.Array R.U R.DIM2 (L.V2 Double) -> m (R.Array R.U R.DIM2 (Double, Int, Int))
getGradientDestinations gradient = R.computeP $ R.traverse gradient id destinations
  where
    (R.Z R.:. w R.:. h) = R.extent gradient
    speed = 200.0
    destinations getGradient p@(R.Z R.:. x R.:. y) =
      let
        L.V2 gx gy = getGradient p
        xd = ((x + floor (speed * gx)) `max` 0) `min` (w - 1)
        yd = ((y + floor (speed * gy)) `max` 0) `min` (h - 1)
      in
        (1.0, xd, yd)

backtrack :: R.DIM2 -> (R.DIM2 -> Double) -> (R.DIM2 -> (L.V2 Double)) -> R.DIM2 -> Double
backtrack (R.Z R.:. w R.:. h) getQuantity getGradient p@(R.Z R.:. x R.:. y) = getQuantity (R.Z R.:. xb R.:. yb)
  where
    L.V2 gx gy = getGradient p
    speed = 1000.0
    xb = ((x + floor (speed * gx)) `max` 0) `min` (w - 1)
    yb = ((y + floor (speed * gy)) `max` 0) `min` (h - 1)
--}