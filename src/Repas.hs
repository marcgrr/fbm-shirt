{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repas where

import qualified Linear as L
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import qualified Data.Array.Repa.Repr.Unboxed as RU

toRepa :: R.DIM2 -> (L.V2 Double -> Double) -> Double -> R.Array R.D R.DIM2 Double
toRepa sh f scale = R.fromFunction sh f' where
  f' (R.Z R.:. x R.:. y) = f $ L.V2 ((fromIntegral x) / scale) ((fromIntegral y) / scale)

max :: (R.Shape sh, R.Source r a, RE.Elt a, RU.Unbox a, Num a, Ord a, Monad m) => R.Array r sh a -> m (Maybe a)
max array
  | (R.size shape) == 0 = return Nothing
  | otherwise = do
    result <- R.foldAllP Prelude.max (array R.! (R.fromIndex shape 0)) array
    return $ Just result
  where
    shape = R.extent array

min :: (R.Shape sh, R.Source r a, RE.Elt a, RU.Unbox a, Num a, Ord a, Monad m) => R.Array r sh a -> m (Maybe a)
min array
  | (R.size shape) == 0 = return Nothing
  | otherwise = do
    result <- R.foldAllP Prelude.min (array R.! (R.fromIndex shape 0)) array
    return $ Just result
  where
    shape = R.extent array

scale01 :: forall r sh a m. (RE.Load r sh a, R.Shape sh, R.Source r a, RE.Elt a, RU.Unbox a, Ord a, Fractional a, Monad m) => R.Array r sh a -> m (R.Array R.D sh a)
scale01 array = do
  computedArray <- R.computeP array :: m (R.Array R.U sh a)
  minValMay <- Repas.min computedArray
  maxValMay <- Repas.max computedArray
  case (minValMay, maxValMay) of
    (Just minVal, Just maxVal) ->
      return $ R.map (\v -> (v - minVal) / (maxVal - minVal)) computedArray
    _ -> return $ R.delay array