{-# LANGUAGE ScopedTypeVariables #-}

module Math.ProxN.Proximity where

import Math.ProxN.Tree2N
import Math.ProxN.VecN

import Control.Applicative

prox :: (VecNClass n, Tree2NClass n) =>
        Double -> VecN n Double -> VecTree n Double -> [VecN n Double]
prox tol2 vec tree = proximity tol2 vec tree []

proximity :: forall n. (VecNClass n, Tree2NClass n) =>
             Double -> VecN n Double -> VecTree n Double -> ([VecN n Double] -> [VecN n Double])
proximity _ _ Tree2NLeaf = id
proximity tol2 vec (Tree2NBranch v t) =
  if distance2 vec v < tol2
  then (v :) . foldTree proximity' (pure (.)) t
  else proximity tol2 vec (chooseNode vec v t)
  where
    proximity' :: VecTree n Double -> ([VecN n Double] -> [VecN n Double])
    proximity' Tree2NLeaf = id
    proximity' (Tree2NBranch v2 t2) = addOrNot . foldTree proximity' (pure (.)) t2
      where
        addOrNot = if distance2 vec v2 < tol2 then (v2 :) else id
