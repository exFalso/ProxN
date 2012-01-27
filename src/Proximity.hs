{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Proximity where

import Tree2N
import VecN

import Control.Applicative

prox :: (VecNClass (VecN n), Tree2NClass n (Treed n)) =>
        Double -> VecN n Double -> Tree2N n (VecN n Double) -> [VecN n Double]
prox tol2 vec tree = proximity tol2 vec tree []

proximity :: forall n. (VecNClass (VecN n), Tree2NClass n (Treed n)) =>
             Double -> VecN n Double -> Tree2N n (VecN n Double) -> ([VecN n Double] -> [VecN n Double])
proximity _ _ Tree2NLeaf = id
proximity tol2 vec (Tree2NBranch v t) =
  if distance2 vec v < tol2
  then (v :) . foldTree proximity' (pure (.)) t
  else proximity tol2 vec (chooseNode vec v t)
  where
    proximity' :: Tree2N n (VecN n Double) -> ([VecN n Double] -> [VecN n Double])
    proximity' Tree2NLeaf = id
    proximity' (Tree2NBranch v2 t2) = addOrNot . foldTree proximity' (pure (.)) t2
      where
        addOrNot = if distance2 vec v2 < tol2 then (v2 :) else id
