{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls #-}

module Math.ProxN.Peano where

data Zero
data Succ n

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine = Succ Eight
type Ten = Succ Nine

class Peano p where
  fromPeano :: p -> Int

instance Peano Zero where
  fromPeano _ = 0

instance (Peano p) => Peano (Succ p) where
  fromPeano _ = 1 + (fromPeano (undefined :: p))

class (Peano p) => Logarithm p where
  logarithm :: p -> Double -> Double
  logarithm _ a = log a / log (fromIntegral (fromPeano (undefined :: p)))

instance Logarithm Zero

instance (Peano a) => Logarithm (Succ a)
