{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs, UndecidableInstances #-}

module Math.ProxN.VecN ( VecNClass(..)
                       , VecN(..)
                       ) where

import Math.ProxN.Pretty

import Control.Applicative
import Control.Monad(liftM2)
import Data.Functor
import Data.Foldable
import Data.Monoid

import qualified Math.ProxN.Peano as P

--data VecNil a = VecNil
--data VecTCons v a = !a :<: !(v a)

instance Show (VecN P.Zero a) where
  show = const "[]"

instance (Show a, Show (VecN n a)) => Show (VecN (P.Succ n) a) where
  show (a :<: v) = "[" ++ show a ++ "]" ++ show v

instance Functor (VecN P.Zero) where
  fmap _ VecNil = VecNil

instance (Functor (VecN n)) => Functor (VecN (P.Succ n)) where
  fmap f (a :<: v) = f a :<: fmap f v

instance Foldable (VecN P.Zero) where
  foldMap _ VecNil = mempty

instance (Foldable (VecN n)) => Foldable (VecN (P.Succ n)) where
  foldMap f (a :<: v) = f a `mappend` foldMap f v

instance Applicative (VecN P.Zero) where
  pure _ = VecNil
  (<*>) _ _ = VecNil

instance (Applicative (VecN n)) => Applicative (VecN (P.Succ n)) where
  pure a = a :<: pure a
  (f :<: v1) <*> (a :<: v2) = f a :<: (v1 <*> v2)

instance (Show a) => Pretty (VecN P.Zero a) where
  prettyPrint = prettyPut . show

instance (Show a, Show (VecN n a)) => Pretty (VecN (P.Succ n) a) where
  prettyPrint = prettyPut . show

class ( P.Peano n
      , Applicative (VecN n)
      , Functor (VecN n)) => VecNClass n where
  data VecN n :: * -> *
  distance2 :: (Num a) => VecN n a -> VecN n a -> a  
  vecCartProd :: VecN n [a] -> [VecN n a]
  fromList :: [a] -> Maybe (VecN n a)

instance (VecNClass r) => VecNClass (P.Succ r) where
  data VecN (P.Succ r) a = !a :<: !(VecN r a)
  distance2 (a :<: v1) (b :<: v2) = (a - b) ^ (2 :: Int) + distance2 v1 v2
  vecCartProd (as :<: vs) = liftM2 (:<:) as (vecCartProd vs)
                        -- [ a :<: v | a <- as
                        --           , v <- vecCartProd vs ]
  fromList [] = Nothing
  fromList (a : as) = (a :<:) <$> fromList as

instance VecNClass P.Zero where
  data VecN P.Zero a = VecNil
  distance2 _ _ = 0
  vecCartProd VecNil = [VecNil]
  fromList _ = Just VecNil
