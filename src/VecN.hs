{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs, UndecidableInstances #-}

module VecN ( VecN
            , VecTNil(..)
            , VecTCons(..)
            , VecNClass(..)
            ) where

import Pretty

import Control.Applicative
import Control.Monad(liftM2)
import Data.Functor
import Data.Foldable
import Data.Monoid

import qualified Peano as P

data VecTNil a = VecTNil
data VecTCons v a = !a :<: !(v a)

instance Show (VecTNil a) where
  show = const "[]"

instance (Show a, Show (v a)) => Show (VecTCons v a) where
  show (a :<: v) = "[" ++ show a ++ "]" ++ show v

instance Functor VecTNil where
  fmap _ VecTNil = VecTNil

instance (Functor v) => Functor (VecTCons v) where
  fmap f (a :<: v) = f a :<: fmap f v

instance Foldable VecTNil where
  foldMap _ VecTNil = mempty

instance (Foldable v) => Foldable (VecTCons v) where
  foldMap f (a :<: v) = f a `mappend` foldMap f v

instance Applicative VecTNil where
  pure _ = VecTNil
  (<*>) _ _ = VecTNil

instance (Applicative v) => Applicative (VecTCons v) where
  pure a = a :<: pure a
  (f :<: v1) <*> (a :<: v2) = f a :<: (v1 <*> v2)

instance (Show a) => Pretty (VecTNil a) where
  pretty f n = (f n ++) . show

instance (Show a, Show (v a)) => Pretty (VecTCons v a) where
  pretty f n = (f n ++) . show

class ( Functor (VecN n)
      , Foldable (VecN n) ) => Vectify n where
  type VecN n :: * -> *

instance Vectify P.Zero where
  type VecN P.Zero = VecTNil

instance (Vectify r) => Vectify (P.Succ r) where
  type VecN (P.Succ r) = VecTCons (VecN r)

class ( Functor v --(VecN n)
      , Applicative v --(VecN n)
      ) => VecNClass v where
  distance2 :: (Num a) => v a -> v a -> a  
  vecCartProd :: v [a] -> [v a]
  fromList :: [a] -> Maybe (v a)

instance VecNClass VecTNil where
  distance2 _ _ = 0
  vecCartProd VecTNil = [VecTNil]
  fromList _ = Just (VecTNil)

instance (VecNClass v) => VecNClass (VecTCons v) where
  distance2 (a :<: v1) (b :<: v2) = (a - b) ^ (2 :: Int) + distance2 v1 v2
  vecCartProd (as :<: vs) = liftM2 (:<:) as (vecCartProd vs)
                        -- [ a :<: v | a <- as
                        --           , v <- vecCartProd vs ]
  fromList [] = Nothing
  fromList (a : as) = (a :<:) <$> fromList as
