{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GADTs, ScopedTypeVariables, TypeFamilies, BangPatterns #-}

-- Treeify constructs a depth N binary tree (that is 2^N nodes) in the type level. This will be used to construct a Tree2N with branching factor 2^N

module Tree2N ( Tree2N(..)
              , Tree2NClass(..)
              , Treed
              , TreeTBranch(..)
              , TreeTNil(..)
              , VecTree
              , fold2NTree
              , Tree2N.fromList
              , depth
              , size
              , tree0
              , tree1
              , tree2
              , tree3 ) where

import VecN
import Pretty

import Data.Functor
import Control.Applicative

import qualified Peano as P

  -- Tree with branching factor 2^N
data Tree2N n a where
  Tree2NBranch :: !a -> !(Treed n (Tree2N n a)) -> Tree2N n a
  Tree2NLeaf :: Tree2N n a

instance (Functor (Treed n)) => Functor (Tree2N n) where
  fmap _ Tree2NLeaf = Tree2NLeaf
  fmap f (Tree2NBranch a t) = Tree2NBranch (f a) (fmap (fmap f) t)

instance (Applicative (Treed n)) => Applicative (Tree2N n) where
  pure a = Tree2NBranch a (pure Tree2NLeaf)
  Tree2NLeaf <*> _ = Tree2NLeaf
  _ <*> Tree2NLeaf = Tree2NLeaf
  (Tree2NBranch f tf) <*> (Tree2NBranch a ta) = Tree2NBranch (f a) (fmap (<*>) tf <*> ta)

instance (Show a, Show (Treed n (Tree2N n a))) => Show (Tree2N n a) where
  show Tree2NLeaf = "Tree2NLeaf"
  show (Tree2NBranch a t) = "Tree2NBranch " ++ show a ++ " " ++ show t

instance (Pretty a, Pretty (Treed n (Tree2N n a))) => Pretty (Tree2N n a) where
  pretty f n Tree2NLeaf = f n ++ "_\n"
  pretty f n (Tree2NBranch a tr) = pretty f n a ++ "\n" ++ pretty f (succ n) tr

data TreeTBranch t a = t a :/\: t a
data TreeTNil a = TreeTNil a

instance Functor TreeTNil where
  fmap f (TreeTNil a) = TreeTNil (f a)

instance (Functor t) => Functor (TreeTBranch t) where
  fmap f (t1 :/\: t2) = fmap f t1 :/\: fmap f t2

instance Applicative TreeTNil where
  pure = TreeTNil
  (TreeTNil f) <*> (TreeTNil a) = TreeTNil (f a)

instance (Applicative t) => Applicative (TreeTBranch t) where
  pure a = pure a :/\: pure a
  (f1 :/\: f2) <*> (a :/\: b) = (f1 <*> a) :/\: (f2 <*> b)

instance (Show a) => Show (TreeTNil a) where
  show (TreeTNil a) = show a

instance (Show a, Show (t a)) => Show (TreeTBranch t a) where
  show (t1 :/\: t2) = "(" ++ show t1 ++ " :/\\: " ++ show t2 ++ ")"

instance (Pretty a) => Pretty (TreeTNil a) where
  pretty f n (TreeTNil a) = pretty f n a

instance (Pretty a, Pretty (t a)) => Pretty (TreeTBranch t a) where
  pretty f n (t1 :/\: t2) = f n ++ "(\n" ++
                            pretty f (succ n) t1 ++
                            pretty f (succ n) t2 ++
                            f n ++ ")\n"

class ( Functor (Treed n)
      , Applicative (Treed n)
      ) => Treeify n where
  type Treed n :: * -> *

instance Treeify P.Zero where
  type Treed P.Zero = TreeTNil

instance (Treeify r) => Treeify (P.Succ r) where
  type Treed (P.Succ r) = TreeTBranch (Treed r)

insertNode :: (Ord a, Tree2NClass n (Treed n)) =>
              VecN n a -> Tree2N n (VecN n a) -> Tree2N n (VecN n a)
insertNode v Tree2NLeaf = Tree2NBranch v (pure Tree2NLeaf)
insertNode v1 (Tree2NBranch v2 r) = Tree2NBranch v2 $ swapNode (insertNode v1) v1 v2 r

-- convenience

class (Functor t, Applicative t) => Tree2NClass n t | t -> n where
  swapNode :: (Ord a) => (b -> b) -> VecN n a -> VecN n a -> t b -> t b
  foldTree :: (a -> b) -> VecN n (b -> b -> b) -> t a -> b
  chooseNode :: (Ord a) => VecN n a -> VecN n a -> t b -> b

instance Tree2NClass P.Zero TreeTNil where
  swapNode f VecTNil VecTNil (TreeTNil !b) = TreeTNil (f b)
  foldTree f VecTNil (TreeTNil a) = f a
  chooseNode VecTNil VecTNil (TreeTNil !b) = b

instance (Tree2NClass n t) => Tree2NClass (P.Succ n) (TreeTBranch t) where
  swapNode f (!a1 :<: (!v1)) (!a2 :<: (!v2)) (!t1 :/\: (!t2)) =
    if a1 < a2 then swap t1 :/\: t2 else t1 :/\: swap t2
    where swap = swapNode f v1 v2
  foldTree f (g :<: v) (t1 :/\: t2) = g (foldTree f v t1) (foldTree f v t2)
  chooseNode (!a1 :<: (!v1)) (!a2 :<: (!v2)) (!t1 :/\: (!t2)) =
    if a1 < a2 then choose t1 else choose t2
    where choose = chooseNode v1 v2

fromList :: (Ord a, Tree2NClass n (Treed n)) =>
            [VecN n a] -> Tree2N n (VecN n a)
fromList = foldl (flip insertNode) Tree2NLeaf

tree0 :: Tree2N P.Zero Int
tree0 = Tree2NLeaf

tree1 :: Tree2N P.One Int
tree1 = Tree2NBranch 0
        (TreeTNil Tree2NLeaf :/\: TreeTNil Tree2NLeaf)

tree2 :: Tree2N P.Two Int
tree2 = Tree2NBranch 0
        (
          (TreeTNil Tree2NLeaf :/\:
           TreeTNil Tree2NLeaf
          ) :/\:
          (TreeTNil Tree2NLeaf :/\:
           TreeTNil Tree2NLeaf
          )
        )

tree3 :: Tree2N P.Three Int
tree3 = Tree2NBranch 0
        (
          (
            (TreeTNil Tree2NLeaf :/\:
             TreeTNil Tree2NLeaf
            ) :/\:
            (TreeTNil Tree2NLeaf :/\:
             TreeTNil Tree2NLeaf
            )
          ) :/\:
          (
            (TreeTNil Tree2NLeaf :/\:
             TreeTNil Tree2NLeaf
            ) :/\:
            (TreeTNil Tree2NLeaf :/\:
             TreeTNil Tree2NLeaf
            )
          )
        )


type VecTree n a = Tree2N n (VecN n a)

fold2NTree :: (Tree2NClass n (Treed n)) =>
              b -> VecN n (b -> b -> b) -> (a -> b -> b) -> Tree2N n a -> b
fold2NTree b _ _ Tree2NLeaf = b
fold2NTree b fs g (Tree2NBranch a t) =
  g a . foldTree id fs . fmap (fold2NTree b fs g) $ t

depth :: (Tree2NClass n (Treed n), VecNClass (VecN n)) => Tree2N n a -> Int
depth = fold2NTree 0 (pure max) (const succ)

size :: (Tree2NClass n (Treed n), VecNClass (VecN n)) => Tree2N n a -> Int
size = fold2NTree 0 (pure (+)) (const succ)
