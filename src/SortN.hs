{-# LANGUAGE FlexibleContexts #-}

module Main where

import VecN as Vec
import Tree2N as Tree
import Peano
import Pretty
import Proximity

import System.Random
import Control.Applicative
import Control.Monad.Random(runRandT, RandT, getRandomR)
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

_LENGTH :: Int
_LENGTH = 100

randList :: IO [Double]
randList = randomList _LENGTH

randomList :: Int -> IO [Double]
randomList n = withStdGen (replicateM n (getRandomR (1, 100)))

withStdGen :: (MonadIO m) => RandT StdGen m a -> m a
withStdGen r = do
  gen <- liftIO getStdGen
  (a, nextGen) <- runRandT r gen
  liftIO $ setStdGen nextGen
  return a

type Dim = Two

_TOLERANCE2 :: Double
_TOLERANCE2 = 10000.0

main :: IO ()
main = do
  let peano = undefined :: Dim
      siz = _LENGTH ^ fromPeano peano
  ranVec <- fromJust . Vec.fromList <$> randomList (fromPeano peano)
  putStrLn $ "Random Vector: " ++ prettySimp (ranVec :: VecN Dim Double)
  putStrLn $ "Tolerance^2: " ++ show _TOLERANCE2
  putStrLn $ "Smallest depth: " ++ show (logarithm peano (fromIntegral siz))
  replicateM_ 20 $ do
    rlist <- randList
    let tr = Tree.fromList $ vecCartProd (pure rlist) :: VecTree Dim Double
    putStrLn $ "Depth: " ++ show (depth tr)
    putStrLn $ "No. of vecs in proximity: "
      ++ show (length (prox _TOLERANCE2 ranVec tr))
