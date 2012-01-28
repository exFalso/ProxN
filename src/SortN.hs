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

-- number of points
_NUM :: Int
_NUM = 2000

-- r^2
_TOLERANCE2 :: Double
_TOLERANCE2 = 1.0

-- dimension
type Dim = Zero

randomList :: Int -> IO [Double]
randomList n = withStdGen (replicateM n (getRandomR (1, 100)))

withStdGen :: (MonadIO m) => RandT StdGen m a -> m a
withStdGen r = do
  gen <- liftIO getStdGen
  (a, nextGen) <- runRandT r gen
  liftIO $ setStdGen nextGen
  return a

-- generates a random vector, then 20 random vector-trees and calculates the proximity sets of the vector
main :: IO ()
main = do
  let peano = undefined :: Dim
  ranVec <- fromJust . Vec.fromList <$> randomList (fromPeano peano)
  mapM_ putStrLn
    [ "Dimension: " ++ show (fromPeano peano)
    , "Number of points: " ++ show _NUM
    , "Random Vector: " ++ prettySimp (ranVec :: VecN Dim Double)
    , "Tolerance^2: " ++ show _TOLERANCE2
    , "Smallest depth: " ++ show (logarithm peano (fromIntegral _NUM))
    ]
  replicateM_ 20 $ do
    let vecM = Vec.fromList <$> randomList (fromPeano peano)
    tr <- Tree.fromList <$> replicateM _NUM (fromJust <$> vecM)
    putStrLn $ "Depth: " ++ show (depth tr)
    putStrLn $ "Vecs in proximity: "
      ++ show ((prox _TOLERANCE2 ranVec tr))
