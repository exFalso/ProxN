module Main where

import Math.ProxN.VecN as Vec
import Math.ProxN.Tree2N as Tree
import Math.ProxN.Peano
import Math.ProxN.Pretty
import Math.ProxN.Proximity

import System.Random
import Control.Applicative
import Control.Monad.Random(runRandT, RandT, getRandomR)
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

-- number of points
_NUM :: Int
_NUM = 2000

-- range of one coordinate
_RANGE :: (Double, Double)
_RANGE = (0, 100)

-- r^2
_TOLERANCE2 :: Double
_TOLERANCE2 = 500.0

-- dimension
type Dim = Five


------------


randomList :: Int -> IO [Double]
randomList n = withStdGen (replicateM n (getRandomR _RANGE))

withStdGen :: (MonadIO m) => RandT StdGen m a -> m a
withStdGen r = do
  gen <- liftIO getStdGen
  (a, nextGen) <- runRandT r gen
  liftIO $ setStdGen nextGen
  return a

-- generates a random vector, then 20 random vector-trees of _NUM vectors and calculates the proximity sets of the vector
main :: IO ()
main = do
  let peano = undefined :: Dim
  ranVec <- fromJust . Vec.fromList <$> randomList (fromPeano peano)
  mapM_ putStrLn
    [ "Dimension: " ++ show (fromPeano peano)
    , "Number of points: " ++ show _NUM
    , "Random Vector: " ++ prettySimp (ranVec :: VecN Dim Double)
    , "Tolerance^2: " ++ show _TOLERANCE2
    , "Depth lower bound: " ++ show (logarithm peano (fromIntegral _NUM))
    ]
  replicateM_ 20 $ do
    let vecM = Vec.fromList <$> randomList (fromPeano peano)
    tr <- Tree.fromList <$> replicateM _NUM (fromJust <$> vecM)
    putStrLn $ "Depth: " ++ show (depth tr)
    putStrLn $ "Vecs in proximity: "
      ++ show ((prox _TOLERANCE2 ranVec tr))
