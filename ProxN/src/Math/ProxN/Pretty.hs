{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.ProxN.Pretty( Pretty(..)
                        , Pretty1(..)
                        , prettyPut
                        , prettyNl
                        , prettyIndent
                        ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

newtype PrettyM a = PrettyM (StateT (String -> String)
                             (Reader ((String -> String),
                                      (String -> String))) a)
                  deriving (Monad, Functor, Applicative,
                            MonadReader ((String -> String), (String -> String)))

prettyPut :: String -> PrettyM ()
prettyPut str = PrettyM . modify $ (. (str ++))

prettyNl :: PrettyM ()
prettyNl = do
  (_, ind) <- ask
  PrettyM . modify $ (. (('\n' :) . ind))

prettyIndent :: PrettyM a -> PrettyM a
prettyIndent pr = do
  local (\(indUnit, ind) -> (indUnit, indUnit . ind)) $ pr

runPrettyM :: (String -> String) -> PrettyM a -> (a, String)
runPrettyM indUnit (PrettyM pm) =
  let (ret, fs) = runReader (runStateT pm id) (indUnit, id) in
  (ret, fs "")

class Pretty p where
  prettyPrint :: p -> PrettyM ()
  
  pretty :: (String -> String) -> p -> String
  pretty indUnit = snd . runPrettyM indUnit . prettyPrint
  
  prettySimp :: p -> String
  prettySimp = pretty (' ' :)

class Pretty1 p1 where
  prettyPrint1 :: (Pretty p) => p1 p -> PrettyM ()
  
  pretty1 :: (Pretty p) => (String -> String) -> p1 p -> String
  pretty1 indUnit = snd . runPrettyM indUnit . prettyPrint1
  
  prettySimp1 :: (Pretty p) => p1 p -> String
  prettySimp1 = pretty1 (' ' :)
