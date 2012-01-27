module Pretty(Pretty(..), prettySimp) where

class Pretty p where
  pretty :: (Int -> String) -> Int -> p -> String

prettySimp :: (Pretty p) => p -> String
prettySimp = pretty (\n -> replicate n ' ') 0