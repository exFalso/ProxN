module Show1 where

class Show1 v where
  show1 :: (Show a) => v a -> String
