{-# LANGUAGE DeriveGeneric #-}
module Common where

import GHC.Generics
import Data.Serialize

instance Serialize Command

data Command = SayHi
             | SayYo deriving (Show, Generic)

