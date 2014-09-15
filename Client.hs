{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import qualified Data.ByteString.Char8 as B8
import           Network
import           System.IO.Streams  (InputStream, OutputStream)
import qualified System.IO.Streams  as Streams
import System.IO
import Common
import           Control.Applicative
import Data.Serialize
import qualified Data.Foldable as F 

secs :: Int -> Int
secs = (*1000000)

main :: IO ()
main = do
  handle <- connectTo "localhost" (PortNumber 3333)
  hSetBuffering handle NoBuffering 
  forever $ do
    is <- Streams.handleToInputStream handle
    Streams.connectTo Streams.stdout =<< 
       Streams.map (B8.pack . show) =<< 
         (decodeFromStream is :: IO (InputStream Command))

                    




