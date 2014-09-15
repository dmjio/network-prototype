{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
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
  is <- Streams.handleToInputStream handle
  forever $ do
     F.mapM_ handleBS =<< Streams.read is
  where
    handleBS bs = 
      case decode bs :: Either String Command of
        Left err  -> error $ "Couldn't read stream: " ++ err
        Right cmd -> putStrLn $ "Got command: " ++ show cmd
                    




