{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import           Data.List
import           Network
import           System.IO.Streams  (InputStream, OutputStream)
import qualified System.IO.Streams  as Streams
import System.IO
import Control.Exception
import Data.Either
import Common
import Data.Serialize
import Control.Concurrent.Chan

secs :: Int -> Int
secs = (*1000000)

main :: IO ()
main = do
  commandList <- newChan :: IO (Chan ByteString)
  forkIO $ forever $ do
         cmd <- getLine
         case cmd of
           "yo" -> writeChan commandList (encode SayYo)
           "hi" -> writeChan commandList (encode SayHi)
           _ -> putStrLn "invalid command"
  putStrLn "Server started"
  socket <- listenOn (PortNumber 3333)
  forever $ do
    (handle, hostname, _) <- accept socket
    hSetBuffering handle NoBuffering
    putStrLn $ "Client connected: " ++ hostname
    forkIO $ writeToClient commandList =<< Streams.handleToOutputStream handle
  where
    writeToClient commandList os = do
         void $ forkFinally (action os commandList) errorHandler
    action os commandList = 
        forever $ do
          Streams.connectTo os =<< 
            Streams.fromByteString =<< 
             readChan commandList
    errorHandler result = 
        when (isLeft result) $ 
             putStrLn "Client Disconnected"













