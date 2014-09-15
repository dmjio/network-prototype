{-# LANGUAGE DeriveGeneric #-}
module Common where

import           Control.Applicative
import           Control.Exception
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import qualified Data.Foldable       as F
import           Data.Serialize
import           GHC.Generics
import           System.IO.Streams   (InputStream, OutputStream)
import qualified System.IO.Streams   as Streams

instance Serialize Command

data Command = SayHi
             | SayYo deriving (Show, Generic)

decodeFromStream
    :: Serialize a
    => InputStream ByteString
    -> IO (InputStream a)
decodeFromStream = Streams.map f
  where
    f :: Serialize a => ByteString -> a
    f bs = case decode bs of
             Left err -> error "Couldn't decode"
             Right val -> val

-- encodeFromOutputStream
--     :: Serialize a
--     => OutputStream ByteString
--     -> IO (InputStream a)
-- decodeFromOutputStream = Streams.map f
--   where
--     f :: Serialize a => ByteString -> a
--     f bs = case decode bs of
--              Left err -> error "Couldn't decode"
--              Right val -> val







