module StyxNet where

import StyxData
import StyxSrv

import Control.Monad.State

import Data.Binary.Get
import Data.ByteString

type BuffererT m = StateT (ByteString -> m (), Decoder ByteString) m

msgDecoder :: Decoder ByteString
msgDecoder = runGetIncremental (fmap fromIntegral getWord32le >>= getByteString)

addBuffer :: Monad m => ByteString -> BuffererT m ()
addBuffer addbs = do
  (sink, decoder) <- get
  case decoder `pushChunk` addbs of
    Fail _ _ why -> error why -- TODO: something else
    Partial fn -> put (sink, fn Nothing)
    Done rest _ r -> do
      lift $ sink r
      put (sink, msgDecoder)
      addBuffer rest

evalBuffererT :: Monad m => (ByteString -> m ()) -> BuffererT m a -> m a
evalBuffererT sink = flip evalStateT (sink, msgDecoder)
