module StyxNet (runSrvIO) where

import StyxData
import StyxSrv

import System.IO

import Control.Monad.State

import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString
import Data.ByteString.Lazy (toStrict)

msgDecoder :: Decoder ByteString
msgDecoder = runGetIncremental (fmap (subtract 4 . fromIntegral) getWord32le >>= getByteString)

runSrvIO :: MonadIO m => SrvHandler m -> Handle -> m ()
runSrvIO sh hnd =
  runStyxT $ go msgDecoder
  where
    go decoder =
      case decoder of
        Fail _ _ why -> error why -- TODO: something else
        Done rest _ r ->
          case runGetIncremental getTtaggedMessage `pushChunk` r of
            Fail _ _ why -> error why -- TODO: ..
            Partial bleh -> error "message too short"
            Done _ _ r -> do
              liftIO $ System.IO.putStrLn $ "< " ++ show r
              input sh send r
              go msgDecoder
        Partial fn -> do
          more <- liftIO $ hGetSome hnd 8192
          go $ fn $ Just more
    send msg = do
      liftIO $ System.IO.putStrLn $ "> " ++ show msg
      liftIO $ hPut hnd $ toStrict (runPut $ putWord32le (fromIntegral . (+4) $ Data.ByteString.length bs) >> putByteString bs)
      where
        bs = toStrict $ runPut $ putRtaggedMessage msg
