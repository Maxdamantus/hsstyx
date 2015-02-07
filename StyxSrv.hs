module StyxSrv where

import Control.Arrow (first)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Data.Word
import Data.ByteString
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as U

import StyxData

type Err m = String -> StyxT m ()
type Resp m t = t -> StyxT m ()
type Handler m i o = i -> Err m -> Resp m o -> StyxT m ()

{-
writeRmessage :: ((Tag, Rmessage) -> ByteString)
writeRmessage (tag, rmsg) =
  ..
  -}

notYetImpl :: Handler m i o
notYetImpl _ err _ = err "not yet implemented"

data FidHandler m = FidHandler {
  fhCreate :: Handler m (ByteString, Perm, Mode) (Qid, Word32),
  fhRead :: Handler m (Word64, Word32) ByteString,
  fhWrite :: Handler m (Word64, ByteString) Word32,
  fhRemove :: Handler m () (),
  fhWalk :: Handler m [ByteString] [Qid],
  fhOpen :: Handler m Mode (Qid, Word32)
}

nilFidHandler :: FidHandler m
nilFidHandler = FidHandler n n n n n n
  where n = notYetImpl

data SrvHandler m = SrvHandler {
  shAttach :: Handler m (ByteString, ByteString) (Qid, FidHandler m)
}

type StyxT m = StateT (M.Map Fid (Maybe (FidHandler m))) m

runStyxT :: Monad m => StyxT m a -> m a
runStyxT s = evalStateT s M.empty

-- TODO: maybe move `sh` and `out` into the state.

input :: Monad m => SrvHandler m -> (RtaggedMessage -> m ()) -> TtaggedMessage -> StyxT m ()
input sh out (TtaggedMessage tag tmsg) = case tmsg of
  Tversion msize version ->
    resp $ Rversion msize version
  -- Tauth
  -- Tflush, somehow—doesn't really fit
  Tattach fid afid uname aname -> checkNoFid fid $ do
    modify $ M.insert fid Nothing
    shAttach sh (uname, aname) err $ \(qid, fh) -> do
      modify $ M.insert fid $ Just fh -- what if the client is bad?
      resp $ Rattach qid
  Twalk fid newfid wnames -> checkFid fid $ \fh ->
    (if fid /= newfid then checkNoFid newfid else id) $ do
      modify $ M.insert newfid $ Nothing
      fhWalk fh wnames err $ resp . Rwalk
  Topen fid mode -> checkFid fid $ \fh ->
    fhOpen fh mode err $ \(qid, iounit) ->
      resp $ Ropen qid iounit
  Tcreate fid name perm mode -> checkFid fid $ \fh ->
    fhCreate fh (name, perm, mode) err $ \(qid, iounit) -> do
      -- TODO: replace fid
      resp $ Rcreate qid iounit
  Tread fid offs len -> checkFid fid $ \fh ->
    fhRead fh (offs, len) err $ resp . Rread
  where
    resp rmsg = do
      -- what if the filesystem is bad? shouldn't respond to the same request twice
      lift $ out (RtaggedMessage tag rmsg)
    checkNoFid fid okay = do
      map <- get
      if fid `M.member` map
        then err $ "duplicate " ++ show fid
        else okay
    checkFid fid okay = do
      map <- get
      case fid `M.lookup` map of
        Nothing -> err $ "non-existent " ++ show fid
        Just Nothing -> err $ "non-initialised " ++ show fid
        Just (Just fh) -> okay fh
    err = resp . Rerror . U.fromString
