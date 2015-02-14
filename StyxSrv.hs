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

notYetImpl :: Handler m i o
notYetImpl _ err _ = err "not yet implemented"

clunkHandler :: Handler m () ()
clunkHandler () err resp = resp ()

data FidHandler m = FidHandler {
  fhCreate :: Handler m (ByteString, Perm, OMode) (FidHandler m, Qid, Word32),
  fhRead :: Handler m (Word64, Word32) ByteString,
  fhWrite :: Handler m (Word64, ByteString) Word32,
  fhRemove :: Handler m () (),
  fhWalk :: Handler m [ByteString] (FidHandler m, [Qid]),
  fhOpen :: Handler m OMode (FidHandler m, Qid, Word32),
  fhStat :: Handler m () Stat,
  fhWstat :: Handler m Stat (),
  fhClunk :: Handler m () ()
}

instance Show (FidHandler m) where
  show fh = "FidHandler{ .. }"

nilFidHandler :: FidHandler m
nilFidHandler = FidHandler n n n n n n n n clunkHandler
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
    resp $ Rversion msize $ U.fromString "9P2000"
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
      fhWalk fh wnames err $ \(newfh, qids) -> (if fid /= newfid then checkReservedFid newfid else id) $ do
        modify $ M.insert newfid $ Just newfh
        resp $ Rwalk qids
  Topen fid mode -> checkFid fid $ \fh ->
    -- TODO: defensiveness seems inconsistent .. maybe the aim should just be for it not to crash
    fhOpen fh mode err $ \(newfh, qid, iounit) -> do
      modify $ M.insert fid $ Just newfh
      resp $ Ropen qid iounit
  Tcreate fid name perm mode -> checkFid fid $ \fh ->
    fhCreate fh (name, perm, mode) err $ \(newfh, qid, iounit) -> do
      modify $ M.insert fid $ Just newfh
      resp $ Rcreate qid iounit
  Tread fid offs len -> checkFid fid $ \fh ->
    fhRead fh (offs, len) err $ resp . Rread
  Twrite fid offs bs -> checkFid fid $ \fh ->
    fhWrite fh (offs, bs) err $ resp . Rwrite
  Tstat fid -> checkFid fid $ \fh ->
    fhStat fh () err $ resp . Rstat
  Twstat fid stat -> checkFid fid $ \fh ->
    fhWstat fh stat err $ const $ resp Rwstat
  Tclunk fid -> checkFid fid $ \fh -> do
    modify $ M.delete fid
    -- fid is removed before the response
    fhClunk fh () err $ const $ resp Rclunk
  _ -> err "implementation failure"
  where
    resp rmsg = do
      -- what if the filesystem is bad? shouldn't respond to the same request twice
      lift $ out (RtaggedMessage tag rmsg)
    checkNoFid fid okay = do
      okay
      {-
      map <- get
      if fid `M.member` map
        then err $ "duplicate " ++ show fid
        else okay
        -}
    checkFid fid okay = do
      map <- get
      case fid `M.lookup` map of
        Nothing -> err $ "non-existent " ++ show fid
        Just Nothing -> err $ "non-initialised " ++ show fid
        Just (Just fh) -> okay fh
    checkReservedFid fid okay = do
      map <- get
      case fid `M.lookup` map of
        Just Nothing -> okay
        _ -> err $ show fid ++ " no longer reserved"
    err = resp . Rerror . U.fromString
