import Control.Arrow (first)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Data.Word
import Data.ByteString
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as U

newtype Fid = Fid Word32
  deriving (Show, Read, Eq, Ord)

newtype Tag = Tag Word32
  deriving (Show, Read, Eq)

newtype Mode = Mode Word8
  deriving (Show, Read, Eq)

newtype Perm = Perm Word32
  deriving (Show, Read, Eq)

newtype Type = Type Word8
  deriving (Show, Read, Eq)

newtype Version = Version Word32
  deriving (Show, Read, Eq)

newtype Path = Path Word64
  deriving (Show, Read, Eq)

data Qid = Qid Type Version Path
  deriving (Show, Read, Eq)

data Tmessage =
  Tversion Word32 ByteString |
  Tauth Fid ByteString ByteString |
  Tflush Tag |
  Tattach Fid Fid ByteString ByteString |
  Twalk Fid Fid [ByteString] |
  Topen Fid Mode |
  Tcreate Fid ByteString Perm Mode |
  Tread Fid Word64 Word32
  deriving (Show, Read)

data Rmessage =
  Rversion Word32 ByteString |
  Rauth Qid |
  Rerror ByteString |
  Rflush |
  Rattach Qid |
  Rwalk [Qid] |
  Ropen Qid Word32 |
  Rcreate Qid Word32 |
  Rread ByteString
  deriving (Show, Read)

type Err m = String -> StyxT m ()
type Resp m t = t -> StyxT m ()
type Handler m i o = i -> Err m -> Resp m o -> StyxT m ()

notYetImpl :: Handler m i o
notYetImpl _ err _ = err "not yet implemented"

data FidHandler m = FidHandler {
  fhCreate :: Handler m (ByteString, Perm, Mode) (Qid, Word32),
  fhRead :: Handler m (Word64, Word32) ByteString,
  fhWrite :: Handler m (Word64, ByteString) Word32,
  fhRemove :: Handler m () ()
}

nilFidHandler :: FidHandler m
nilFidHandler = FidHandler n n n n
  where n = notYetImpl

data SrvHandler m = SrvHandler {
  shAttach :: Handler m (ByteString, ByteString) (Qid, FidHandler m)
}

type StyxT m = StateT (M.Map Fid (Maybe (FidHandler m))) m

input :: Monad m => SrvHandler m -> ((Tag, Rmessage) -> m ()) -> (Tag, Tmessage) -> StyxT m ()
input sh out (tag, tmsg) = case tmsg of
  Tversion msize version ->
    resp $ Rversion msize version
  -- Tauth
  -- Tflush
  Tattach fid afid uname aname -> checkNoFid fid $ do
    modify $ M.insert fid Nothing
    shAttach sh (uname, aname) err $ \(qid, fh) -> do
      modify $ M.insert fid $ Just fh -- what if the client is bad?
      resp $ Rattach qid
  -- Twalk
  -- Topen
  -- Tcreate
  Tread fid offs len -> checkFid fid $ \fh ->
    fhRead fh (offs, len) err $ resp . Rread
  where
    resp rmsg = do
      -- what if the filesystem is bad? shouldn't respond to the same request twice
      lift $ out (tag, rmsg)
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
