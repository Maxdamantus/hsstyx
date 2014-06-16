import Control.Arrow (first)

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

type Err m = String -> StyxT m ()
type Resp m t = t -> StyxT m ()
type Handler m i o = i -> Resp m o -> Err m -> StyxT m ()

notYetImpl :: Handler m i o
notYetImpl _ _ err = err "not yet implemented"

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

data StyxState m = StyxState (M.Map Fid (Maybe (FidHandler m)))

newtype StyxT m a = StyxT { runStyxT :: StyxState m -> m (a, StyxState m) }

instance Functor m => Functor (StyxT m) where
  fmap f st = StyxT $ \s ->
    fmap (first f) $ runStyxT st s

instance Monad m => Monad (StyxT m) where
  return v = StyxT $ \s ->
    return (v, s)
  a >>= bfn = StyxT $ \s -> do
    (r, s') <- runStyxT a s
    runStyxT (bfn r) s'

-- instance MonadTrans

modify :: Monad m => (Map Fid (FidHandler m) -> Map Fid (FidHandler m)) -> StyxT m ()
modify f = StyxT $ \(StyxState s) -> ((), return $ StyxState $ f s)

input :: Monad m => SrvHandler m -> ((Tag, Rmessage) -> m ()) -> (Tag, Tmessage) -> StyxT m ()
input sh out (tag, tmsg) = modify input'
  where
    input' hndlrs = case tmsg of
      Tversion msize version -> out (tag, Rversion msize version)
      Tattach fid afid uname aname -> shAttach (uname, aname) $ \(qid, fh) -> do
        lift $ out ..
      _ -> err "not yet implemented"
    where
{-      checkNoFid fid = if fid `M.member` hndlrs
        then return True
        else err ("duplicate " ++ show fid) >> return False
      checkUsableFid fid = if not (fid `M.member` hndlrs) -}
      err = out . Rerror . U.fromString
