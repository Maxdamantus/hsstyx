module StyxData where

import Data.Functor
import Data.Word
import Data.ByteString
import Data.Binary

newtype Fid = Fid Word32
  deriving (Show, Read, Eq, Ord)

instance Binary Fid where
  put (Fid w) = put w
  get = Fid <$> get

newtype Tag = Tag Word32
  deriving (Show, Read, Eq)

instance Binary Tag where
  put (Tag w) = put w
  get = Tag <$> get

newtype Mode = Mode Word8
  deriving (Show, Read, Eq)

instance Binary Mode where
  put (Mode w) = put w
  get = Mode <$> get

newtype Perm = Perm Word32
  deriving (Show, Read, Eq)

instance Binary Perm where
  put (Perm w) = put w
  get = Perm <$> get

newtype Type = Type Word8
  deriving (Show, Read, Eq)

instance Binary Type where
  put (Type w) = put w
  get = Type <$> get

newtype Version = Version Word32
  deriving (Show, Read, Eq)

instance Binary Version where
  put (Version w) = put w
  get = Version <$> get

newtype Path = Path Word64
  deriving (Show, Read, Eq)

instance Binary Path where
  put (Path w) = put w
  get = Path <$> get

data Qid = Qid Type Version Path
  deriving (Show, Read, Eq)

instance Binary Qid where
  put (Qid t v p) = put (t, v, p)
  get = do
    (t, v, p) <- get
    return (Qid t v p)

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

data TtaggedMessage = TtaggedMessage Tag Tmessage
  deriving (Show, Read)

instance Binary TtaggedMessage where
  put (TtaggedMessage tag msg) =
    case msg of
      Tversion msize version -> put (100 :: Word16, tag, msize, version)
      Tauth afid uname aname -> put (102 :: Word16, afid, uname, aname)
  get = do
    msgType <- get
    tag <- get
    msg <- case msgType :: Word16 of
      100 -> do{ (msize, version) <- get; return $ Tversion msize version }
      102 -> do{ (afid, uname, aname) <- get; return $ Tauth afid uname aname }
    return $ TtaggedMessage tag msg

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

data RtaggedMessage = RtaggedMessage Tag Rmessage
  deriving (Show, Read)
