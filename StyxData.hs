module StyxData where

import Control.Applicative
import Data.Word
import Data.ByteString
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- not using Binary class, should make lists/ByteStrings/words (le) easier
class SBinary t where
  sput :: t -> Put
  sget :: Get t

instance SBinary Word8 where
  sput = putWord8
  sget = getWord8

instance SBinary Word16 where
  sput = putWord16le
  sget = getWord16le

instance SBinary Word32 where
  sput = putWord32le
  sget = getWord32le

instance SBinary Word64 where
  sput = putWord64le
  sget = getWord64le

instance (SBinary a, SBinary b) => SBinary (a, b) where
  sput (a, b) = sput a >> sput b
  sget = (,) <$> sget <*> sget

instance (SBinary a, SBinary b, SBinary c) => SBinary (a, b, c) where
  sput (a, b, c) = sput a >> sput b >> sput c
  sget = (,,) <$> sget <*> sget <*> sget

instance (SBinary a, SBinary b, SBinary c, SBinary d) => SBinary (a, b, c, d) where
  sput (a, b, c, d) = sput a >> sput b >> sput c >> sput d
  sget = (,,,) <$> sget <*> sget <*> sget <*> sget

instance SBinary ByteString where
  sput bs = sput (fromIntegral $ Data.ByteString.length bs :: Word16) >> putByteString bs
  sget = do{ len <- get; getByteString (fromIntegral (len :: Word16)) }

instance SBinary a => SBinary [a] where
  sput l = sput (fromIntegral $ Prelude.length l :: Word16) >> mapM_ sput l
  sget = do{ len <- get; sequence $ Prelude.replicate (fromIntegral (len :: Word16)) sget }

newtype Fid = Fid Word32
  deriving (Show, Read, Eq, Ord)

instance SBinary Fid where
  sput (Fid w) = sput w
  sget = Fid <$> sget

newtype Tag = Tag Word32
  deriving (Show, Read, Eq)

instance SBinary Tag where
  sput (Tag w) = sput w
  sget = Tag <$> sget

newtype Mode = Mode Word8
  deriving (Show, Read, Eq)

instance SBinary Mode where
  sput (Mode w) = sput w
  sget = Mode <$> sget

newtype Perm = Perm Word32
  deriving (Show, Read, Eq)

instance SBinary Perm where
  sput (Perm w) = sput w
  sget = Perm <$> sget

newtype Type = Type Word8
  deriving (Show, Read, Eq)

instance SBinary Type where
  sput (Type w) = sput w
  sget = Type <$> sget

newtype Version = Version Word32
  deriving (Show, Read, Eq)

instance SBinary Version where
  sput (Version w) = sput w
  sget = Version <$> sget

newtype Path = Path Word64
  deriving (Show, Read, Eq)

instance SBinary Path where
  sput (Path w) = sput w
  sget = Path <$> sget

data Qid = Qid Type Version Path
  deriving (Show, Read, Eq)

instance SBinary Qid where
  sput (Qid t v p) = sput (t, v, p)
  sget = do
    (t, v, p) <- sget
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
  -- TODO: Twrite, Tclunk, Tremove, Tstat, Twstat
  deriving (Show, Read)

data TtaggedMessage = TtaggedMessage Tag Tmessage
  deriving (Show, Read)

instance SBinary TtaggedMessage where
  sput (TtaggedMessage tag msg) = do
    sput (msgType :: Word16)
    putMsg
    where
      (msgType, putMsg) = case msg of
        Tversion msize version -> (100, sput (tag, msize, version))
        Tauth afid uname aname -> (102, sput (afid, uname, aname))
        Tflush oldtag -> (104, sput oldtag)
        Tattach fid afid uname aname -> (106, sput (fid, afid, uname, aname))
        Twalk fid newfid nwnames -> (108, sput (fid, newfid, nwnames))
        Topen fid mode -> (110, sput (fid, mode))
        Tcreate qid name perm mode -> (112, sput (qid, name, perm, mode))
        Tread fid offset count -> (114, sput (fid, offset, count))
  sget = do
    msgType <- sget
    tag <- sget
    msg <- case msgType :: Word16 of
      100 -> Tversion <$> sget <*> sget
      102 -> Tauth <$> sget <*> sget <*> sget
      104 -> Tflush <$> sget
      106 -> Tattach <$> sget <*> sget <*> sget <*> sget
      108 -> Twalk <$> sget <*> sget <*> sget
      110 -> Topen <$> sget <*> sget
      112 -> Tcreate <$> sget <*> sget <*> sget <*> sget
      114 -> Tread <$> sget <*> sget <*> sget
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
  -- TODO: Rwrite, Rclunk, Rremove, Rstat, Rwstat
  deriving (Show, Read)

data RtaggedMessage = RtaggedMessage Tag Rmessage
  deriving (Show, Read)
