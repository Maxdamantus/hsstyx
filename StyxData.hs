module StyxData where

import Control.Applicative
import Data.Word
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
  sput bs = sput (fromIntegral $ BS.length bs :: Word16) >> putByteString bs
  sget = do{ len <- sget; getByteString (fromIntegral (len :: Word16)) }

instance SBinary a => SBinary [a] where
  sput l = sput (fromIntegral $ Prelude.length l :: Word16) >> mapM_ sput l
  sget = do{ len <- sget; sequence $ Prelude.replicate (fromIntegral (len :: Word16)) sget }

newtype Fid = Fid Word32
  deriving (Show, Read, Eq, Ord)

instance SBinary Fid where
  sput (Fid w) = sput w
  sget = Fid <$> sget

newtype Tag = Tag Word16
  deriving (Show, Read, Eq)

instance SBinary Tag where
  sput (Tag w) = sput w
  sget = Tag <$> sget

newtype FMode = FMode Word32
  deriving (Show, Read, Eq)

instance SBinary FMode where
  sput (FMode w) = sput w
  sget = FMode <$> sget

newtype OMode = OMode Word8
  deriving (Show, Read, Eq)

instance SBinary OMode where
  sput (OMode w) = sput w
  sget = OMode <$> sget

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

newtype Dev = Dev Word32
  deriving (Show, Read, Eq)

instance SBinary Dev where
  sput (Dev w) = sput w
  sget = Dev <$> sget

newtype Time = Time Word32
  deriving (Show, Read, Eq)

instance SBinary Time where
  sput (Time w) = sput w
  sget = Time <$> sget

data Stat = Stat{
  stType :: Word16,
  stDev :: Dev,
  stQid :: Qid,
  stMode :: FMode,
  stAtime, stMtime :: Time,
  stLength :: Word64,
  stName, stUid, stGid, stMuid :: ByteString
}
  deriving (Show, Read, Eq)

instance SBinary Stat where
  sput (Stat typ dev qid mode atime mtime length name uid gid muid) =
    -- WHY DOES IT HAVE TEH LENGTH TWICE?!?#@*(
    sput $ BSL.toStrict $ runPut $ sput ((typ, dev, qid, mode), (atime, mtime), (length, name), (uid, gid, muid))
  sget = skip 2 >> Stat <$> sget <*> sget <*> sget <*> sget <*> sget <*> sget <*> sget <*> sget <*> sget <*> sget <*> sget

data MsgType =
  TTversion | TRversion |
  TTauth | TRauth |
  TTattach | TRattach |
  TTerror | TRerror |
  TTflush | TRflush |
  TTwalk | TRwalk |
  TTopen | TRopen |
  TTcreate | TRcreate |
  TTread | TRread |
  TTwrite | TRwrite |
  TTclunk | TRclunk |
  TTremove | TRremove |
  TTstat | TRstat |
  TTwstat | TRwstat |
  TTlast
  deriving (Show, Read, Eq, Enum)

instance SBinary MsgType where
  sput t = sput ((fromIntegral $ fromEnum t) + 100 :: Word8)
  sget = do{ n <- sget; return $ toEnum $ fromIntegral $ (n :: Word8) - 100 }

data Tmessage =
  Tversion Word32 ByteString |
  Tauth Fid ByteString ByteString |
  Tflush Tag |
  Tattach Fid Fid ByteString ByteString |
  Twalk Fid Fid [ByteString] |
  Topen Fid OMode |
  Tcreate Fid ByteString Perm OMode |
  Tread Fid Word64 Word32 |
  Twrite Fid Word64 ByteString |
  Tclunk Fid |
  Tremove Fid |
  Tstat Fid |
  Twstat Fid Stat
  deriving (Show, Read)

data TtaggedMessage = TtaggedMessage Tag Tmessage
  deriving (Show, Read)

instance SBinary TtaggedMessage where
  sput (TtaggedMessage tag msg) = do
    sput msgType
    putMsg
    where
      (msgType, putMsg) = case msg of
        Tversion msize version -> (TTversion, sput (tag, msize, version))
        Tauth afid uname aname -> (TTauth, sput (afid, uname, aname))
        Tflush oldtag -> (TTflush, sput oldtag)
        Tattach fid afid uname aname -> (TTattach, sput (fid, afid, uname, aname))
        Twalk fid newfid nwnames -> (TTwalk, sput (fid, newfid, nwnames))
        Topen fid mode -> (TTopen, sput (fid, mode))
        Tcreate qid name perm mode -> (TTcreate, sput (qid, name, perm, mode))
        Tread fid offset count -> (TTread, sput (fid, offset, count))
        Twrite fid offset wdata -> (TTwrite, sput (fid, offset, fromIntegral $ BS.length wdata :: Word32) >> putByteString wdata)
        Tclunk fid -> (TTclunk, sput fid)
        Tremove fid -> (TTremove, sput fid)
        Tstat fid -> (TTstat, sput fid)
        Twstat fid stat -> (TTwstat, sput (fid, BSL.toStrict $ runPut $ sput stat))
  sget = do
    msgType <- sget
    tag <- sget
    msg <- case msgType of
      TTversion -> Tversion <$> sget <*> sget
      TTauth -> Tauth <$> sget <*> sget <*> sget
      TTflush -> Tflush <$> sget
      TTattach -> Tattach <$> sget <*> sget <*> sget <*> sget
      TTwalk -> Twalk <$> sget <*> sget <*> sget
      TTopen -> Topen <$> sget <*> sget
      TTcreate -> Tcreate <$> sget <*> sget <*> sget <*> sget
      TTread -> Tread <$> sget <*> sget <*> sget
      TTwrite -> Twrite <$> sget <*> sget <*> do{ len <- sget; getByteString (fromIntegral (len :: Word32)) }
      TTclunk -> Tclunk <$> sget
      TTremove -> Tremove <$> sget
      TTstat -> Tstat <$> sget
      TTwstat -> Twstat <$> sget <*> do{ skip 2; sget }
      _ -> error ("unhandled: " ++ show msgType)
    return $ TtaggedMessage tag msg

getTtaggedMessage :: Get TtaggedMessage
getTtaggedMessage = sget

putTtaggedMessage :: TtaggedMessage -> Put
putTtaggedMessage = sput

data Rmessage =
  Rversion Word32 ByteString |
  Rauth Qid |
  Rerror ByteString |
  Rflush |
  Rattach Qid |
  Rwalk [Qid] |
  Ropen Qid Word32 |
  Rcreate Qid Word32 |
  Rread ByteString |
  Rwrite Word32 |
  Rclunk |
  Rremove |
  Rstat Stat |
  Rwstat
  deriving (Show, Read)

data RtaggedMessage = RtaggedMessage Tag Rmessage
  deriving (Show, Read)

instance SBinary RtaggedMessage where
  sput (RtaggedMessage tag msg) = do
    sput (msgType, tag)
    putMsg
    where
      (msgType, putMsg) = case msg of
        Rversion msize version -> (TRversion, sput (msize, version))
        Rauth aqid -> (TRauth, sput aqid)
        Rerror ename -> (TRerror, sput ename)
        Rflush -> (TRflush, return ())
        Rattach qid -> (TRattach, sput qid)
        Rwalk wqids -> (TRwalk, sput wqids)
        Ropen qid iounit -> (TRopen, sput (qid, iounit))
        Rcreate qid iounit -> (TRcreate, sput (qid, iounit))
        -- 4-byte length
        Rread rdata -> (TRread, sput (fromIntegral $ BS.length rdata :: Word32) >> putByteString rdata)
        Rwrite count -> (TRwrite, sput count)
        Rclunk -> (TRclunk, return ())
        Rremove -> (TRremove, return ())
        Rstat stat -> (TRstat, sput (BSL.toStrict $ runPut $ sput stat))
        Rwstat -> (TRwstat, return ())
  sget = do
    (msgType, tag) <- sget
    msg <- case msgType of
      TRversion -> Rversion <$> sget <*> sget
      TRauth -> Rauth <$> sget
      TRerror -> Rerror <$> sget
      TRflush -> return Rflush
      TRattach -> Rattach <$> sget
      TRwalk -> Rwalk <$> sget
      TRopen -> Ropen <$> sget <*> sget
      TRcreate -> Rcreate <$> sget <*> sget
      TRread -> Rread <$> do{ len <- sget; getByteString (fromIntegral (len :: Word32)) }
      TRwrite -> Rwrite <$> sget
      TRclunk -> return Rclunk
      TRremove -> return Rremove
      TRstat -> Rstat <$> do{ skip 2; sget }
      TRwstat -> return Rwstat
    return $ RtaggedMessage tag msg

getRtaggedMessage :: Get RtaggedMessage
getRtaggedMessage = sget

putRtaggedMessage :: RtaggedMessage -> Put
putRtaggedMessage = sput
