module Ramfs where

import StyxSrv
import StyxData
import StyxUtil

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Arrow (first, second)

import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U

data Object = Object Contents
  deriving (Show, Read)

data Contents = CFile BS.ByteString | CDir (M.Map BS.ByteString Word64)
  deriving (Show, Read)

type RamfsState = (Word64, M.Map Word64 Object)

type RamfsT m = StateT RamfsState m

getObj :: Monad m => Word64 -> RamfsT m (Maybe Object)
getObj path = liftM (M.lookup path . snd) get

putObj :: Monad m => Word64 -> Object -> RamfsT m ()
putObj path obj = modify $ second $ M.insert path obj

getObjS :: Monad m => Word64 -> Err (RamfsT m) -> (Object -> StyxT (RamfsT m) ()) -> StyxT (RamfsT m) ()
getObjS path err okay = lift (getObj path) >>= \mo ->
  case mo of
    Nothing -> err "object doesn't exist"
    Just o -> okay o

fhFor :: Monad m => Word64 -> FidHandler (RamfsT m)
fhFor path = it
  where
    it = nilFidHandler{
      fhStat = \() err resp -> getObjS path err $ \obj -> do
        let (qid, len) =
              case obj of
                Object (CDir ls) -> (Qid (Type 0x80) (Version 0) (Path path), M.size ls)
                Object (CFile bs) -> (Qid (Type 0) (Version 0) (Path path), BS.length bs)
        resp $ Stat 0 (Dev 0) qid (FMode (modeAdd qid + 0777)) (Time 0) (Time 0) (fromIntegral len) (U.fromString ".") (U.fromString "nobody") (U.fromString "nobody") (U.fromString "nobody")
      ,
      fhWalk = \dirs err resp -> getObjS path err $ \obj -> do
        case obj of
          Object (CFile _) -> err "not a directory"
          Object (CDir ls) -> walker it (fmap fhFor . flip M.lookup ls) dirs err resp
      ,
      fhOpen = \mode err resp -> getObjS path err $ \obj -> do
        fhStat it () err $ \stat ->
          case obj of
            Object (CFile _) -> resp (openFileFh path, stQid stat, 8192)
            Object (CDir ls) -> do
              let ls' = M.toList ls
              let names = map fst ls'
              fhStatList (map (fhFor . snd) ls') err $ \stats ->
                resp (nilFidHandler{
                  fhRead = dirRead (zipWith (\name stat -> stat{ stName = name }) names stats)
                }, stQid stat, 8192)
    }

-- could later remember how the file was opened
openFileFh :: Monad m => Word64 -> FidHandler (RamfsT m)
openFileFh path = it
  where
    it = nilFidHandler{
      fhRead = \offscount err resp -> getObjS path err $ \obj -> do
        case obj of
          Object (CFile bs) -> bsRead bs offscount err resp
          _ -> err "how did I get here?" -- let the days go by
    }

empty :: RamfsState
empty = (1, M.fromList [(0, Object (CDir M.empty))])

runRamfsT :: Monad m => RamfsT m () -> RamfsState -> m RamfsState
runRamfsT = execStateT
