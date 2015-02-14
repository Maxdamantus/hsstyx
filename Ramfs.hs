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

putNewObj :: Monad m => Object -> RamfsT m Word64
putNewObj obj = do
  modify $ \(counter, map) ->
    (succ counter, M.insert counter obj map)
  liftM (pred . fst) get

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
          Object (CFile _) -> fileWalker it dirs err resp
          Object (CDir ls) -> walker it (fmap fhFor . flip M.lookup ls) dirs err resp
      ,
      fhOpen = \mode err resp -> getObjS path err $ \obj -> do
        fhStat it () err $ \stat ->
          case obj of
            Object (CFile _) -> resp ((openFileFh path){ fhStat = fhStat it {- .. see below -} }, stQid stat, 8192)
            Object (CDir ls) -> do
              let ls' = M.toList ls
              let names = map fst ls'
              fhStatList (map (fhFor . snd) ls') err $ \stats ->
                resp (nilFidHandler{
                  fhStat = fhStat it, -- for create; I suspect clients aren't actually allowed to rely on this
                  fhRead = dirRead (zipWith (\name stat -> stat{ stName = name }) names stats)
                }, stQid stat, 8192)
      ,
      fhCreate = \(name, Perm perm, omode) err resp -> getObjS path err $ \obj ->
        case obj of
          Object (CFile _) -> err "not a directory"
          Object (CDir ls) -> case M.lookup name ls of
            Just _ -> err "file exists"
            Nothing -> do
              let newobj = case perm `div` (2^24) of
                    0 -> Object $ CFile BS.empty
                    0x80 -> Object $ CDir M.empty
              newpath <- lift $ putNewObj newobj
              lift $ putObj path (Object $ CDir $ M.insert name newpath ls)
              fhOpen (fhFor newpath) omode err resp
      ,
      fhWstat = \stat err resp -> resp () -- to stop it complaining for now
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
      ,
      fhWrite = \(offs, wdata) err resp -> getObjS path err $ \obj -> do
        let offs' = fromIntegral offs
        case obj of
          Object (CFile bs) ->
            if offs' > BS.length bs
              then err "can't write after the end"
              else do
                let newbs = BS.concat [BS.take offs' bs, wdata, BS.drop (offs' + BS.length wdata) bs]
                lift $ putObj path (Object $ CFile newbs)
                resp $ fromIntegral $ BS.length wdata
          _ -> err "I don't belong here"
    }

empty :: RamfsState
empty = (1, M.fromList [(0, Object (CDir M.empty))])

runRamfsT :: Monad m => RamfsT m () -> RamfsState -> m RamfsState
runRamfsT = execStateT
