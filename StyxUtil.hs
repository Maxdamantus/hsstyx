module StyxUtil where

import StyxSrv
import StyxData

import qualified Data.Binary.Put as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as U

-- need to think about how to make paths unique ..
-- I generally despise such things (similar to inode numbers in POSIX).

dirFh :: Monad m => String -> StyxT m [(String, FidHandler m)] -> FidHandler m
dirFh name contentsM = it
  where
    qid = Qid (Type 0x80) (Version 0) (Path 0)
    it = nilFidHandler{
      fhStat = \_ err resp ->
        resp $ Stat 0x2e00 (Dev 0x7F57) qid (FMode 0o20000000500) (Time 0xbe038000) (Time 0xbe038000) 0x54d8 (U.fromString ".") (U.fromString "max") (U.fromString "max") (U.fromString "max")
      ,
      fhWalk = \dirs err resp ->
        case dirs of
          [] -> resp (it, [])
          a:as -> do
            contents <- contentsM
            case lookup (U.toString a) contents of
              Nothing -> err "file does not exist"
              Just fh ->
                fhStat fh () err $ \Stat{ stQid = nextqid } ->
                  case as of
                    [] -> resp (fh, [nextqid])
                    _ -> fhWalk fh as err $ \(newfh, qids) ->
                      resp (newfh, nextqid:qids)
      ,
      fhOpen = \mode err resp -> do
        contents <- contentsM
        -- TODO: need to actually lower read requests to lie along stat borders
        let z stats = resp (it{ fhRead = fhRead $ readFh $ BS.concat $ map (BSL.toStrict . P.runPut . sput) stats }, qid, 8192)
        let join (fname, fh) next after = fhStat fh () err $ \fstat ->
              next $ fstat{ stName = U.fromString fname }:after
        foldr join z contents []
}

readFh :: BS.ByteString -> FidHandler m
readFh contents = it
  where
    it = nilFidHandler{
      fhStat = \_ err resp ->
        resp $ Stat 0x2e00 (Dev 0x7F57) qid (FMode 0o100) (Time 0xbe038000) (Time 0xbe038000) 0x54d (U.fromString ".") (U.fromString "max") (U.fromString "max") (U.fromString "max")
      ,
      fhRead = \(offs, count) err resp ->
        resp $ BS.take (fromIntegral count) $ BS.drop (fromIntegral offs) $ contents
      ,
      fhWalk = \dirs err resp ->
        case dirs of
          [] -> resp (it, [])
          _ -> err "not a directory"
      ,
      fhOpen = \mode err resp ->
        resp (it, qid, 8192)
    }
    qid = Qid (Type 0) (Version 0) (Path 2)
