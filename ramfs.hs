import StyxData
import StyxSrv
import StyxNet
import StyxUtil
import qualified Ramfs as R

import Network

sh =
  SrvHandler (\_ err resp -> resp (qid, R.fhFor 0))
  where
    qid = Qid (Type 0x80) (Version 0) (Path 0)

main = do
  sock <- listenOn (PortNumber 1256)
  putStrLn "listening"
  (hnd, _, _) <- accept sock
  putStrLn "connected"
  endState <- R.runRamfsT (runSrvIO sh hnd) R.empty
  print endState
