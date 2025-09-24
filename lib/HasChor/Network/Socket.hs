module HasChor.Network.Socket where

import HasChor.Location
import HasChor.Network hiding (run)
import HasChor.Control.Monad.Freer

import Data.List
import Control.Monad.IO.Class

import Network.Network

newtype TCPConfig = TCPConfig
  { -- ^ Association list, associating locations to IP addresses and port numbers
    locs :: [(LocTm, (String, Int))]
  }

findLoc :: TCPConfig -> LocTm -> (String, Int)
findLoc TCPConfig{ locs = locs } l = go locs
  where
    go :: [(LocTm, (String, Int))] -> (String, Int)
    go [] = error $ "cannot find location: " ++ show l
    go ((l', p):xs)
      | l == l'   = p
      | otherwise = go xs

runNetworkTCP :: MonadIO m => TCPConfig -> LocTm -> Network m a -> m a
runNetworkTCP cfg self prog = do
  undefined
  where
    runNetworkMain :: MonadIO m => Network m a -> m a
    runNetworkMain = interpFreer handler
      where
        handler :: MonadIO m => NetworkSig m a -> m a
        handler (Run m)    = m
        handler (Send a l) = do
          sockfd <- liftIO $ socket AF_INET SOCK_STREAM

          let (address, port) = findLoc cfg l
              server_addr = mkSockAddr port (Just address)
          liftIO $ connect sockfd server_addr

          liftIO $ sendString sockfd (show a)

          liftIO $ close sockfd
          return ()

        handler (Recv l)   = do
          server_fd <- liftIO $ socket AF_INET SOCK_STREAM

          let (_, port) = findLoc cfg self
              server_addr = mkSockAddr port Nothing

          liftIO $ bind server_fd server_addr
          liftIO $ listen server_fd

          (client_fd, client_addr) <- liftIO $ accept server_fd

          s <- liftIO $ recvString client_fd 1024

          liftIO $ close client_fd
          liftIO $ close server_fd

          return $ read s
        handler (BCast a)  = mapM_ handler $ map (Send a) ((map fst (locs cfg)) \\ [self])


-- instance Backend HttpConfig where
--   runNetwork = runNetworkHttp