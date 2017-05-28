{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.Client where

import           Control.Concurrent               (threadDelay)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      as DP
import           Control.Distributed.Process.Node as DPN
import           Control.Lens
import           Control.Monad

import           Data.Binary
import           Data.Typeable
import           GHC.Generics

import           Network.Socket

import           Hchain.BlockChain

type VersionData = Int
type BlockIdent = Hash
data ProtocolMsg = VersionMsg VersionData
                 | VerackMsg VersionData
                 | VerackAcceptMsg
                 | GetBlocks (Maybe BlockIdent)
                 deriving (Show, Generic, Typeable)
instance Binary ProtocolMsg

start :: HostName -> ServiceName -> [String] -> BlockChain (Block a) -> IO ()
start host port seeds chain = P2P.bootstrap host port (map P2P.makeNodeId seeds) initRemoteTable (mainProcess chain)

mainProcess :: BlockChain (Block a) -> Process ()
mainProcess chain = do
  node <- spawnLocal (initLoop chain)
  return ()

initLoop :: BlockChain (Block a) -> Process ()
initLoop chain = do
  liftIO $ threadDelay 1000000

  newChain <- getSelfPid >>= connectToNetwork chain
  getSelfPid >>= register "mainLoop"

  mainLoop newChain

mainLoop :: BlockChain (Block a) -> Process ()
mainLoop chain = do
  self <- getSelfPid

  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    VersionMsg i -> do
      liftIO $ putStrLn ("Received version " ++ show i)
      DP.send sender (self, VerackMsg 1)
      mainLoop chain
    VerackMsg i -> do
      liftIO $ putStrLn ("Verack received for version" ++ show i)
      DP.send sender (self, VerackAcceptMsg)
      mainLoop chain
    VerackAcceptMsg -> do
      liftIO $ putStrLn "Verack accept"
      mainLoop chain
    GetBlocks (Just hash) -> do
      liftIO $ putStrLn "Getblocks with initial received"
      mainLoop chain
    GetBlocks Nothing -> do
      liftIO $ putStrLn "Getblocks for the first time received"
      mainLoop chain

connectToNetwork :: BlockChain (Block a) -> ProcessId -> Process (BlockChain (Block a))
connectToNetwork chain pid = do
  liftIO $ putStrLn "Sending connection request"
  P2P.nsendCapable "mainLoop" (pid, GetBlocks (lastBlockHash chain))
  P2P.nsendCapable "mainLoop" (pid, VersionMsg 1)
  return chain
  where
    lastBlockHash []     = Nothing
    lastBlockHash (x:xs) = Just $ _bHash x
