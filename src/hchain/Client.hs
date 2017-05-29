{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.Client (start) where

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
data ProtocolMsg = GetBlocksMsg (Maybe BlockIdent)
                 | InvMsg [BlockIdent]
                 | GetDataMsg BlockIdent
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

  liftIO $ putStrLn ("My pid " ++ show self)
  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    GetBlocksMsg hash -> onGetBlocks self sender hash chain
    InvMsg hashes     -> onInv self sender hashes chain

connectToNetwork :: BlockChain (Block a) -> ProcessId -> Process (BlockChain (Block a))
connectToNetwork chain pid = do
  liftIO $ putStrLn "Sending connection request"
  P2P.nsendCapable "mainLoop" (pid, GetBlocksMsg (lastBlockHash chain))
  return chain
  where
    lastBlockHash []     = Nothing
    lastBlockHash (x:xs) = Just $ _bHash x

onGetBlocks :: ProcessId -> ProcessId -> Maybe BlockIdent -> BlockChain (Block a) -> Process ()
onGetBlocks self sender msg chain = case msg of
  (Just hash) -> do
    liftIO $ putStrLn "Getblocks with initial received"
    let hashes = hashesFrom hash
    liftIO $ putStrLn ("Going to send hashes " ++ show hashes)
    DP.send sender (self, InvMsg hashes)
    mainLoop chain
  Nothing -> do
    liftIO $ putStrLn "Getblocks for the first time received"
    DP.send sender (self, InvMsg initialHashes)
    mainLoop chain
  where
    hashesFrom hash = firstHashes 500 . takeWhile (\block -> _bHash block /= hash) $ chain
    initialHashes = firstHashes 500 . reverse $ chain
    firstHashes n = map _bHash . take n

onInv :: ProcessId -> ProcessId -> [BlockIdent] -> BlockChain (Block a) -> Process ()
onInv self sender hashes chain = do
  liftIO $ putStrLn $ "Received InvMsg with hashes " ++ show hashes
  mainLoop chain
