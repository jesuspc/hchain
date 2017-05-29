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
import           Hchain.Transaction

import           Data.List                        (find)
import           Data.Maybe                       (fromJust)

type VersionData = Int
type BlockIdent = Hash
data ProtocolMsg = GetBlocksMsg (Maybe BlockIdent)
                 | InvMsg [BlockIdent]
                 | GetDataMsg BlockIdent
                 deriving (Show, Generic, Typeable)

instance Binary ProtocolMsg
instance Binary Transaction
instance (Typeable a, Binary a) => Binary (Block a)

start :: (Show a, Typeable a, Binary a, BContent a) => HostName -> ServiceName -> [String] -> BlockChain (Block a) -> IO ()
start host port seeds chain = P2P.bootstrap host port (map P2P.makeNodeId seeds) initRemoteTable (mainProcess chain)

mainProcess :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
mainProcess chain = do
  node <- spawnLocal (initLoop chain)
  return ()

initLoop :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
initLoop chain = do
  liftIO $ threadDelay 1000000

  liftIO $ putStrLn $ "My chain looks like " ++ show chain

  newChain <- getSelfPid >>= connectToNetwork chain
  getSelfPid >>= register "mainLoop"

  mainLoop newChain

mainLoop :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
mainLoop chain = do
  self <- getSelfPid

  liftIO $ putStrLn ("My pid " ++ show self)
  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    GetBlocksMsg hash -> onGetBlocks self sender hash chain
    InvMsg hashes     -> onInv self sender hashes chain
    GetDataMsg hash   -> onGetData self sender hash chain

connectToNetwork :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> ProcessId -> Process (BlockChain (Block a))
connectToNetwork chain pid = do
  liftIO $ putStrLn "Sending connection request"
  P2P.nsendCapable "mainLoop" (pid, GetBlocksMsg (lastBlockHash chain))
  return chain
  where
    lastBlockHash []     = Nothing
    lastBlockHash (x:xs) = Just $ _bHash x

onGetBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> Maybe BlockIdent -> BlockChain (Block a) -> Process ()
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
    initialHashes = firstHashes 500 chain
    firstHashes n = map _bHash . take n . reverse

onInv :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> [BlockIdent] -> BlockChain (Block a) -> Process ()
onInv self sender hashes chain = do
  liftIO $ putStrLn $ "Received InvMsg with hashes " ++ show hashes
  let getBlocks = map (getBlock sender) hashes'
  newChain <- foldl addToChain (return chain) getBlocks
  liftIO $ putStrLn $ "New chain looks like " ++ show newChain
  mainLoop newChain
  where
    hashes' = filter (`notElem` blockhashes) hashes
    blockhashes = map _bHash chain
    addToChain :: (Show a, Typeable a, Binary a, BContent a) => Process (BlockChain (Block a)) -> Process (Block a) -> Process (BlockChain (Block a))
    addToChain chain' blockGetter = do
      block <- blockGetter
      c <- chain'
      let newChain = addValidBlock block c
      case newChain of
        Just nchain -> return nchain
        Nothing     -> return c

getBlock :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> BlockIdent -> Process (Block a)
getBlock sender hash = do
  self <- getSelfPid
  DP.send sender (self, GetDataMsg hash)
  (sender, block) <- expect :: (Typeable a, Binary a, BContent a) => Process (ProcessId, Block a)
  return block

onGetData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> BlockIdent -> BlockChain (Block a) -> Process ()
onGetData self sender hash chain = do
  liftIO $ putStrLn $ "Received GetData with hash " ++ show hash
  DP.send sender (self, blockForHash hash chain)
  mainLoop chain
  where
    blockForHash h = fromJust . find ((== h) . _bHash)
