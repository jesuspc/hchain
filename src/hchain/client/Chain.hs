module Hchain.Client.Chain (spawnProcess) where

import           Control.Concurrent              (threadDelay)
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process     as DP

import           Data.Binary
import           Data.Typeable
import           GHC.Generics

import           Hchain.BlockChain

import           Data.List                       (find)
import           Data.Maybe                      (fromJust)

import           Hchain.Client.Protocol

processTypeId :: String
processTypeId = "chain"

tBlock :: String
tBlock = "block"

spawnProcess :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
spawnProcess chain = do
  liftIO $ threadDelay 1000000

  liftIO $ putStrLn $ "My chain looks like " ++ show chain

  newChain <- getSelfPid >>= connectToNetwork chain
  getSelfPid >>= register processTypeId

  mainLoop newChain

connectToNetwork :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> ProcessId -> Process (BlockChain (Block a))
connectToNetwork chain pid = do
  liftIO $ putStrLn "Sending connection request"
  P2P.nsendCapable processTypeId (pid, GetBlocksMsg (lastBlockHash chain))
  return chain
  where
    lastBlockHash []      = Nothing
    lastBlockHash (x:_xs) = Just $ bInvItem $ _bHash x

mainLoop :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
mainLoop chain = do
  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    GetBlocksMsg hash -> onGetBlocks sender hash chain
    InvMsg hashes     -> onInv sender hashes chain
    GetDataMsg hash   -> onGetData sender hash chain

onGetBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> Maybe InvItem -> BlockChain (Block a) -> Process ()
onGetBlocks sender msg chain = case msg of
  (Just ("block", hash)) -> do
    liftIO $ putStrLn "Getblocks with initial received"
    sendBlockRange sender (Just hash) chain
    mainLoop chain
  (Just (t, hash)) -> do
    liftIO $ putStrLn $ "Getblocks with " ++ show t ++ "received: Ignoring"
    mainLoop chain
  Nothing -> do
    liftIO $ putStrLn "Getblocks for the first time received"
    sendBlockRange sender Nothing chain
    mainLoop chain

onInv :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> [InvItem] -> BlockChain (Block a) -> Process ()
onInv sender hashes chain = do
  liftIO $ putStrLn $ "Received InvMsg with hashes " ++ show hashes
  newChain <- addMissingBlocks sender hashes chain
  liftIO $ putStrLn $ "New chain looks like " ++ show newChain
  mainLoop newChain

onGetData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> BlockChain (Block a) -> Process ()
onGetData sender hash chain = do
  liftIO $ putStrLn $ "Received GetData with hash " ++ show hash
  sendBlockData sender hash chain
  mainLoop chain

sendBlockRange :: ProcessId -> Maybe Hash -> BlockChain (Block a) -> Process ()
sendBlockRange pid mh chain =
  let firstHashes n = map _bHash . take n . reverse
      selector = case mh of
                   Just h  -> takeWhile (\block -> _bHash block /= h)
                   Nothing -> id
      hashes = firstHashes 500 . selector $ chain
  in do
    self <- getSelfPid
    liftIO $ putStrLn ("Going to send hashes " ++ show hashes)
    DP.send pid (self, InvMsg (map bInvItem hashes))

addMissingBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> [InvItem] -> BlockChain (Block a) -> Process (BlockChain (Block a))
addMissingBlocks pid invs chain =
  let
    invs' = map bInvItem $ filter (`notElem` map _bHash chain) (map invHash invs)
    getBlocks = map (getBlock pid) invs'
  in foldl addToChain (return chain) getBlocks

addToChain :: (Show a, Typeable a, Binary a, BContent a) => Process (BlockChain (Block a)) -> Process (Block a) -> Process (BlockChain (Block a))
addToChain chain blockGetter = do
  self <- getSelfPid
  block <- blockGetter
  c <- chain
  let newChain = addValidBlock block c
  case newChain of
    Just nchain -> do
      P2P.nsendCapable processTypeId (self, InvMsg [bInvItem $ _bHash block])
      return nchain
    Nothing     -> return c

getBlock :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> Process (Block a)
getBlock sender inv = do
  self <- getSelfPid
  DP.send sender (self, GetDataMsg inv)
  (_sender, block) <- expect :: (Typeable a, Binary a, BContent a) => Process (ProcessId, Block a)
  return block

sendBlockData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> BlockChain (Block a) -> Process ()
sendBlockData pid h chain =
  let blockForHash h = fromJust . find ((== h) . _bHash)
  in do
    self <- getSelfPid
    DP.send pid (self, blockForHash (invHash h) chain)

bInvItem :: Hash -> InvItem
bInvItem = mkInvItem tBlock
