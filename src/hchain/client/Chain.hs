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
  self <- getSelfPid

  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    GetBlocksMsg hash -> onGetBlocks self sender hash chain
    InvMsg hashes     -> onInv self sender hashes chain
    GetDataMsg hash   -> onGetData self sender hash chain

onGetBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> Maybe InvItem -> BlockChain (Block a) -> Process ()
onGetBlocks self sender msg chain = case msg of
  (Just ("block", hash)) -> do
    liftIO $ putStrLn "Getblocks with initial received"
    let hashes = hashesFrom hash
    liftIO $ putStrLn ("Going to send hashes " ++ show hashes)
    DP.send sender (self, InvMsg (map bInvItem hashes))
    mainLoop chain
  (Just (t, hash)) -> do
    liftIO $ putStrLn $ "Getblocks with " ++ show t ++ "received: Ignoring"
    mainLoop chain
  Nothing -> do
    liftIO $ putStrLn "Getblocks for the first time received"
    DP.send sender (self, InvMsg (map bInvItem initialHashes))
    mainLoop chain
  where
    hashesFrom hash = firstHashes 500 . takeWhile (\block -> _bHash block /= hash) $ chain
    initialHashes = firstHashes 500 chain
    firstHashes n = map _bHash . take n . reverse

onInv :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> [InvItem] -> BlockChain (Block a) -> Process ()
onInv self sender hashes chain = do
  liftIO $ putStrLn $ "Received InvMsg with hashes " ++ show hashes
  let getBlocks = map (getBlock sender . bInvItem) hashes'
  newChain <- foldl addToChain (return chain) getBlocks
  liftIO $ putStrLn $ "New chain looks like " ++ show newChain
  mainLoop newChain
  where
    hashes' = filter (`notElem` blockhashes) (map snd hashes)
    blockhashes = map _bHash chain
    addToChain :: (Show a, Typeable a, Binary a, BContent a) => Process (BlockChain (Block a)) -> Process (Block a) -> Process (BlockChain (Block a))
    addToChain chain' blockGetter = do
      block <- blockGetter
      c <- chain'
      let newChain = addValidBlock block c
      case newChain of
        Just nchain -> do
          P2P.nsendCapable processTypeId (self, InvMsg [bInvItem $ _bHash block])
          return nchain
        Nothing     -> return c

getBlock :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> Process (Block a)
getBlock sender hash = do
  self <- getSelfPid
  DP.send sender (self, GetDataMsg hash)
  (_sender, block) <- expect :: (Typeable a, Binary a, BContent a) => Process (ProcessId, Block a)
  return block

onGetData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> ProcessId -> InvItem -> BlockChain (Block a) -> Process ()
onGetData self sender hash chain = do
  liftIO $ putStrLn $ "Received GetData with hash " ++ show hash
  DP.send sender (self, blockForHash (snd hash) chain)
  mainLoop chain
  where
    blockForHash h = fromJust . find ((== h) . _bHash)

bInvItem :: Hash -> InvItem
bInvItem = mkInvItem tBlock
