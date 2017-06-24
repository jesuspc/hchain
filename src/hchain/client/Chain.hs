module Hchain.Client.Chain (spawnProcess) where

import           Control.Concurrent              (threadDelay)
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process     as DP

import           Data.Binary
import           Data.Typeable

import           Hchain.BlockChain
import           Hchain.Client.Protocol

import           Data.List                       (find)

import           Control.Concurrent.MVar

processTypeId :: String
processTypeId = "chain"

spawnProcess :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> MVar (BlockChain (Block a)) -> Process ()
spawnProcess chain storage = do
  liftIO $ threadDelay 1000000

  liftIO $ putStrLn $ "My chain looks like " ++ show chain

  newChain <- getSelfPid >>= connectToNetwork chain
  getSelfPid >>= register processTypeId

  _ <- liftIO $ putMVar storage chain

  mainLoop newChain [] storage

connectToNetwork :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> ProcessId -> Process (BlockChain (Block a))
connectToNetwork chain pid = do
  liftIO $ putStrLn "Sending connection request"
  P2P.nsendCapable processTypeId (pid, GetBlocksMsg (lastBlockHash chain))
  return chain
  where
    lastBlockHash []      = Nothing
    lastBlockHash (x:_xs) = Just $ bInvItem $ _bHash x

mainLoop :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> [STx a] -> MVar (BlockChain (Block a)) -> Process ()
mainLoop chain txs storage = do
  liftIO $ putStrLn "START Putting Mvar"
  _ <- liftIO $ swapMVar storage chain
  liftIO $ putStrLn "FIN putting Mvar"
  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    GetBlocksMsg hash -> onGetBlocks sender hash chain txs storage
    InvMsg hashes     -> onInv sender hashes chain txs storage
    GetDataMsg hash   -> onGetData sender hash chain txs storage

onGetBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> Maybe InvItem -> BlockChain (Block a) -> [STx a] -> MVar (BlockChain (Block a)) -> Process ()
onGetBlocks sender msg chain txs storage = case msg of
  (Just (InvBlock, hash)) -> do
    liftIO $ putStrLn "Getblocks with initial received"
    sendBlockRange sender (Just hash) chain
    mainLoop chain txs storage
  (Just (InvTx, _)) -> do
    liftIO $ putStrLn "Getblocks with TX received, ignoring..."
    mainLoop chain txs storage
  Nothing -> do
    liftIO $ putStrLn "Getblocks for the first time received"
    sendBlockRange sender Nothing chain
    mainLoop chain txs storage

onInv :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> [InvItem] -> BlockChain (Block a) -> [STx a] -> MVar (BlockChain (Block a)) -> Process ()
onInv sender hashes chain txs storage = do
  liftIO $ putStrLn $ "Received InvMsg with hashes " ++ show hashes
  liftIO $ putStrLn "Going to add missing blocks"
  newChain <- addMissingBlocks sender blockHashes chain
  liftIO $ putStrLn "Going to add missing tx"
  _newTxs <- addMissingTxs sender txHashes chain txs
  liftIO $ putStrLn $ "New chain looks like " ++ show newChain
  mainLoop newChain txs storage
  where
    blockHashes = filter ((== InvBlock) . fst) hashes
    txHashes = filter ((== InvTx) . fst) hashes

onGetData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> BlockChain (Block a) -> [STx a] -> MVar (BlockChain (Block a)) -> Process ()
onGetData sender hash chain txs storage = do
  liftIO $ putStrLn $ "Received GetData with hash " ++ show hash
  sendBlockData sender hash chain
  mainLoop chain txs storage

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

addMissingTxs :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> [InvItem] -> BlockChain (Block a) -> [STx a] -> Process [STx a]
addMissingTxs pid invs chain txs =
  let
    invs' = map tInvItem $ filter (`notElem` map fst txs) (map invHash invs)
    getTxs = map (getTx pid) invs'
  in foldl (addToTxs (return chain)) (return txs) getTxs

addMissingBlocks :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> [InvItem] -> BlockChain (Block a) -> Process (BlockChain (Block a))
addMissingBlocks pid invs chain =
  let
    invs' = map bInvItem $ filter (`notElem` map _bHash chain) (map invHash invs)
    getBlocks = map (getBlock pid) invs'
  in foldl addToChain (return chain) getBlocks

addToTxs :: (Show a, Typeable a, Binary a, BContent a) => Process (BlockChain (Block a)) -> Process [STx a] -> Process (Maybe (STx a)) -> Process [STx a]
addToTxs chain txs txGetter = do
  self <- getSelfPid
  mtx <- txGetter
  c <- chain
  txs' <- txs
  case foldl addBlock' (Just c) (map snd txs') of
    Just _chain' -> case mtx of
      Just tx -> do
        P2P.nsendCapable processTypeId (self, InvMsg [tInvItem $ fst tx])
        return $ txs' ++ [tx]
      Nothing -> return txs'
    Nothing -> return txs'
  where
    addBlock' Nothing _   = Nothing
    addBlock' (Just xs) x = addBlock id x xs

addToChain :: (Show a, Typeable a, Binary a, BContent a) => Process (BlockChain (Block a)) -> Process (Maybe (Block a)) -> Process (BlockChain (Block a))
addToChain chain blockGetter = do
  self <- getSelfPid
  liftIO $ putStrLn "Getting block"
  mblock <- blockGetter
  c <- chain
  case mblock of
    Just block -> case addValidBlock block c of
                    Just nchain -> do
                      -- Cancel mining if needed, remove txs that were present in the block
                      liftIO $ putStrLn $ "Adding block" ++ show block
                      P2P.nsendCapable processTypeId (self, InvMsg [bInvItem $ _bHash block])
                      return nchain
                    Nothing     -> do
                      liftIO $ putStrLn $ "Can't add block" ++ show block
                      return c
    Nothing -> return c

getBlock :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> Process (Maybe (Block a))
getBlock sender inv = do
  self <- getSelfPid
  DP.send sender (self, GetDataMsg inv)
  (_sender, block) <- expect :: (Typeable a, Binary a, BContent a) => Process (ProcessId, Maybe (Block a))
  liftIO $ putStrLn ("received block " ++ show block)
  return block

getTx :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> Process (Maybe (STx a))
getTx sender inv = do
  self <- getSelfPid
  DP.send sender (self, GetDataMsg inv)
  (_sender, block) <- expect :: (Typeable a, Binary a, BContent a) => Process (ProcessId, Maybe (STx a))
  return block

sendTxData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> [STx a] -> Process ()
sendTxData pid (InvTx, h) txs = do
  self <- getSelfPid
  DP.send pid (self, find ((== h) . fst) txs)
sendTxData _ _ _ = return ()

sendBlockData :: (Show a, Typeable a, Binary a, BContent a) => ProcessId -> InvItem -> BlockChain (Block a) -> Process ()
sendBlockData pid (InvBlock, h) chain = do
  let blockData = find ((== h) . _bHash) chain
  self <- getSelfPid
  liftIO $ putStrLn $ "Going to send block data for hash" ++ show h ++ " | " ++ show blockData
  DP.send pid (self, blockData)
sendBlockData _ _ _ = return ()

bInvItem :: Hash -> InvItem
bInvItem = mkInvItem InvBlock

tInvItem :: Hash -> InvItem
tInvItem = mkInvItem InvTx
