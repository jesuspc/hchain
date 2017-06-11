module Hchain.Client.Transaction (spawnProcess) where

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process      as DP
import           Control.Distributed.Process.Node as DPN

import           Hchain.Client.Protocol

spawnProcess :: ProcessId -> Process ()
spawnProcess chainProcess = do
  liftIO $ threadDelay 1000000
  mainLoop chainProcess

mainLoop :: ProcessId -> Process ()
mainLoop chainProcess = do
  (sender, msg) <- expect :: Process (ProcessId, ProtocolMsg)

  case msg of
    InvMsg invs    -> do
      onInv sender invs
      mainLoop chainProcess
    GetDataMsg inv -> do
      onGetData sender inv
      mainLoop chainProcess
    otherwise      -> mainLoop chainProcess

onInv :: ProcessId -> [InvItem] -> Process ()
onInv sender invs = undefined

onGetData :: ProcessId -> InvItem -> Process ()
onGetData sender inv = undefined
