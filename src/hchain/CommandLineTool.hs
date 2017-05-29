module Hchain.CommandLineTool (initCommandLine) where

import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      as DP
import           Control.Distributed.Process.Node as DPN

import           Control.Monad

initCommandLine :: ProcessId -> Process ()
initCommandLine listenerPid = forever $ do
  cmd <- liftIO getLine
  case words cmd of
    ["tx", from, to, amount] -> sendTx listenerPid from to (read amount :: Int)
    _                        -> liftIO . putStrLn $ "tx <from> <to> <amount>"

sendTx :: ProcessId -> String -> String -> Int -> Process ()
sendTx listenerPid from to amount = undefined --do
  -- self <- getSelfPid
  -- DP.send listenerPid, (self, )
