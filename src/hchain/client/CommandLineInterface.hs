module Hchain.Client.CommandLineInterface (spawnProcess) where

import           Control.Distributed.Process as DP

import           Control.Monad

spawnProcess :: ProcessId -> Process ()
spawnProcess chainPid = forever $ do
  cmd <- liftIO getLine
  case words cmd of
    ["tx", from, to, amount] -> sendTx chainPid from to (read amount :: Int)
    _                        -> liftIO . putStrLn $ "tx <from> <to> <amount>"

sendTx :: ProcessId -> String -> String -> Int -> Process ()
sendTx listenerPid from to amount = undefined --do
  -- self <- getSelfPid
  -- DP.send listenerPid, (self, )
