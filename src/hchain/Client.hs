module Hchain.Client (start) where

import qualified Control.Distributed.Backend.P2P    as P2P
import           Control.Distributed.Process        as DP
import           Control.Distributed.Process.Node   as DPN

import           Data.Binary
import           Data.Typeable

import           Network.Socket

import           Hchain.BlockChain
import           Hchain.Client.Chain                as Chain
import           Hchain.Client.CommandLineInterface as CommandLine

import           Control.Concurrent.MVar

start :: (Show a, Typeable a, Binary a, BContent a) => HostName -> ServiceName -> [String] -> BlockChain (Block a) -> MVar (BlockChain (Block a)) -> IO ()
start host port seeds chain storage = P2P.bootstrap host port (map P2P.makeNodeId seeds) initRemoteTable mainProcess
  where
    mainProcess = do
      loopPid <- spawnLocal $ Chain.spawnProcess chain storage
      -- _commandLinePid <- spawnLocal $ CommandLine.spawnProcess loopPid
      return ()
