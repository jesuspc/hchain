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

start :: (Show a, Typeable a, Binary a, BContent a) => HostName -> ServiceName -> [String] -> BlockChain (Block a) -> IO ()
start host port seeds chain = P2P.bootstrap host port (map P2P.makeNodeId seeds) initRemoteTable (mainProcess chain)

mainProcess :: (Show a, Typeable a, Binary a, BContent a) => BlockChain (Block a) -> Process ()
mainProcess chain = do
  loopPid <- spawnLocal $ Chain.spawnProcess chain
  _commandLinePid <- spawnLocal $ CommandLine.spawnProcess loopPid
  return ()
