module Main where

import           Data.Maybe
import           Hchain.BlockChain
import           Hchain.Client
import           Hchain.Transaction
import           Network.Socket

-- newtype SimpleBContent = SimpleBContent String deriving (Show)

-- instance BContent SimpleBContent where
--   serial = show

main :: IO ()
main = do
  let initialChain = mkInitialChain (Coinbase 10 "Jesus")
  let blockchain = mineBlock (Transaction 3 "Jesus" "Sandra") <$>
                   mineBlock (Transaction 5 "Jesus" "Sandra") initialChain
  mapM_ print blockchain

startNode :: HostName -> ServiceName -> [String] -> IO ()
startNode host port seeds = do
  let initialChain = mkInitialChain (Coinbase 10 "Jesus")
  let blockchain = mineBlock (Transaction 3 "Jesus" "Sandra") <$>
                   mineBlock (Transaction 5 "Jesus" "Sandra") initialChain
  start host port seeds (fromJust $ fromJust blockchain)

startEmptyNode :: HostName -> ServiceName -> [String] -> IO ()
startEmptyNode host port seeds = do
  let initialChain = mkInitialChain (Coinbase 10 "Jesus")
  start host port seeds initialChain
