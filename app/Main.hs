module Main where

import           BlockChain
import           Transaction

-- newtype SimpleBContent = SimpleBContent String deriving (Show)

-- instance BContent SimpleBContent where
--   serial = show

main :: IO ()
main = do
  let initialChain = mkInitialChain (Coinbase 10 "Jesus")
  let blockchain = addBlock (Transaction 3 "Jesus" "Sandra") <$>
                   addBlock (Transaction 5 "Jesus" "Sandra") initialChain
  mapM_ print blockchain
