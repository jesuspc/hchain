module Main where

import           BlockChain
import           Transaction

newtype SimpleBContent = SimpleBContent String deriving (Show)

instance BContent SimpleBContent where
  serial = show

main :: IO ()
main = do
  stuff <- Prelude.getLine
  let initialChain = mkInitialChain (SimpleBContent "")
  let blockchain = addBlock (SimpleBContent stuff) initialChain
  mapM_ print blockchain
