{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Digest.Pure.SHA       (sha256, showDigest)
import           Data.String.Utils          (startswith)
import           Lib

type Hash = String
type BNum = Int
type BNonce = Int
newtype SimpleBContent = SimpleBContent String deriving (Show)
data Block = Block { _num :: BNum, _nonce :: BNonce, _content :: SimpleBContent, _prevH :: Hash, _bHash :: Hash } deriving (Show)
$(makeLenses ''Block)
type BlockChain = [Block]

class BContent a where
  serial :: a -> String

instance BContent SimpleBContent where
  serial = show

main :: IO ()
main = do
  stuff <- Prelude.getLine
  let initialChain = mkInitialChain
  let blockchain = addBlock (SimpleBContent stuff) initialChain
  mapM_ print blockchain

mkInitialChain :: BlockChain
mkInitialChain = [mine mkInitialBlock]

mkInitialBlock :: Block
mkInitialBlock = Block 1 1 (SimpleBContent "") emptyHash emptyHash
  where emptyHash = "0000000000000000000000000000000000000000000000000000000000000000"

isValidChain :: BlockChain -> Bool
isValidChain = all $ checkSignature . view bHash

addBlock :: SimpleBContent -> BlockChain -> BlockChain
addBlock content chain@(x:xs) = nextBlock : chain
  where nextBlock = mine $ mkBlock (x ^. num + 1) content (x ^. bHash)

mkBlock :: BNum -> SimpleBContent -> Hash -> Block
mkBlock n content prevh = Block n 1 content prevh ""

hash :: Block -> Hash
hash block = showDigest $ sha256 (C8.pack (serialize block))

serialize :: Block -> String
serialize block = show (block ^. num) ++ show (block ^. nonce) ++ serial (block ^. content) ++ block ^. prevH

checkSignature :: Hash -> Bool
checkSignature = startswith "0000"

mine :: Block -> Block
mine block
  | checkSignature computedHash = block & bHash .~ computedHash
  | otherwise = mine (block & nonce +~ 1)
  where computedHash = hash block
