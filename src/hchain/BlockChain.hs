{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.BlockChain (BlockChain, Hash, Block (..), content, BContent (..), isValidChain, addBlock, mkInitialChain) where

import qualified Data.ByteString.Lazy.Char8 as C8
import           Control.Lens
import           Data.Digest.Pure.SHA       (sha256, showDigest)
import           Data.String.Utils          (startswith)
import Data.Maybe (fromJust)

import           Data.Typeable
import           GHC.Generics

type Hash = String
type BNum = Int
type BNonce = Int
data Block a = Block { _num :: BNum, _nonce :: BNonce, _content :: a, _prevH :: Hash, _bHash :: Hash } deriving (Show, Generic, Typeable)
$(makeLenses ''Block)
type BlockChain a = [a]

class BContent a where
  serial :: a -> String
  mApply :: a -> [a] -> (a -> b) -> Maybe b

mkInitialChain :: BContent a => a -> BlockChain (Block a)
mkInitialChain c = [mine (mkInitialBlock c)]

mkInitialBlock :: BContent a => a -> Block a
mkInitialBlock c = Block 1 1 c emptyHash emptyHash
  where emptyHash = "0000000000000000000000000000000000000000000000000000000000000000"

isValidChain :: BContent a => BlockChain (Block a) -> Bool
isValidChain = all $ checkSignature . view bHash

addBlock :: BContent a => a -> BlockChain (Block a) -> Maybe (BlockChain (Block a))
addBlock c chain@(x:xs)
  | null contentBlock = Nothing
  | otherwise = Just $ mine (fromJust contentBlock) : chain
  where
    contentBlock = mApply c (reverse contents) cont
    cont = mkBlock (x ^. num + 1) (x ^. bHash)
    contents = map _content chain

mkBlock :: BContent a => BNum -> Hash -> a -> Block a
mkBlock n prevh c = Block n 1 c prevh ""

hash :: BContent a => Block a -> Hash
hash block = showDigest $ sha256 (C8.pack (serialize block))

serialize :: BContent a => Block a -> String
serialize block = show (block ^. num) ++ show (block ^. nonce) ++ serial (block ^. content) ++ block ^. prevH

checkSignature :: Hash -> Bool
checkSignature = startswith "0000"

mine :: BContent a => Block a -> Block a
mine block
  | checkSignature computedHash = block & bHash .~ computedHash
  | otherwise = mine (block & nonce +~ 1)
  where computedHash = hash block
