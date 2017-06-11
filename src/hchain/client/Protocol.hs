{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.Client.Protocol where

import           Data.Binary
import           Data.Typeable
import           GHC.Generics

import           Hchain.BlockChain (Hash)

type InvType = String
type InvItem = (InvType, Hash)
data ProtocolMsg = GetBlocksMsg (Maybe InvItem)
                 | InvMsg [InvItem]
                 | GetDataMsg InvItem
                 deriving (Show, Generic, Typeable)

instance Binary ProtocolMsg

invType :: InvItem -> InvType
invType = fst

invHash :: InvItem -> Hash
invHash = snd

mkInvItem :: InvType -> Hash -> InvItem
mkInvItem t h = (t, h)
