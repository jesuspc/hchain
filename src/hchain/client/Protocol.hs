{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.Client.Protocol where

import           Data.Binary
import           Data.Typeable
import           GHC.Generics

import           Hchain.BlockChain (Hash)

type TxIden = String
data InvType = InvBlock | InvTx deriving (Show, Generic, Typeable, Eq)
type InvItem = (InvType, Hash)
data ProtocolMsg = GetBlocksMsg (Maybe InvItem)
                 | InvMsg [InvItem]
                 | GetDataMsg InvItem
                 deriving (Show, Generic, Typeable)
type STx a = (TxIden, a)

instance Binary InvType
instance Binary ProtocolMsg

invType :: InvItem -> InvType
invType = fst

invHash :: InvItem -> Hash
invHash = snd

mkInvItem :: InvType -> Hash -> InvItem
mkInvItem t h = (t, h)
