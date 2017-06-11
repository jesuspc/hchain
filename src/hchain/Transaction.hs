{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Hchain.Transaction (foldChain, Transaction (..)) where

import           Control.Lens
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (fromJust)
import           Hchain.BlockChain (BContent (..), Block (..), BlockChain,
                                    content)

import           Data.Binary
import           Data.Typeable
import           GHC.Generics

type State = Map.Map Actor TokenAmount
type Actor = String
type Recipient = Actor
type Sender = Actor
type TokenAmount = Int
data Transaction = Coinbase TokenAmount Recipient
                 | Transaction TokenAmount Sender Recipient
                 deriving (Show, Generic, Typeable)

instance BContent Transaction where
  serial (Coinbase n r)      = "Coinbase " ++ show n ++ show r
  serial (Transaction n r s) = "Tx " ++ show n ++ show r ++ show s

  mApply tx txs cont = foldTxs txs >>= applyTransaction tx >> (Just $ cont tx)

instance BContent [Transaction] where
  serial = concatMap serial
  mApply xs xys cont = foldTxs (concat xys ++ xs) >> (Just $ cont xs)

instance Binary Transaction

initialState :: State
initialState = Map.empty

foldChain :: BlockChain (Block Transaction) -> State
foldChain chain = foldChain' reversedChain initialState
  where
    reversedChain = reverse chain
    foldChain' xs state = foldl
        (\ state' x -> fromJust (applyTransaction (x ^. content) state')) state xs

foldTxs :: [Transaction] -> Maybe State
foldTxs = foldTxs' (Just initialState)
  where
    foldTxs' = foldl applyTx
    applyTx Nothing _       = Nothing
    applyTx (Just state) tx = applyTransaction tx state

applyTransaction :: Transaction -> State -> Maybe State
applyTransaction tx s = case tx of
                          (Coinbase n r) -> Just (s & at r %~ increaseValue n)
                          (Transaction n se r) -> applyTransaction' n se r
  where
    amount Nothing  = 0
    amount (Just v) = v
    increaseValue v oldV = Just (amount oldV + v)
    decreaseValue v oldV = Just (amount oldV - v)
    applyTransaction' n' se' r'
      | amount (finalS ^. at se') < 0 = Nothing
      | otherwise = Just finalS
      where
        finalS = (s & at r' %~ increaseValue n') & at se' %~ decreaseValue n'
