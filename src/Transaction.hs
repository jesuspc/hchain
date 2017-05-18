module Transaction where

import           BlockChain      (BContent (..))
import           Control.Lens
import           Data.Map.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

type State = Map.Map Actor TokenAmount
type Actor = String
type Recipient = Actor
type Sender = Actor
type TokenAmount = Int
data Transaction = Coinbase TokenAmount Recipient | Transaction TokenAmount Sender Recipient deriving (Show)

instance BContent Transaction where
  serial (Coinbase n r)      = "Coinbase " ++ show n ++ show r
  serial (Transaction n r s) = "Tx " ++ show n ++ show r ++ show s

initialState :: State
initialState = Map.insert "foo" 1 Map.empty

applyTransaction :: Transaction -> State -> (Maybe Transaction, State)
applyTransaction tx s = case tx of
                          (Coinbase n r) -> (Just tx, s & at r %~ increaseValue n)
                          (Transaction n se r) -> applyTransaction' n se r
  where
    amount Nothing  = 0
    amount (Just v) = v
    increaseValue v oldV = Just (amount oldV + v)
    decreaseValue v oldV = Just (amount oldV - v)
    applyTransaction' n' se' r'
      | amount (finalS ^. at se') < 0 = (Nothing, s)
      | otherwise = (Just tx, finalS)
      where
        finalS = (s & at r' %~ increaseValue n') & at se' %~ decreaseValue n'
