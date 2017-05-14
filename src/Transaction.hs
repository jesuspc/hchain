module Transaction where

import           BlockChain      (BContent (..))
import qualified Data.Map.Strict as Map

type State = Map.Map Actor TokenAmount
type Actor = String
type Recipient = Actor
type Sender = Actor
type TokenAmount = Int
data Transaction = Coinbase TokenAmount Recipient | Transaction TokenAmount Recipient Sender deriving (Show)

instance BContent Transaction where
  serial (Coinbase n r)      = "Coinbase " ++ show n ++ show r
  serial (Transaction n r s) = "Tx " ++ show n ++ show r ++ show s

applyTransaction :: Transaction -> State -> (Maybe [Transaction], State)
applyTransaction = undefined
