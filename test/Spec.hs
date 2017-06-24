import           Hchain.BlockChain
import           Hchain.Client
import           Hchain.Transaction
import           Network.Socket

import           Test.Hspec

import           Control.Concurrent.MVar

import           Control.Concurrent      (threadDelay)

main :: IO ()
main = hspec $ do
  describe "Hchain.BlockChain" $ do
    it "composes" $ do
      let initialChain = mkInitialChain (Coinbase 10 "Jesus")
      let blockchain = mineBlock (Transaction 5 "Jesus" "Sandra") initialChain >>= mineBlock (Transaction 3 "Jesus" "Sandra")

      blockchain `shouldBe` Just testChain

  describe "Hchain.Client" $ do
    it "distributes blocks" $ do
      let chain = testChain

      mvar4000 <- newEmptyMVar
      mvar4001 <- newEmptyMVar
      startNode "localhost" "4000" [] mvar4000
      threadDelay 2000000
      startEmptyNode "localhost" "4001" ["localhost:4000"] mvar4001

      threadDelay 2300000

      chain4000 <- readMVar mvar4000
      chain4001 <- readMVar mvar4001

      chain4000 `shouldBe` chain
      chain4001 `shouldBe` chain

    it "distributes transactions" $ do
      pending

testChain :: BlockChain (Block Transaction)
testChain = [Block {_num = 3,
                    _nonce = 12041,
                    _content = Transaction 3 "Jesus" "Sandra",
                    _prevH = "0000761b8bc0a56e7aeccde808ff1f3c5248b823e8006d9b5e59c61e2e0235a6",
                    _bHash = "0000a0fc102a90f666ab9bb07475ceece5e48b1b6d97222ade7e3feadce29c86"},
              Block {_num = 2,
                    _nonce = 5312,
                    _content = Transaction 5 "Jesus" "Sandra",
                    _prevH = "00008b6c26ab36b2a4a6ab64b78198bc42c5d99bbba638400e778049497b73b8",
                    _bHash = "0000761b8bc0a56e7aeccde808ff1f3c5248b823e8006d9b5e59c61e2e0235a6"},
              Block {_num = 1,
                    _nonce = 4538,
                    _content = Coinbase 10 "Jesus",
                    _prevH = "0000000000000000000000000000000000000000000000000000000000000000",
                    _bHash = "00008b6c26ab36b2a4a6ab64b78198bc42c5d99bbba638400e778049497b73b8"}]

startNode :: HostName -> ServiceName -> [String] -> MVar (BlockChain (Block Transaction)) -> IO ()
startNode host port seeds = start host port seeds testChain

startEmptyNode :: HostName -> ServiceName -> [String] -> MVar (BlockChain (Block Transaction)) -> IO ()
startEmptyNode host port seeds storage = do
  let initialChain = mkInitialChain (Coinbase 10 "Jesus")
  start host port seeds initialChain storage
