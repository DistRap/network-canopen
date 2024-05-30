module SDOSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Network.CAN
import Network.CANOpen.Types (NodeID(..), Mux(..))
import Network.CANOpen.SDO
--import Network.CANOpen.SDO.Types (SDORequest, SDOReply)

import Util (testBus, busResult)
--TestBusState(..))
  -- (TestBus(..), , runTestBus)

n1 :: NodeID
n1 = NodeID 1

spec :: Spec
spec = do
  describe "SDO" $ do
    it "Reads Word8" $
      (testBus
        [
          CANMessage
            (sdoReplyID n1)
            [0x4F, 0x13, 0x42, 0x1, 0x3, 0x7, 0x0, 0x0]
        ]
        $ sdoClientUpload n1 (Mux 0x4213 1)
      )
      `shouldBe`
      ( [0x3]
      , busResult
        [ CANMessage
            (sdoRequestID n1)
            [0x40, 0x13, 0x42, 0x1, 0, 0, 0, 0]
        ]
      )
