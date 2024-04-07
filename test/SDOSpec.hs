module SDOSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Network.CAN
import Network.CANOpen.Types (NodeID(..))
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
    it "Works" $
      (testBus
        [
          CANMessage
            (sdoReplyID n1)
            [0x1, 0x2]
        ]
        $ sdoClient n1
      )
      `shouldBe`
      (()
      , busResult
        [ CANMessage
            (sdoRequestID n1)
            [0x40, 0xE8, 0x3, 0x1, 0, 0, 0, 0]
        , CANMessage
            (sdoReplyID n1)
            [0x1, 0x2]
        ]
      )
