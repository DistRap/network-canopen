module SDOSpec (spec) where

import Test.Hspec (Spec, describe, it)

import Network.CAN
import Network.CANOpen.Types (NodeID(..), Mux(..))
import Network.CANOpen.SDO

import Util (okBusResult, withTestBus, shouldReturnInSim)

n1 :: NodeID
n1 = NodeID 1

spec :: Spec
spec = do
  describe "SDO" $ do
    it "Reads Word8" $
      (withTestBus
        [
          CANMessage
            (sdoReplyID n1)
            [0x4F, 0x13, 0x42, 0x1, 0x3, 0x7, 0x0, 0x0]
        ]
        $ \bus -> sdoClientUpload bus n1 (Mux 0x4213 1)

      )
      `shouldReturnInSim`
      ( [0x3]
      , okBusResult
        [ CANMessage
            (sdoRequestID n1)
            [0x40, 0x13, 0x42, 0x1, 0, 0, 0, 0]
        ]
      )
    it "Writes Word16" $
      (withTestBus
        [
          CANMessage
            (sdoReplyID n1)
            [0x60, 0x13, 0x42, 0x1]
        ]
        $ \bus -> sdoClientDownload bus n1 (Mux 0x4213 1) [0x13, 0x37]

      )
      `shouldReturnInSim`
      ( ()
      , okBusResult
        [ CANMessage
            (sdoRequestID n1)
            [0x2B, 0x13, 0x42, 0x1, 0x13, 0x37]
        ]
      )
