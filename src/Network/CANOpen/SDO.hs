module Network.CANOpen.SDO where

import Network.CAN (MonadCAN(..), CANArbitrationField, CANMessage(..))
--import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.SDO.Types (SDORequest(..))
import Network.CANOpen.Types (NodeID(..), Mux(..))

import qualified Network.CAN
import qualified Network.CANOpen.Serialize

sdoClient
  :: MonadCAN m
  => NodeID
  -> m ()
sdoClient nID = do
  let
    sendReq = send . sdoRequest nID

  sendReq $ SDORequestUploadInit (Mux 1000 1)

  recv >>= send

sdoRequest
  :: NodeID
  -> SDORequest
  -> CANMessage
sdoRequest nID req =
  CANMessage
    (sdoRequestID nID)
    $ Network.CANOpen.Serialize.pad 8
    $ Network.CANOpen.Serialize.runPut req

sdoRequestID
  :: NodeID
  -> CANArbitrationField
sdoRequestID (NodeID i) =
  Network.CAN.standardID (0x600 + fromIntegral i)

sdoReplyID
  :: NodeID
  -> CANArbitrationField
sdoReplyID (NodeID i) =
  Network.CAN.standardID (0x580 + fromIntegral i)

